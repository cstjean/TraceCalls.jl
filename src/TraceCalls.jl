__precompile__()
module TraceCalls

using MacroTools
using Base.Test: @inferred
using ClobberingReload
using ClobberingReload: run_code_in, module_code, RevertibleCodeUpdate, only,
    is_function_definition, get_function, is_call_definition, EmptyRevertibleCodeUpdate,
    is_fancy_constructor_definition, ModDict, RelocatableExpr, MakeRelocatableExpr
using ClobberingReload: combinedef, combinearg, longdef1, splitdef, splitarg
using DataStructures: OrderedDict
using Memoize
using Base: url

export @traceable, @trace, Trace, prune, FontColor, Bold,
    is_inferred, map_is_inferred, redgreen, greenred, @trace_inferred,
    compare_past_trace, filter_func, apply_macro, @stacktrace, measure

""" When `TraceCalls.active[]` is `false`, `@traceable ...` is an identity macro
(it doesn't modify the function at all) """
const active = fill(true)

const revertible_definitions = RevertibleCodeUpdate[]
const traceable_definitions = OrderedDict()  # We use an OrderedDict because in case we
                                             # accidentally store the same definition
                                             # twice, at least the latter one takes
                                             # precedence.

################################################################################
# Utils

macro ignore_errors(error_rval, expr)
    quote
        try
            $(esc(expr))
        catch e
            $(esc(error_rval))
        end
    end
end


################################################################################

""" A `Trace` object represents a function call. It has fields `func, args, kwargs,
called, value`, with `func(args...; kwargs...) = value`. `called::Vector{Trace}` of the
immediate traceable function calls that happened during the execution of this trace.

There are no accessors for `Trace`; please reference its fields directly.
For instance, `filter(tr->isa(tr.args[1], Int), trace)` will select all calls whose
first argument is an `Int`. """
type Trace
    func             # the function/callable called
    args::Tuple      # the positional arguments
    kwargs::Tuple    # the keyword arguments
    called::Vector{Trace}  
    value       # This is the return value of the func(args...) call, but it's also where
                # the result of `map(f, ::Trace)` will be stored.
end

function Base.copy!(dest::Trace, src::Trace)
    dest.func = src.func
    dest.args = src.args
    dest.kwargs = src.kwargs
    dest.called = src.called
    dest.value = src.value
end

tree_size(tr::Trace) = 1 + mapreduce(tree_size, +, 0, tr.called)
Base.copy(tr::Trace) = Trace(tr.func, tr.args, tr.kwargs, tr.called, tr.value)
immutable NotReturned end   # special flag value
Base.push!(tr::Trace, sub_trace::Trace) = push!(tr.called, sub_trace)
Base.getindex(tr::Trace, i::Int) = tr.called[i]
Base.getindex(tr::Trace, i::Int, j::Int, args...) = tr.called[i][j, args...]
Base.length(tr::Trace) = length(tr.called)
Base.url(tr::Trace) = @ignore_errors "" Base.url(which(tr))
Base.which(tr::Trace) = apply_macro(:@which, tr)
Base.less(tr::Trace) = apply_macro(:@less, tr)
Base.edit(tr::Trace) = apply_macro(:@edit, tr)
# I've disabled iteration because it doesn't align with our desired `Base.map`'s
# behaviour, and it's kinda useless anyway.
# Base.start(tr::Trace) = 1
# Base.next(tr::Trace, i::Int) = (tr[i], i+1)
# Base.done(tr::Trace, i::Int) = i == length(tr)+1
(tr::Trace)() = tr.func(tr.args...; tr.kwargs...)
apply_macro(mac::Symbol, tr::Trace, mod::Module=Main) =
    eval(mod, Expr(:macrocall, mac,
                   (isempty(tr.kwargs) ?
                    # Needs special casing because @inferred chokes on kwargs-less
                    # funcalls otherwise.
                    :($(tr.func)($(map(QuoteNode, tr.args)...))) :
                    :($(tr.func)($(map(QuoteNode, tr.args)...); $(tr.kwargs...))))))
apply_macro(mac::Expr, tr::Trace, mod::Module=Main) =
    (mac.head==:macrocall ? apply_macro(only(mac.args), tr, mod) :
     error("Unable to call macro $mac"))

""" `map(f, tr::Trace)` recursively applies the function f to each `Trace` in `tr`,
and stores the result in `Trace.value` """
Base.map(f, tr::Trace) = Trace(tr.func, tr.args, tr.kwargs,
                               [map(f, c) for c in tr.called], f(tr))
Base.map(mac::Union{Symbol, Expr}, tr::Trace) = map(sub->apply_macro(mac, sub), tr)

top_level_dummy() = error("top_level_dummy is not callable")

const is_tracing = fill(false)
top_trace(fun) = Trace(fun, (), (), [], nothing)
const trace_data = top_trace(top_level_dummy)
const current_trace = fill(trace_data)

is_traceable(def) =
    (is_function_definition(def) && (di=splitdef(def); !is_call_definition(di)) &&
     !is_fancy_constructor_definition(di))

""" `@traceable_loose(expr)` is like `traceable(expr)`, but doesn't error on non-function
definitions (it's a helper for `@traceable begin ... end`) """
macro traceable_loose(expr)
    is_traceable(expr) ? esc(:($TraceCalls.@traceable $expr)) : esc(expr)
end

""" Turns `::Int=5` into `some_gensym::Int=5` """
function handle_missing_arg(arg)
    arg_name, arg_type, is_splat, default = splitarg(arg)
    combinearg(arg_name === nothing ? gensym() : arg_name, arg_type, is_splat, default)
end

"""  Takes a function definition, and returns a traceing version of it. """
function tracing_code(fdef::Expr)::Expr
    arg_name(arg) = splitarg(arg)[1]
    is_splat(arg) = splitarg(arg)[3]
    arg_name_splat(arg) = is_splat(arg) ? Expr(:..., arg_name(arg)) : arg_name(arg)
    function typed_arg(arg)
        name, arg_type = splitarg(arg)
        if is_splat(arg) arg_type = Any end
        return :($name::$arg_type)
    end
    
    di = splitdef(fdef)
    di[:args] = map(handle_missing_arg, di[:args])
    di[:kwargs] = map(handle_missing_arg, di[:kwargs])
    fname = di[:name]

    do_body_di = copy(di)
    do_body_di[:name] = do_body = isa(fname, Expr) ? gensym() : gensym(fname)

    updated_fun_di = copy(di)
    @gensym new_trace prev_trace e res
    passed_args = map(arg_name_splat, di[:args])
    passed_kwargs = [is_splat(kwa) ?
                     arg_name_splat(kwa) :
                     :(($(Expr(:quote, arg_name(kwa))), $(arg_name(kwa))))
                     for kwa in di[:kwargs]]
    updated_fun_di[:body] = quote
        $prev_trace = $TraceCalls.current_trace[]
        $new_trace =
            $TraceCalls.Trace($fname, ($(passed_args...),),
                              ($(passed_kwargs...),),
                              [], $TraceCalls.NotReturned())
        $TraceCalls.current_trace[] = $new_trace
        push!($prev_trace, $new_trace)
        try
            $res = $do_body($(map(arg_name_splat, di[:args])...);
                            $(passed_kwargs...))
            $new_trace.value = $res
            return $res
        catch $e
            $new_trace.value = $e
            rethrow()
        finally
            $TraceCalls.current_trace[] = $prev_trace
        end
    end
    quote
        @inline $(combinedef(do_body_di))
        $(combinedef(updated_fun_di))
    end
end

""" `@traceable function foo(...) ... end` marks a function definition for the `@trace`
macro. See the README for details. """
macro traceable(fdef::Expr)
    if !active[] return esc(fdef) end
    @assert !is_call_definition(fdef) "@traceable cannot handle callable object definitions"

    if fdef.head == :block
        # TODO: just process it right away... It will be faster
        return esc(quote
            $([:($TraceCalls.@traceable_loose $expr) for expr in fdef.args]...)
        end)
    end

    file = (@__FILE__) == "" ? nothing : (@__FILE__)
    mod = current_module()
    di = splitdef(fdef)
    signature = (module_name(mod),         # loose approximation of the real signature
                 :($(di[:name])($([splitarg(arg)[2] for arg in di[:args]]...))))
    
    traceable_definitions[signature] =
          RevertibleCodeUpdate(tracing_code,
                               CodeUpdate(ModDict(mod=>Set([MakeRelocatableExpr(fdef)]))))

    return esc(fdef)
end

# There might be an issue with the memo if we decide to macroexpand the code, since the
# macroexpansion depends on the environment. It's probably a negligible issue.
@memoize Dict{Tuple{Expr}, Any} function traceable_update_handle_expr(expr0::Expr)
    expr = ClobberingReload.strip_docstring(expr0)
    is_traceable(expr) ? tracing_code(expr) : nothing
end
traceable_update_handle_expr(::Any) = nothing
clear_handle_expr_memo!() =
    empty!(eval(TraceCalls, Symbol("##traceable_update_handle_expr_memoized_cache")))

""" `traceable_update(obj)::RevertibleCodeUpdate` produces the traceable code for the
given object. """
function traceable_update end

custom_when_missing(x) = warn(x)
custom_when_missing(fail::ClobberingReload.UpdateInteractiveFailure) =
    warn("Use `@traceable` to trace methods defined interactively.")
traceable_update(obj::Union{Module, String}) =
    update_code_revertible(traceable_update_handle_expr, obj)
traceable_update(f::Union{Function, Type}) =
    update_code_revertible(traceable_update_handle_expr, f, when_missing=custom_when_missing)

traceable_update(tup::Tuple) = merge(map(traceable_update, tup)...)
traceable_update(tup::Tuple{}) = EmptyRevertibleCodeUpdate()

""" The `RevertibleCodeUpdate` for the code from the `@traceable` macros. """
traceable_macro_update() = merge(EmptyRevertibleCodeUpdate(),
                                 values(traceable_definitions)...)

function with_tracing_definitions(body::Function, obj)
    upd = merge(traceable_update(obj), traceable_macro_update())
    upd() do
        body()
    end
end

""" `recording_trace(fun::Function)` sets up a fresh Trace in trace_data, then executes
`fun` and returns the final Trace object """
function recording_trace(fun::Function)
    copy!(trace_data, top_trace(fun))
    try
        trace_data.value = fun()
    catch e
        trace_data.value = e
    end
    res = copy(trace_data)
    copy!(trace_data, top_trace(fun)) # don't hang on to that memory unnecessarily
    res
end    

function tracing(body::Function, to_trace=())
    with_tracing_definitions(to_trace) do
        # To debug, just use `with_tracing_definitions()` interactively
        recording_trace(body)
    end
end

macro trace(expr)
    :($TraceCalls.tracing(()->$(esc(expr))))
end

macro trace(to_trace, expr)
    :($TraceCalls.tracing(()->$(esc(expr)), $(esc(to_trace))))
end

################################################################################

const tab_def = """<style type="text/css">
<!--
.tab { margin-left: 40px; }
-->
</style>"""

immutable FontColor
    color
    content
end
immutable Bold
    content
end
return_val_html(x) =  val_html(Bold(FontColor("green", x)))
return_val_html(x::Exception) = val_html(FontColor("red", x))
""" `TraceCalls.val_html(x)` is the HTML used by `TraceCalls` to display each value
(arguments and return values). Customize it by overloading it. Defaults to `string(x)`.
"""
val_html(x) = repr(x)
val_html(x::FontColor) = """<font color=$(x.color)>""" * val_html(x.content) * """</font>"""
val_html(x::Bold) = "<b>" * val_html(x.content) * "</b>"

function Base.show(io::IO, ::MIME"text/html", tr::Trace)
    write(io, call_html(tr.func, tr))
    write(io, """<ul>""")
    for called in tr.called
        write(io, "<li>")
        show(io, MIME"text/html"(), called)
        write(io, "</li>")
    end
    write(io, "</ul>")
end

kwa_eql(kwarg::Tuple) = "$(first(kwarg))=$(val_html(last(kwarg)))"
kwargs_html(kwargs) = "; " * join(map(kwa_eql, kwargs), ", ")
args_html(kwargs) = join(map(val_html, kwargs), ", ")
kwargs_html(kwargs::Tuple{}) = ""
sub_called_html(tr::Trace) =
     """<ul>""" * mapreduce(x->"<li>"*trace_html(x)*"</li>", *, "", tr.called) * "</ul>"

url_func_name(tr::Trace) =
    (url(tr) == "" ? string(tr.func) :
     """<a href="$(url(tr))" target="_blank" style="color: black; text-decoration: none; border-bottom: 1px #C3C3C3 dotted">$(tr.func)</a>""")

call_html(::Any, tr::Trace) =
    # Could use CSS https://www.computerhope.com/issues/ch001034.htm
    "<pre>$(url_func_name(tr))($(args_html(tr.args))$(kwargs_html(tr.kwargs))) => $(return_val_html(tr.value))</pre>"

trace_html(tr::Trace) = call_html(tr.func, tr) * sub_called_html(tr)

""" `filter_cutting(f::Function, tr::Trace)` filters all subtraces (and their
callees) for which `f(tr)` is false. """
filter_cutting(f::Function, tr::Trace) =
    Trace(tr.func, tr.args, tr.kwargs,
          [filter_cutting(f, sub_tr) for sub_tr in tr.called if f(sub_tr)],
          tr.value)

filter_descendents(f, tr) = # helper
    # Special casing because of #18852
    isempty(tr.called) ? [] : [t for sub in tr.called for t in filter_(f, sub)]
filter_(f, tr) =
    f(tr) ? [Trace(tr.func, tr.args, tr.kwargs, filter_descendents(f, tr),
                   tr.value)] : filter_descendents(f, tr)
Base.filter(f::Function, tr::Trace) =
    Trace(tr.func, tr.args, tr.kwargs, filter_descendents(f, tr), tr.value)

""" `filter_func(functions::Vector, tr::Trace)` keeps only Trace objects whose function
is one of `functions` """
filter_func(functions::Vector, tr::Trace) = filter(tr->tr.func in functions, tr)
filter_func(func::Function, tr::Trace) = filter_func([func], tr)

""" `collect(tr::Trace)` returns a vector of all `Trace` objects within `tr`. """
Base.collect(tr::Trace) = Trace[tr; mapreduce(collect, vcat, [], tr)]

""" `prune(tr::Trace, max_depth::Int, max_length::Int=1000000000)` prunes the Trace-tree
maximum tree depth, and maximum length (number of branches in each node). 
(convenient to first explore a trace at a high-level) """
prune(tr::Trace, max_depth::Int, max_length::Int=1000000000) =
    Trace(tr.func, tr.args, tr.kwargs,
          [prune(sub_tr, max_depth-1, max_length)
           for sub_tr in tr.called[1:min(length(tr), max_length)] if max_depth > 0],
          tr.value)

function is_inferred(tr::Trace)
    try
        apply_macro(:@inferred, tr, Base.Test)
        return true
    catch e
        if isa(e, ErrorException)&&contains(e.msg, "does not match inferred return type")
            return false
        else
            rethrow()
        end
    end
end
map_is_inferred(tr::Trace) = redgreen(map(is_inferred, tr))
""" `@trace_inferred ...some_expression...` computes the trace of `some_expression`,
and shows `true` for all type-stable function calls in the trace, and `false` otherwise.
"""
macro trace_inferred(expr)
    esc(:($TraceCalls.map_is_inferred($TraceCalls.@trace $expr)))
end

function redgreen(x::Number)
    # red is ff0000, of course...
    hex(frac) = round(UInt8, frac*255)
    f = clamp(x, 0.0, 1.0) * 1.0
    return bytes2hex([hex(1-f), hex(f), UInt8(0)])
end
redgreen(x::Bool) = x ? "green" : "red"

""" `redgreen(tr::Trace)` colors all `value`s as shades of red/green, with
0/false being pure red and 1/true being pure green. """
redgreen(tr::Trace) =
    map(sub->FontColor(redgreen(sub.value), sub.value), tr)
""" `greenred(tr::Trace)` is like `redgreen`, but with 0/false=green, 1/true=red. """
greenred(tr::Trace) =
    map(sub->FontColor(redgreen(1-sub.value), sub.value), tr)

Base.maximum(tr::Trace) = maximum(sub.value for sub in collect(tr))
Base.round(tr::Trace, n::Int) = map(sub->round(sub.value, n), tr)
Base.normalize(tr::Trace, div=tr.value) =
    map(sub->sub.value / div, tr)

measure(mac_or_fun::Union{Expr, Function}, tr::Trace) =
    greenred(round(normalize(map(mac_or_fun, tr)), 4))


only_exceptions(trace::Trace) = filter(tr->tr.value isa Exception, trace)
macro stacktrace(to_trace, expr)
    esc(:($TraceCalls.only_exceptions(@trace($to_trace, $expr))))
end

macro stacktrace(expr)
    esc(:($TraceCalls.@stacktrace $expr))
end

################################################################################

immutable IsEqual
    a
    b
end

iseql(isd::IsEqual) = iseql(isd.a, isd.b)
iseql(a::Nullable, b::Nullable) = isequal(a, b)
iseql(a, b) = a == b
val_html(isd::IsEqual) =
    val_html(FontColor(redgreen(iseql(isd)),
                       iseql(isd) ? string("Same", val_html(isd.a)) :
                       "<u>before</u>: $(val_html(isd.a)) <u>vs. now:</u> $(val_html(isd.b))"))


""" `compare_past_trace(old_trace::Trace; filter_out_equal=true))` reruns every subtrace
in `old_trace`, and shows in red where the new result differs from the old.
If `filter_out_equal==true`, only show the non-equal results. """
function compare_past_trace(old_trace::Trace; filter_out_equal=true)
    tr2 = map(old_trace) do sub_tr
        IsEqual(sub_tr.value, sub_tr()) end
    return filter_out_equal ? filter(subtr->!iseql(subtr.value), tr2) : tr2
end

end # module
