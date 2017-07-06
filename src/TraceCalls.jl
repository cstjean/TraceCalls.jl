__precompile__()
module TraceCalls

using MacroTools
using MacroTools: combinedef, combinearg, longdef1
using Base.Test: @inferred
using ClobberingReload
using ClobberingReload: run_code_in, module_code, RevertibleCodeUpdate
using DataStructures: OrderedDict
using Memoize

export @traceable, @trace, Trace, limit_depth, FontColor, Bold,
    is_inferred, map_is_inferred, redgreen, greenred, @trace_inferred,
    compare_past_trace, traceable!, filter_func, call_macro

""" When `TraceCalls.active[]` is `false`, `@traceable ...` is an identity macro
(it doesn't modify the function at all) """
const active = fill(true)

const revertible_definitions = RevertibleCodeUpdate[]
const traceable_definitions = OrderedDict()  # We use an OrderedDict because in case we
                                             # accidentally store the same definition
                                             # twice, at least the latter one takes
                                             # precedence.

""" A `Trace` object represents a function call. It has fields `func, args, kwargs,
called, value`, with `func(args...; kwargs...) = value`. `called::Vector{Trace}` of the
immediate traceable function calls that happened during the execution of this trace.

There are no accessors for `Trace`; please reference its fields directly.
For instance, `filter(tr->isa(tr.args[1], Int), trace)` will select all calls whose
first argument is an `Int`. """
mutable struct Trace
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

Base.copy(tr::Trace) = Trace(tr.func, tr.args, tr.kwargs, tr.called, tr.value)
struct NotReturned end   # special flag value
Base.push!(tr::Trace, sub_trace::Trace) = push!(tr.called, sub_trace)
Base.getindex(tr::Trace, i::Int) = tr.called[i]
Base.getindex(tr::Trace, i::Int, j::Int, args...) = tr.called[i][j, args...]
Base.length(tr::Trace) = length(tr.called)
# We've disabled iteration because it doesn't align with our desired `Base.map`'s
# behaviour, and it's kinda useless anyway.
# Base.start(tr::Trace) = 1
# Base.next(tr::Trace, i::Int) = (tr[i], i+1)
# Base.done(tr::Trace, i::Int) = i == length(tr)+1
(tr::Trace)() = tr.func(tr.args...; tr.kwargs...)
call_macro(mac::Symbol, tr::Trace, mod::Module=Main) =
    eval(mod, Expr(:macrocall, mac, (isempty(tr.kwargs) ?
                                     # Needs special casing because @inferred chokes
                                     # on kwargs-less funcalls otherwise.
                                     :($(tr.func)($(tr.args...))) :
                                     :($(tr.func)($(tr.args...); $(tr.kwargs))))))
call_macro(mac::Expr, tr::Trace, mod::Module=Main) =
    (mac.head==:macrocall ? call_macro(only(mac.args), tr, mod) :
     error("Unable to call macro $mac"))

""" `map(f, tr::Trace)` recursively applies the function f to each `Trace` in `tr`,
and stores the result in `Trace.value` """
Base.map(f, tr::Trace) = Trace(tr.func, tr.args, tr.kwargs,
                               [map(f, c) for c in tr.called], f(tr))
Base.map(mac::Union{Symbol, Expr}, tr::Trace) = map(sub->call_macro(mac, sub), tr)

top_level_dummy() = error("top_level_dummy is not callable")

const is_tracing = fill(false)
top_trace(fun) = Trace(fun, (), (), [], nothing)
const trace_data = top_trace(top_level_dummy)
const current_trace = fill(trace_data)

is_call_definition(fundef) = @capture(splitdef(fundef)[:name], (a_::b_) | (::b_))
is_function_definition(expr::Expr) = longdef1(expr).head == :function
is_function_definition(::Any) = false

is_traceable(def) = is_function_definition(def) && !is_call_definition(def)

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

function tracing_code(fdef::Expr)::Expr # returns the tracing function definitions
    arg_name(arg) = splitarg(arg)[1]
    function typed_arg(arg)
        global uu = fdef
        global vv = arg
        name, arg_type, is_splat = splitarg(arg)
        if is_splat arg_type = Any end
        return :($name::$arg_type)
    end
    
    di = splitdef(fdef)
    di[:args] = map(handle_missing_arg, di[:args])
    di[:kwargs] = map(handle_missing_arg, di[:kwargs])
    fname = di[:name]

    do_body_di = copy(di)
    do_body_di[:name] = do_body = fname isa Expr ? gensym() : gensym(fname)
    do_body_di[:args] = map(typed_arg, [di[:args]..., di[:kwargs]...])
    do_body_di[:kwargs] = []

    updated_fun_di = copy(di)
    @gensym new_trace prev_trace e res
    all_args_symbol = map(arg_name, [di[:args]..., di[:kwargs]...])
    updated_fun_di[:body] = quote
        $prev_trace = $TraceCalls.current_trace[]
        $new_trace =
            $TraceCalls.Trace($fname, ($(map(arg_name, di[:args])...),),
                              ($([:($(Expr(:quote, arg_name(kwa)))=>$(arg_name(kwa)))
                                  for kwa in di[:kwargs]]...),),
                              [], $TraceCalls.NotReturned())
        $TraceCalls.current_trace[] = $new_trace
        push!($prev_trace, $new_trace)
        try
            $res = $do_body($(all_args_symbol...))
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
          RevertibleCodeUpdate(CodeUpdate([EvalableCode(tracing_code(fdef), mod, file)]),
                               CodeUpdate([EvalableCode(fdef, mod, file)]))

    return esc(fdef)
end

# There might be an issue here if we decide to macroexpand the code, since the
# macroexpansion depends on the environment. It's probably a negligible issue.
@memoize Dict{Tuple{Expr, Function}, Any} traceable_update_handle_expr(expr::Expr,
                                                                   is_trace_fn=_->true) =
    (is_traceable(expr) && is_trace_fn(expr)) ? tracing_code(expr) : nothing
traceable_update_handle_expr(::Any) = nothing
clear_handle_expr_memo!() =
    empty!(eval(TraceCalls, Symbol("##traceable_update_handle_expr_memoized_cache")))

""" `get_function(mod::Module, fundef::Expr)::Function` returns the `Function` which this
`fundef` is defining. This code works purely because we're only using it in a clobbering
context (i.e. the Function already exists)"""
get_function(mod::Module, fundef::Expr)::Function = eval(mod, splitdef(fundef)[:name])

traceable_update(mod::Module) =
    update_code_revertible_fn(traceable_update_handle_expr, mod)
function traceable_update(mod::Module, file::String, functions::Set{Function})
    is_trace(expr) = get_function(mod, expr) in functions
    update_code_revertible_fn(expr->traceable_update_handle_expr(expr, is_trace), mod,
                              file)
end

""" `traceable!(module_name)` makes every[1] function in `module_name` traceable.

[1] Certain conditions apply. Use `@traceable` on individual functions for
more consistent results.
"""
function traceable!(mod::Module)
    push!(revertible_definitions, traceable_update(mod))
    nothing
end

# TODO: rename
trace!() = (#foreach(apply_code!, revertible_definitions);
            foreach(apply_code!, values(traceable_definitions)))
untrace!() = (#foreach(revert_code!, revertible_definitions);
              foreach(revert_code!, values(traceable_definitions)))

function with_tracing_definitions(body::Function, ::Tuple{})
    trace!()
    try
        body()
    finally
        untrace!()
    end
end    

with_tracing_definitions(body::Function, tup::Tuple) =
    with_tracing_definitions(tup[1]) do
        with_tracing_definitions(tup[2:end]) do
            body()
        end
    end

function with_tracing_definitions(body::Function, mod::Module)
    upd = traceable_update(mod)
    apply_code!(upd)
    try
        body()
    finally
        revert_code!(upd)
    end
end

""" `recording_trace(fun::Function)` sets up a fresh Trace in trace_data, then executes
`fun` and returns the final Trace object """
function recording_trace(fun::Function)
    copy!(trace_data, top_trace(fun))
    fun()
    res = copy(trace_data)
    copy!(trace_data, top_trace(fun)) # don't hang on to that memory unnecessarily
    res
end    

function tracing(body::Function, to_trace=())
    with_tracing_definitions(to_trace) do
        # To debug, just use `with_tracing_definitions()` interactively
        recording_trace() do
            try
                trace_data.value = @eval $body() # necessary to @eval because of world age
            catch e
                trace_data.value = e
            end
        end
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

struct FontColor
    color
    content
end
struct Bold
    content
end
return_val_html(x) =  val_html(Bold(FontColor("green", x)))
return_val_html(x::Exception) = val_html(FontColor("red", x))
""" `TraceCalls.val_html(x)` is the HTML used by `TraceCalls` to display each value
(arguments and return values). Customize it by overloading it. Defaults to `string(x)`.
"""
val_html(x) = string(x)
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

kwa_eql(kwarg::Pair) = "$(first(kwarg))=$(val_html(last(kwarg)))"
kwargs_html(kwargs) = "; " * join(map(kwa_eql, kwargs), ", ")
args_html(kwargs) = join(map(val_html, kwargs), ", ")
kwargs_html(kwargs::Tuple{}) = ""
sub_called_html(tr::Trace) =
     """<ul>""" * mapreduce(x->"<li>"*trace_html(x)*"</li>", *, "", tr.called) * "</ul>"

call_html(::Any, tr::Trace) =
    # Could use CSS https://www.computerhope.com/issues/ch001034.htm
    "<pre>$(tr.func)($(args_html(tr.args))$(kwargs_html(tr.kwargs))) => $(return_val_html(tr.value))</pre>"

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

""" `limit_depth(::Trace, n::Int)` prunes the Trace-tree to a depth of `n` (convenient
to first explore a trace at a high-level) """
limit_depth(tr::Trace, n::Int) =
    Trace(tr.func, tr.args, tr.kwargs,
          [limit_depth(sub_tr, n-1) for sub_tr in tr.called if n > 0],
          tr.value)

function is_inferred(tr::Trace)
    try
        call_macro(:@inferred, tr, Base.Test)
        return true
    catch e
        if e isa ErrorException && contains(e.msg, "does not match inferred return type")
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

""" `time(tr::Trace, timing_macro=:@elapsed)` times each subtrace within `tr` using
`@elapsed` (though it is recommended to use `BenchmarkTools.@belapsed` to profile short
functions) """
Base.time(tr::Trace, timing_macro=:@elapsed) =
    greenred(round(normalize(map(timing_macro, tr)), 4))

################################################################################

struct IsEqual
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
If `filter_out_equal==true`, only show the equal """
function compare_past_trace(old_trace::Trace; filter_out_equal=true)
    tr2 = map(old_trace) do sub_tr
        IsEqual(sub_tr.value, sub_tr()) end
    return filter_out_equal ? filter(subtr->!iseql(subtr.value), tr2) : tr2
end

end # module
