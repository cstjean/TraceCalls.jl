__precompile__()
module TraceCalls

using Requires
using MacroTools
using Base.Test: @inferred
using ClobberingReload: combinedef, combinearg, longdef1, splitdef, splitarg
using DataStructures: OrderedDict, OrderedSet
using Memoize
using Base: url

export @traceable, @trace, Trace, prune, FontColor, Bold,
    is_inferred, map_is_inferred, redgreen, greenred, @trace_inferred,
    compare_past_trace, filter_func, apply_macro, @stacktrace, measure, tree_size,
    is_mutating, REPR, filter_cutting, NoTraceable, trace_log

include("code_update.jl")

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

""" Overload `TraceCalls.store(x)` for specific types to change how `TraceCalls` stores
its arguments. For example, `TraceCalls.store(x::Vector) = copy(x)`. See also ?REPR. """
store(x) = x

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
            $TraceCalls.Trace($fname,
                              map($TraceCalls.store, ($(passed_args...),)),
                              # We don't call it on kwargs. Does any function ever mutate
                              # a kwarg? Seems unlikely.
                              ($(passed_kwargs...),),
                              [], $TraceCalls.NotReturned())
        $TraceCalls.current_trace[] = $new_trace
        push!($prev_trace, $new_trace)
        try
            $res = $do_body($(map(arg_name_splat, di[:args])...);
                            $(passed_kwargs...))
            $new_trace.value = $TraceCalls.store($res)
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
                               CodeUpdate(ModDict(mod=>OrderedSet([MakeRelocatableExpr(fdef)]))))

    return esc(fdef)
end

# There might be an issue with the memo if we decide to macroexpand the code, since the
# macroexpansion depends on the environment. It's probably a negligible issue.
@memoize Dict{Tuple{Expr}, Any} function traceable_update_handle_expr(expr0::Expr)
    expr = strip_docstring(expr0)
    is_traceable(expr) ? tracing_code(expr) : nothing
end
traceable_update_handle_expr(::Any) = nothing
clear_handle_expr_memo!() =
    empty!(eval(TraceCalls, Symbol("##traceable_update_handle_expr_memoized_cache")))

""" `traceable_update(obj)::RevertibleCodeUpdate` produces the traceable code for the
given object. """
function traceable_update end

custom_when_missing(x) = warn(x)
custom_when_missing(fail::UpdateInteractiveFailure) =
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


""" `@trace (foo, NoTraceable()) ...` will only trace `foo`; not the `@traceable`
functions. """
struct NoTraceable end
traceable_update(::NoTraceable) = EmptyRevertibleCodeUpdate()


function with_tracing_definitions(body::Function, obj)
    if obj isa Tuple && NoTraceable() in obj
        upd = traceable_update(obj)
    else
        upd = merge(traceable_update(obj), traceable_macro_update())
    end
    upd() do
        body()
    end
end

""" `get_error_or_value(f::Function)` runs `f()` in a try and returns either its outcome
or the caught exception. """
function get_error_or_value(f)
    try
        f()
    catch e
        e
    end
end

""" `recording_trace(fun::Function)` sets up a fresh Trace in trace_data, then executes
`fun` and returns the final Trace object """
function recording_trace(fun::Function)
    copy!(trace_data, top_trace(fun))
    trace_data.value = get_error_or_value(fun)
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

struct FontColor
    color
    content
end
struct Bold
    content
end
""" `TraceCalls.show_return_val(io::IO, mime, x)` is the function used by `TraceCalls` to
display each return value. Defaults to calling `show_val`. """
show_return_val(io::IO, mime, x) =  show_val(io, mime, Bold(FontColor("green", x)))
show_return_val(io::IO, mime, x::Exception) = show_val(io, mime, FontColor("red", x))
""" `TraceCalls.show_val(io::IO, mime, x)` is the HTML used by `TraceCalls` to display
each value (arguments and return values). Customize it by overloading. Defaults to
`show(io, x)`. """
show_val(io::IO, _, x) = show(io, x)
function show_val(io::IO, mime::MIME"text/html", x::FontColor)
    write(io, """<font color=$(x.color)>""")
    show_val(io, mime, x.content)
    write(io, """</font>""")
end
function show_val(io::IO, mime::MIME"text/html", x::Bold)
    write(io, "<b>")
    show_val(io, mime, x.content)
    write(io, "</b>")
end
show_val(io::IO, mime::MIME"text/plain", x::Union{Bold, FontColor}) =
    show_val(io, mime, x.content)

const largest_show_size = fill(100)

function is_tree_very_large(tr::Trace, warn=true)
    size = tree_size(tr)
    if (r = (size > largest_show_size[] && largest_show_size[] > 0)) && warn
        Base.warn("Trace is very large (size $size > $(TraceCalls.largest_show_size[])); it was pruned before displaying. Enter `TraceCalls.largest_show_size[] = -1` to disable this behaviour.")
    end
    r
end

# Cut down tr until it's below largest_show_size
function cut_tree_reasonable(tr::Trace)
    s = Int(floor(sqrt(largest_show_size[]))) # using sqrt is silly; should be log?
    while tree_size(tr) >= largest_show_size[]
        tr = prune(tr, s, s)
        s -= 1
    end
    tr
end
   
function Base.show(io::IO, mime::MIME"text/html", tr::Trace)
    if is_tree_very_large(tr) return show(io, mime, cut_tree_reasonable(tr)); end
    show_call(io::IO, mime, tr)
    margin = "4px"  # a decent compromise, default is 9px
    write(io, """<ul style="  margin-top: $margin; margin-bottom: $margin;">""")
    for called in tr.called
        write(io, "<li>")
        show(io, MIME"text/html"(), called)
        write(io, "</li>")
    end
    write(io, "</ul>")
end

call_indentation = 3
indent = 0
function Base.show(io::IO, mime::MIME"text/plain", tr::Trace)
    if is_tree_very_large(tr,
                          # Avoid printing the warning twice in IJulia.
                          # https://github.com/JuliaLang/IJulia.jl/issues/574
                          !isdefined(Main, :IJulia))
        return show(io, mime, cut_tree_reasonable(tr))
    end
    write(io, string(" " ^ indent, "- "))
    show_call(io, mime, tr.func, tr)
    write(io, "\n")
    global indent += call_indentation
    try
        for called in tr.called
            show(io, mime, called)
        end
    finally
        indent -= call_indentation
    end
end

function show_kwargs(io::IO, mime, kwargs)
    write(io, "; ")
    for (i, (sym, val)) in enumerate(kwargs)
        write(io, string(sym::Symbol))
        write(io, "=")
        show_val(io, mime, val)
        if i != length(kwargs) write(io, ", ") end
    end
end
function show_args(io::IO, mime, args)
    for (i, arg) in enumerate(args)
        show_val(io, mime, arg)
        if i != length(args) write(io, ", ") end
    end
end
show_kwargs(io::IO, mime, kwargs::Tuple{}) = nothing

is_atom = false

@require Juno begin
    import Media
    using Juno: Tree

    if Juno.isactive()
        global is_atom = true

        juno_tree(tr::Trace) =
            Tree(HTML(TraceCalls.call_html(tr)), map(juno_tree, tr.called))
        function Juno.render(inl::Juno.Inline, tr::Trace)
            Juno.render(inl, juno_tree(tr))
        end
    end
end

function show_func_name(io::IO, mime::MIME"text/html", tr::Trace)
    color = is_atom ? "" : "color: black;"
    write(io, url(tr) == "" ? string(tr.func) :
          """<a href="$(url(tr))" target="_blank" style="$color text-decoration: none; border-bottom: 1px #C3C3C3 dotted">$(tr.func)</a>""")
end

show_func_name(io::IO, mime::MIME"text/plain", tr::Trace) = write(io, string(tr.func))

""" `show_call(io::IO, mime, ::Any, tr::Trace)` is called to display
each trace.  Overload it for specific functions with
`TraceCalls.show_call(io::IO, mime, ::typeof(function_name), tr::Trace) = ...` """
function show_call end

function show_call_core(io, mime, tr)
    show_func_name(io, mime, tr)
    write(io, "(")
    show_args(io, mime, tr.args)
    show_kwargs(io, mime, tr.kwargs)
    write(io, ") => ")
    show_return_val(io, mime, tr.value)
end

function show_call(io::IO, mime::MIME"text/html", ::Any, tr::Trace)
    # Could use CSS https://www.computerhope.com/issues/ch001034.htm
    write(io, """<pre style="display: inline">""")
    show_call_core(io, mime, tr)
    write(io, "</pre>")
end

show_call(io::IO, mime::MIME"text/plain", ::Any, tr::Trace) =
    show_call_core(io, mime, tr)
show_call(io::IO, mime, tr::Trace) = show_call(io, mime, tr.func, tr)

call_html(tr::Trace) = get_io_output(io->show_call(io, MIME"text/html"(), tr))

""" `get_io_output() do io ... write(io) ...` returns everything that was written
to `io`, as a string. """
function get_io_output(fn::Function)
    buf = IOBuffer()
    fn(buf)
    return String(take!(buf))
end

struct REPR
    text
    html
    function REPR(x)
        s_text = 
        s_html = IOBuffer(); show_val(s_html, MIME"text/html"(), x)
        new(get_io_output() do io; show_val(io, MIME"text/plain"(), x) end,
            get_io_output() do io; show_val(io, MIME"text/html"(), x) end)
    end
end
show_val(io::IO, ::MIME"text/plain", r::REPR) = write(io, r.text)
show_val(io::IO, ::MIME"text/html", r::REPR) = write(io, r.html)

################################################################################

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
Base.collect(tr::Trace) = Trace[tr; mapreduce(collect, vcat, [], tr.called)]

""" `prune(tr::Trace, max_depth::Int, max_length::Int=1000000000)` prunes the Trace-tree
maximum tree depth, and maximum length (number of branches in each node). 
(convenient to first explore a trace at a high-level) """
prune(tr::Trace, max_depth::Int, max_length::Int=1000000000) =
    Trace(tr.func, tr.args, tr.kwargs,
          [prune(sub_tr, max_depth-1, max_length)
           for sub_tr in tr.called[1:min(length(tr), max_length)] if max_depth > 0],
          tr.value)

is_mutating(tr::Trace) = is_mutating(tr.func)
is_mutating(f::Function) = last(string(f)) == '!'
is_mutating(typ::Type) = typ

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

""" `redgreen(tr::Trace; map=identity)` colors all `value`s as shades of red/green, with
`map(value) == 0/false` being pure red and `1/true` being pure green (so `map` can be
used to normalize the values into that range). """
redgreen(tr::Trace; map=identity) =
    TraceCalls.map(sub->FontColor(redgreen(map(sub.value)), sub.value), tr)
""" `greenred(tr::Trace)` is like `redgreen`, but with 0/false=green, 1/true=red. """
greenred(tr::Trace; map=identity) = redgreen(tr::Trace; map=x->1-map(x))

Base.maximum(tr::Trace) = maximum(sub.value for sub in collect(tr))
Base.round(tr::Trace, n::Int) = map(sub->round(sub.value, n), tr)
Base.normalize(tr::Trace, div=tr.value) =
    map(sub->sub.value / div, tr)

"""
    measure(mac_or_fun::Union{Expr, Function}, tr::Trace; normalize=false,
            threshold=0)`

Apply `mac_or_fun` to every function call in `tr`, with a `green->red` color scheme.
Remove all function calls whose result is below `threshold`. """
function measure(mac_or_fun::Union{Expr, Function}, tr::Trace; normalize=false,
                 threshold=0)
    tr() # run it once, to get the JIT behind us
    filter_thresh(tr) = filter_cutting(t->t.value>=threshold, tr)
    tr2 = map(mac_or_fun, tr)
    if normalize
        greenred(round(filter_thresh(TraceCalls.normalize(tr2)), 4))
    else
        greenred(filter_thresh(tr2); map=x->x/tr2.value)
    end
end


only_exceptions(trace::Trace) = filter(tr->tr.value isa Exception, trace)
macro stacktrace(to_trace, expr)
    esc(:($TraceCalls.only_exceptions(@trace($to_trace, $expr))))
end

macro stacktrace(expr)
    esc(:($TraceCalls.@stacktrace () $expr))
end

################################################################################

immutable IsEqual
    a
    b
end

iseql(isd::IsEqual) = iseql(isd.a, isd.b)
iseql(tr::Trace) = iseql(tr.value)
iseql(a::Nullable, b::Nullable) = isequal(a, b)
iseql(a, b) = a == b
function show_val(io::IO, mime::MIME"text/html", isd::IsEqual)
    write(io, """<font color=$(redgreen(iseql(isd)))>""")
    if iseql(isd)
        write(io, "Same: ")
        show_val(io, mime, isd.a)
    else
        write(io, "<u>before</u>: ")
        show_val(io, mime, isd.a)
        write(io, " <u>vs. now:</u> ")
        show_val(io, mime, isd.b)
    end
    write(io, """</font>""")
end

""" `compare_past_trace(old_trace::Trace; filter_out_equal=true))` reruns every function
call in `old_trace`, and shows in red where the new result differs from the old.  If
`filter_out_equal==true`, only show the non-equal results. Override
`TraceCalls.iseql(a, b)` to get custom comparisons. """
function compare_past_trace(old_trace::Trace; filter_out_equal=true)
    tr2 = map(sub_tr->IsEqual(sub_tr.value, get_error_or_value(sub_tr)),
              old_trace)
    return filter_out_equal ? filter(!iseql, tr2) : tr2
end

""" `@traceable trace_log(args...; kwargs...) = nothing` is a dummy, always-traced
function you can call to add extra information to your traces. For instance,
`trace_log(:inside_foo; y=2, sqrt_of_x=sqrt(x))` """
@traceable trace_log(args...; kwargs...) = nothing

end # module
