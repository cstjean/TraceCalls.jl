__precompile__()
module TraceCalls

using MacroTools, Utils
using MacroTools: combinedef
using Base.Test: @inferred
using ClobberingReload

export @traceable, @trace, Trace, limit_depth, FontColor,
    is_inferred, map_is_inferred, redgreen, greenred, @trace_inferred,
    compare_past_trace, traceable!, filter_func

""" When `TraceCalls.active[]` is `false`, `@traceable ...` is an identity macro
(it doesn't modify the function at all) """
const active = fill(true)

""" A `Trace` object represents a function call. It has fields `func, args, kwargs,
called, value`, with `func(args...; kwargs...) = value`. `called::Vector{Trace}` of the
immediate traceable function calls that happened during the execution of this trace.

There are no accessors for `Trace`; please reference its fields directly.
For instance, `filter(tr->isa(tr.args[1], Int), trace)` will select all calls whose
first argument is an `Int`. """
mutable struct Trace
    func::Function
    args::Tuple
    kwargs::Tuple
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
call_mac(mac::Symbol, tr::Trace, mod::Module=Main) =
    eval(mod, Expr(:macrocall, mac, (isempty(tr.kwargs) ?
                                     # Needs special casing because @inferred chokes
                                     # on kwargs-less funcalls otherwise.
                                     :($(tr.func)($(tr.args...))) :
                                     :($(tr.func)($(tr.args...); $(tr.kwargs))))))
call_mac(mac::Expr, tr::Trace, mod::Module=Main) =
    (mac.head==:macrocall ? call_mac(only(mac.args), tr, mod) :
     error("Unable to call macro $mac"))

""" `map(f, tr::Trace)` recursively applies the function f to each `Trace` in `tr`,
and stores the result in `Trace.value` """
Base.map(f, tr::Trace) = Trace(tr.func, tr.args, tr.kwargs,
                               [map(f, c) for c in tr.called], f(tr))
Base.map(mac::Union{Symbol, Expr}, tr::Trace) = map(sub->call_mac(mac, sub), tr)

top_level_dummy() = error("top_level_dummy is not callable")

const is_tracing = fill(false)
top_trace(fun) = Trace(fun, (), (), [], nothing)
const trace_data = top_trace(top_level_dummy)
const current_trace = fill(trace_data)

function split_curly(e)
    if @capture(e, name_{args__})
        return name, args
    else
        return (e, ())
    end
end

is_function_definition(expr) = 
    try
        parse_function_definition(expr)
        true
    catch e
        false
    end

""" `@traceable_loose(expr)` is like `traceable(expr)`, but doesn't error on non-function
definitions (it's a helper for `@traceable begin ... end`) """
macro traceable_loose(expr)
    is_function_definition(expr) ? esc(:($TraceCalls.@traceable $expr)) : esc(expr)
end

""" `@traceable function foo(...) ... end` marks a function definition for the `@trace`
macro. See the README for details. """
macro traceable(fdef::Expr)
    if !active[] return esc(fdef) end

    if fdef.head == :block
        return esc(quote
            $([:($TraceCalls.@traceable_loose $expr) for expr in fdef.args]...)
        end)
    end

    arg_name(arg) = splitarg(arg)[1]
    handle_missing_arg(arg::Void) = gensym() # handle name-free arguments like ::Int
    handle_missing_arg(arg::Symbol) = arg
    function typed_arg(arg)
        name, arg_type, is_splat = splitarg(arg)
        if is_splat arg_type = Any end
        return :($(handle_missing_arg(name))::$arg_type)
    end
    
    di = splitdef(fdef)
    fname = di[:name]

    do_body_di = copy(di)
    do_body_di[:name] = do_body = fname isa Expr ? gensym() : gensym(fname)
    do_body_di[:args] = map(typed_arg, [di[:args]..., di[:kwargs]...])
    do_body_di[:kwargs] = []

    updated_fun_di = copy(di)
    @gensym new_trace prev_trace e res
    all_args_symbol = map(handle_missing_arg ∘ arg_name, [di[:args]..., di[:kwargs]...])
    updated_fun_di[:body] = quote
        if !$TraceCalls.is_tracing[] return $do_body($(all_args_symbol...)) end
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
    return esc(quote
        @inline $(combinedef(do_body_di))
        $(combinedef(updated_fun_di))
    end)
end

""" `traceable!(module_name)` makes every[1] function in `module_name` traceable.

[1] Certain conditions apply. Try it and see. Use `@traceable` on individual functions for
more consistent results.
"""
function traceable!(mod)
    creload_diving(mod) do code
        map(code) do expr
            # Everything which isn't an include, and isn't a function definition gets
            # ignored. That's okay! creload clobbers the existing definitions, and we
            # don't need to clobber _everything_.
            if @capture(expr, include(any_))
                expr
            elseif is_function_definition(expr)
                :($TraceCalls.@traceable $expr)
            end
        end
    end
end

function tracing(fun::Function)
    copy!(trace_data, top_trace(fun))
    try
        is_tracing[] = true
        trace_data.value = fun()
    catch e
        trace_data.value = e
    finally
        is_tracing[] = false
    end
    return copy(trace_data)
end

macro trace(expr)
    :($TraceCalls.tracing(()->$(esc(expr))))
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

kwa_eql(kwarg::Pair) = "$(first(kwarg))=$(val_html(second(kwarg)))"
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
        call_mac(:@inferred, tr, Base.Test)
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
