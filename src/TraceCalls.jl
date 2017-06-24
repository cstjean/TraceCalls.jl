__precompile__()
module TraceCalls

using MacroTools, Utils
using Base.Test: @inferred

export @traceable, @trace, Trace, filter_trace, limit_depth, map_trace, FontColor,
    collect_trace, is_inferred, map_is_inferred, redgreen, greenred, @trace_inferred,
    compare_past_trace

const active = fill(true)

mutable struct Trace
    func::Function
    args::Tuple
    kwargs::Tuple
    called::Vector{Trace}
    return_value
end

function Base.copy!(dest::Trace, src::Trace)
    dest.func = src.func
    dest.args = src.args
    dest.kwargs = src.kwargs
    dest.called = src.called
    dest.return_value = src.return_value
end

Base.copy(tr::Trace) = Trace(tr.func, tr.args, tr.kwargs, tr.called, tr.return_value)
struct NotReturned end
Base.push!(tr::Trace, sub_trace::Trace) = push!(tr.called, sub_trace)
Base.getindex(tr::Trace, i::Int) = tr.called[i]
Base.length(tr::Trace) = length(tr.called)
Base.start(tr::Trace) = 1
Base.next(tr::Trace, i::Int) = (tr[i], i+1)
Base.done(tr::Trace, i::Int) = i == length(tr)+1
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

""" `map_trace(f, tr::Trace)` recursively applies the function f to each `Trace` in `tr`,
and stores the result in `Trace.return_value` """
map_trace(f, tr::Trace) = Trace(tr.func, tr.args, tr.kwargs,
                                [map_trace(f, c) for c in tr.called], f(tr))
map_trace(mac::Union{Symbol, Expr}, tr::Trace) = map_trace(sub->call_mac(mac, sub), tr)

top_level_dummy() = error("top_level_dummy is not callable")

const is_tracing = fill(false)
top_trace(fun) = Trace(fun, (), (), [], nothing)
const trace_data = top_trace(top_level_dummy)
const current_trace = fill(trace_data)

split_curly(s::Symbol) = (s, ())
function split_curly(e::Expr)
    @assert @capture(e, name_{args__}) "@traceable cannot handle $e"
    return name, args
end

""" `@traceable_loose(expr)` is like `traceable(expr)`, but doesn't error on non-function
definitions (it's a helper for `@traceable begin ... end`) """
macro traceable_loose(expr)
    try
        parse_function_definition(expr)
    catch e
        return esc(expr)
    end
    esc(:($TraceCalls.@traceable $expr))
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

    func, args, kwargs, body_block, ret_type = parse_function_definition(fdef)
    fname, params = split_curly(func)

    arg_name(arg) = splitarg(arg)[1]
    handle_missing_arg(arg) =  # handle name-free arguments like ::Int
        arg_name(arg)===nothing ? :($(gensym())::$(splitarg(arg)[2])) : arg
    args = map(handle_missing_arg, args)
    all_args = [args..., kwargs...]
    @gensym new_trace prev_trace e res
    do_body = gensym(fname)
    esc(quote
        @inline function $do_body{$(params...)}($(all_args...))
            $body_block
        end
        function $func($(args...); $(kwargs...))::$ret_type
            if $TraceCalls.is_tracing[]
                $prev_trace = $TraceCalls.current_trace[]
                $new_trace =
                 $TraceCalls.Trace($fname, ($(map(arg_name, args)...),),
                                   ($([:($(Expr(:quote, arg_name(kwa)))=>$(arg_name(kwa)))
                                       for kwa in kwargs]...),),
                                   [], $TraceCalls.NotReturned())
                $TraceCalls.current_trace[] = $new_trace
                push!($prev_trace, $new_trace)
            end
            try
                $res = $do_body($(map(arg_name, all_args)...))
                if $TraceCalls.is_tracing[]
                    $new_trace.return_value = $res
                end
                return $res
            catch $e
                if $TraceCalls.is_tracing[]
                     $new_trace.return_value = $e
                end
                rethrow()
            finally
                if $TraceCalls.is_tracing[]
                    $TraceCalls.current_trace[] = $prev_trace
                end
            end
        end
    end)
end

function tracing(fun::Function)
    copy!(trace_data, top_trace(fun))
    try
        is_tracing[] = true
        trace_data.return_value = fun()
    catch e
        trace_data.return_value = e
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
    "<pre>$(tr.func)($(args_html(tr.args))$(kwargs_html(tr.kwargs))) => $(return_val_html(tr.return_value))</pre>"

trace_html(tr::Trace) = call_html(tr.func, tr) * sub_called_html(tr)

""" `filter_trace_cutting(f::Function, tr::Trace)` filters all subtraces (and their
callees) for which `f(tr)` is false. """
filter_trace_cutting(f::Function, tr::Trace) =
    Trace(tr.func, tr.args, tr.kwargs,
          [filter_trace_cutting(f, sub_tr) for sub_tr in tr.called if f(sub_tr)],
          tr.return_value)

filter_descendents(f, tr) =
    # Special casing because of #18852
    isempty(tr.called) ? [] : [t for sub in tr.called for t in filter_trace_(f, sub)]
filter_trace_(f, tr) =
    f(tr) ? [Trace(tr.func, tr.args, tr.kwargs, filter_descendents(f, tr),
                   tr.return_value)] : filter_descendents(f, tr)
filter_trace(f::Function, tr::Trace) =
    Trace(tr.func, tr.args, tr.kwargs, filter_descendents(f, tr), tr.return_value)

""" `collect_trace(tr::Trace)` returns a vector of all `Trace` objects within `tr`. """
collect_trace(tr::Trace) = Trace[tr; mapreduce(collect_trace, vcat, [], tr)]

""" `limit_depth(::Trace, n::Int)` prunes the Trace-tree to a depth of `n` (convenient
to first explore a trace at a high-level) """
limit_depth(tr::Trace, n::Int) =
    Trace(tr.func, tr.args, tr.kwargs,
          [limit_depth(sub_tr, n-1) for sub_tr in tr.called if n > 0],
          tr.return_value)

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
map_is_inferred(tr::Trace) = redgreen(map_trace(is_inferred, tr))
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

""" `redgreen(tr::Trace)` colors all `return_value`s as shades of red/green, with
0/false being pure red and 1/true being pure green. """
redgreen(tr::Trace) =
    map_trace(sub->FontColor(redgreen(sub.return_value), sub.return_value), tr)
""" `greenred(tr::Trace)` is like `redgreen`, but with 0/false=green, 1/true=red. """
greenred(tr::Trace) =
    map_trace(sub->FontColor(redgreen(1-sub.return_value), sub.return_value), tr)

Base.maximum(tr::Trace) = maximum(sub.return_value for sub in collect_trace(tr))
Base.round(tr::Trace, n::Int) = map_trace(sub->round(sub.return_value, n), tr)
Base.normalize(tr::Trace, div=tr.return_value) =
    map_trace(sub->sub.return_value / div, tr)

""" `time(tr::Trace, timing_macro=:@elapsed)` times each subtrace within `tr` using
`@elapsed` (though it is recommended to use `BenchmarkTools.@belapsed` to profile short
functions) """
Base.time(tr::Trace, timing_macro=:@elapsed) =
    greenred(round(normalize(map_trace(timing_macro, tr)), 4))

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
    tr2 = map_trace(old_trace) do sub_tr
        IsEqual(sub_tr.return_value, sub_tr()) end
    return filter_out_equal ? filter_trace(subtr->!iseql(subtr.return_value), tr2) : tr2
end

end # module
