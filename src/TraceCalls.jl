__precompile__()
module TraceCalls

using MacroTools, Utils
using Base.Test: @inferred

export @traceable, @trace, Trace, filter_trace, limit_depth, map_trace, FontColor,
    collect_trace, is_inferred, redgreen

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

macro traceable(fdef)
    if !active[] return esc(fdef) end

    func, args, kwargs, body_block, ret_type = parse_function_definition(fdef)
    fname, params = split_curly(func)

    arg_name(arg) = splitarg(arg)[1]
    handle_missing_arg(arg) =  # handle name-free arguments like ::Int
        arg_name(arg)===nothing ? :($(gensym())::$(splitarg(arg)[2])) : arg
    args = map(handle_missing_arg, args)
    all_args = [args..., kwargs...]
    @gensym do_body new_trace prev_trace e res
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

filter_trace(f::Function, tr::Trace) =
    Trace(tr.func, tr.args, tr.kwargs,
          [filter_trace(f, sub_tr) for sub_tr in tr.called if f(sub_tr)],
          tr.return_value)

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

redgreen(tr::Trace) =
    map_trace(sub->FontColor(sub.return_value ? "green" : "red", sub.return_value), tr)



end # module
