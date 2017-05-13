__precompile__()
module TraceCalls

using QuickTypes, MacroTools, Utils
import Unrolled

export @traceable, @trace, Trace, filter_trace

@qmutable Trace(func::Function, args::Tuple, kwargs::Tuple, called::Vector{Trace},
                return_value)

function Base.copy!(dest::Trace, src::Trace)
    dest.func = src.func
    dest.args = src.args
    dest.kwargs = src.kwargs
    dest.called = src.called
    dest.return_value = src.return_value
end

Base.copy(tr::Trace) = Trace(tr.func, tr.args, tr.kwargs, tr.called, tr.return_value)
@qstruct NotReturned()
Base.push!(tr::Trace, sub_trace::Trace) = push!(tr.called, sub_trace)
Base.getindex(tr::Trace, i::Int) = tr.called[i]
Base.length(tr::Trace) = legnth(tr.called)

top_level() = nothing

const is_tracing = fill(false)
top_trace() = Trace(top_level, (), (), [], nothing)
const trace_data = top_trace()
const current_trace = fill(trace_data)

macro traceable(fdef)
    func, args, kwargs, body_block, ret_type = parse_function_definition(fdef)
    arg_name = Unrolled.function_argument_name
    all_args = map(arg_name, [args..., kwargs...])
    do_body, new_trace, prev_trace = gensym(), gensym(), gensym()
    esc(quote
        @inline function $do_body($(all_args...))
            $body_block
        end
        function $func($(args...); $(kwargs...))::$ret_type
            if $TraceCalls.is_tracing[]
                $prev_trace = $TraceCalls.current_trace[]
                $new_trace =
                 $TraceCalls.Trace($func, ($(map(arg_name, args)...),),
                                   ($([:($(Expr(:quote, arg_name(kwa)))=>$(arg_name(kwa)))
                                       for kwa in kwargs]...),),
                                   [], $TraceCalls.NotReturned())
                $TraceCalls.current_trace[] = $new_trace
                push!($prev_trace, $new_trace)
            end
            try
                res = $do_body($(all_args...))
                if $TraceCalls.is_tracing[]
                    $new_trace.return_value = res
                end
                return res
            finally
                if $TraceCalls.is_tracing[]
                    $TraceCalls.current_trace[] = $prev_trace
                end
            end
        end
    end)
end

function tracing(fun::Function)
    copy!(trace_data, top_trace())
    try
        is_tracing[] = true
        fun()
    finally
        is_tracing[] = false
    end
    return copy(trace_data)
end

macro trace(expr)
    :($TraceCalls.tracing(()->$(esc(expr))))
end

################################################################################

const indentation = fill(10)

const tab_def = """<style type="text/css">
<!--
.tab { margin-left: 40px; }
-->
</style>"""

val_html(x) = string(x)

Base.show(io::IO, ::MIME"text/html", tr::Trace) =
    write(io, tab_def * "<pre>"*trace_html(tr)*"</pre>")

kwa_eql(kwarg::Pair) = "$(first(kwarg))=$(val_html(second(kwarg)))"
kwargs_html(kwargs) = "; " * join(map(kwa_eql, kwargs), ", ")
args_html(kwargs) = join(map(val_html, kwargs), ", ")
kwargs_html(kwargs::Tuple{}) = ""
sub_called_html(tr::Trace, indent) =
     """<ul>""" * mapreduce(x->"<li>"*trace_html(x, indent+indentation)*"</li>", *, "", tr.called) * "</ul>"

    # (isempty(tr.called) ? "" :
    #  """<p style="margin-left: $(indent)px">""" * mapreduce(x->trace_html(x, indent+indentation), *, "", tr.called) * "</p>")
call_html(::Any, tr::Trace) =
    # Could use CSS https://www.computerhope.com/issues/ch001034.htm
    "$(tr.func)($(args_html(tr.args))$(kwargs_html(tr.kwargs))) = $(val_html(tr.return_value))<br>"

trace_html(tr::Trace, indent=indentation) = call_html(tr.func, tr) * sub_called_html(tr, indent)

filter_trace(f::Function, tr::Trace) =
    Trace(tr.func, tr.args, tr.kwargs,
          [filter_trace(f, sub_tr) for sub_tr in tr.called if f(sub_tr)],
          tr.return_value)

end # module
