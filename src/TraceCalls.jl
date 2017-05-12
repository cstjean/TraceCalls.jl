module TraceCalls

using QuickTypes, Utils
using Unrolled

export @traceable, @trace

@qmutable Trace(fname, args, kwargs, called::Vector{Trace}, return_value)

function Base.copy!(dest::Trace, src::Trace)
    dest.fname = src.fname
    dest.args = src.args
    dest.kwargs = src.kwargs
    dest.called = src.called
    dest.return_value = src.return_value
end

Base.copy(tr::Trace) = Trace(tr.fname, tr.args, tr.kwargs, tr.called, tr.return_value)
@qstruct NotReturned()
Base.push!(tr::Trace, sub_trace::Trace) = push!(tr.called, sub_trace)

const is_tracing = fill(false)
top_trace() = Trace(Main, (), (), [], nothing)
const trace_data = top_trace()
const current_trace = fill(trace_data)

macro traceable(fdef)
    fname, args, kwargs, body_block, ret_type = parse_function_definition(fdef)
    arg_name = Unrolled.function_argument_name
    all_args = map(arg_name, [args..., kwargs...])
    do_body, new_trace, prev_trace = gensym(), gensym(), gensym()
    esc(quote
        @inline function $do_body($(all_args...))
            $body_block
        end
        function $fname($(args...); $(kwargs...))::$ret_type
            if $TraceCalls.is_tracing[]
                $prev_trace = $TraceCalls.current_trace[]
                $new_trace =
                 $TraceCalls.Trace($(Expr(:quote, fname)), ($(map(arg_name, args)...),),
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
    @dlet(is_tracing[]=true) do
        fun()
    end
    return copy(trace_data)
end

macro trace(expr)
    :($TraceCalls.tracing(()->$(esc(expr))))
end

################################################################################

Base.show(io::IO, ::MIME"text/html", tr::Trace) =
    write(io, "<pre>"*trace_html(tr)*"</pre>")

kwa_eql(kwarg::Pair) = "$(first(kwarg))=$(second(kwarg))"
kwargs_html(kwargs) = "; " * join(map(kwa_eql, kwargs), ", ")
kwargs_html(kwargs::Tuple{}) = ""
trace_html(tr::Trace) = 
    # Could use CSS https://www.computerhope.com/issues/ch001034.htm
    "$(tr.fname)($(tr.args...)$(kwargs_html(tr.kwargs))) = $(tr.return_value)<br><blockquote>" * mapreduce(trace_html, *, "", tr.called) * "</blockquote>"


end # module
