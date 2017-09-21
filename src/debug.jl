only_exceptions(trace::Trace) = filter(tr->tr.value isa Exception, trace)
macro stacktrace(to_trace, expr)
    esc(:($TraceCalls.only_exceptions(@trace($to_trace, $expr))))
end

macro stacktrace(expr)
    esc(:($TraceCalls.@stacktrace () $expr))
end

################################################################################
# compare_past_trace

struct IsEqual
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
        try_show_val(io, mime, isd.a)
    else
        write(io, "<u>before</u>: ")
        try_show_val(io, mime, isd.a)
        write(io, " <u>vs. now:</u> ")
        try_show_val(io, mime, isd.b)
    end
    write(io, """</font>""")
end
function show_val(io::IO, mime::MIME"text/plain", isd::IsEqual)
    if iseql(isd)
        with_crayon(io, Crayon(foreground=:green)) do
            write(io, "Same: ")
            try_show_val(io, mime, isd.a)
        end
    else
        with_crayon(io, Crayon(foreground=:red)) do
            with_crayon(io, Crayon(underline=true)) do
                write(io, "before")
            end
            write(io, ": ")
            try_show_val(io, mime, isd.a)
            write(io, " ")
            with_crayon(io, Crayon(underline=true)) do
                write(io, "vs. now")
            end
            write(io, ": ")
            try_show_val(io, mime, isd.b)
        end
    end
end

""" `compare_past_trace(old_trace::Trace; filter_out_equal=true))` reruns every function
call in `old_trace`, and shows in red where the new results differ from the old.  If
`filter_out_equal==true`, only show the non-equal results. Override
`TraceCalls.iseql(a, b)` to implement custom comparisons. """
function compare_past_trace(old_trace::Trace; filter_out_equal=true)
    tr2 = map(sub_tr->IsEqual(sub_tr.value, get_error_or_value(sub_tr)),
              old_trace)
    return filter_out_equal ? filter(!iseql, tr2) : tr2
end
