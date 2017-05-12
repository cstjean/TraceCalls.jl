using TraceCalls
using Base.Test

@traceable h(a; k=3) = 2
g(x::Int) = 10+h(5.0)+h(10)
@traceable f(x) = g(x+2)
@trace f(10)

TraceCalls.val_html(::Float64) = "some_float"
TraceCalls.call_html(::typeof(f), tr::Trace) = "f was called"
@trace f(10)
