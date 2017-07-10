using TraceCalls
using TraceCalls: tree_size
using Base.Test

@traceable h(a; k=3) = 2
g(x::Int) = 10+h(5.0)+h(10)
@traceable f(x) = g(x+2)
@trace f(10)

TraceCalls.val_html(::Float64) = "some_float"
TraceCalls.call_html(::typeof(f), tr::Trace) = "f was called"
@trace f(10)

# Check if symbols are correctly handled in macros
@traceable bar(; x=2) = [x,x]
@traceable bar2(x) = [x,x]
map(:@elapsed, @trace(bar(; x=:y)))
map(:@elapsed, @trace(bar2(:y)))

@test tree_size(@trace Base.isnull get(Nullable(10))) == 2
