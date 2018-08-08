@testset "Code update" begin
    include("code_update.jl")
end

@spawn for i in 1:10
    sleep(5 * 60)
    # Periodically print during testing so that Travis doesn't kill the process
    println("Dear Travis, please spare this slow process a few more minutes of runtime ($i).")
    flush(STDOUT)
end

function ctree_size(tr::Trace)
    # tree_size, but first make sure that it wasn't an error
    @assert !isa(tr.value, Exception)
    tree_size(tr)
end

@traceable h(a; k=3) = 2
g(x::Int) = 10+h(5.0)+h(10)
@traceable f(x) = g(x+2)
@test ctree_size(@trace f(10)) == 4

TraceCalls.show_val(io::IO, ::MIME"text/plain", ::Float64) = write(io, "some_float")
TraceCalls.show_call(io::IO, ::MIME"text/plain", ::typeof(f), tr::Trace) =
    write(io, "f was called")
@trace f(10)

# Check if symbols are correctly handled in macros
@traceable bar(; x=2) = [x,x]
@traceable bar2(x) = [x,x]
map(:@elapsed, @trace(bar(; x=:y)))
map(:@elapsed, @trace(bar2(:y)))

# splatting
@traceable splat(args::Int...)::Int = sum(args)
t = @trace splat(1,2,3)
@test t[1]() == 6
@test t[1].args == (1,2,3)

# kwarg splatting
@traceable kwsplat(;kwargs...) = map(first, kwargs)
t = @trace kwsplat(x=1,y=2,z=3)
@test t[1]() == [:x, :y, :z]

include("incl.jl")
@test ctree_size(@trace "incl.jl" couch()) == 2

@test ctree_size(@trace Base.isnull get(Nullable(10))) == 2


# compare_past_trace
@traceable function foo(x)
    my_length(x) + bar(x)
end
@traceable my_length(x) = length(x)
@traceable bar(x) = 10

u = [10,20,30]
tr = @trace foo(u)
push!(u, 40)
@test 0.25 == mean([TraceCalls.iseql(tr.value)
                    for tr in collect(compare_past_trace(tr, filter_out_equal=false))])


# NoTraceable
using TraceCalls
@traceable foo(x) = x+2
@testset "NoTraceable" begin
    @test ctree_size(@trace foo(10)) == 2
    @test ctree_size(@trace (NoTraceable(),) foo(10)) == 1
end


# filtering
@traceable f1(x) = f2(x) + f3(x)
@traceable f2(x) = x-10
@traceable f3(x) = f4(x)+20
@traceable f4(x) = 2*x
@testset "Filtering" begin
    tr = @trace f1(5) + f4(10)
    @test ctree_size(filter_lineage(sub->sub.func==f3, tr)) == 4
    @test ctree_size(filter(sub->sub.func==f3, tr)) == 2
    @test ctree_size(filter_cutting(sub->sub.func!=f3, tr)) == 4
end

# generated functions
@testset "Generated functions" begin
    include("incl.jl")
    tr_mouse = @trace "incl.jl" generated_mouse("hey")
    tr_mouse2 = @trace generated_mouse generated_mouse("hey")
    @test TraceCalls.value(tr_mouse) == "hey DataType"
    @test ctree_size(tr_mouse) == ctree_size(tr_mouse2) == 2
end

# JuliaLang#23809
tr_missing = @trace "incl.jl" generated_missing("hey")
@test ctree_size(tr_missing) == 2
