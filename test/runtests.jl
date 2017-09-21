using TraceCalls
using TraceCalls: tree_size
using Base.Test

include("code_update.jl")

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

@test ctree_size(@trace foo(10)) == 2
@test ctree_size(@trace (NoTraceable(),) foo(10)) == 1


# filtering
@traceable f1(x) = f2(x) + f3(x)
@traceable f2(x) = x-10
@traceable f3(x) = f4(x)+20
@traceable f4(x) = 2*x
tr = @trace f1(5) + f4(10)
@test ctree_size(filter_lineage(sub->sub.func==f3, tr)) == 4
@test ctree_size(filter(sub->sub.func==f3, tr)) == 2
@test ctree_size(filter_cutting(sub->sub.func!=f3, tr)) == 4

# generated functions
include("incl.jl")
tr_mouse = @trace "incl.jl" generated_mouse("hey")
@test TraceCalls.value(tr_mouse) == "hey DataType"
@test ctree_size(tr_mouse) == 2

################################################################################
# Testing with popular packages
import ClobberingReload
root_of(mod::Module) =
    joinpath(dirname(ClobberingReload.module_definition_file(string(mod))), "..")


using JuMP
example = joinpath(root_of(JuMP), "examples", "basic.jl")
@test ctree_size(@trace JuMP include(example)) > 10


using Gadfly
# Most of the complexity in Gadfly comes from Base.show(::Gadfly.PlotObject, ...), and
# that's not tested here.
@test ctree_size(@trace Gadfly plot(x=rand(10), y=rand(10))) > 5


# Knet should be tested again either once they've updated to 0.6, or once 
# https://github.com/JuliaLang/julia/issues/22729 is fixed.
# using KNet
# @trace Knet include(joinpath(root_of(Knet), "examples", "linreg.jl"))


using DataStructures: binary_minheap, OrderedDict
@test ctree_size(@trace DataStructures binary_minheap([2,11, 14])) > 3
# This looks innocent, but it helped us find a problem with typed slurped args
@test ctree_size(@trace DataStructures.OrderedDict OrderedDict(1=>2, 3=>4)) >= 1

using PyCall
@test ctree_size(@trace PyCall pyimport(:math)[:pi]) > 5


using Optim   # From https://github.com/JuliaNLSolvers/Optim.jl
rosenbrock(x) =  (1.0 - x[1])^2 + 100.0 * (x[2] - x[1]^2)^2
@test ctree_size(@trace Optim optimize(rosenbrock, zeros(2), BFGS())) > 50


# We can use DifferentialEquations.jl, but that's a huge download and install
# Example from http://docs.juliadiffeq.org/stable/tutorials/ode_example.html
using OrdinaryDiffEq, DiffEqBase
# interp_summary is to fix some issue - lost the link
DiffEqBase.interp_summary(::OrdinaryDiffEq.InterpolationData) = "42"
f(t,u) = 1.01*u
u0=1/2
tspan = (0.0,1.0)
prob = ODEProblem(f,u0,tspan)
@test ctree_size(@trace OrdinaryDiffEq OrdinaryDiffEq.solve(prob,Tsit5(),reltol=1e-8,abstol=1e-8)) > 20


using LightGraphs
A = [
    0 1 1
    1 0 1
    1 1 0
]
graph = Graph(A)
@test ctree_size(@trace LightGraphs adjacency_matrix(graph)) > 5


################################################################################

using NBInclude
nbinclude("../README.ipynb")
