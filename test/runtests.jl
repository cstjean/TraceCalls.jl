using TraceCalls
using TraceCalls: tree_size
using NBInclude
using Base.Test

@testset "TraceCalls" begin

@testset "Basics" begin
    include("basics.jl")
end

################################################################################
# Testing with popular packages
root_of(mod::Module) =
    joinpath(dirname(TraceCalls.module_definition_file(string(mod))), "..")


@testset "JuMP" begin
    using JuMP
    example = joinpath(root_of(JuMP), "examples", "basic.jl")
    @test ctree_size(@trace JuMP include(example)) > 10
end

# Disabled because I think it brought in Cairo, which caused homebrew issues compiling
# GCC...? Not sure - July'18
# @testset "Gadfly" begin
#     using Gadfly
#     # Most of the complexity in Gadfly comes from Base.show(::Gadfly.PlotObject, ...), and
#     # that's not tested here.
#     @test ctree_size(@trace Gadfly plot(x=rand(10), y=rand(10))) > 5
# end

# Knet should be tested again either once they've updated to 0.6, or once 
# https://github.com/JuliaLang/julia/issues/22729 is fixed.
# using KNet
# @trace Knet include(joinpath(root_of(Knet), "examples", "linreg.jl"))


@testset "DataStructures" begin
    using DataStructures: binary_minheap, OrderedDict
    @test ctree_size(@trace DataStructures binary_minheap([2,11, 14])) > 3
    # This looks innocent, but it helped us find a problem with typed slurped args
    @test ctree_size(@trace DataStructures.OrderedDict OrderedDict(1=>2, 3=>4)) >= 1
end

@testset "PyCall" begin
    using PyCall
    @test ctree_size(@trace PyCall pyimport(:math)[:pi]) > 5
end

@testset "Optim" begin
    using Optim   # From https://github.com/JuliaNLSolvers/Optim.jl
    rosenbrock(x) =  (1.0 - x[1])^2 + 100.0 * (x[2] - x[1]^2)^2
    @test ctree_size(@trace Optim optimize(rosenbrock, zeros(2), BFGS())) > 50
end

# August'18: this example no longer works (even without TraceCalls usage), and
# I don't have time to fix it ATM.
# @testset "DiffEq" begin
#     # We can use DifferentialEquations.jl, but that's a huge download and install
#     # Example from http://docs.juliadiffeq.org/stable/tutorials/ode_example.html
#     using OrdinaryDiffEq, DiffEqBase
#     # interp_summary is to fix some issue - lost the link
#     DiffEqBase.interp_summary(::OrdinaryDiffEq.InterpolationData) = "42"
#     f(t,u) = 1.01*u
#     u0=1/2
#     tspan = (0.0,1.0)
#     prob = ODEProblem(f,u0,tspan)
#     @test ctree_size(@trace OrdinaryDiffEq OrdinaryDiffEq.solve(prob,Tsit5(),reltol=1e-8,abstol=1e-8)) > 20
# end

@testset "LightGraphs" begin
    using LightGraphs
    A = [
        0 1 1
        1 0 1
        1 1 0
    ]
    graph = Graph(A)
    @test ctree_size(@trace LightGraphs adjacency_matrix(graph)) > 5
end

################################################################################

@testset "README" begin
    @nbinclude("../README.ipynb")
end

end
