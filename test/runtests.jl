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

################################################################################
# Testing with popular packages
import ClobberingReload
root_of(mod::Module) =
    joinpath(dirname(ClobberingReload.module_definition_file(string(mod))), "..")


using JuMP
path = joinpath(root_of(JuMP), "examples", "basic.jl")
@test tree_size(@trace JuMP include(path)) > 10


using Gadfly
# Most of the complexity in Gadfly comes from Base.show(::Gadfly.PlotObject, ...), and
# that's not tested here.
@test tree_size(@trace Gadfly plot(x=rand(10), y=rand(10))) > 5


# Knet should be tested again either once they've updated to 0.6, or once 
# https://github.com/JuliaLang/julia/issues/22729 is fixed.
# using KNet
# @trace Knet include(joinpath(root_of(Knet), "examples", "linreg.jl"))


# We can use DifferentialEquations.jl, but that's a huge download and install
# Example from http://docs.juliadiffeq.org/stable/tutorials/ode_example.html
using OrdinaryDiffEq, DiffEqBase
DiffEqBase.interp_summary(::OrdinaryDiffEq.InterpolationData) = "42"
f(t,u) = 1.01*u
u0=1/2
tspan = (0.0,1.0)
prob = ODEProblem(f,u0,tspan)
@trace sol = OrdinaryDiffEq.solve(prob,Tsit5(),reltol=1e-8,abstol=1e-8)
