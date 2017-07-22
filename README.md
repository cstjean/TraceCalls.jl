# TraceCalls

[![Build Status](https://travis-ci.org/cstjean/TraceCalls.jl.svg?branch=master)](https://travis-ci.org/cstjean/TraceCalls.jl)

TraceCalls.jl is a functional tracing package for debugging, exploring and profiling
Julia code, built on top of [Revise.jl](https://github.com/timholy/Revise.jl)

# Installation

```julia
Pkg.checkout("https://github.com/cstjean/TraceCalls.jl")
```

# Documentation 

[User Manual](http://nbviewer.jupyter.org/github/cstjean/TraceCalls.jl/blob/master/README.ipynb)

# Interaction with Revise.jl

TraceCalls.jl imports [Revise.jl](https://github.com/timholy/Revise.jl),
which triggers automatic reloading behaviour for subsequently-loaded packages. This
should be fine for most users, but you can [turn it
off](https://github.com/timholy/Revise.jl#manual-revision) if it's an issue.