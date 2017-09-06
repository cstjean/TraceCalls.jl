# TraceCalls

[![Build Status](https://travis-ci.org/cstjean/TraceCalls.jl.svg?branch=master)](https://travis-ci.org/cstjean/TraceCalls.jl)

TraceCalls.jl is a functional tracing package for [exploring](http://nbviewer.jupyter.org/github/cstjean/TraceCalls.jl/blob/master/README.ipynb#Manipulating-traces), [debugging](http://nbviewer.jupyter.org/github/cstjean/TraceCalls.jl/blob/master/README.ipynb#Debugging-with-traces) and [profiling](http://nbviewer.jupyter.org/github/cstjean/TraceCalls.jl/blob/master/README.ipynb#Profiling) Julia code, built on top of [Revise.jl](https://github.com/timholy/Revise.jl). It supports the REPL, [IJulia](https://github.com/JuliaLang/IJulia.jl) and [Atom](http://junolab.org/).

# Installation

```julia
Pkg.add("TraceCalls")
```

# Documentation 

[User Manual](http://nbviewer.jupyter.org/github/cstjean/TraceCalls.jl/blob/master/README.ipynb)

# Interaction with Revise.jl

TraceCalls.jl imports [Revise.jl](https://github.com/timholy/Revise.jl),
which triggers automatic reloading behaviour for subsequently-loaded packages. This
should be fine for most users, but you can [turn it
off](https://github.com/timholy/Revise.jl#manual-revision) if it's an issue.

# Support

TraceCalls has a lot of functionality; if you have any question, please don't hesitate to [file an issue](https://github.com/cstjean/TraceCalls.jl/issues), or [ask on Discourse](https://discourse.julialang.org/) and ping me (@cstjean).