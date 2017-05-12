# TraceCalls

[![Build Status](https://travis-ci.org/cstjean/TraceCalls.jl.svg?branch=master)](https://travis-ci.org/cstjean/TraceCalls.jl)

[![Coverage Status](https://coveralls.io/repos/cstjean/TraceCalls.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/cstjean/TraceCalls.jl?branch=master)

[![codecov.io](http://codecov.io/github/cstjean/TraceCalls.jl/coverage.svg?branch=master)](http://codecov.io/github/cstjean/TraceCalls.jl?branch=master)

`TraceCalls` is a debugging tool for understanding program execution. All methods
that are preceded by `@traceable` can be traced with `@trace`

![Screenshot](Screenshot.png)

The return value of `@trace` is a `Trace` structure, which is displayed nicely in Jupyter.
To define a custom printing or analysis of the traces, use `dumps(trace)` to see how
the data is stored.

#### Performance

The `@traceable` macro should have minimal impact on performance when `@trace` isn't
used, but still, it'd be a bad idea to trace small performance-critical functions (that
is called a million times per second, say)