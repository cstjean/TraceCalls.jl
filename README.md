# TraceCalls

[![Build Status](https://travis-ci.org/cstjean/TraceCalls.jl.svg?branch=master)](https://travis-ci.org/cstjean/TraceCalls.jl)

[![Coverage Status](https://coveralls.io/repos/cstjean/TraceCalls.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/cstjean/TraceCalls.jl?branch=master)

[![codecov.io](http://codecov.io/github/cstjean/TraceCalls.jl/coverage.svg?branch=master)](http://codecov.io/github/cstjean/TraceCalls.jl?branch=master)

`TraceCalls` is a debugging tool for understanding program execution, similar to Common
Lisp's `TRACE`. `@traceable` augments a function's code to store its calling information
(but see [this note on performance](README.md#Performance)), then `@trace` returns
the call-tree of all `@traceable` function that are called within its scope. For example:

![Screenshot](Screenshot.png)

The return value of `@trace` is a `Trace` object, which is displayed nicely in
HTML-capable environments. Try `dumps(trace)` to see how the data is stored, or perform
extra analysis.

#### Custom printing

When working with large objects, traces can become unwieldy. Custom printing can be
achieved by overloading either `Base.string`, or `TraceCalls.val_html` and
`TraceCalls.call_html`:

![Screenshot_Custom](Screenshot_Custom.png)

#### Performance

The `@traceable` macro should have minimal impact on performance when `@trace` isn't
used, and no impact on type-stability. There should be no need to remove the `@traceable`
annotations after debugging is done. Nevertheless, it would be a bad idea to trace small
performance-critical functions.