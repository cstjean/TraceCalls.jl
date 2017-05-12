# TraceCalls

[![Build Status](https://travis-ci.org/cstjean/TraceCalls.jl.svg?branch=master)](https://travis-ci.org/cstjean/TraceCalls.jl)

[![Coverage Status](https://coveralls.io/repos/cstjean/TraceCalls.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/cstjean/TraceCalls.jl?branch=master)

[![codecov.io](http://codecov.io/github/cstjean/TraceCalls.jl/coverage.svg?branch=master)](http://codecov.io/github/cstjean/TraceCalls.jl?branch=master)

`TraceCalls` is a debugging tool for understanding program execution. `@traceable`
augments a function's code to store that it was called when `@trace` is used. Example:

![Screenshot](Screenshot.png)

The return value of `@trace` is a `Trace` object, which is displayed nicely in
HTML-capable environments. Try `dumps(trace)` to see how the data is stored, or perform
extra analysis.

#### Custom printing

There is often irrelevant information in the function calls, or large objects that
should would be better printed concisely. This can be done either by overloading
`Base.string`, or `TraceCalls.val_html` and `TraceCalls.call_html`:

![Screenshot_Custom](Screenshot_Custom.png)

#### Performance

The `@traceable` macro should have minimal impact on performance when `@trace` isn't
used. Nevertheless, it would be a bad idea to trace small performance-critical functions.