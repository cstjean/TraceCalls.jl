# 0.1.0

#### User interface changes

- New feature, argument indexing: `trace[1, 2, :vec]` gets the `:vec` argument of the second call of the first call.
- Similar to `vec[end]`, `stacktrace[bottom-1]` returns the second-to-last call in a
stack trace (see `@stacktrace`)
- `sum(fn, trace)` and `all(fn, trace)` were added. The passed functions should accept
a `Trace` object.
- New function: `highlight(pred, trace)`. See `?highlight`.

