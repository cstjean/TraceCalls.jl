# 0.1.0

- New feature, argument indexing: `trace[1, 2, :vec]` gets the `:vec` argument of the second call of the first call.
- Similar to `vec[end]`, `stacktrace[bottom-1]` returns the second-to-last call in a
stack trace (see `@stacktrace`). Similarly for `top`.
- `sum(fn, trace)` and `all(fn, trace)` apply `fn(::Trace)` to each argument.
- `code_warntype(::Trace), code_llvm(::Trace), ...` return the corresponding code.
- New function: `highlight(pred, trace)`. See `?highlight`.
- Profiling with `measure` was significantly improved, with a new option `explore_worst`
added for quickly surverying large traces. See `?measure` and the user manual.
- New user manual sections, "Saving traces to disk" and "Tracing tips"
- New profiling feature, `trace_benchmark`. See the manual - "Grouping and benchmarking"
