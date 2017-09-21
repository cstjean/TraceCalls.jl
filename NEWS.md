# 0.1.0

- New feature, argument indexing by name: `trace[1, 2, :vec]` gets the `:vec` argument of the second call of the first call.
- Similar to `vec[end-1]`, `stacktrace[bottom-1]` returns the second-to-last call in a
stack trace (see `@stacktrace`). Similarly for `top`.
- `code_warntype(::Trace), code_llvm(::Trace), ...` return the corresponding code.
- New function: `highlight(pred, trace)`. See `?highlight`.
- Profiling with `measure` was significantly improved, with a new option `explore_worst`
added for quickly surverying large traces. See `?measure` and the user manual.
- New user manual sections, "Saving traces to disk" and "Tracing tips"
- New profiling features, `trace_benchmark` and `@compilation_times`. See the manual - "Grouping and benchmarking"
- Functions that are defined behind macros (such as `@inline`, or `@traitfn`) are now
traceable.
- Generated functions are now traceable.
- Support tracing functions loaded by interactively including a file.
