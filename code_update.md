# Code source

`source(fn::Function)` returns a vector of each `Expr` that defines the methods of `fn`.
This is heuristic; it might miss out on some of fn's methods (though it will warn in that
case). Try `source(Base.which)` for an example. 

# Code transformations

`TraceCalls` now also provides a post-hoc code-transformation utility,
`update_code_revertible`. This is meant for building diagnostic tools, not for everyday
usage. For example, this code adds a counter to every function in the module `AA`:

```julia
using AA, ClobberingReload, MacroTools

counter = fill(0)
function add_counter(fdef)
    di = ClobberingReload.splitdef(fdef)
    di[:body] = quote $counter .+= 1; $(di[:body]) end
    ClobberingReload.combinedef(di)
end
module_update = update_code_revertible(AA) do code
    if @capture(code, function name_(args__) body__ end)
        # See `?ClobberingReload.splitdef` for a better function-processing utility
        :(function $name($(args...))
            $counter[] += 1
            $(body...)
          end)
    end
end
module_update() do
    # Temporarily apply the update, for the duration of this do-block 
    AA.f(10)
end
AA.f(10)

println(counter[])  # 1
```

`update_code_revertible(some_function) do code ... end` works the same way, updating the code behind each of the methods of `some_function`.

This functionality is the basis of [TraceCalls.jl](https://github.com/cstjean/TraceCalls.jl). It's a hack, and can fail for any number of reason. Do not rely on this for mission-critical functionality.

Avoid storing the output of `update_code_revertible`, since if the files are modified
and reloaded by `creload` or `Revise.jl`, then the code-update object will be out-of-sync.
A better way to improve runtime is to memoize the result of the code transformation
function.
