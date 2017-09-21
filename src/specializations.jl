# This code should perhaps be moved into a separate package

specializations(f::Function) =
    MethodInstance[spec for m in methods(f).ms for spec in specializations(m)]
specializations(m::Method) = gather_specializations(m.specializations)
gather_specializations(::Void) = MethodInstance[]  # needs special-casing...
function gather_specializations(specs::Union{Base.TypeMapEntry, Base.TypeMapLevel})
    res = MethodInstance[]
    Base.visit(specs) do spec
        push!(res, spec)
    end
    res
end

is_compiled(spec::MethodInstance) = spec.fptr!=C_NULL

"""    number_of_specializations(obj::Module/Function)

For each function/method in `obj`, return the number of specializations, and the
number of _compiled_ specializations (excluding those that were merely inferred). """
function number_of_specializations(mod::Module)
    res = []
    for name in names(mod, true) # see ?names
        r = @ignore_errors nothing eval(mod, name)
        if r isa Function
            specs = specializations(r)
            if length(specs) > 0
                push!(res, (r, length(specs), count(is_compiled, specs)))
            end
        end
    end
    sort(res, by=last, rev=true)
end
number_of_specializations(fun::Function) =
    sort([(specs = specializations(m);
           (m, length(specs), count(is_compiled, specs)))
          for m in methods(fun)], by=last, rev=true)

is_inferred(spec::MethodInstance) =
    isleaftype(spec.rettype)   # seems reasonable, but not 100% sure
