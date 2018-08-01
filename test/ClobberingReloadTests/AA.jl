""" Docstring """
module AA

export x, y

include("CC.jl")

struct Part
    u
end
    
foo(x) = x+100

const x = 1009
y = 2005001

# Broken until JuliaLang#17618 is backported / Julia 0.6
## immutable ParametricTest{T}
##     x::T
## end

end
