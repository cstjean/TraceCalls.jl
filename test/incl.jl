couch() = 2
apple() = :orange

begin
    plant() = "happy"
    """ good boy """
    dog() = 3
    @inline inlined() = 5
end

################################################################################

using SimpleTraits
isnice(::Type{Int}) = true
isnice(::Type{Float64}) = false
@traitdef IsNice{X}
@traitdef BelongTogether{X,Y} # traits can have several parameters

@traitimpl IsNice{Int}
@traitimpl BelongTogether{Int,String}

@traitimpl IsNice{X} <- isnice(X)

@traitfn f{X; IsNice{X}}(x::X) = "Very nice!"
@traitfn f{X; !IsNice{X}}(x::X) = "Not so nice!"

################################################################################

@generated generated_mouse(x::String) = :(string(x, " ", $(typeof(x))))
@generated generated_missing(::Any) = 42  # JuliaLang#23809

################################################################################

module Moo
# Unsupported. FIXME? - September'17
export cow
cow() = 1
end
