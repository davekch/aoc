module Utils

include("Graphs.jl")
export Graphs
include("Geometry.jl")
export Geometry


lines(s::AbstractString) = split(strip(s), "\n")
export lines

int(s::AbstractString)::Int = parse(Int, s)
export int

ints(s::AbstractString)::Vector{Int} = map(int, [s[idx] for idx in findall(r"-?[0-9]+", s)])
export ints

digits(s::AbstractString)::Vector{Int} = map(int, [s[idx] for idx in findall(r"[0-9]", s)])
export digits

end # module Utils

