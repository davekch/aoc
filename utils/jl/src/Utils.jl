module Utils

include("Graphs.jl")
export Graphs
include("Geometry.jl")
export Geometry


lines(s::AbstractString) = split(strip(s), "\n")
export lines

int(s)::Int = parse(Int, s)
export int

ints(s::AbstractString)::Vector{Int} = map(int, [s[idx] for idx in findall(r"-?[0-9]+", s)])
export ints

digits(s::AbstractString)::Vector{Int} = map(int, [s[idx] for idx in findall(r"[0-9]", s)])
export digits

unzip(a) = map(x->getfield.(a, x), fieldnames(eltype(a)))
export unzip

read_range(line::AbstractString) = range(parse.(Int, split(line, '-'))...)
export read_range

"""
parses the input into a grid of type Dict{Point2D, Char}
"""
function read_grid(input)
    grid = Dict{Geometry.Point2D, Char}()
    for (y, line) in input |> strip |> lines |> enumerate
        for (x, c) in line |> enumerate
            grid[Geometry.Point2D(x, y)] = c
        end
    end
    grid
end
export read_grid


"""
read_as_matrix(input::AbstractString)
parses the input into a grid (2D array/matrix)
"""
function read_as_matrix(input::AbstractString)
    raw_data = strip(input)
    lines = collect.(split(raw_data, '\n'))
    return permutedims([lines...;;])
end
export read_as_matrix


function invert_dict(d)
    inverted = Dict()
    for (k, v) in d
        for nk in v
            if nk ∉ keys(inverted)
                inverted[nk] = []
            end
            push!(inverted[nk], k)
        end
    end
    inverted
end
export invert_dict

end # module Utils

