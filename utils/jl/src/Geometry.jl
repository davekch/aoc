module Geometry
import Base: +, -, *,==
import Combinatorics: combinations
using ..Utils
using DataStructures

mutable struct Point2D{T<:Integer}
    x::T
    y::T
end
Point2D(p::Vector{T}) where T <: Integer = Point2D(p[1], p[2])
export Point2D

Base.show(io::IO, p::Point2D) = print(io, "P($(p.x), $(p.y))")

Base.hash(p::Point2D) = hash("$(p.x);$(p.y)")

# this makes the point act as a 0-dimensional scalar
# this allows broadcasting like this: Point2D(x, y) .+ [Point2D(...), ...]
# broadcasting like [p1, p2] .+ [p3, p4] is still possible ofc
Base.broadcastable(p::Point2D) = Ref(p)

to_vec(p::Point2D) = [p.x, p.y]
export to_vec

# make Point2D iterable, this allows (x, y) = Point2D(1, 2)
function Base.iterate(p::Point2D, state=1)
    if state == 1
        return (p.x, 2)
    elseif state == 2
        return (p.y, 3)
    end
    nothing
end

+(p1::Point2D, p2::Point2D) = Point2D(p1.x + p2.x, p1.y + p2.y)
+(p::Point2D, (x, y)) = Point2D(p.x + x, p.y + y)
-(p1::Point2D, p2::Point2D) = Point2D(p1.x - p2.x, p1.y - p2.y)
-(p::Point2D) = Point2D(-p.x, -p.y)
*(a::Integer, p::Point2D) = Point2D(a * p.x, a * p.y)
==(p1::Point2D, p2::Point2D) = p1.x == p2.x && p1.y == p2.y

"""
scalar product
"""
dot(p1::Point2D, p2::Point2D) = p1.x * p2.x + p1.y * p2.y
⋅ = dot
export dot, ⋅

"""
squared euclidean distance
"""
dist2(p1::Point2D, p2::Point2D) = (p1.x - p2.x)^2 + (p1.y - p1.y)^2
export dist2

manhattan(p1::Point2D, p2::Point2D) = abs(p1.x - p2.x) + abs(p1.y - p2.y)
export manhattan


"""
return the four horizontally and vertically adjacent points
"""
neighbours4(p::Point2D) = [
    Point2D(p.x+1, p.y), Point2D(p.x-1, p.y),
    Point2D(p.x, p.y+1), Point2D(p.x, p.y-1)
]
export neighbours4

"""
return the 8 horizontally, vertically and diagonally adjacent points
"""
neighbours8(p::Point2D) = [Point2D(p.x + i, p.y + j) for i = -1:1, j = -1:1 if !(i == j == 0)]
export neighbours8


"""
rotate around the origin by 90 degrees to the right (clockwise)
"""
rot90r(p::Point2D) = [0 1; -1 0] * to_vec(p) |> Point2D
export rot90r

"""
rotate around the origin by 90 degrees to the left (anticlockwise)
"""
rot90l(p::Point2D) = [0 (-1); 1 0] * to_vec(p) |> Point2D
export rot90l


"""
given a list of n-dimensional tuples of ranges (example: (1:3, 2:8) represents
a 2D rectangle with x ∈ 1:3, y∈2:8), calculate the volume of all intersections
CAUTION: only correct if shapes overlap only pairwise
"""
function nd_intersection_volume(hypercubes)
    V = 0
    # calculate pairwise intersection volumes and add them up
    for (c1, c2) in combinations(hypercubes, 2)
        v = 1
        for (s1, s2) in zip(c1, c2)
            v *= length(intersect(s1, s2))
        end
        V += v
    end
    V
end
export nd_intersection_volume


"""
given a list of n-dimensional tuples of ranges  (example: (1:3, 2:8) represents
a 2D rectangle with x ∈ 1:3, y∈2:8), calculate the volume of the union
CAUTION: only correct if shapes overlap only pairwise
"""
function nd_union_volume(hypercubes)
    V = 0
    for cube in hypercubes
        v = 1
        for side in cube
            v *= length(side)
        end
        V += v
    end
    V - nd_intersection_volume(hypercubes)
end
export nd_union_volume


"""
calculates the area of a polygon specified by an ordered sequence of
integer vertex points (implements shoelace)
"""
function polyarea(points)
    A = 0
    N = length(points)
    for i = 1:N
        x1,y1 = points[i]
        x2,y2 = points[i%N + 1]
        A += (y1 + y2) * (x1 - x2)
    end
    abs(A) // 2
end
export polyarea


"""
given a set or list of points that describe a boundary, and a point inside the
boundary, perform flood-fill algorithm and return a set of all points inside boundary
"""
function flood_fill(boundary, start)
    filled = Set()
    q = Queue{Point2D}()
    enqueue!(q, start)
    while !isempty(q)
        current = dequeue!(q)
        push!(filled, current)
        for neighbour in neighbours8(current)
            if neighbour ∉ filled && neighbour ∉ boundary && neighbour ∉ q
                enqueue!(q, neighbour)
            end
        end
    end
    filled
end
export flood_fill


# ------------ drawing -----------------

"""
return minx, maxx, miny, maxy in a collection of points
"""
function corners(ps)
    xs, ys = unzip(ps)
    minx = minimum(xs)
    maxx = maximum(xs)
    miny = minimum(ys)
    maxy = maximum(ys)
    minx, maxx, miny, maxy
end
export corners

"""
turn a grid::Dict{Point2D, Any} into a pretty string
"""
function grid_to_string(grid; empty='.')
    pretty = ""
    minx, maxx, miny, maxy = corners(keys(grid))
    points = keys(grid)
    for y in miny:maxy
        for x in minx:maxx
            p = Point2D(x, y)
            if p ∈ points
                pretty *= string(grid[p])
            else
                pretty *= empty
            end
        end
        pretty *= '\n'
    end
    pretty
end
export grid_to_string


end # module Geometry
