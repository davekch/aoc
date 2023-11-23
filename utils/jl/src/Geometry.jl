module Geometry
import Base: +, -, *

mutable struct Point2D{T<:Integer}
    x::T
    y::T
end
Point2D(p::Vector{T}) where T <: Integer = Point2D(p[1], p[2])
export Point2D

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


end # module Geometry