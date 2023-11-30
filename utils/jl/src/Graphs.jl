module Graphs

using DataStructures

abstract type AbstractGraph{Node} end
export AbstractGraph

"""
return a list of neighbors for a node in a graph
"""
function neighbors(graph::AbstractGraph{Node}, node::Node) where {Node} end

"""
perform breadth first search on a graph, returning
a dict mapping nodes to their parents
"""
function BFS(graph::AbstractGraph{Node}, startnode::Node, endnode::Node) where {Node}
    q = Queue{Node}()
    enqueue!(q, startnode)
    path::Dict{Node,Union{Node,Nothing}} = Dict()
    path[startnode] = nothing
    while !isempty(q)
        v = dequeue!(q)
        if v == endnode
            return path
        end
        for n ∈ neighbors(graph, v)
            if n ∉ keys(path)
                path[n] = v
                enqueue!(q, n)
            end
        end
    end
    path
end
export BFS

"""
find the shortest path between startnode and endnode in a
graph using the path found by BFS
"""
function shortestpath(bfs_result::Dict{Node,Union{Node,Nothing}}, startnode::Node, endnode::Node) where {Node}
    if endnode ∉ keys(bfs_result)
        return []  # no path found
    end
    path = [endnode]
    # walk backwards through parents of each node
    while (parent = bfs_result[last(path)]) !== nothing
        append!(path, [parent])
        if parent == startnode
            break
        end
    end
    reverse(path)
end
export shortestpath


end # module Graphs
