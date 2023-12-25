module Graphs

using DataStructures


"""
return a list of neighbors for a node in a graph
"""
function neighbours(graph, node::Node) where {Node} end

"""
perform breadth first search on a graph, returning
a dict mapping nodes to their parents
"""
function BFS(graph, startnode::Node, endnode::Node) where {Node}
    q = Queue{Node}()
    enqueue!(q, startnode)
    path::Dict{Node,Union{Node,Nothing}} = Dict()
    path[startnode] = nothing
    while !isempty(q)
        v = dequeue!(q)
        if v == endnode
            return path
        end
        for n ∈ neighbours(graph, v)
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


"""
find all possible paths from start to finish in a graph
"""
function possible_paths(grid, start::Node, finish::Node) where {Node}
    paths = Vector{Node}[]
    q = Queue{Vector{Node}}()
    enqueue!(q, [start])
    while !isempty(q)
        current_path = dequeue!(q)
        current = last(current_path)
        # println(length(q))
        while current != finish
            ns = neighbours(grid, current) |> filter(n->n∉current_path)
            if length(ns) == 0
                break
            else
                current, others... = ns
                for n in others
                    enqueue!(q, vcat(current_path, [n]))
                end
                push!(current_path, current)
            end
        end
        # check if we actually made it to the end
        if last(current_path) == finish
            push!(paths, current_path)
        end
    end
    paths
end
export possible_paths


end # module Graphs
