from queue import Queue, PriorityQueue
from collections import defaultdict

from aoc.data import GraphABC, WeightedGraphABC


def BFS[Node](graph: GraphABC[Node], start: Node, finished=None, visualize=None) -> dict[Node, Node | None]:
    """breadth first search on graph.
    optionally provide a function `finished` that terminates the search
    once it returns true when called with the current node
    """
    queue = Queue()
    queue.put(start)
    path = {start: None}
    while not queue.empty():
        v = queue.get()

        if visualize is not None:
            visualize(graph, v)

        if finished is not None and finished(v):
            return path

        for n in graph.neighbours(v):
            if n not in path:
                path[n] = v
                queue.put(n)
    return path


def shortestpath(bfs_result, start, end) -> list:
    """takes a dict as returned from BFS and returns the
    shortest path from start to end"""
    if end not in bfs_result:
        return []
    path = [end]
    while parent:=bfs_result[path[-1]]:
        path.append(parent)
        if parent == start:
            break
    return list(reversed(path))


def dijkstra[Node](graph: WeightedGraphABC[Node], start: Node, finished=None, visualize=None):
    """
    runs dijkstra on graph
    returns last node visited, node -> distance to start dict, node -> node path dict
    """
    queue = PriorityQueue()
    queue.put((0, start))
    distances = defaultdict(lambda: 1000000000)
    distances[start] = 0
    path = {start: None}
    while not queue.empty():
        _, current = queue.get()

        if visualize is not None:
            visualize(graph, path, start, current)

        if finished is not None and finished(current):
            return current, distances, path
        for n in graph.neighbours(current):
            if n in path:
                continue
            d = distances[current] + graph.distance(current, n)
            if d < distances[n]:
                distances[n] = d
                path[n] = current
                queue.put((d, n))
    return current, distances, path
