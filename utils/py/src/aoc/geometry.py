def manhattan(p1, p2):
    """
    manhattan distance between two n-D points of any iterable type
    """
    return sum([abs(x2 - x1) for x1, x2 in zip(p1, p2)])
