def manhattan(p1, p2):
    """
    manhattan distance between two n-D points of any iterable type
    """
    return sum([abs(x2 - x1) for x1, x2 in zip(p1, p2)])


class Vec:
    """
    n-dimensional vectors

        Vec(1,2) + Vec(3,4)
        Vec(1,2,3) - Vec(5,6,7)
        x, y = p1
        p1.x, p1.y

    """

    def __init__(self, *coords):
        self.coords = tuple(coords)

    def __eq__(self, other):
        if not isinstance(other, Vec):
            return False
        return self.coords == other.coords

    def __hash__(self):
        return hash(tuple(self.coords))

    @property
    def x(self):
        return self.coords[0]

    @property
    def y(self):
        return self.coords[1]

    def __iter__(self):
        return iter(self.coords)

    def __add__(self, other):
        return Vec(*[x1+x2 for x1, x2 in zip(self, other)])

    def __sub__(self, other):
        return Vec(*[x1-x2 for x1, x2 in zip(self, other)])

    def __neg__(self):
        return Vec(*[-c for c in self.coords])

    def __rmul__(self, other):
        if isinstance(other, Vec):
            return Vec(*[x1 * x2 for x1, x2 in zip(self, other)])
        return Vec(*[other * x for x in self.coords])

    def __repr__(self):
        return f"Vec({','.join([str(x) for x in self])})"

    def __lt__(self, other):
        # needed to be pushed to a priority queue
        if not isinstance(other, Vec):
            raise NotImplementedError()
        return self.abs2() < other.abs2()

    def abs2(self):
        """squared euclidean distance"""
        return sum([x**2 for x in self])

    def dot(self, other):
        return sum([x1 * x2 for x1, x2 in zip(self, other)])

    def dist2(self, other):
        """pythagorean distance squared"""
        return sum([(x1 - x2)**2 for x1, x2 in zip(self, other)])


class Direction:
    """
    collection of discrete 2D direction vectors in a downwards-pointing coordinate system

        ------> +x
        |
        |
        v  +y

    """
    N = Vec(0, -1)
    NE = Vec(1, -1)
    E = Vec(1, 0)
    SE = Vec(1, 1)
    S = Vec(0, 1)
    SW = Vec(-1, 1)
    W = Vec(-1, 0)
    NW  = Vec(-1, -1)


def neighbours4(p: Vec) -> list[Vec]:
    """
    get the 4 horizontally and vertically adjacent points
    """
    return [p + d for d in [Direction.N, Direction.E, Direction.S, Direction.W]]


def neighbours8(p: Vec) -> list[Vec]:
    """
    get all 8 adjacent points
    """
    return [
        p + d for d in [
            Direction.N,
            Direction.NE,
            Direction.E,
            Direction.SE,
            Direction.S,
            Direction.SW,
            Direction.W,
            Direction.NW,
        ]
    ]