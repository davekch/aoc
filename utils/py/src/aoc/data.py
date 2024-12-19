from abc import ABC, abstractmethod
from typing import Iterable
from numbers import Number
from typing import TypeVar, Generic


Node = TypeVar("Node")


class GraphABC(ABC, Generic[Node]):
    def __init__(self, graph):
        self.graph = graph

    @abstractmethod
    def neighbours(self, node: Node) -> Iterable[Node]:
        ...


class WeightedGraphABC(GraphABC, Generic[Node]):
    @abstractmethod
    def distance(self, node1: Node, node2: Node) -> Number:
        ...
