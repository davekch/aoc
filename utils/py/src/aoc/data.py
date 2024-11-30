from abc import ABC, abstractmethod
from typing import Iterable
from numbers import Number


class GraphABC[Node](ABC):
    @abstractmethod
    def neighbours(self, node: Node) -> Iterable[Node]:
        ...


class WeightedGraphABC[Node](GraphABC, ABC):
    @abstractmethod
    def distance(self, node1: Node, node2: Node) -> Number:
        ...
