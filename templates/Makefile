CXX := g++


all: test solver run clean
	
run: solver
	./solver

solver: Solver.cpp
	$(CXX) -o solver Solver.cpp
	
test: runner
	./runner

runner: runner.cpp
	$(CXX) -o $@ $<

runner.cpp: SolverTestSuite.hpp
	cxxtestgen -o $@ --error-printer $^

clean:
	rm runner.cpp runner
