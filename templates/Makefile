CXX := g++
UTIL_DIR = ../utils
SRC = Solver.cpp $(UTIL_DIR)/Utils.cpp
OBJ = $(SRC:.cpp = .o)
CFLAGS = -I$(UTIL_DIR) -O


all: test solver run clean
	
run: solver
	./solver

solver: $(OBJ)
	$(CXX) $(CFLAGS) -o $@ $(OBJ)
	
test: runner
	./runner

runner: runner.cpp Solver.hpp $(UTIL_DIR)/Utils.cpp
	$(CXX) $(CFLAGS) -o $@ $^

runner.cpp: SolverTestSuite.hpp
	cxxtestgen -o $@ --error-printer $^

clean:
	rm runner.cpp runner
