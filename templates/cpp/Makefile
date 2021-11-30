CXX := g++
UTIL_DIR = ../utils/cpp
INC_DIR = ../utils/cpp
SRC = Main.cpp Solver.cpp $(UTIL_DIR)/Utils.cpp
OBJ = $(SRC:.cpp = .o)
CFLAGS = -I$(UTIL_DIR) -I$(INC_DIR) -O


all: test main run clean
	
run: main
	./main

main: $(OBJ)
	$(CXX) $(CFLAGS) -o $@ $(OBJ)
	
test: runner
	./runner

runner: runner.cpp Solver.cpp $(UTIL_DIR)/Utils.cpp
	$(CXX) $(CFLAGS) -o $@ $^

runner.cpp: SolverTestSuite.hpp $(INC_DIR)/SolverTemplate.hpp
	cxxtestgen -o $@ --error-printer $^

clean:
	rm runner.cpp runner main
