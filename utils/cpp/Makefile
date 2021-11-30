CXX := g++

all: test clean
	
test: runner
	./runner

runner: runner.cpp Utils.cpp Utils.hpp
	$(CXX) -o $@ $^

runner.cpp: UtilsTestSuite.hpp
	cxxtestgen -o $@ --error-printer $^

clean:
	rm runner.cpp runner
