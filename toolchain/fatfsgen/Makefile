# Makefile for fatfsgen

CXX  = g++
CXXFLAGS = -Wall -O2

fatfsgen : fatfsgen.o
	$(CXX) fatfsgen.o -o fatfsgen

fatfsgen.o : fatfsgen.cpp casefold.h
	$(CXX) $(CXXFLAGS) -c fatfsgen.cpp -o fatfsgen.o

casefold.h : make-table.rb CaseFolding.txt
	./make-table.rb >casefold.h

clean:
	rm -f *.o fatfsgen
