// main.cpp - simple C++ project entry point
// Build: g++ -std=c++17 -O2 -Wall -Wextra -o main main.cpp
// Or with CMake: create a CMakeLists.txt and build with cmake/Make

#include <iostream>
#include "board_class.h"

int main(int argc, char* argv[]) {
    if (argc > 1) {
        std::cout << "Arguments:";
        for (int i = 1; i < argc; ++i) std::cout << ' ' << argv[i];
        std::cout << '\n';
    }


    // create a board instance
    Board board;
    board.print();
    board.printAvailablePieces();
    board.printAvailablePositions();

    board.add_one_random();
    board.print_verbose();




    return 0;
}