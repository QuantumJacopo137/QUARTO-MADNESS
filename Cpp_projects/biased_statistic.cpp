// THIS CODE WILL RUN MONTE CARLO SIMULATIONS TO ASSESS WINNING PROBABILITIES
// BASED ON A GIVEN BOARD STATE INPUT FROM "board.txt" FILE
// THE MC SIMULATION IS DONE BY EXTRACTING RANDOM PIECES AND PLACING THEM
// AT RANDOM AVAILABLE POSITIONS, UP TO A MAXIMUM DEPTH OR UNTIL A WIN
// OR DRAW IS REACHED.

// THE BIAS IN THIS CASE COMES FROM THE FACT THAT WHEN IS PLACING A RANDOM PIECE,
// IT PREFERS POSITIONS THAT CONTRIBUTE TO LINES WITH MORE FILLED SLOTS,
// SO THAT IF IT HAS A CHANCE TO WIN, IT WILL TAKE IT.



#include <iostream>
#include <iomanip>
#include <sstream>
#include "board_class.h"

int main(int argc, char* argv[]) {
    if (argc > 1) {
        std::cout << "Arguments:";
        for (int i = 1; i < argc; ++i) std::cout << ' ' << argv[i];
        std::cout << '\n';
    }


    // create a board instance
    Board input_board;
    input_board.read_from_file("board.txt");

    int max_MC_iterations = 100000000;
    int max_depth = 15;
    if (max_depth > input_board.holes_count()) {
        max_depth = input_board.holes_count();
    }
    std::random_device rd;  // Hardware random number generator
    std::mt19937 gen(rd()); // Mersenne Twister generator

    srand(time(NULL));

    //std::cout << rd() << '\n';
    //std::cout << gen() << '\n';
    //std::cout << rand()%16 << '\n';

    

    //input_board.print_empty_slots();

    
    //input_board.print_the_board();
    

    //input_board.win_check();   
    //input_board.print_verbose();
    


    //std::cout << "NEW COPY ********************\n";



    Board copy_board;

    //copy_board.copy_from(input_board);

    std::vector<int> win_counts(max_depth, 0);
    int draws = 0;


   
    for (int mc = 0; mc < max_MC_iterations; ++mc){
        copy_board.reset();
        copy_board.copy_from(input_board);
        for (int iter = 0; iter < max_depth; ++iter){
            copy_board.add_one_random_biased();
            //copy_board.print_the_board();
            //copy_board.print_empty_slots();
            copy_board.win_check();
    

        if (copy_board.winning_status()){
            //copy_board.print_the_board();

            //std::cout << "We have a winner!\n";

            win_counts[iter]++;
            break;

        }


        if (copy_board.holes_count()==0){
            draws++;
            break;
        } 

        if (copy_board.holes_count()==0){
            std::cout << "Draw game, no more holes left after " << (mc+1) << " games.\n";
            //copy_board.print_verbose();
            copy_board.print_the_board();
            copy_board.win_check(true);
            std::cout << "Winning status at end: " << (copy_board.winning_status() ? "YES" : "NO") << '\n';
            return 0;   
        }
    }
    }   


    std::cout << "Win statistics over " << max_MC_iterations << " Monte Carlo simulations:\n";
    for (int d = 0; d < max_depth; ++d){
        std::cout << "Wins at depth " << (d+1) << ": " << win_counts[d] << " (" << std::fixed << std::setprecision(log10(max_MC_iterations)-2) << (static_cast<double>(win_counts[d]) / max_MC_iterations * 100.0) << "%)\n";
    }
    std::cout << "-------------------------\n";
    std::cout << "Draws: " << draws << " (" << std::fixed << std::setprecision(log10(max_MC_iterations)-2) << (static_cast<double>(draws) / max_MC_iterations * 100.0) << "%)\n";

    return 0;
}