// THIS IS THE SAME AS check_one_piece.cpp,
// BUT INSTEAD OF RANDOMLY PLACING THE NEXT PIECE,
// IT USES A BIASED STRATEGY TO PREFER POSITIONS
// THAT ARE CLOSER TO WINNING LINES,
// THUS INCREASING THE CHANCES OF WINNING
// OR BLOCKING THE OPPONENT FROM WINNING   



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

     // select a random piece from available pieces
    std::random_device rd;  // Hardware random number generator
    std::mt19937 gen(rd()); //


    int max_MC_iterations = 10000;
    int max_depth = 15;
    if (max_depth > input_board.holes_count()) {
        max_depth = input_board.holes_count();
    }


    std::string three_chars;
    std::vector<std::string> possible_pieces = {"Bsh", "Bsf", "Bch", "Bcf",
                                                "bsh", "bsf", "bch", "bcf",
                                                "Wsh", "Wsf", "Wch", "Wcf",
                                                "wsh", "wsf", "wch", "wcf"};
    // prompt for a 3-char piece code, crop/pad and validate against possible_pieces
    while (true) {
        std::cout << '\n' << "Enter piece code (3 chars, e.g. Bsh): ";
        std::string line;
        if (!std::getline(std::cin, line)) {
            std::cerr << "No input; aborting.\n";
            std::exit(1);
        }

        // crop to length 3
        if (line.size() < 3) {
            std::cout << "Input too short (need 3 characters). Try again.\n" << '\n';
            continue;
        }
        three_chars = line.substr(0, 3);

        // check legitimacy against the allowed piece codes
        bool legit = false;
        legit = (std::find(possible_pieces.begin(), possible_pieces.end(), three_chars) != possible_pieces.end());
        if (!legit) {
            std::cout << "Invalid piece code \"" << three_chars << "\". Allowed codes are:";
            for (const auto &p : possible_pieces) std::cout << ' ' << p;
            std::cout << "\nPlease try again.\n" << '\n';
            continue;
        }

        // basic availability check: ensure there's at least one empty hole on the board
        // (If your Board class exposes a more specific availability API, replace this check.)
        if (input_board.holes_count() <= 0) {
            std::cout << "No available holes on the board to place pieces. Choose another board or exit.\n";
            std::exit(1);
        }

        bool is_avail = input_board.is_available_piece(three_chars);
        if (!is_avail) {
            std::cout << "Piece " << three_chars << " is not available to play. Choose another piece.\n" << '\n';
            continue;
        }

        // accepted
        std::cout << "Selected piece: " << three_chars << '\n';
        break;
    }




    Board sim_board;
    Board copy_board;
    int x_avail;
    int y_avail;



    std::vector<int> win_depths(max_depth);
    std::vector<int> games_num(max_depth+1);
    std::fill(win_depths.begin(), win_depths.end(), 0);
    std::fill(games_num.begin(), games_num.end(), 0);
    games_num[0] = 1;
    //Board sim_board;
    
    

   
    
    
    std::vector<std::vector<double>> Results(4, std::vector<double> (input_board.holes_count()));

    for (int h=0; h < input_board.holes_count(); ++h) {
        x_avail = input_board.get_hole_coordinate(h, 0);
        y_avail = input_board.get_hole_coordinate(h, 1);

        copy_board.reset();
        copy_board.copy_from(input_board);

        copy_board.add_one(input_board.piece_string_to_id(three_chars), x_avail, y_avail);

        copy_board.win_check();
        //copy_board.print_the_board();
        std::cout << '\n';

        if (copy_board.winning_status()) {
            std::cout << "Immediate win detected by placing " << three_chars << " at (" << x_avail << ", " << y_avail << ")\n";
            Results[0][h] = x_avail;
            Results[1][h] = y_avail;
            Results[2][h] = 1.0;
            Results[3][h] = 1.0;
            continue;
        }


        int win_count = 0;
        int win_count_player = 0;
        for (int MC_sim = 0; MC_sim < max_MC_iterations; ++MC_sim) {
        sim_board.reset();
        sim_board.copy_from(copy_board);
        // Here you would implement the Monte Carlo simulation logic
        // For example, randomly play moves up to max_depth and evaluate the outcome
   
        for (int depth = 0; depth < max_depth-1; ++depth) {
            // Randomly select a piece and position from available one
            sim_board.add_one_random_biased();
            sim_board.win_check();
            games_num[depth+1]++;
            if (sim_board.winning_status()) {
                win_depths[depth]++;
                win_count++;
                if (depth % 2 == 0) {
                    win_count_player++;
                }
                else {
                    win_count_player--;
                }
                break; // Stop simulation on win
            }
            
        }
    }
        Results[0][h] = x_avail;
        Results[1][h] = y_avail;
        Results[2][h] = double(win_count) / double(max_MC_iterations);
        Results[3][h] = double(win_count_player) / double(win_count);
        //std::cout << '\n'<< "Wins = " << win_count << " out of " << max_MC_iterations << " simulations.\n"; 
    }


    //for (int i=0; i < input_board.holes_count(); ++i) {
    //    std::cout << "Position (" << int(Results[0][i]) << ", " << int(Results[1][i]) << ") -> Win Probability: " << Results[2][i]*100.0 << "%" << '\n';
    //}

    std::cout << '\n';

    std::cout << "================================ Ranked Positions ===============================\n";
    std::cout << "|                                    " << three_chars << "                                        |\n";
    std::cout << "---------------------------------------------------------------------------------\n";
    {
        // sort indices by Results[2][i] descending and print
        int n = input_board.holes_count();
        std::vector<int> idx(n);
        for (int i = 0; i < n; ++i) idx[i] = i;

        std::sort(idx.begin(), idx.end(), [&](int a, int b) {
            return Results[3][a] > Results[3][b];
        });

        
        for (int rank = 0; rank < n; ++rank) {
            int i = idx[rank];
            double pct = Results[2][i] * 100.0;
            double wpp = Results[3][i] * 100.0;
            std::cout << (rank + 1) << "]" <<'\t' << "Position (" << int(Results[0][i]) << ", " << int(Results[1][i])
                      << ")   " << '\t' <<  "-> Net_win/tot_win: "   << std::fixed << std::setprecision(int(log10(max_MC_iterations))-2) << wpp << "%" << '\t' << "Draw prob: " << 100-pct << "%" << '\n';
        }
    }


    
    return 0;
}