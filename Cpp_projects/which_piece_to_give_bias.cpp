// THIS CODE WILL USE A SIMILAR SCRITP TO check_one_piece_biased.cpp,
// TRYING ALL THE AVAILABLE HOLES FOR A GIVEN PIECE,
// AND FOR EACH HOLE, RUN A MONTE CARLO SIMULATION
// WITH A BIASED STRATEGY TO PREFER PLACING PIECES
// CLOSER TO WINNING LINES,
// TO ESTIMATE THE WIN PROBABILITY
// AND THE NET WIN PROBABILITY (WINS - OPPONENT WINS) // FOR EACH HOLE,
// THEN RANK THE HOLES BASED ON THESE METRICS




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
    
    

    std::vector<std::vector<double>> Stats(input_board.holes_count(), std::vector<double> (input_board.holes_count()+2, 0.0));
    std::vector<std::vector<double>> the_best(input_board.holes_count(), std::vector<double> (input_board.holes_count()+2, 0.0));
    
   
    
    
    std::vector<std::vector<double>> Results(4, std::vector<double> (input_board.holes_count()));

    for (int p = 0; p < input_board.holes_count(); p++){
        // EXTRACT ONE AVAILABLE PIECE AS A TRIAL PIECE
        int piece_id = input_board.get_available_piece_id(p);
        std::cout << "Selected piece ID: " << input_board.piece_id_to_string(piece_id) << " (" << piece_id << ")\n";

        // PUT THIS PIECE IN ANY AVAILABLE HOLE AND RUN MONTE CARLO SIMULATIONS
        for (int h=0; h < input_board.holes_count(); ++h) {
            // GET THE COORDINATES OF THE h-th AVAILABLE HOLE
            x_avail = input_board.get_hole_coordinate(h, 0);
            y_avail = input_board.get_hole_coordinate(h, 1);

            // COPY THE INPUT BOARD TO A WORKING BOARD
            copy_board.reset();
            copy_board.copy_from(input_board);

            // ADD THE TRIAL PIECE TO THE WORKING BOARD AT THE SELECTED HOLE
            copy_board.add_one(piece_id, x_avail, y_avail);

            copy_board.win_check();
            //copy_board.print_the_board();
            std::cout << '\n';

            if (copy_board.winning_status()) {
                std::cout << "DO NOT GIVE THE PIECE " << input_board.piece_id_to_string(piece_id)
                 << ", IT'LL WIN IN (" << x_avail << ", " << y_avail << ")\n";
            
                int col = p + 2;
                if (!Stats.empty() && col < static_cast<int>(Stats[0].size())) {
                    for (size_t r = 0; r < Stats.size(); ++r) {
                        Stats[r][col] = -1.0;
                        Results[3][h] = 1.0;
                    }
                }
                break;
            }


            int win_count = 0;
            int win_count_player = 0;
            for (int MC_sim = 0; MC_sim < max_MC_iterations; ++MC_sim) {
                // COPY THE NEW BOARD TO THE SIMULATION BOARD
                sim_board.reset();
                sim_board.copy_from(copy_board);
                
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

            Stats[h][p+2] = Results[3][h];

        }


        // find index of minimum in Results[3][:]
        int best_idx = -1;
        double best_val = 1e300; // large initial value

        for (size_t i = 0; i < Results[3].size(); ++i) {
            double v = Results[3][i];
            if (!(v == v)) continue; // skip NaN
            if (v < best_val) {
                best_val = v;
                best_idx = static_cast<int>(i);
            }
        }

        if (best_idx == -1) {
            std::cout << "No valid values found in Results[3]\n";
        } else {
            the_best[best_idx][p + 2] = -best_val;
            std::cout << "Best hole for piece " << input_board.piece_id_to_string(piece_id)
             << " is at (" << Results[0][best_idx] << ", " << Results[1][best_idx]
              << ") with net win probability " << -best_val << "\n";
        }
    }

for (size_t i = 0; i < input_board.holes_count(); ++i) {
        Stats[i][0] = Results[0][i]; // x coordinate
        Stats[i][1] = Results[1][i]; // y coordinate
        the_best[i][0] = Results[0][i]; // x coordinate
        the_best[i][1] = Results[1][i]; // y coordinate
    }


    // Print the results
    std::cout << std::fixed << std::setprecision(2);

    std::cout << '\n';
    std::cout << '\n';
    // PRINT THE WINNING PROBABILITY TABLE
    std::cout << "(x)" << '\t' << "(y)" << '\t';

    for (size_t i = 0; i < input_board.holes_count(); ++i) {
        std::cout << '\t' << input_board.piece_id_to_string(input_board.get_available_piece_id(i))<< '\t';
    }
    std::cout << '\n';

    for (size_t r = 0; r < Stats.size(); ++r) {
        for (size_t c = 0; c < Stats[r].size(); ++c) {
            if (c == 0 || c == 1)
                std::cout << static_cast<int>(Stats[r][c]) << '\t';
            else{
            std::cout << '\t' << 100*Stats[r][c] << '\t';
            }
        }
        std::cout << '\n';
    }
    // Under each we pring the average score for each piece
    std::vector <double> col_avgs(input_board.holes_count(), 0.0);
    std::cout << "------------------------------------------------------------------------------------------------------------------------------------------------------------------\n";
    std::cout << "AVRG:" << '\t' << '\t';
    for (size_t c = 2; c < Stats[0].size(); ++c) {
        double col_sum = 0.0;
        int valid_count = 0;
        for (size_t r = 0; r < Stats.size(); ++r) {
            double v = Stats[r][c];
            if (v == v) { // skip NaN
                col_sum += v;
                valid_count++;
            }
        }
        double col_avg = (valid_count > 0) ? (col_sum / valid_count) : 0.0;
        col_avgs[c-2] = col_avg;
        std::cout << '\t' << 100*col_avg << '\t';
    }   
    std::cout << '\n';
    std::cout << "------------------------------------------------------------------------------------------------------------------------------------------------------------------\n";


    



    std::cout << '\n';
    std::cout << '\n';
    //// PRINT THE BEST NET WIN PROBABILITY TABLE
    //std::cout << "(x)" << '\t' << "(y)" << '\t';

    std::cout << "============================= Average Net Win Probability per Piece =============================\n";
    for (size_t i = 0; i < input_board.holes_count(); ++i) {
        if (col_avgs[i] == -1) continue;
        std::cout  << "> " << input_board.piece_id_to_string(input_board.get_available_piece_id(i))<< ":    " << 100*col_avgs[i] << "%" << '\n';
    }
    std::cout << "=================================================================================================\n";
    


    
    input_board.print_the_board();

    input_board.win_check(true);


    return 0;
}