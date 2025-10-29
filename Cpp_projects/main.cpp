// main.cpp - simple C++ project entry point
// Build: g++ -std=c++17 -O2 -Wall -Wextra -o main main.cpp
// Or with CMake: create a CMakeLists.txt and build with cmake/Make

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

    int max_MC_iterations = 10000000;
    int max_depth = 15;
    if (max_depth > input_board.holes_count()) {
        max_depth = input_board.holes_count();
    }

    int win_count = 0;
    std::vector<int> win_depths(max_depth);
    std::vector<int> games_num(max_depth+1);
    std::fill(win_depths.begin(), win_depths.end(), 0);
    std::fill(games_num.begin(), games_num.end(), 0);
    games_num[0] = 1;
    Board sim_board;
    

    

    
    for (int MC_sim = 0; MC_sim < max_MC_iterations; ++MC_sim) {
        sim_board.reset();
        sim_board.copy_from(input_board);
        // Here you would implement the Monte Carlo simulation logic
        // For example, randomly play moves up to max_depth and evaluate the outcome
   
        for (int depth = 0; depth < max_depth; ++depth) {
            // Randomly select a piece and position from available one
            sim_board.add_one_random();
            sim_board.win_check();
            games_num[depth+1]++;
            if (sim_board.winning_status()) {
                win_depths[depth]++;
                win_count++;
                break; // Stop simulation on win
            }
            
        }
        //std::cout << "Simulation " << (MC_sim + 1) << " result:\n"; 
        //sim_board.print_the_board();
        //std::cout << '\n';
    }
    
    
    std::cout << "Monte Carlo simulations completed: " << max_MC_iterations << '\n';
    std::cout << "Total wins detected: " << win_count << '\n';
    std::cout << "Win distribution by depth:\n";
    std::cout << "--------------------------------------------------------------------------------------------------------\n";
    std::cout << "| Depth | Wins | Games Played | Win Probability | Non-win Probability | Cumulative Non-win Probability |\n";
    std::cout << "--------------------------------------------------------------------------------------------------------\n";
    std::cout << "|   0   |  " << "- " << "  |     " << games_num[0] << "      |       " << "N/A" << "       |        " << "N/A" << "        |            " << "N/A" << "                |\n";
    
    
    double prob_n ;
    double anti_prob_n ;
    double prob_prod = 1.0;

    // helper to center a string into a fixed width (truncates if too long)
    auto center = [](const std::string &s, int width) {
        if ((int)s.size() >= width) return s.substr(0, width);
        int pad = width - (int)s.size();
        int left = pad / 2;
        int right = pad - left;
        return std::string(left, ' ') + s + std::string(right, ' ');
    };

    for (int d = 0; d < max_depth; ++d) {
        prob_n = 100.0 * win_depths[d] / static_cast<double>(games_num[d + 1]);
        anti_prob_n = 100.0 - prob_n;
        prob_prod *= (anti_prob_n / 100.0);

        // prepare percent strings with 2 decimal places
        auto fmt_percent = [&](double v){
            std::ostringstream oss;
            oss << std::fixed << std::setprecision(int(log10(max_MC_iterations))) << v << '%';
            return oss.str();
        };

        std::string depth_s = std::to_string(d + 1);
        std::string wins_s = std::to_string(win_depths[d]);
        std::string games_s = std::to_string(games_num[d + 1]);
        std::string prob_s = fmt_percent(prob_n);
        std::string anti_prob_s = fmt_percent(anti_prob_n);
        std::string cum_s = fmt_percent(prob_prod * 100.0);

        // column widths match the header: 7,6,13,16,20,33
        std::cout << "|"
                  << center(depth_s, 7)  << "|"
                  << center(wins_s, 6)   << "|"
                  << center(games_s, 13) << "|"
                  << center(prob_s, 16)  << "|"
                  << center(anti_prob_s, 20) << "|"
                  << center(cum_s, 33)   << "|\n";
    }
    
    std::cout << "--------------------------------------------------------------------------------------------------------\n";
    std::cout << '\n';
    std::cout << "Cumulative estimated win probability: " << (static_cast<double>(100*win_count) / max_MC_iterations) << '\n';

 
    std::cout << '\n' << "Probability of hitting a draw situation in a full game: " <<  ((prob_prod) * 100.0) << "%" << '\n';

    
    return 0;
}