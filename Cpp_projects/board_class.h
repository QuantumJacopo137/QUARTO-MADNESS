#pragma once

#include <array>
#include <algorithm>
#include <stdexcept>
#include <iostream>
#include <iomanip>
#include <numeric> // for std::iota
#include <vector>
#include <cstdio>
#include <bitset>
#include <random>

class Board {
public:
    static constexpr int Rows = 16;
    static constexpr int Cols = 2;
    using Matrix = std::array<std::array<int, Cols>, Rows>;

    // default: all zeros, not winning, available_pieces 0..15
    Board() : board_matrix_(), winning_status_(false) {
        for (auto &row : board_matrix_) row.fill(0);
        available_pieces_.resize(Rows);
        std::iota(available_pieces_.begin(), available_pieces_.end(), 0);
        initializeAvailablePositions();
        empty_slots_.resize(10, 4); // 4 empty slots for each of the 10 win lines
    }

    // copy attributes from another Board instance
    void copy_from(const Board &other) {
        // validate incoming matrix before copying
        validateMatrix(other.board_matrix_);
        board_matrix_ = other.board_matrix_;
        available_positions_ = other.available_positions_;
        available_pieces_ = other.available_pieces_;
        winning_status_ = other.winning_status_;
        empty_slots_ = other.empty_slots_;
    }


    // initialize available positions
    void initializeAvailablePositions() {
        // it needs to be (-2,-2) to (+2,+2), skipping the (0,0) position
        
        for (int x = -2; x <= 2; ++x) {
            for (int y = -2; y <= 2; ++y) {
                if (x == 0 || y == 0) continue; // skip (0,0)
                available_positions_.push_back({x, y});
            }
        }
    }

    // construct from existing matrix (validates values)
    explicit Board(const Matrix &m, bool winning = false) : board_matrix_(m), winning_status_(winning) {
        validateMatrix(board_matrix_);
        available_pieces_.resize(Rows);
        std::iota(available_pieces_.begin(), available_pieces_.end(), 0);
    }

    // accessors
    int getCell(int r, int c) const {
        validateIndices(r, c);
        return board_matrix_.at(r).at(c);
    }

    int get_hole_coordinate(int index, int coord) const {
        validateIndices(index, 0);
        if (coord != 0 && coord != 1) throw std::out_of_range("Coordinate index must be 0 or 1");
        return available_positions_.at(index).at(coord);   
    }


    int piece_string_to_id(std::string token){
        std::unordered_map<std::string, int> dict;
        dict["Bsh"] = 0; // 0000 -> Black, Tall, Square, Hollow
        dict["Bsf"] = 1; // 0001 -> Black, Tall, Square, Filled
        dict["Bch"] = 2; // 0010 -> Black, Tall, Circle, Hollow
        dict["Bcf"] = 3; // 0011 -> Black, Tall, Circle, Filled
        dict["bsh"] = 4; // 0100 -> Black, Short, Square, Hollow
        dict["bsf"] = 5; // 0101 -> Black, Short, Square, Filled
        dict["bch"] = 6; // 0110 -> Black, Short, Circle, Hollow
        dict["bcf"] = 7; // 0111 -> Black, Short, Circle, Filled
        dict["Wsh"] = 8; // 1000 -> White, Tall, Square, Hollow
        dict["Wsf"] = 9; // 1001 -> White, Tall, Square, Filled
        dict["Wch"] = 10; // 1010 -> White, Tall, Circle, Hollow
        dict["Wcf"] = 11; // 1011 -> White, Tall, Circle, Filled
        dict["wsh"] = 12; // 1100 -> White, Short, Square, Hollow
        dict["wsf"] = 13; // 1101 -> White, Short, Square, Filled
        dict["wch"] = 14; // 1110 -> White, Short, Circle, Hollow
        dict["wcf"] = 15; // 1111 -> White, Short, Circle, Filled
        return dict[token];
    }

    const Matrix& board_matrix() const { return board_matrix_; }

     // read a 4x4 matrix of 3-char strings from a file in the current working directory
    // file format: 16 whitespace-separated tokens (row-major) or 4 tokens per line
    void read_from_file(const std::string &filename) {
        std::FILE *f = std::fopen(filename.c_str(), "r");
        if (!f) throw std::runtime_error("Failed to open file: " + filename);

        std::vector<std::vector<std::string>> file_matrix_(4, std::vector<std::string>(4));
        for (int r = 0; r < 4; ++r) {
            for (int c = 0; c < 4; ++c) {
                char buffer[4]; // 3 chars + null terminator
                if (std::fscanf(f, "%3s", buffer) != 1) {
                    std::fclose(f);
                    throw std::runtime_error("Failed to read token from file: " + filename);
                }
                file_matrix_[r][c] = std::string(buffer);
            }
        }
        
        std::fclose(f);


        

        std::unordered_map<std::string, int> dict;
        dict["Bsh"] = 0; // 0000 -> Black, Tall, Square, Hollow
        dict["Bsf"] = 1; // 0001 -> Black, Tall, Square, Filled
        dict["Bch"] = 2; // 0010 -> Black, Tall, Circle, Hollow
        dict["Bcf"] = 3; // 0011 -> Black, Tall, Circle, Filled
        dict["bsh"] = 4; // 0100 -> Black, Short, Square, Hollow
        dict["bsf"] = 5; // 0101 -> Black, Short, Square, Filled
        dict["bch"] = 6; // 0110 -> Black, Short, Circle, Hollow
        dict["bcf"] = 7; // 0111 -> Black, Short, Circle, Filled
        dict["Wsh"] = 8; // 1000 -> White, Tall, Square, Hollow
        dict["Wsf"] = 9; // 1001 -> White, Tall, Square, Filled
        dict["Wch"] = 10; // 1010 -> White, Tall, Circle, Hollow
        dict["Wcf"] = 11; // 1011 -> White, Tall, Circle, Filled
        dict["wsh"] = 12; // 1100 -> White, Short, Square, Hollow
        dict["wsf"] = 13; // 1101 -> White, Short, Square, Filled
        dict["wch"] = 14; // 1110 -> White, Short, Circle, Hollow
        dict["wcf"] = 15; // 1111 -> White, Short, Circle, Filled

        std::unordered_map<int, int> mat_index_to_boardmat_index;
        mat_index_to_boardmat_index[0] = +2;
        mat_index_to_boardmat_index[1] = +1;
        mat_index_to_boardmat_index[2] = -1;
        mat_index_to_boardmat_index[3] = -2;


        printf("File matrix loaded from %s:\n", filename.c_str());
        

        for (int i = 0; i < 4; ++i) {
            for (int j = 0; j < 4; ++j) {
                std::cout << file_matrix_[i][j] << '\t';
            }
            std::cout << '\n';
        }
        std::cout << '\n';


        this->reset();

        std::string piece;
        int piece_id;
        int x, y;

        for (int i = 0; i < 4; ++i) {
            for (int j = 0; j < 4; ++j) {
                //std::cout << "Processing cell (" << i << ", " << j << "): " << file_matrix_[i][j] << '\n';
                piece = file_matrix_[i][j]; // Put the string in a variable
                if (piece == "0") continue; // empty cell
                piece_id = dict[piece];     // map it to piece ID
                // Convert matrix indices to board coordinates (0,1,2,3)->(-2,-1,1,2)
                y = mat_index_to_boardmat_index[i]; 
                x = mat_index_to_boardmat_index[3-j];
           
                this->add_one(piece_id, x, y);
            }
        }
        
        winning_status_ = false; // reset winning status when loading from file

        std::cout << "In binary, the board is:\n";
        this->print_the_board();
        std::cout << '\n';
    }



    
    bool is_available_piece(std::string &token){
        std::unordered_map<std::string, int> dict;
        dict["Bsh"] = 0; // 0000 -> Black, Tall, Square, Hollow
        dict["Bsf"] = 1; // 0001 -> Black, Tall, Square, Filled
        dict["Bch"] = 2; // 0010 -> Black, Tall, Circle, Hollow
        dict["Bcf"] = 3; // 0011 -> Black, Tall, Circle, Filled
        dict["bsh"] = 4; // 0100 -> Black, Short, Square, Hollow
        dict["bsf"] = 5; // 0101 -> Black, Short, Square, Filled
        dict["bch"] = 6; // 0110 -> Black, Short, Circle, Hollow
        dict["bcf"] = 7; // 0111 -> Black, Short, Circle, Filled
        dict["Wsh"] = 8; // 1000 -> White, Tall, Square, Hollow
        dict["Wsf"] = 9; // 1001 -> White, Tall, Square, Filled
        dict["Wch"] = 10; // 1010 -> White, Tall, Circle, Hollow
        dict["Wcf"] = 11; // 1011 -> White, Tall, Circle, Filled
        dict["wsh"] = 12; // 1100 -> White, Short, Square, Hollow
        dict["wsf"] = 13; // 1101 -> White, Short, Square, Filled
        dict["wch"] = 14; // 1110 -> White, Short, Circle, Hollow
        dict["wcf"] = 15; // 1111 -> White, Short, Circle, Filled

        int piece_id = dict[token];
        if (std::find(available_pieces_.begin(), available_pieces_.end(), piece_id) != available_pieces_.end()) {
            return true;
        } else {
            return false;
        }

    }


    bool winning_status() const { return winning_status_; }
    void setWinningStatus(bool s) { winning_status_ = s; }

    // expose available pieces (0..15) - now dynamic size
    const std::vector<int>& available_pieces() const { return available_pieces_; }
    std::vector<int>& available_pieces() { return available_pieces_; }

    // mutators
    // returns true if value is valid and set, false otherwise
    bool setCell(int r, int c, int value) {
        if (!validValue(value)) return false;
        validateIndices(r, c);
        board_matrix_[r][c] = value;
        return true;
    }

    void reset() {
        for (auto &row : board_matrix_) row.fill(0);
        winning_status_ = false;
        available_pieces_.resize(Rows);
        std::iota(available_pieces_.begin(), available_pieces_.end(), 0);
        available_positions_.clear();
        initializeAvailablePositions();
    }

    void add_one(int piece, int x, int y) {
        
        // First we check that eveything is valid:

        // Validate if the piece id is between 0 and 15
        validateIndices(piece, 0);

        // Check that x and y are board coordinates (-2,-1,1,2)
         // validate values to be stored
        if (!validValue(x) || !validValue(y)){
            std::cerr << "Error: Invalid values (" << x << ", " << y << ") for row " << piece << std::endl;
            exit(EXIT_FAILURE);
        }
        //Check the x,y is an available positions
        auto pos_it = std::find_if(available_positions_.begin(), available_positions_.end(),
                                   [x, y](const std::array<int, 2>& pos) { return pos[0] == x && pos[1] == y; });
        if (pos_it == available_positions_.end()) {
            std::cerr << "Error: Position (" << x << ", " << y << ") is not available." << std::endl;
            exit(EXIT_FAILURE);
        }
        
        // ensure both columns in the target row are zero
        if (board_matrix_[piece][0] != 0 || board_matrix_[piece][1] != 0){
            std::cerr << "Error: Attempt to overwrite non-zero row " << piece << std::endl;
            exit(EXIT_FAILURE);
        }

       
        // NOW THAT EVERYTHING IS FINE, WE PROCEED BY UPDATING THE BOARD MATRIX
        board_matrix_[piece][0] = x;
        board_matrix_[piece][1] = y;

        // THEN WE REMOVE THE USED POSITION FROM AVAILABLE POSITIONS
        remove_piece_from_available(piece);
        remove_position_from_available(x, y);
        update_empty_slots(x, y);
    }



    // BE AWARE THAT FOR X: (-2,-1,1,2) THE COLUMN INDICES (j) ARE (0,1,2,3)
    // FOR Y: (-2,-1,1,2) THE ROW INDICES ARE (3,2,1,0)
    int from_board_coordinate_to_index(int coord, bool from_x_to_j) const {
        if (from_x_to_j) {
            switch (coord) {
                case -2: return 0;
                case -1: return 1;
                case  1: return 2;
                case  2: return 3;
                default: throw std::out_of_range("Coordinate must be in the range -2,-1,1,2");
            }
        }
        else {
        switch (coord) {
            case -2: return 3;
            case -1: return 2;
            case  1: return 1;
            case  2: return 0;
            default: throw std::out_of_range("Coordinate must be in the range -2,-1,1,2");
        }
        }
    }

    // BE AWARE THAT FOR X: (0,1,2,3) -> (-2,-1,1,2) j
    // AND FOR Y: (0,1,2,3) -> (2,1,-1,-2) i
    int from_index_to_board_coordinate(int index, bool from_j_to_x) const {
        if (from_j_to_x) {
            switch (index) {
                case 0: return -2;
                case 1: return -1;
                case 2: return  1;
                case 3: return  2;
                default: throw std::out_of_range("Index must be in the range 0..3");
            }
        }else {
        switch (index) {
            case 0: return  2;
            case 1: return  1;
            case 2: return -1;
            case 3: return -2;
            default: throw std::out_of_range("Index must be in the range 0..3");
            }
        }
}

    void update_empty_slots(int x, int y) {
    
// map board coordinates (-2,-1,1,2) to indices (0,1,2,3) and update empty_slots accordingly
        //std::cout << "Updating empty slots for position (" << x << ", " << y << ")\n";
        int j_index = from_board_coordinate_to_index(x, true);        
        int i_index = from_board_coordinate_to_index(y, false); 
        //std::cout << "Mapped to indices (" << i_index << ", " << j_index << ")\n";
        // decrement the appropriate counters for row, column and diagonals
        empty_slots_[i_index] -= 1;
        empty_slots_[j_index + 4] -= 1;
        if (x == -y) {
            empty_slots_[8] -= 1; // main diagonal
        }
        if (x == +y) {
            empty_slots_[9] -= 1; // anti diagonal
        }

    }

    void remove_position_from_available(int x, int y) {
        auto it = std::find_if(available_positions_.begin(), available_positions_.end(),
                               [x, y](const std::array<int, 2>& pos) { return pos[0] == x && pos[1] == y; });
        if (it != available_positions_.end()) {
            available_positions_.erase(it);
        } else {
            throw std::invalid_argument("Position not found in available positions");
        }
    }

    void add_one_random_biased(){
        if (available_pieces_.empty()) {
            std::cerr << "Error: No available pieces to add." << std::endl;
            exit(EXIT_FAILURE);
        }


        std::uniform_int_distribution<> dis(0, available_pieces_.size() - 1);
        //int piece = available_pieces_[dis(gen)];
        int piece = available_pieces_[rand() % available_pieces_.size()];

        //std::cout << "Piece selected randomly (biased): " << piece << " (" << std::bitset<4>(piece) << ")\n";


        std::bitset<4> piece_bits(piece);   

        std::vector<std::bitset<4>> close_to_winning_bits;
       
        bool height_flag = false;
        bool color_flag = false;
        bool shape_flag = false;
        bool fill_flag = false;

     


        for (int es_idx = 0; es_idx < empty_slots_.size(); ++es_idx){
            if (empty_slots_[es_idx]==1){
                // remove this line from available positions
                if (es_idx<4){
                    // row, this is the row index es_idx = i_index
                    int y_coord = from_index_to_board_coordinate(es_idx, false);
                    // now we have the board coordinate
                    for (int k=0; k<16; ++k){
                        if (board_matrix_[k][1]==y_coord){
                            close_to_winning_bits.push_back(std::bitset<4>(k));
                            // found a position in this row, remove it
                        }
                    }
                }else if (3<es_idx && es_idx<8){
                    // column
                    int x_coord = from_index_to_board_coordinate(es_idx-4, true);
                    // now we have the board coordinate
                    for (int k=0; k<16; ++k){
                        if (board_matrix_[k][0]==x_coord){
                            close_to_winning_bits.push_back(std::bitset<4>(k));
                            // found a position in this column, remove it
                        }
                    }
                }else if (es_idx==8){
                    for (int k=0; k<16; ++k){
                        if (board_matrix_[k][0]==board_matrix_[k][1]){
                            close_to_winning_bits.push_back(std::bitset<4>(k));
                            // found a position in this main diagonal, remove it
                        }
                    }
                    // main diagonal
                }else if (es_idx==9){
                    for (int k=0; k<16; ++k){
                        if (board_matrix_[k][0]==-board_matrix_[k][1]){
                            close_to_winning_bits.push_back(std::bitset<4>(k));
                            // found a position in this anti diagonal, remove it
                        }
                    }
                    // anti diagonal
                }

                //std::cout << "es_idx " << es_idx << " has 1 empty slot, close to winning piece bits collected so far:\n";
                //for (int cb_idx=0; cb_idx<close_to_winning_bits.size(); ++cb_idx){
                //    std::cout << "Close to winning piece bits " << cb_idx << ": " << close_to_winning_bits[cb_idx] << '\n';
                //}


        

                // Checking the 3 numbers in close_to_winning_bits to see which bits are common
                height_flag = (close_to_winning_bits[0][3] ^ close_to_winning_bits[1][3]);    // If they are the same number this will be 0
                height_flag = (close_to_winning_bits[0][3] ^ close_to_winning_bits[2][3]) || height_flag; // Only if all the 3 are the same it will be 0
         

                color_flag = close_to_winning_bits[0][2] ^ close_to_winning_bits[1][2];
                color_flag = (close_to_winning_bits[0][2] ^ close_to_winning_bits[2][2]) || color_flag;


                shape_flag = close_to_winning_bits[0][1] ^ close_to_winning_bits[1][1];
                shape_flag = (close_to_winning_bits[0][1] ^ close_to_winning_bits[2][1]) || shape_flag;

                fill_flag = close_to_winning_bits[0][0] ^ close_to_winning_bits[1][0];
                fill_flag = (close_to_winning_bits[0][0] ^ close_to_winning_bits[2][0]) || fill_flag;

                bool any_feature_matched = false;
                any_feature_matched = (!height_flag && (piece_bits[3] == close_to_winning_bits[0][3])) || any_feature_matched;
                any_feature_matched = (!color_flag && (piece_bits[2] == close_to_winning_bits[0][2])) || any_feature_matched;
                any_feature_matched = (!shape_flag && (piece_bits[1] == close_to_winning_bits[0][1])) || any_feature_matched;
                any_feature_matched = (!fill_flag && (piece_bits[0] == close_to_winning_bits[0][0])) || any_feature_matched;

                if (any_feature_matched){
                    // we found a piece that can help to win, we break the loop
                    int hole_x, hole_y;
                    if (es_idx<4){
                        hole_y = from_index_to_board_coordinate(es_idx, false);
                        for (int k=0; k<available_positions_.size(); ++k){
                            if (available_positions_[k][1]==hole_y){
                                hole_x = available_positions_[k][0];
                                add_one(piece, hole_x, hole_y);
                                //std::cout << "Biased placement at (" << hole_x << ", " << hole_y << ")\n";
                                //std::cout << "es_idx = " << es_idx << '\n';
                                //std::cout << "Height flag: " << !height_flag << ", Color flag: " << !color_flag << ", Shape flag: " << !shape_flag << ", Fill flag: " << !fill_flag << '\n';
                                //std::cout << "any_feature_flag: " << any_feature_matched << '\n';
                                return;
                            }
                        }
                    }else if (3<es_idx && es_idx<8){
                        hole_x = from_index_to_board_coordinate(es_idx-4, true);
                        for (int k=0; k<available_positions_.size(); ++k){
                            if (available_positions_[k][0]==hole_x){
                                hole_y = available_positions_[k][1];
                                add_one(piece, hole_x, hole_y);
                                //std::cout << "Biased placement at (" << hole_x << ", " << hole_y << ")\n";
                                //std::cout << "es_idx = " << es_idx << '\n';
                                //std::cout << "Height flag: " << !height_flag << ", Color flag: " << !color_flag << ", Shape flag: " << !shape_flag << ", Fill flag: " << !fill_flag << '\n';
                                //std::cout << "any_feature_flag: " << any_feature_matched << '\n';
                                return;
                            }
                        }
                    }else if (es_idx==8){
                        // main diagonal
                        for (int k=0; k<available_positions_.size(); ++k){
                            if (available_positions_[k][0]==available_positions_[k][1]){
                                hole_x = available_positions_[k][0];
                                hole_y = available_positions_[k][1];
                                add_one(piece, hole_x, hole_y);
                                //std::cout << "Biased placement at (" << hole_x << ", " << hole_y << ")\n";
                                //std::cout << "es_idx = " << es_idx << '\n';
                                //std::cout << "Height flag: " << !height_flag << ", Color flag: " << !color_flag << ", Shape flag: " << !shape_flag << ", Fill flag: " << !fill_flag << '\n';
                                //std::cout << "any_feature_flag: " << any_feature_matched << '\n';
                                return;
                            }
                        }   
                    }else if (es_idx==9){
                        // anti diagonal
                        for (int k=0; k<available_positions_.size(); ++k){
                            if (available_positions_[k][0]==-available_positions_[k][1]){
                                hole_x = available_positions_[k][0];
                                hole_y = available_positions_[k][1];
                                add_one(piece, hole_x, hole_y);
                                //std::cout << "Biased placement at (" << hole_x << ", " << hole_y << ")\n";
                                //std::cout << "es_idx = " << es_idx << '\n';
                                //std::cout << "Height flag: " << !height_flag << ", Color flag: " << !color_flag << ", Shape flag: " << !shape_flag << ", Fill flag: " << !fill_flag << '\n';
                                //std::cout << "any_feature_flag: " << any_feature_matched << '\n';
                                return;
                            }
                        }
                    }

                    break;
                }
                                
                // If any flag is false (0) it means that we have 3 pieces with that same feature
                


                close_to_winning_bits.clear();
            }
        }



        
        // generate a second random index for position
        std::uniform_int_distribution<> pos_dis(0, available_positions_.size() - 1);
        //int pos_index = pos_dis(gen);
        int pos_index = rand() % available_positions_.size();

        int x = available_positions_[pos_index][0];
        int y = available_positions_[pos_index][1];

        this->add_one(piece, x, y);
        /* // add the piece at the random position
        board_matrix_[piece][0] = x;
        board_matrix_[piece][1] = y;

        // remove the used position
        available_positions_.erase(available_positions_.begin() + pos_index);
        remove_piece_from_available(piece); */
    }


    void add_one_random(std::mt19937 &gen) {
        if (available_pieces_.empty()) {
            std::cerr << "Error: No available pieces to add." << std::endl;
            exit(EXIT_FAILURE);
        }
    
        //Mersenne Twister generator
        std::uniform_int_distribution<> dis(0, available_pieces_.size() - 1);
        int piece = available_pieces_[dis(gen)];

        // generate a second random index for position
        std::uniform_int_distribution<> pos_dis(0, available_positions_.size() - 1);
        int pos_index = pos_dis(gen);

        //std::cout << "Random index 1: " << dis(gen) << '\n'; // printing for debugging
        //std::cout << "Random index 2: " << pos_dis(gen) << '\n';
        // printing for debugging
        //printf("Adding piece %d at random position index %d\n", piece, pos_index);
        //printf("Position coordinates: (%d, %d)\n", available_positions_[pos_index][0], available_positions_[pos_index][1]);

        int x = available_positions_[pos_index][0];
        int y = available_positions_[pos_index][1];

        // add the piece at the random position
        add_one(piece, x, y);
        /* board_matrix_[piece][0] = x;
        board_matrix_[piece][1] = y;

        // remove the used position
        available_positions_.erase(available_positions_.begin() + pos_index);
        remove_piece_from_available(piece);
 */        
    }




    // CHECK THIS
    void remove_one(int piece) {
        // validate row index (will throw if out of range)
        validateIndices(piece, 0);
        board_matrix_[piece][0] = 0;
        board_matrix_[piece][1] = 0;
    }

    void remove_piece_from_available(int piece) {
        auto it = std::find(available_pieces_.begin(), available_pieces_.end(), piece);
        if (it != available_pieces_.end()) {
            available_pieces_.erase(it);
        } else {
            throw std::invalid_argument("Piece not found in available pieces");
        }
    }


    // Checks and action

    void win_check(bool verbose=false) {
        // Placeholder for win condition checking logic
        // Set winning_status_ = true if a win condition is met

        // Outline:
        // 1. Check rows, columns and diagonals: if they have a single empty slot skip to the next one
        // 2. If no empty slots, take the 4 numbers in the row/column/diagonal and convert them in 4 4-bit numbers
        // 3. Perform bitwise XOR across the 4 numbers; if the result is different than 15 (1111 in binary), set winning_status_ = true and return

        std::vector<int> possible_win_rows={};
        std::vector<int> possible_win_cols={};
        std::vector<int> possible_win_diags={};


        for (int i=0; i<empty_slots_.size(); ++i){
           if (empty_slots_[i]==0){
                if (i<4){
                    // row
                    possible_win_rows.push_back(from_index_to_board_coordinate(i, false));
                }else if (3<i && i<8){
                    // column
                    possible_win_cols.push_back(from_index_to_board_coordinate(i-4, true));
                }if (i==8){
                    // main diagonal
                    possible_win_diags.push_back(0);
                    
                }if (i==9){
                    // anti diagonal
                    possible_win_diags.push_back(1);
                }
           }
        }


        
        // Now we'll have a list of all the possible rows that we can check for a win
        check_row_win(possible_win_rows, verbose);
        if (winning_status_) return;
        check_col_win(possible_win_cols, verbose);
        if (winning_status_) return;
        check_diag_win(possible_win_diags, verbose);
    }


    void check_row_win(std::vector<int> &possible_win_rows, bool verbose) {
        // Now we'll check each possible winning row
        std::vector<std::bitset<4>> pieces_in_row;
        for (int r : possible_win_rows) {
            pieces_in_row.clear();
            // collect piece bitsets for all pieces placed in row r
            for (int idx = 0; idx < Rows; ++idx) {
                if (board_matrix_[idx][1] == r && board_matrix_[idx][0] != 0) {
                    pieces_in_row.push_back(std::bitset<4>(idx));
                    //printf("Piece %2d found in: %d, %d\n", idx, r, board_matrix_[idx][1]);
                }
            }

            // need exactly 4 pieces to evaluate a win
            if (pieces_in_row.size() != 4){
                std::cerr << "Error: Expected 4 pieces in row " << r << ", found " << pieces_in_row.size() << std::endl;
                exit(EXIT_FAILURE);
            }



            bool tmp = false;
            
            //std::cout << pieces_in_row[0] << '\n' << pieces_in_row[1] << '\n' << pieces_in_row[2] << '\n' << pieces_in_row[3] << '\n' <<'\n';


            for (int bit = 0; bit < 4; ++bit) {
                //std::cout << "Checking bit " << bit << " for row " << r << std::endl;
                tmp = pieces_in_row[0][bit] ^ pieces_in_row[1][bit];
                //std::cout <<  pieces_in_row[0][bit] << " ^ " << pieces_in_row[1][bit] << " = " << tmp << std::endl;
                if (tmp) continue;
                tmp = pieces_in_row[2][bit] ^ pieces_in_row[3][bit];
                //std::cout <<  pieces_in_row[2][bit] << " ^ " << pieces_in_row[3][bit] << " = " << tmp << std::endl;
                if (tmp) continue;
                tmp = pieces_in_row[0][bit] ^ pieces_in_row[2][bit];
                //std::cout <<  pieces_in_row[0][bit] << " ^ " << pieces_in_row[2][bit] << " = " << tmp << std::endl;
                if (tmp) continue;

                    
                winning_status_ = true;
                if (verbose) {
                    std::cout << "Winning condition met in row " << r << " on bit " << bit << '\n';
                }
                return;
            }
        }
    }

    void check_col_win(std::vector<int> &possible_win_cols, bool verbose) {
        // Now we'll check each possible winning column
      std::vector<std::bitset<4>> pieces_in_col;
        for (int c : possible_win_cols) {
            pieces_in_col.clear();
            // collect piece bitsets for all pieces placed in col c
            for (int idx = 0; idx < Rows; ++idx) {
                if (board_matrix_[idx][0] == c && board_matrix_[idx][1] != 0) {
                    pieces_in_col.push_back(std::bitset<4>(idx));
                    //printf("Piece %2d found in: %d, %d\n", idx, board_matrix_[idx][0], c);
                }
            }

            // need exactly 4 pieces to evaluate a win
            if (pieces_in_col.size() != 4){
                std::cerr << "Error: Expected 4 pieces in col " << c << ", found " << pieces_in_col.size() << std::endl;
                exit(EXIT_FAILURE);
            }



            bool tmp = false;

            //std::cout << pieces_in_col[0] << '\n' << pieces_in_col[1] << '\n' << pieces_in_col[2] << '\n' << pieces_in_col[3] << '\n' <<'\n';


            for (int bit = 0; bit < 4; ++bit) {
                //std::cout << "Checking bit " << bit << " for row " << r << std::endl;
                tmp = pieces_in_col[0][bit] ^ pieces_in_col[1][bit];
                //std::cout <<  pieces_in_col[0][bit] << " ^ " << pieces_in_col[1][bit] << " = " << tmp << std::endl;
                if (tmp) continue;
                tmp = pieces_in_col[2][bit] ^ pieces_in_col[3][bit];
                //std::cout <<  pieces_in_col[2][bit] << " ^ " << pieces_in_col[3][bit] << " = " << tmp << std::endl;
                if (tmp) continue;
                tmp = pieces_in_col[0][bit] ^ pieces_in_col[2][bit];
                //std::cout <<  pieces_in_col[0][bit] << " ^ " << pieces_in_col[2][bit] << " = " << tmp << std::endl;
                if (tmp) continue;

                    
                winning_status_ = true;
                if (verbose) {
                    std::cout << "Winning condition met in col " << c << " on bit " << bit << '\n';
                }
                return;
            }
        }
    }

    void check_diag_win(std::vector<int> &possible_win_diags, bool verbose) {

        std::vector<std::bitset<4>> pieces_in_diag;

        for (int d : possible_win_diags) {
            pieces_in_diag.clear();

            // collect piece bitsets for all pieces placed in diagonal d
            for (int idx = 0; idx < Rows; ++idx) {
                if (d == 1 && board_matrix_[idx][0] == board_matrix_[idx][1] && board_matrix_[idx][0] != 0) {
                    pieces_in_diag.push_back(std::bitset<4>(idx));
                    //printf("Piece %2d found in: %d, %d\n", idx, board_matrix_[idx][0], board_matrix_[idx][1]);
                }
                else if (d == 0 &&  (board_matrix_[idx][0] == -board_matrix_[idx][1]) && board_matrix_[idx][0] != 0) {
                    pieces_in_diag.push_back(std::bitset<4>(idx));
                    //printf("Piece %2d found in: %d, %d\n", idx, board_matrix_[idx][0], board_matrix_[idx][1]);
                }
            }

            // need exactly 4 pieces to evaluate a win
            if (pieces_in_diag.size() != 4){
                std::cerr << "Error: Expected 4 pieces in diag " << d << ", found " << pieces_in_diag.size() << std::endl;
                exit(EXIT_FAILURE);
            }



            bool tmp = false;

            //std::cout << pieces_in_diag[0] << '\n' << pieces_in_diag[1] << '\n' << pieces_in_diag[2] << '\n' << pieces_in_diag[3] << '\n' <<'\n';


            for (int bit = 0; bit < 4; ++bit) {
                //std::cout << "Checking bit " << bit << " for row " << r << std::endl;
                tmp = pieces_in_diag[0][bit] ^ pieces_in_diag[1][bit];
                //std::cout <<  pieces_in_diag[0][bit] << " ^ " << pieces_in_diag[1][bit] << " = " << tmp << std::endl;
                if (tmp) continue;;
                tmp = pieces_in_diag[2][bit] ^ pieces_in_diag[3][bit];
                //std::cout <<  pieces_in_diag[2][bit] << " ^ " << pieces_in_diag[3][bit] << " = " << tmp << std::endl;
                if (tmp) continue;;
                tmp = pieces_in_diag[0][bit] ^ pieces_in_diag[2][bit];
                //std::cout <<  pieces_in_diag[0][bit] << " ^ " << pieces_in_diag[2][bit] << " = " << tmp << std::endl;
                if (tmp) continue;;

                    
                winning_status_ = true;
                if (verbose) {
                    std::cout << "Winning condition met in diag " << d << " on bit " << bit << '\n';
                }
                return;
            }
        }
    }


    // utility
    static bool validValue(int v) { return v == -1 || v == 0 || v == 1 || v == +2 || v == -2; }

    // print the board in a simple formatted table
    void print(std::ostream &os = std::cout) const {
        // If the caller used the default std::cout, try to ensure stdout is the terminal.
        // This attempts to redirect stdout to /dev/tty (if available) so that cout goes to the terminal.
        if (&os == &std::cout) {
            static bool redirected = []()->bool{
                // probe the terminal; if it exists, reopen stdout to it
                FILE *probe = std::fopen("/dev/tty", "w");
                if (probe) {
                    std::fflush(stdout);
                    std::freopen("/dev/tty", "w", stdout);
                    std::cout.clear(); // refresh state tied to stdout
                    std::fclose(probe);
                }
                return true;
            }();
            (void)redirected;
        }

        os << "------------------" << '\n';
        os << "> Winning = " << (winning_status_ ? "YES" : "NO") << '\n';
        os << "------------------" << '\n';
        os << "       " << "x | y" << '\n';
        os << '\n';
        for (int r = 0; r < Rows; ++r) {
            os << std::setw(2) << r << ": ";
            for (int c = 0; c < Cols; ++c) {
                int v = board_matrix_[r][c];
                std::string ch = std::to_string(v);
                os << " | " << ch;
            }
            os << '\n';
        }
        os << "------------------" << '\n' <<  '\n';
    }


    void printAvailablePieces(std::ostream &os = std::cout) const {
        os << "Available pieces: " << '\n';
        os << "------------------" << '\n';
        os << "(C=Color, H=Height, S=Shape, F=Fill)" << '\n';
        os << "------------------" << '\n';
        os << "Value:\tCHSF" << '\n';
        for (int v : available_pieces_) {
            os << v << ":" << '\t';
            for (int b = 3; b >= 0; --b) os << ((v >> b) & 1);
            os << '\n';
        }
        os << '\n';
    }


    void printAvailablePositions(std::ostream &os = std::cout) const {
        os << "Available positions (x,y): " << '\n';
        os << "------------------" << '\n';
        for (const auto &pos : available_positions_) {
            os << "(" << pos[0] << ", " << pos[1] << ")\n";
        }
        os << '\n';
    }

    void print_verbose(std::ostream &os = std::cout) const {
        print(os);
        printAvailablePieces(os);
        printAvailablePositions(os);
        print_empty_slots(os);
    }

    void print_empty_slots(std::ostream &os = std::cout) const {
        os << "Empty slots per win line: " << '\n';
        os << "------------------" << '\n';
        os << "$ Rows: " << '\n';
        for (int i = 0; i < 4; ++i) {
            os << "   y =  " << from_index_to_board_coordinate(i, false) << ": " << '\t'<< empty_slots_[i] << '\t' << "ed_idx = " << i << '\n';
        }
        os << "$ Cols: " << '\n';
        for (int i = 4; i < 8; ++i) {
            os << "   x =  " << from_index_to_board_coordinate(i - 4, true) << ": " << '\t'<< empty_slots_[i] <<  '\t' << "ed_idx = " << i <<'\n';
        }
        os << "$ Diagonals: " << '\n';
        os << "   M-diag: " << '\t'<< empty_slots_[8] <<  '\t' << "ed_idx = " << 8 <<'\n';
        os << "   A-diag: " << '\t'<< empty_slots_[9] <<  '\t' << "ed_idx = " << 9 <<'\n';
        os << '\n';
    }

    void print_the_board(){

        std::vector<std::vector<std::string>> display_board(4, std::vector<std::string>(4, "."));
        

        printf("The board layout is:\n");

        for (int r = 0; r < 16; ++r) {
            if (board_matrix_[r][0] != 0 && board_matrix_[r][1] != 0) {
                std::bitset<4> piece_bits(r);

                /* debugging print
                    printf("Piece %2d at position (%2d, %2d): ", r, board_matrix_[r][0], board_matrix_[r][1]);
                    std::cout << '\n';
                */
                // mapping the -2,-1,1,2 to 0,1,2,3
                int col = from_board_coordinate_to_index(board_matrix_[r][0], true);
                int row = from_board_coordinate_to_index(board_matrix_[r][1], false);
                
                display_board[row][col] = piece_bits.to_string(); // convert int to string
                
            }
            else {
               continue;
            }


            

        }
        for (int i = 0; i < 4; ++i) {
                for (int j = 0; j < 4; ++j) {
                    std::cout << display_board[i][j] << '\t';
                }
                std::cout << '\n';
            }

    }


    int holes_count() const {return available_positions_.size();}

private:
    // fixed-size board matrix, the row index is the piece index (0..15), the cols are the x,y positions where the piece is quadranted
    // using 0 for unplaced pieces, the coordinates for x and y span -2 to +2, skipping 0
    Matrix board_matrix_; // 16x2 matrix
    // dynamic list of available positions, is a Nx2 vector of (x,y) pairs
    std::vector<std::array<int, 2>> available_positions_;
    // dynamic list of available pieces, is a vectors containing the ID of the pieces that are not yet placed
    std::vector<int> available_pieces_;

    // list of how many empty slots there are for each win check row/col/diag, in total there are some 10 of them
    // The order is the 4 rows, the 4 cols, and the 2 diagonals (first diagonal and then antidiagonal)
    std::vector<int> empty_slots_;


    // winning status, true if the board is in a winning condition, false otherwise
    bool winning_status_;

      static void validateMatrix(const Matrix &m) {
        for (const auto &row : m)
            for (int v : row)
                if (!validValue(v))
                    throw std::invalid_argument("board matrix contains invalid value");
    }

    static void validateIndices(int r, int c) {
        if (r < 0 || r >= Rows || c < 0 || c >= Cols)
            throw std::out_of_range("Board indices out of range");
    }
};
