#pragma once

#include <array>
#include <algorithm>
#include <stdexcept>
#include <iostream>
#include <iomanip>
#include <numeric> // for std::iota
#include <vector>
#include <cstdio>

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

    const Matrix& board_matrix() const { return board_matrix_; }

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
    }

    void add_one(int piece, int x, int y) {
        // validate row index (will throw if out of range)
        validateIndices(piece, 0);

        if (piece < 0 || piece >= Rows) {
            std::cerr << "Error: Piece index " << piece << " out of range." << std::endl;
            exit(EXIT_FAILURE);
        }
        
        if (abs(x) > 2 || abs(y) > 2) {
            std::cerr << "Error: Values (" << x << ", " << y << ") out of allowed range." << std::endl;
            exit(EXIT_FAILURE);
        }
        // ensure both columns in the target row are zero
        if (board_matrix_[piece][0] != 0 || board_matrix_[piece][1] != 0){
            std::cerr << "Error: Attempt to overwrite non-zero row " << piece << std::endl;
            exit(EXIT_FAILURE);
        }

        // validate values to be stored
        if (!validValue(x) || !validValue(y)){
            std::cerr << "Error: Invalid values (" << x << ", " << y << ") for row " << piece << std::endl;
            exit(EXIT_FAILURE);
        }


        board_matrix_[piece][0] = x;
        board_matrix_[piece][1] = y;

        remove_piece_from_available(piece);
    }

    void add_one_random(){
        if (available_pieces_.empty()) {
            std::cerr << "Error: No available pieces to add." << std::endl;
            exit(EXIT_FAILURE);
        }

        // select a random piece from available pieces
        int random_index = rand() % available_pieces_.size();
        int piece = available_pieces_[random_index];

        // generate a second random index for position
        int pos_index = rand() % available_positions_.size();
        int x = available_positions_[pos_index][0];
        int y = available_positions_[pos_index][1];

        // add the piece at the random position
        board_matrix_[piece][0] = x;
        board_matrix_[piece][1] = y;

        // remove the used position
        available_positions_.erase(available_positions_.begin() + pos_index);
        remove_piece_from_available(piece);
        
    }

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

    void win_check() {
        // Placeholder for win condition checking logic
        // Set winning_status_ = true if a win condition is met


        std::array<std::array<std::array<bool, 4>, 4>, 4> state0; // all false-initialized
        std::array<std::array<std::array<bool, 4>, 4>, 4> state1; // all false-initialized
        std::array<bool, 4> PIECE_bianary;
        for (int r = 0; r < Rows; ++r) {
            int x = board_matrix_[r][0];
            int y = board_matrix_[r][1];
            if (x != 0 || y != 0) {
                // represent r, the piece index, in binary form as 4 bits
                for (int b = 0; b < 4; ++b) {
                    PIECE_bianary[b] = (r >> (3 - b)) & 1;
                }
                

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
    }

    friend std::ostream& operator<<(std::ostream& os, const Board& b) {
        b.print(os);
        return os;
    }

private:
    Matrix board_matrix_;
    std::vector<std::array<int, 2>> available_positions_;

    bool winning_status_;

    // dynamic container so it can shrink down to a scalar
    std::vector<int> available_pieces_;

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
