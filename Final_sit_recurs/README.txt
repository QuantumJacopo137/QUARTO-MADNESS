In the "Board" file you write the board as a 4 subsequent 4x4 arrays, as in the example.

Choose a piece as the reference that will have variables [+1,+1,+1,+1] like, white, tall, square and no hole.

then all the pieces around it will have +1 if they are white as well or square as well, and -1 if they have the opposite propriety.

E.G. [+1, -1, -1, +1] is White, short, circle and no hole piece.

After you have your Board input, you just compile the code, run it and obtain in the results output file all possible winning scenarios. 

Note: the result is not indipendent on the symmetry, so it could be a huge file.
