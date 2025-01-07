How does this code work:

1) Open "Board.inp"
2) Enter the board as a 4x4 grid with tabs between the columns
3) Each piece is identified with 3 letters
	- the first is the color b/w/B/W. Capital letters mean tall piece and the small ones short
	- then, in small caps you specify the SHAPE c = circle, s = square
	- finally, the last letter is for the FILLING s = solid, h = hole
4) Run the program and enter the label of the piece your opponent has given you
5) The output is on the terminal. It shows 3 columns, the first is the row index and the second one the column index 
(1-4) of the board. The third column shows a number that is indicative of the "winns", the grater the number the bigger the odds. If the number is positive, then is in your favour, if negative is against you. Pick the biggest number of them all and place the piece there. 
6) A second tabular is below, that is normalized so that the biggest number (in absolute value terms) equates to +/- 1.




ALGORITHM:

The algorithm reads the input from the file, then it checks for errors. You give the prompt the piece you have been given and then it randomly propagates a large number of games for each possible position, keeping into acount when the win happens. The score keeps into account the number of moves that took to win, the smaller the number the higher the score assigned. The sum of all the scores is the number displayed on the screen.




