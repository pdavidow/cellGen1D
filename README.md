# One-Dimensional Cellular Automata

Take one argument 'n' on the command line, then display perhaps infinite sequence rows of 'n' cells (such as binary-digits, or music notes) each. 

The first row is created randomly. The rule for a cell at position x of row y, is to use a pattern based on prior row's immediate left, top, and right cells.

---------------------------------------------------
stack ghci

main
