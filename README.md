# One-Dimensional Cellular Automata

Take one argument 'n' on the command line, then display perhaps infinite row sequence of 'n' cells each.  A cell has a finite member set, such as music notes A through G. 

The first row in the sequence is created randomly. To generate a new cell on a subsequent row, use a pattern based on prior row's immediate left, top, and right cells.

---------------------------------------------------
stack ghci

main
