In here, the progress made in every task is discussed. Note that this is our own view, and might differ from what is required.

Renzo Schindeler 5964962
Hidde Veer 5721156

1. Bools are convertes to 1/0, and characters to their ASCII values
2. Works properly
3. a seperate parser looks for a comment indicator (//), and discards everything from the comment until the end of a line
4. assignments are right associative (only '=' as far as we could find), everything else is left associative and follows operator priorities.
5. For's are lexed and parsed normally, and converted in a while in the grammar, since that was, in our opinion, the most efficient way.
6. Function calls work, but passing variables does not work entirely. The environment exist, but does not work properly since 11. isn't complete either.
7. only the left operand is considered at first for && and ||, and depeding on the results, it may branch over the right operand.
8. Works properly
9. Methods can have results, which are passed using the RR. A bool of 1 is stored in R4, to indicate that something has been returned, and that the function can therefore be treated as an expression.
10. A beginning has been made, but does not work perfectly. Assigning a single variable words, but multiple variables does not work.
    There are some things in the code that have been adjusted to make a beginning.
11. We support a DivideByZero exception, which prints an error if there is a division by zero.