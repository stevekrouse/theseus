The first way I tried to solve this problem was by a breadth first search
that would return all possible optimal solutions. However, for mazes 6 and up,
the solutions were so long (and, more importantly, there was so many possible
solutions) that it was taking way too long to get even one answer. Thus, I
needed a way to get rid of some solutions that were almost identical to
others. My girlfriend (who, if you remember, helped me solve the recursive
maze) came up with a key insight that I could remove the solutions
whose both positions arrive at the same point at the same time. This is
because this would indicate that these mazes have "converged." Convergence
happened most often in key places where you need to trap the minotaur to win.

I got all 10 of my mazes from http://www.logicmazes.com/theseus.html
I would reccomend that you type in "solve 5" to get the solution
to get the fifth maze and then follow along on the website.
You can also type "maze 5" to see my representation of the 5th maze.
I just used the 5th maze for example. You can do the same for all 10.

