# Q-Learning-Finance
This was a project to familiarize myself with programming in R and also exploring the Q learning algorithm.
I wanted to build an agent that can trade on a given market and generate profits with its trades.

The version for buying and selling one stock at a time (and only having one stock available) seemed to somewhat work.
When Implementing the ability for the agent to buy multiples of stocks on the same day I found that the algorithm does not seem to handle this well. I believe this is because the State space and the action space both grow very fast and the state space gets too big to explore in a reasonably amount of Episodes.

Because of this I think Q learning is not a good algorithm for solving the given problem. At least in the short time I couldn't figure out a way to keep the state space to reasonable dimensions. Keeping the action space small was already nontrivial.
Will keep this repo in this state until I maybe find time and interest to get back to this problem.
