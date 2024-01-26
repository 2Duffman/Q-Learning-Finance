# MSFT - Microsoft
# R6C0 - Shell
# VOW3 - Volkswagen
# DIS - Disney
# JNJ - Johnson & Johnson
# agent should be able to buy and sell these stocks each day. Use q learning to find the best strategy.
# Load the stock data from data.csv
stock_data <- read.csv("historical data/MSFT.csv")
max_money <- 100000 # Maximum amount of money available
max_stocks <- 1000 # Maximum number of stocks available
num_stocks <- 1 # Number of stocks to trade

# Set the learning parameters
num_episodes <- 1000  # Number of episodes
alpha <- 0.1  # Learning rate
gamma <- 0.9  # Discount factor
epsilon <- 0.1  # Exploration rate

# Define the maximum and minimum states
max_state <- c(max_money, max_stocks)
min_state <- c(0, 0)

# Initialize the Q-table with zeros
num_states <- max_money * max_stocks * num_stocks
num_actions <- 3 * num_stocks
Q <- array(0, dim = c(max_state[1] - min_state[1] + 1, max_state[2] - min_state[2] + 1, num_actions))

# Define the reward function
reward <- function(money, stocks, prices,
                                     money_yesterday, stocks_yesterday, prices_yesterday, first_day) {
    if (first_day) {
        return(0)
    } else {
        return(sum(money + stocks * prices) - sum(money_yesterday + stocks_yesterday * prices_yesterday))
    }
}

# Define the Q-learning algorithm
for (episode in 1:num_episodes) {
    # Initialize the state
    state <- c(10000, 0)
    
    # Iterate over the stock data
    consecutive_dates <- 0
    for (i in 1:nrow(stock_data)) {
        # Check if the current date is the same as the previous date
        if (i > 1 && stock_data$Date[i] == stock_data$Date[i-1]) {
            consecutive_dates <- consecutive_dates + 1
        } else {
            consecutive_dates <- 1
        }
        
        # Choose an action if consecutive dates reach 5,
        # this ensures the agent only operates on days where all stocks are available
        if (consecutive_dates == num_stocks) {
            if (runif(1) < epsilon) {
                action <- sample(1:num_actions, 1)
            } else {
                action <- which.max(Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, ])
            }
            
            # Reset consecutive_dates counter
            consecutive_dates <- 0
        }
    
        
        # Update the state based on the action
        new_state <- state
        if (action == 1 && state[2] > 0) {
            # Sell stock
            new_state[2] <- state[2] - 1
            new_state[1] <- state[1] + stock_data$Close[i]
        } else if (action == 2) {
            # Buy stock
            new_state[2] <- state[2] + 1
            new_state[1] <- state[1] - stock_data$Close[i]
        } else if (action == 3) {
            # Hold stock
            new_state <- state
        }
        # Calculate the reward
        r <- reward(new_state[1], new_state[2], stock_data$Close[i],
                    state[1], state[2], ifelse(i > 1, stock_data$Close[i-1], 0), i == 1)
            
        # Update the Q-table
        Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action] <- Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action] + alpha * (r + gamma * max(Q[new_state[1] - min_state[1] + 1, new_state[2] - min_state[2] + 1, ]) - Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action])
            
        # Update the state
        state <- new_state
    }
    print(paste("Episode", episode, "completed with net worth: ", state[1] + state[2] * stock_data$Close[nrow(stock_data)]))
}