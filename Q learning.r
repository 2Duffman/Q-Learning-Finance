# MSFT - Microsoft
# R6C0 - Shell
# VOW3 - Volkswagen
# DIS - Disney
# JNJ - Johnson & Johnson
# agent should be able to buy and sell these stocks each day. Use q learning to find the best strategy.
# Load the stock data from data.csv
stock_data <- read.csv("historical data/MSFT.csv")
# Set the agent parameters
max_money <- 100000 # Maximum amount of money available
max_stocks <- 300 # Maximum number of stocks available
num_stocks <- 1 # Number of different stocks
max_transaction <- 10 # maximum number of stocks to buy/sell each day

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
num_actions <- 3 * num_stocks * max_transaction
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
                action <- sample(0:(num_actions-1), 1)
                #print(paste("random action: ", action))
            } else {
                action <- which.max(Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, ])
                if (action >= num_actions) {
                    action <- num_actions - 1
                }
                #print(paste("max action: ", action))
            }
            
            # Reset consecutive_dates counter
            consecutive_dates <- 0
        }
    
        
        # Update the state based on the action
        new_state <- state
        #buy=0, sell=1, hold=2
        action_category <- floor(action/(max_transaction)) %% 3
        #print(paste("action_category: ", action_category))
        #picks one of the stocks (indexed 2 to num_stocks+1)
        action_stock <- floor(action/(max_transaction*3)) + 2
        #print(paste("action_stock: ", action_stock))
        #picks the number of stocks to buy/sell
        action_amount <- action %% max_transaction
        #print(paste("action_amount: ", action_amount))
        #Price of the picked stock
        Price <- stock_data$Close[i]
        if (action_category == 0) {
            # buy stock
            if (action_amount * Price > state[1]) {
                action_amount <- floor(state[1] / Price)
            }
            if (action_amount + state[action_stock] > max_stocks) {
                action_amount <- max_stocks - state[action_stock]
            }
            new_state[action_stock] <- state[action_stock] + action_amount
            new_state[1] <- state[1] - Price * action_amount
        } else if (action_category == 1) {
            # sell stock
            if (action_amount  > state[action_stock]) {
                action_amount <- state[action_stock]
            }
            new_state[action_stock] <- state[action_stock] - action_amount
            new_state[1] <- state[1] + Price * action_amount
        } else if (action == 3) {
            # Hold stock
            new_state <- state
        }
        # Calculate the reward
        r <- reward(new_state[1], new_state[2], Price,
                    state[1], state[2], ifelse(i > 1, stock_data$Close[i-1], 0), i == 1)
        # Check if indices are within the bounds
        if (new_state[1] - min_state[1] + 1 > dim(Q)[1] || new_state[2] - min_state[2] + 1 > dim(Q)[2]) {
            print(paste("new_state: ", new_state[1], new_state[2]))
            print(paste("min_state: ", min_state[1], min_state[2]))
        } else {
            # Update the Q-table
            Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action + 1] <- Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action + 1] + alpha * (r + gamma * max(Q[new_state[1] - min_state[1] + 1, new_state[2] - min_state[2] + 1, ]) - Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action + 1])
        }  
        # Update the state
        state <- new_state
}
    print(paste("Episode", episode, "completed with net worth: ", state[1] + state[2] * stock_data$Close[nrow(stock_data)]))
}