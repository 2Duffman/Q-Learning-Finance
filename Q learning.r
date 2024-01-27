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
max_stocks <- 100 # Maximum number of stocks available
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
#Q table is an array with dimensions = num_stocks+4
#The states are (money, stock1, stock2,...)
#the actions are (which stock, buy/sell/hold, how many)
Q <- array(0, dim = c(max_state[1] - min_state[1] + 1, max_state[2] - min_state[2] + 1, num_stocks, 3, max_transaction))

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
            Price <- round(stock_data$Close[i])
            if (runif(1) < epsilon) {
                action_category <- sample(1:3, 1)
                action_stock <- sample(1:num_stocks, 1)
                if (action_category == 1) {
                    #buy up to max_transaction stocks, limited by the money available
                    action_amount <- sample(1:floor(min(max_transaction, state[action_stock+1]/Price)), 1)
                } else {
                    #sell up to max_transaction stocks, limited by the stocks available
                    action_amount <- sample(1:min(max_transaction, state[action_stock+1]), 1)
                }
            } else {

                actions <- apply(Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, , , ], 2, which.max)
                print(actions[4])
                #action_stock <- which.max(Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, , ,])
                #action_category <- which.max(Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action_stock, , ])
                #action_amount <- which.max(Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action_stock, action_category, ])
                
            }   
            
            # Reset consecutive_dates counter
            consecutive_dates <- 0
        }
    
        
        # Update the state based on the action
        new_state <- state
        if (action_category == 1) {
            # Buy stock
            new_state[action_stock+1] <- state[action_stock+1] + action_amount
            new_state[1] <- state[1] - Price * action_amount
            #print("bought", action_amount)
            #print("now there are ", new_state[action_stock+1])
            #print("and ", new_state[1], "money")
        } else if (action_category == 2) {
            # Sell stock
            new_state[action_stock+1] <- state[action_stock+1] - action_amount
            new_state[1] <- state[1] + Price * action_amount
            #print("sold", action_amount, "stocks")
            #print("now there are ", new_state[action_stock+1], "stocks")
            #print("and ", new_state[1], "money")
        } else if (action_category == 3) {
            # Hold stock
            new_state <- state
        }
        # Calculate the reward
        r <- reward(new_state[1], new_state[2], Price,
                    state[1], state[2], ifelse(i > 1, round(stock_data$Close[i-1]), 0), i == 1)
            
        # Update the Q-table
        #print(dim(Q))
        #print(state[1] - min_state[1] + 1)
        #print(state[2]- min_state[2] + 1)
        #print(action_stock)
        #print(action_category)
        #print(action_amount)
        Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action_stock, action_category, action_amount] <- Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action_stock, action_category, action_amount] + alpha * (r + gamma * max(Q[new_state[1] - min_state[1] + 1, new_state[2] - min_state[2] + 1, , , ]) - Q[state[1] - min_state[1] + 1, state[2] - min_state[2] + 1, action_stock, action_category, action_amount])
            
        # Update the state
        state <- new_state
    }
    print(paste("Episode", episode, "completed with money: ", state[1] ,"stocks:", state[2] ))#* stock_data$Close[nrow(stock_data)]))
}