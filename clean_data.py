import os
import pandas as pd

def add_stock_column(csv_folder):
    # List all CSV files in the folder
    csv_files = [file for file in os.listdir(csv_folder) if file.endswith('.csv')]
    
    # Iterate through each CSV file
    for file in csv_files:
        # Read the CSV file
        df = pd.read_csv(os.path.join(csv_folder, file))
        
        # Extract stock name from filename
        stock_name = os.path.splitext(file)[0]
        
        # Add 'stock' column
        df['stock'] = stock_name
        
        # Save the cleaned CSV file
        df.to_csv(os.path.join(csv_folder, file), index=False)


def merge_csv_files(csv_folder, output_file='data.csv'):
    # List all CSV files in the folder
    csv_files = [file for file in os.listdir(csv_folder) if file.endswith('.csv')]
    
    # Initialize an empty list to store DataFrames
    dfs = []
    
    # Iterate through each CSV file
    for file in csv_files:
        # Read the CSV file
        df = pd.read_csv(os.path.join(csv_folder, file))
        
        # Append the DataFrame to the list
        dfs.append(df)
    
    # Concatenate DataFrames along the rows
    merged_df = pd.concat(dfs, ignore_index=True)
    
    # Sort merged DataFrame by 'Date' column
    merged_df.sort_values(by='Date', inplace=True)
    
    # Save the merged DataFrame to a new CSV file
    merged_df.to_csv(output_file, index=False)

# Example usage:
csv_folder = "./historical data"
#add_stock_column(csv_folder)
merge_csv_files(csv_folder)