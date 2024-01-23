import pandas as pd
import matplotlib.pyplot as plt

# Path to the CSV file
file_path = 'path_to_your_file.csv'

# Reading the CSV file with a different encoding if needed
try:
    data = pd.read_csv(file_path, encoding='ISO-8859-1')
except UnicodeDecodeError:
    data = pd.read_csv(file_path, encoding='utf-8')

# Displaying the column variables
print("Column Variables:")
print(data.columns.tolist())

# Checking for missing values
print("\nMissing Values:")
print(data.isnull().sum())

# Running summary analysis of the columns
print("\nSummary Analysis:")
print(data.describe())

# Identifying numerical columns for histograms
numerical_columns = data.select_dtypes(include=['float64', 'int64']).columns.tolist()

# Creating histograms for the numerical columns
fig, axes = plt.subplots(nrows=1, ncols=len(numerical_columns), figsize=(15, 5))
for i, col in enumerate(numerical_columns):
    data[col].hist(ax=axes[i])
    axes[i].set_title(col)
plt.tight_layout()
plt.show()
