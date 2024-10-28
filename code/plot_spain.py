import pandas as pd
import matplotlib.pyplot as plt

# Load the data
df = pd.read_csv("../data/fert_data_subnational.csv")

# Print the head
df.head()

# Select spain
df_esp = df[df["country"] == "Spain"]

# Plot the TFR ratio
fig, ax = plt.subplots()
#
for reg in df_esp["region"].unique():
    ax.plot(x = df_esp["year"], 
            y = df_esp["tfr_ratio"])
plt.show()