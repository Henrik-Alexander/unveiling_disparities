# Import the packages
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import math

# Set the font
#style.use("functions/style")
plt.rcParams['font.family'] = 'serif'
plt.rcParams.update({"font.size": 18})
plt.rcParams["legend.markerscale"] = 2

# Set seed
np.random.seed(1234)

# Load the data ----------------------------

df = pd.read_csv("../data/fert_data_subnational.csv")
fert_nat = pd.read_csv("../data/fert_data_national.csv")


# Functions  ----------------------------

# Create the colours
viridis_palette = ["#440154FF", "#31688EFF", "#21908CFF", "#35B779FF", "#8FD744FF", "#FDE725FF", "#443A83FF", "grey"]
col_dict = {"Mexico": viridis_palette[4],
            "Colombia": viridis_palette[6],
            "United States": viridis_palette[5],
            "Australia": viridis_palette[0],
            "France": viridis_palette[2],
            "Germany": viridis_palette[3],
            "Finland": viridis_palette[1],
            "Spain": viridis_palette[7]
    }

# Create the marker
marker_palette = ["o", "v", "s", "X", "p", "D", "H", "<"]
marker_dict = {"Mexico": marker_palette[0],
            "United States": marker_palette[1],
            "Australia": marker_palette[2],
            "France": marker_palette[3],
            "Germany": marker_palette[4],
            "Finland": marker_palette[5], 
            "Colombia": marker_palette[6],
            "Spain": marker_palette[7]
            }


def inset_subplot(box, region, data, variable = "mac", offset = 0.1, n = 10, size = 100, fontsize = 10):
    if variable not in ["mac", "tfr"]:
        raise ValueError("Provide mac or tfr as variable.")
    # Create the variables
    v_female = f"{variable}_female"
    v_male = f"{variable}_male"

    # Inset the subplot
    ax_inset = ax.inset_axes(box)
    for country in unique_countries:
        tmp = data[data["country"] == country]
        ax_inset.scatter(tmp[v_female], tmp[v_male], c = col_dict[country], marker = marker_dict[country],
                edgecolor = "white", linewidths = 0.3, s = size, alpha = 0.8, label = country)
    ax_inset.plot([min, max], [min, max], c = "#000000", zorder = 0)

    ax_inset.set_xlim(region[0], region[1])
    ax_inset.set_ylim(region[2], region[3])
    ax_inset.grid(visible = True, axis = "both", which = "major", linestyle = ":", linewidth = 0.5, zorder = 0, color = "grey")
    # Connector settings
    ax.indicate_inset_zoom(ax_inset,
                        edgecolor = "black",
                        linestyle = (0, (0, 0, 1, 1)),
                        linewidth = 2.5,
                        label = "_Hidden")
    # Axis and labels
    ax_inset.set_xticklabels([])
    ax_inset.set_yticklabels([])
    ax_inset.tick_params(axis = "both", which = "both", length = 0)

    # Add annotations
    tmp = data[(data[v_female] >= region[0]) & (data[v_male] <= region[3])].sample(n)
    for i, (x, y) in enumerate(zip(tmp[v_female], tmp[v_male])):
        if (region[0] + offset) <= x <= (region[1] - offset) and (region[2] + offset) <= y <= (region[3]-offset):
            label = tmp.region.iloc[i]
            ax_inset.annotate(label,
                            (x, y),
                            textcoords = "offset points",
                            xytext = (0.1, 0.1),
                            ha = "center",
                            fontsize = fontsize)
            
def sort_legend(ax):
    handles, labels = ax.get_legend_handles_labels()
    labels, handles = zip(*sorted(zip(labels, handles), key = lambda t: t[0]))
    ax.legend(handles, labels)


#%% Plot the MACs ---------------------------

# Select the important columns
macs = df[["mac_male", "mac_female", "year", "region", "country"]]
macs = macs.dropna()

# Get the minimum and maximum value
min = math.floor(macs.mac_female.min() * (1 - 0.005))
max = math.ceil(macs.mac_male.max() * (1 + 0.005))
unique_countries = macs.country.unique()

# Create the colour vector
cols = [col_dict[country] for country in macs["country"]]


# Create the plot ---------------------------

fig, ax = plt.subplots(figsize = (10, 10))
ax.plot([0, 1], [0, 1], transform = ax.transAxes, c = "#000000", zorder = 0)
for country in unique_countries:
    tmp = macs[macs["country"] == country]
    ax.scatter(tmp["mac_female"], tmp["mac_male"], c = col_dict[country], marker = marker_dict[country],  
            edgecolor = "white", linewidths = 0.1, label = country, s = 60)

# Insert the subplots
region_high = [31, 32.3, 34.4, 35.7]
box_topright = [0.05, 0.7, 0.25, 0.25]
inset_subplot(box = box_topright, region = region_high, data = macs, n = 3, size = 70, fontsize = 20)
region_low = [26, 28, 30, 32]
box_bottomright = [0.3, 0.05, 0.25, 0.25]
inset_subplot(box = box_bottomright, region = region_low, data = macs, n = 3, size = 70, fontsize = 20)

# Adjust the plot characteristics
ax.axis([min, max, min, max])
sort_legend(ax)
ax.set_xlabel("MAC$_{Female}$")
ax.set_ylabel("MAC$_{Male}$")
plt.xticks(ticks = np.arange(24, 37, step = 1))
plt.yticks(ticks = np.arange(24, 37, step = 1))
plt.grid(visible = True, axis = "both", which = "major", linestyle = ":", linewidth = 0.3, zorder = 0, color = "grey")
plt.savefig("../figures/mac_male_female_zoom.pdf", format = "pdf", bbox_inches = "tight")
plt.show()

#%% Plot the TFRs -----------------------

# Select the important columns
tfrs = df[["tfr_male", "tfr_female", "year", "region", "country"]]
tfrs = tfrs.dropna()

# Get the minimum and maximum value
min = math.floor(tfrs.tfr_female.min() * (1 - 0.005))
max = math.ceil(tfrs.tfr_male.max() * (1 + 0.005))
unique_countries = tfrs.country.unique()

# Create the colour vector
cols = [col_dict[country] for country in tfrs["country"]]

# Create the plot ---------------------------

fig, ax = plt.subplots(figsize = (10, 10))
ax.plot([0, 1], [0, 1], transform = ax.transAxes, c = "#000000", zorder = 0)
for country in unique_countries:
    tmp = tfrs[tfrs["country"] == country]
    ax.scatter(tmp["tfr_female"], tmp["tfr_male"], c = col_dict[country], marker = marker_dict[country],
            edgecolor = "white", linewidths = 0.2, label = country, s=60)

# Insert the subplots
region_low = [0.6, 1.5, 0.6, 1.5]
box_bottomright = [0.05, 0.55, 0.35, 0.35]
inset_subplot(box = box_bottomright, variable = "tfr", data = tfrs, region = region_low,
              n = 3, size = 100, fontsize = 20)

# Adjust the plot characteristics
ax.axis([min, max, min, max])
ax.legend()
sort_legend(ax)
ax.set_xlabel("TFR$_{Female}$")
ax.set_ylabel("TFR$_{Male}$")
plt.xticks(ticks = np.arange(0, 11, step = 1))
plt.yticks(ticks = np.arange(0, 11, step = 1))
plt.grid(visible = True, axis = "both", which = "major", linestyle = ":", linewidth = 0.3, zorder = 0, color = "grey")
plt.savefig("../figures/tfr_male_female_zoom.pdf", format = "pdf", bbox_inches = "tight")
plt.show()

#%% Plot the tfr ratios over time ---------------------------------

# Plot the lines
fig, ax = plt.subplots(figsize = (10, 10))
ax.axhline(y = 1, c = "black", zorder = 0)
def plot_tfr_time(country, data = df, alpha = 0.6):
    tmp = data[data["country"] == country]
    regions = tmp["region"].unique()
    for region in regions:
        reg_data = tmp[tmp["region"] == region]
        ax.plot(reg_data["year"], reg_data["tfr_ratio"],
                    color = col_dict[country],
                    zorder = 50, alpha = alpha)
        
# Plot the country average
def plot_tfr_national(country, data = fert_nat):
    tmp = data[(fert_nat["country"] == country) & (data["year"] >= 1990)]
    ax.plot(tmp["year"], tmp["tfr_ratio"],
            color = col_dict[country],
             zorder = 80, label = country,)
    ax.scatter(tmp["year"], tmp["tfr_ratio"],
        color = col_dict[country], marker = marker_dict[country],
                     edgecolor = "white", linewidths = 0.5, s = 80,
                     zorder = 80)

for country in unique_countries:
    plot_tfr_time(country)
    plot_tfr_national(country)
    
plt.grid(visible = True, axis = "both", which = "major", linestyle = ":", linewidth = 0.3, zorder = 0, color = "grey")

sort_legend(ax)
ax.legend(loc = "upper center", ncol = 3,
          shadow = False, fancybox = True,
          bbox_to_anchor=(0.5, 1))
ax.set_ylabel(r"$\frac{TFR_{male}}{TFR_{female}}$")
ax.set_xlabel("Year")
plt.savefig("../figures/trend_tfr_ratio.pdf", format = "pdf", bbox_inches = "tight")
plt.show()
        

### Create individual state plots ------------------------------

for country in unique_countries:
    fig, ax = plt.subplots(figsize = (10, 10))
    ax.axhline(y = 1, c = "black", zorder = 0)
    plot_tfr_time(country)
    plot_tfr_national(country)
    plt.grid(visible = True, axis = "both", which = "major", linestyle = ":", linewidth = 0.3, zorder = 0, color = "grey")
    plt.title(country)
    ax.set_ylabel(r"$\frac{TFR_{male}}{TFR_{female}}$")
    ax.set_xlabel("Year")
    plt.savefig(f"../figures/tfr_ratio_{country}.pdf", format = "pdf", bbox_inches = "tight")
    plt.show()

#%% Create country specific panels -------------------------


# Function to create row and column values
def create_row_col(number):
    col = number % 2
    row = number // 2
    return col, row


def plot_tfr_time(country, data = df, alpha = 0.6, number=0):
    tmp = data[data["country"] == country]
    regions = tmp["region"].unique()
    col, row = create_row_col(number)
    for region in regions:
        reg_data = tmp[tmp["region"] == region]
        axs[row, col].plot(reg_data["year"], reg_data["tfr_ratio"],
                    color = col_dict[country],
                    zorder = 50, alpha = alpha)
        
# Plot the country average
def plot_tfr_national(country, data = fert_nat, number=0):
    col, row = create_row_col(number)
    tmp = data[(fert_nat["country"] == country) & (data["year"] >= 1990)]
    axs[row, col].axhline(y = 1, c = "black", zorder = 0)
    axs[row, col].plot(tmp["year"], tmp["tfr_ratio"],
            color = col_dict[country], zorder = 80, label = country,)
    axs[row, col].scatter(tmp["year"], tmp["tfr_ratio"],
        color = col_dict[country], marker = marker_dict[country],
                     edgecolor = "white", linewidths = 0.5, s = 80,
                     zorder = 80)
    
    

# Iterate over the different panels
fig, axs = plt.subplots(4, 2, figsize = [25, 13])
for i in range(len(unique_countries)):
    country = unique_countries[i]
    row, col = create_row_col(i)
    print(country, ":", )
    # Create the plots
    plot_tfr_national(country = country, number = i)
    plot_tfr_time(country = country, number = i)
    axs[row, col].set_ylim([0.7, 1.35])
    axs[row, col].title.set_text(country)
plt.savefig("../figures/tfr_ratio_panel.pdf", format = "pdf", bbox_inches = "tight")
plt.show()



# Plot columbia -------------------------------------------

col_reg = pd.read_csv("U:/data/col/tfr_regional.csv")
col_nat = pd.read_csv("U:/data/col/tfr_national.csv")

# Estimate the difference
col_reg["difference"] = (col_reg["tfr_ratio_wa"] - col_reg["tfr_ratio"]) / col_reg["tfr_ratio"]

# Filter
col_reg = col_reg[col_reg["difference"] < 0]

# Estimate the differences
col_reg.loc[col_reg["region"] == 'Archipelago of San Andrés, Providencia and Santa Catalina', "region"] = 'Archipelago of San Andrés'

# Sort the data
col_nat = col_nat.sort_values("year")
col_regions = col_reg["region"].unique()

# Plot the results
fig, ax = plt.subplots(figsize = [20, 10])
for region in col_regions:
    tmp = col_reg[col_reg["region"] == region]
    tmp = tmp.sort_values("year")
    ax.plot(tmp["year"], tmp["difference"], linewidth = 2, label = region)
    #axs[panel].plot(tmp["year"], tmp["tfr_ratio_wa"], ":", linewidth = 2)
    ax.set_ylim([-0.4, 0.2])
    ax.axhline(y=0, c="black", zorder=0, linewidth =0.5)
    ax.legend(loc = "upper center", ncol = 4,
          shadow = False, fancybox = True,
          bbox_to_anchor=(0.5, 1))
plt.ylabel("Overestimation of TFR ratio")
plt.xlabel("Year")
plt.axhline(y = 1, c = "black", zorder = 0, linewidth =0.5)
plt.xticks(range(1998,2020,2))
plt.savefig("../figures/tfr_ratio_Colombia.pdf", format = "pdf", bbox_inches = "tight")
plt.show()

# Plot one region


# Save the plot
print("File completely run!")