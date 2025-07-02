import pandas as pd
import matplotlib.pyplot as plt

# === CONFIGURATION ===
csv_file = 'data/agents_plotting_data.csv'  # your data file
x_lim = (-100, 100)     # fixed x-axis limits
y_lim = (-100, 100)   # fixed y-axis limits

# === LOAD DATA ===
df = pd.read_csv(csv_file, sep=r'\s+')
#df = df.dropna(subset=['pos_x', 'pos_y']) # drop rows with NaN in pos_x or pos_y

# === Test if data is loaded correctly ===

print(df.columns)
print(df.head())

# === PLOT ===
plt.figure(figsize=(8, 8))
plt.scatter(df['pos_x'], df['pos_y'], color='black', s=2)  # small black dots

padding_x = (df['pos_x'].max() - df['pos_x'].min()) * 0.05
padding_y = (df['pos_y'].max() - df['pos_y'].min()) * 0.05

plt.xlim(df['pos_x'].min() - padding_x, df['pos_x'].max() + padding_x)
plt.ylim(df['pos_y'].min() - padding_y, df['pos_y'].max() + padding_y)

plt.xlabel('X Position')
plt.ylabel('Y Position')
plt.title('Agent Distribution')
plt.gca().set_facecolor('white')
plt.grid(False)
plt.axis('equal')

plt.tight_layout()
plt.savefig('agent_plot.png')
print("Plot saved as agent_plot.png")