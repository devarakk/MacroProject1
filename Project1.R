install.packages("dplyr")   # For data manipulation
install.packages("tidyr") #For data cleaning
install.packages("ggplot2") # For graphs and charts
install.packages("corrplot") # For correlation matrices
install.packages("knitr") # For table formatting
install.packages("kableExtra")  # For enhanced table styling

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(knitr)  
library(kableExtra) 

# Part 1 - Data Cleaning

# Specify the path to your csv files
df <- read.csv("PennTable.csv")

# Pivot to long format (Years as rows)
df <- df %>%
  pivot_longer(
    cols = starts_with("X"),  # Columns starting with "X" (e.g., X1989, X1990)
    names_to = "Year",        # Create a "Year" column
    names_prefix = "X",       # Remove the "X" prefix
    values_to = "Value"       # Store values in a "Value" column
  )

# Pivot to wide format (Variable.Code as columns)
df <- df %>%
  select(`Country`, Year, `Variable.code`,Value) %>%
  pivot_wider(
    names_from = `Variable.code`,   # Series Code becomes column names
    values_from = Value           # Values for the corresponding Series Code
  )

# Part 2 - GDP growth rate
df <- df %>%
  arrange(Country, Year) %>%  # Ensure data is sorted by country and Year
  group_by(Country) %>%       # Calculate growth rate for each country
  mutate(GDP_per_capita = rgdpe/pop) %>%
  mutate(GDP_per_capita_growth_rate = (GDP_per_capita - lag(GDP_per_capita)) / lag(GDP_per_capita) * 100) %>%
  ungroup()  # Remove the grouping

# Plotting GDP per capita growth rate for each country using ggplot2
ggplot(df, aes(x = Year, y = GDP_per_capita_growth_rate, color = Country,group = Country)) +
  geom_line(size = 1) +          # Line plot for each country
  geom_point() +                 # Points for each Year
  labs(
    title = "Annual GDP per Capita Growth Rate by Country",
    x = "Year",
    y = "Growth Rate (%)",
    color = "Country"
  ) +
  scale_x_discrete(
    breaks = seq(min(as.numeric(df$Year)), max(as.numeric(df$Year)), by = 5)  # Show every second Year (adjust as needed)
  )+
  theme_minimal() +
  theme(legend.position = "right")  # Place the legend on the right
  
# Analyze average and standard deviation of growth rates
growth_analysis <- df %>%
  group_by(Country) %>%
  summarize(
    Avg_Growth = mean(GDP_per_capita_growth_rate, na.rm = TRUE),
    Std_Dev_Growth = sd(GDP_per_capita_growth_rate, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_Growth))
  
# Print the summary table
print(growth_analysis)

# Create a table with rounded values
growth_analysis <- data.frame(
  Country = c("Iraq", "Yemen", "Saudi Arabia"),
  Average_Growth = c(4.96, 4.39, 4.2),
  Std_Dev_Growth = c(24.7, 15.9, 11.5)
)

# Display the table
kable(growth_analysis, col.names = c("Country", "Average Growth", "Standard Deviation of Growth")) %>%
  kable_styling(full_width = FALSE, position = "center")
growth_analysis %>%
  kbl(
    col.names = c("Country", "Average Growth", "Standard Deviation of Growth"),
    caption = "Real GDP Per Capita Growth Analysis by Country",
    align = "c",  # Center-align all columns
    digits = 2  # Round numeric columns to 2 decimal places
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3") %>%  # Header row formatting
  column_spec(2, color = "white", background = "#4CAF50") %>%  # Highlight Avg Growth column
  column_spec(3, color = "white", background = "#2196F3") %>%  # Highlight Std Dev Growth column
  footnote(
    general = "Source: Penn World Table version 10.01 (1989 onward).",
    general_title = "Note: "
  )  

# Part 3 - Growth Changes in Iraq
# Filter data for Iraq
iraq_data <- df %>%
  filter(Country == "Iraq")  # Include data only from 1989 onward
  
# Compute average GDP per capita growth rates for specified periods
iraq_growth_summary <- iraq_data %>%
  mutate(Period = case_when(
    Year >= 1989 & Year <= 2002 ~ "1989-2002",
    Year >= 2003 & Year <= 2011 ~ "2003-2011",
    Year >= 2012 & Year <= 2019 ~ "2012-2019",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Period)) %>%  # Exclude rows without a specified period
  group_by(Period) %>%
  summarize(
    Avg_GDP_per_capita_growth_rate = mean(GDP_per_capita_growth_rate, na.rm = TRUE)
  )
  
# Display the summary
print(iraq_growth_summary)

# Part 4 - Determinants of Growth
df <- df %>%
  arrange(Country, Year) %>%  # Ensure data is sorted by country and Year
  group_by(Country) %>%       # Calculate growth rate for each country
  mutate(pop_growth_rate = (pop - lag(pop)) / lag(pop) * 100) %>%  # Population growth rate in %
  ungroup()  # Remove the grouping


# Scatterplot of GDP per capita growth rate vs. Population growth rate
ggplot(df, aes(x = pop_growth_rate, y = GDP_per_capita_growth_rate, color = Country)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Line of best fit
  labs(
    title = "Scatterplot of GDP per Capita Growth vs. Population Growth",
    x = "Population Growth Rate (%)",
    y = "GDP per Capita Growth Rate (%)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Scatterplot of GDP per capita growth rate vs. Employment numbers
ggplot(df, aes(x = emp, y = GDP_per_capita_growth_rate, color = Country)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Line of best fit
  labs(
    title = "Scatterplot of GDP per Capita Growth vs. Employment",
    x = "Employment (in millions)",
    y = "GDP per Capita Growth Rate (%)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Scatterplot of GDP per capita growth rate vs. Exports
ggplot(df, aes(x = pl_x, y = GDP_per_capita_growth_rate, color = Country)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Line of best fit
  labs(
    title = "Scatterplot of GDP per Capita Growth vs. Exports",
    x = "Price level of Exports",
    y = "GDP per Capita Growth Rate (%)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Scatterplot of GDP per capita growth rate vs. Imports
ggplot(df, aes(x = pl_m, y = GDP_per_capita_growth_rate, color = Country)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Line of best fit
  labs(
    title = "Scatterplot of GDP per Capita Growth vs. Imports",
    x = "Price level of Imports",
    y = "GDP per Capita Growth Rate (%)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Scatterplot of GDP per capita growth rate vs. Human Capital
ggplot(df, aes(x = hc, y = GDP_per_capita_growth_rate, color = Country)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Line of best fit
  labs(
    title = "Scatterplot of GDP per Capita Growth vs. Human Capital Index",
    x = "Human Capital Index",
    y = "GDP per Capita Growth Rate (%)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Calculate the correlation matrix between GDP per capita growth rate and all variables
correlation_matrix <- df %>%
  select(GDP_per_capita_growth_rate, pop_growth_rate,emp,hc,pl_x,pl_m) %>%
  cor(use = "complete.obs")  # Calculate correlation, ignoring NA values

# Display correlation matrix
print(correlation_matrix)

  

  