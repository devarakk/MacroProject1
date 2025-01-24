install.packages("readxl")  # For reading Excel files
install.packages("dplyr")   # For data manipulation
install.packages("ggplot2")
install.packages("corrplot")

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

# Specify the path to your Excel file
file_path <- "Penn Data.xlsx"
sheet_name <- "Countries of Interest"  # Replace with the actual sheet name you want to read


  df <- read_excel(file_path, sheet = sheet_name)
  
  # Check if the required columns exist
  required_columns <- c("country","rgdpe", "pop")
  if (!all(required_columns %in% colnames(df))) {
    stop("The required columns 'country' or 'rgdpe' or 'pop' are missing in the Excel sheet.")
  }
  
# Filter the data to start from the year 1989
  df <- df %>%
    filter(year >= 1989) %>%
    group_by(country) %>%
    mutate(
      GDP_per_capita = rgdpe / pop,
      GDP_per_capita_growth_rate = (GDP_per_capita - lag(GDP_per_capita)) / lag(GDP_per_capita) * 100,
      pop_growth_rate = (pop - lag(pop)) / lag(pop) * 100  # Population growth rate in %
    ) %>%
    ungroup()
  
  # Filter data for Iraq
  iraq_data <- df %>%
    filter(country == "Iraq" & year >= 1989)  # Include data only from 1989 onward
  
  # Compute average GDP per capita growth rates for specified periods
  iraq_growth_summary <- iraq_data %>%
    mutate(Period = case_when(
      year >= 1989 & year <= 2002 ~ "1989-2002",
      year >= 2003 & year <= 2011 ~ "2003-2011",
      year >= 2012 & year <= 2019 ~ "2012-2019",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(Period)) %>%  # Exclude rows without a specified period
    group_by(Period) %>%
    summarize(
      Avg_GDP_per_capita_growth_rate = mean(GDP_per_capita_growth_rate, na.rm = TRUE)
    )
  
  # Display the summary
  print(iraq_growth_summary)

    # Scatterplot of GDP per capita growth rate vs. Population growth rate
  ggplot(df, aes(x = pop_growth_rate, y = GDP_per_capita_growth_rate, color = country)) +
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
  
  # Calculate the correlation matrix between GDP per capita growth rate and Population growth rate
  correlation_matrix <- df %>%
    select(GDP_per_capita_growth_rate, pop_growth_rate,emp,hc,pl_x,pl_m) %>%
    cor(use = "complete.obs")  # Calculate correlation, ignoring NA values

  # Display correlation matrix
  print(correlation_matrix)

  # Create a correlation plot
  corrplot(correlation_matrix, method = "circle", type = "upper", order = "hclust", 
           addCoef.col = "black", tl.col = "black", tl.srt = 45)
  
  # Plotting GDP per capita growth rate for each country using ggplot2
  ggplot(df, aes(x = year, y = GDP_per_capita_growth_rate, color = country)) +
    geom_line(size = 1) +          # Line plot for each country
    geom_point() +                 # Points for each year
    labs(
      title = "Annual GDP per Capita Growth Rate by Country",
      x = "year",
      y = "Growth Rate (%)",
      color = "Country"
    ) +
    theme_minimal() +
    theme(legend.position = "right")  # Place the legend on the right
  