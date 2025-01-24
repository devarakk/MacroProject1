install.packages("readxl")  # For reading Excel files
install.packages("dplyr")   # For data manipulation
install.packages("ggplot2")

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Specify the path to your Excel file
file_path <- "Penn Data.xlsx"
sheet_name <- "Countries of Interest"  # Replace with the actual sheet name you want to read

# Read the Excel file from the specific sheet into a data frame
tryCatch({
  df <- read_excel(file_path, sheet = sheet_name)
  
  # Check if the required columns exist
  required_columns <- c("country","rgdpe", "pop")
  if (!all(required_columns %in% colnames(df))) {
    stop("The required columns 'country' or 'rgdpe' or 'pop' are missing in the Excel sheet.")
  }
  
  # Calculate GDP per capita
  df <- df %>%
    filter(year >= 1989) %>%
    group_by(country) %>%
    mutate(
      GDP_per_capita = rgdpe / pop,
      GDP_per_capita_growth_rate = c(NA, diff(log(GDP_per_capita)) * 100)  # Annual growth rate in %
    )
  
  # Plotting GDP per capita growth rate for each country using ggplot2
  ggplot(df, aes(x = year, y = GDP_per_capita_growth_rate, color = country)) +
    geom_line(size = 1) +          # Line plot for each country
    geom_point() +                 # Points for each year
    labs(
      title = "Annual GDP per Capita Growth Rate by Country",
      x = "Year",
      y = "Growth Rate (%)",
      color = "Country"
    ) +
    theme_minimal() +
    theme(legend.position = "right")  # Place the legend on the right
  
}, error = function(e) {
  # Handle errors (e.g., file not found, missing sheet, missing columns)
  message("An error occurred: ", e$message)
})
