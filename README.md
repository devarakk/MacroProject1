# MacroProject1
for Macro Theory Project 1-Growth
1. Project Structure:
The three countries were chosen to be from the Middle East, representing different income levels and GDP per capita. Potential variables of interest were considered, and we agreed that there had to be data for at least 30 years. Data was collected from the Penn World Table. Data cleaning and visualization were done in R. The project write-up was done in Word, with meeting notes taken in OneNote. 

2. Data Sources:
The data source was the Penn World Table version 10.01, collected at https://pwt-data-tool.streamlit.app/. The data for the chosen variables and countries were downloaded as a csv file. The countries were Yemen, Saudi Arabia, and Iraq. The years were from 1989 to 2019. 2019 was the last year with data, and 1989 was the oldest year that all countries had data populated. The variables were "Expenditure-side real GDP at chained PPPs (in mil. 2017US$)", "Population (in millions)", "Number of persons engaged (in millions)", "Human capital index, based on years of schooling and returns to education", "Price level of exports, price level of USA GDPo in 2017=1", "Price level of imports, price level of USA GDPo in 2017=1."

3. Instructions for replicating your work:
Go to the given website above, and choose the same countries, years, and variables. Download the csv file. Utilize the R scrip in the repository to clean the data and then to create the graphs for GDP per capita annual growth over time for each country, as well as its relationship with the variables of interest. There is also a section to calculate the average GDP per capita annual growth for three periods in Iraq's history, which correspond with the First Gulf War, the Second Gulf War, and afterward. Finally, there is code to display a correlation matrix.
