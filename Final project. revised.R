# Load necessary libraries
library(tidyverse)
library(lubridate)
library(maps)
library(leaflet)
library(plotly)
library(ggplot2)

# Load and preprocess data
my_data <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-dataset/main/data/OxCGRT_compact_national_v1.csv") # updated May 15
my_data$Date <- ymd(my_data$Date)
my_data <- my_data %>%
  mutate(MajorityVaccinated_numeric = ifelse(MajorityVaccinated == "V", 1,
                                             ifelse(MajorityVaccinated == "NV", 0, NA))) %>%
  rename(region = CountryName)

# Summarize data by country
df_summarized <- my_data %>%
  group_by(region) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

# Calculate total COVID-19 cost measure
df_summarized_covid <- df_summarized %>%
  mutate(
    total_covid_cost = E1_Income.support + E2_Debt.contract.relief + E3_Fiscal.measures +
      E4_International.support + H4_Emergency.investment.in.healthcare + H5_Investment.in.vaccines +
      V3_Vaccine.Financial.Support..summary. + GovernmentResponseIndex_Average
  )
#Print total_covid_cost
print(df_summarized$total_covid_cost)

# Calculate average COVID-19  cost measure per vaccinated individual
df_summarized <- df_summarized %>%
  mutate(
    average_covid_cost_per_vac_ind = total_covid_cost / df_summarized$PopulationVaccinated
  )
#print the average_covid_cost_per_vac_ind
print(df_summarized$average_covid_cost_per_vac_ind)

#NB : it's just to have an idea of the measure per vaccinated individual.
#However, the normal population of each country would allow us to calculate the average cost per capita. Since they don't the normal population in the dataset I just once the vaccinated populatio.

# Prepare world map data
world_map <- map_data("world")
map_data <-left_join(world_map, df_summarized, by = "region")

# Visualizations
# Interactive map for population vaccinated
map1 <- ggplot(map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = PopulationVaccinated), color = "black") +
  labs(title = "Population Vaccinated", x = "Longitude", y = "Latitude")
plotly_map1 <- ggplotly(map1)

# Scatter plot for population vaccinated vs confirmed deaths
plot1 <- ggplot(df_summarized, aes(x = PopulationVaccinated, y = ConfirmedDeaths)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Population Vaccinated vs Confirmed Deaths", x = "Population Vaccinated (log scale)", y = "Confirmed Deaths (log scale)")

# Scatter plot for confirmed cases vs confirmed deaths
plot2 <- ggplot(df_summarized, aes(x = ConfirmedCases, y = ConfirmedDeaths)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Confirmed Cases vs Confirmed Deaths", x = "Confirmed Cases (log scale)", y = "Confirmed Deaths (log scale)")

# Plot COVID-19 income support by country
map3 <- ggplot(map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = E1_Income.support), color = "black") +
  labs(title = "Income Support", x = "Longitude", y = "Latitude")

# Top 10 countries bar plot for income support

top_countries <- df_summarized %>%
  top_n(10, E1_Income.support)  # Adjust the number as needed

bar_plot_top <- ggplot(top_countries, aes(x = reorder(region, E1_Income.support), y = E1_Income.support)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Countries by Income Support during COVID-19", x = "Country", y = "Income Support") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Convert ggplot to plotly for interactivity
bar_plot_interactive <- ggplotly(bar_plot_top, tooltip = "text")

# Show the interactive plot
bar_plot_interactive


# Linear regression analysis and plots
lm_income_debt <- lm(E2_Debt.contract.relief ~ E1_Income.support, data = df_summarized)
plot_income <- ggplot(df_summarized, aes(x = E1_Income.support, y = E2_Debt.contract.relief)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Income Support vs Debt Relief (Simple Linear Regression)", x = "Income Support", y = "Debt Relief")

# Linear model for international support
lm_income_international <- lm(E4_International.support ~ E1_Income.support, data = df_summarized)
plot_international <- ggplot(df_summarized, aes(x = E1_Income.support, y = E4_International.support)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Income Support vs International Support", x = "Income Support", y = "International Support")

# Linear model for fiscal measures
lm_income_fiscal <- lm(E3_Fiscal.measures ~ E1_Income.support, data = df_summarized)
plot_fiscal <- ggplot(df_summarized, aes(x = E1_Income.support, y = E3_Fiscal.measures)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Income Support vs Fiscal Measures", x = "Income Support", y = "Fiscal Measures")

# Multiple regression
lm_inc_Fis_Int <- lm(E2_Debt.contract.relief ~ E1_Income.support + E3_Fiscal.measures + E4_International.support, data = df_summarized)
plot_multiple <- ggplot(df_summarized, aes(x = E1_Income.support, y = E2_Debt.contract.relief)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Income Support vs Debt Relief (Multiple Regression)", x = "Income Support", y = "Debt Relief")

# Plotting the relationship between income support, debts, fiscal measures, and international support

# filter data for each regression model
df_income <- df_summarized[complete.cases(df_summarized$E1_Income.support, df_summarized$E2_Debt.contract.relief), ]
df_fiscal <- df_summarized[complete.cases(df_summarized$E3_Fiscal.measures, df_summarized$E2_Debt.contract.relief), ]
df_international <- df_summarized[complete.cases(df_summarized$E4_International.support, df_summarized$E2_Debt.contract.relief), ]

# Plotting the relationship between income suport, debts, fiscal measures and international support
plot4 <- ggplot(df_summarized, aes(x = E1_Income.support, y = E2_Debt.contract.relief)) +
  
  geom_line(data = df_income, aes(x = E1_Income.support, y = fitted(lm_income_debt)), color = "blue") +  # Simple Linear Regression
  geom_line(data = df_fiscal, aes(x = E1_Income.support, y = fitted( lm_income_fiscal)), color = "red") +  # Fiscal Measures
  geom_line(data = df_international, aes(x = E1_Income.support, y = fitted(lm_income_international)), color = "green") +  # International Support
  geom_line(data = df_summarized, aes(x = E1_Income.support, y = fitted(lm_inc_Fis_Int)), color = "orange") +  # Multiple Regression
  labs(title = "Relationship between Income Support, Fiscal Measures, International Support, and Debt Relief",
       x = "Income Support", y = "Debt Relief")

# Plot COVID-19 Facial Coverings by Country
map4 <- ggplot(map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = H6M_Facial.Coverings), color = "green") +
  labs(title = "Facial Coverings by Country", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Display the plots

print(plotly_map1)
print(plot1)
print(plot2)
print(map3)
print(bar_plot_interactive)
print(plot_income)
print(plot_international)
print(plot_fiscal)
print(plot_multiple)
print(plot4)
print(map4)
