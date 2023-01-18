#install.packages("usmap")
library("dplyr")
library("tidyr")
library("ggplot2")
library("usmap")

# Question 1
# How are incomes and house values related to each other?
# year 2013 -2015


# Bringing in the dataframes
house_data = read.csv("State_Zhvi_AllHomes.csv")
income_data = read.csv("US_Income_Kaggle.csv")


# Altering the dataframes for easy plotting
income_data <- mutate(income_data, Median_Mean = income_data$Median - income_data$Mean)

income_data_state <- income_data %>% 
  group_by(State_Name) %>% 
  summarize(average_median_earning = mean(Median)) 

house_data_state <- house_data %>%
  mutate(State_Name = RegionName) %>% 
  mutate(average_home_price = Ave_13.15) %>% 
  select(State_Name, average_home_price) %>% 
  as.data.frame()

combined_data <- full_join(income_data_state, house_data_state, by = "State_Name")

# Plotting the dataframe
relation_house_earning <- ggplot(data = combined_data, mapping = aes(x = average_median_earning, y = average_home_price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(75000, 175000) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Relation between Median Earning and House Price", x = "Median Earning", y = "House Price") 

# Getting an r value
# cor.test(combined_data$average_median_earning, combined_data$average_home_price)

# Question 2, 3

# Load the dataset again for other computations
house_df <- read.csv('State_Zhvi_AllHomes.csv')
gdp_df <- read.csv('bea-gdp-by-state.csv')
income_df <- read.csv('US_Income_Kaggle.csv')

# Compute the average of the medians 
income_df_by_state <- income_df %>%
  group_by(State_Name) %>%
  summarize(mean_median = mean(Median)) %>%
  mutate(RegionName = State_Name, state = State_Name)

# Compute the ratio of house value to household income.
income_house_df <- house_df %>%
  left_join(income_df_by_state, by = 'RegionName') %>%
  select(RegionName, Ave_13.15, mean_median) %>%
  mutate(ratio = Ave_13.15/mean_median, state=RegionName)

# Make a dataframe that is presentable and contains all information
all_df <- income_house_df %>%
  rename(
    Median_household_income = mean_median,
    Average_house_value = Ave_13.15
  )

all_df$state <- NULL

# Make a bar plot using house value to income ratio
ratio_bar <- ggplot(data = income_house_df, aes(x=RegionName, y=ratio)) +
  geom_bar(stat="identity") + 
  labs(title = "House Value to Household Income Ratio") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Make a bar plot using house value
house_value_bar <- ggplot(data = income_house_df, aes(x=RegionName, y=Ave_13.15)) +
  geom_bar(stat="identity") + 
  labs(title = "Average House values by State") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Make a bar plot income
income_bar <- ggplot(data = income_df_by_state, aes(x=RegionName, y=mean_median)) +
  geom_bar(stat="identity") + 
  labs(title = "Household income by State") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Make a us map plot of the ratio
ratio_map <- plot_usmap(data = income_house_df, values = "ratio", color = "red") + 
  scale_fill_continuous(name = "House Value to Household Income Ratio", label = scales::comma,
                        low = 'white', high = 'red') + 
  labs(title = "House Value to Household Income Ratio") +
  theme(legend.position = "right")

# Make a us map plot of the house value
house_value_map <- plot_usmap(data = income_df_by_state, values = "mean_median", color = "red") + 
  scale_fill_continuous(name = "Household income by State", label = scales::comma,
                        low = 'white', high = 'red') + 
  labs(title = "Household income by State") +
  theme(legend.position = "right")

# Make a us map plot of the household income
income_map <- plot_usmap(data = income_house_df, values = "Ave_13.15", color = "red") + 
  scale_fill_continuous(name = "Average House values by State", label = scales::comma,
                        low = 'white', high = 'red') + 
  labs(title = "Average House values by State") +
  theme(legend.position = "right")
#plot(income_map)

# Question 4
# How are gdp and house values related to each other?
gdp_df <- read.csv("bea-gdp-by-state.csv")
household_df <- read.csv("State_Zhvi_AllHomes.csv")

gdp_df <- gdp_df %>% 
  select(Area, X2013.2015)
colnames(gdp_df) <- c("State", "avg_gdp")

household_df <- household_df %>% 
  select(RegionName, Ave_13.15) 
colnames(household_df) <- c("State", "avg_household")

new_df <- left_join(gdp_df, household_df, by = "State") %>% 
  filter(!is.na(avg_household)) 

house_gdp_plot <- ggplot(data = new_df, mapping = aes(x = avg_household, y = avg_gdp)) +
  geom_point(shape = 18, size = 3) +
  geom_smooth(method = "lm") +
  labs(
    title = "House value - GDP", 
    x = "Average House value ($)", 
    y = "Average GDP ($)"
  ) 


# pearson value (r)
#cor.test(new_df$avg_household, new_df$avg_gdp)
