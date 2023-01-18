library("shiny")
library("ggplot2")
source("analysis.R")

# The UI is the result of calling the `fluidPage()` layout function

page_one <- tabPanel(
  "About", 
  titlePanel("Relations between House Values, GDP and Household incomes"),
    h4("This app was made by: Jangho Cho, Seoyoung Cho, Inho Baik"),
    br(),
    p("This app consists of interactive USA map application where user can view average household income, 
              average house values and the ratio between the two by each state. Also consisting the information about the correlation between GDP and house cost of each US states"),
    h3("Data Sources:"),
    tags$a(href="https://www.zillow.com/research/data/", "House Data"),
    br(),
    tags$a(href="https://www.kaggle.com/solorzano/gdp-per-capita-in-us-states", "GDP Data"),
    br(),
    tags$a(href="https://www.kaggle.com/goldenoakresearch/us-household-income-stats-geo-locations/version/1", "Income Data"),
    br(),
    tags$a(href="https://info201a-wi20.github.io/project-report-jhc95/", "Our Data Report"),
    h3("About the Data"),
    br(),
    p("We are interested in the relationships between the household income and the house value in US. We will use Zillow dataset to analyze how house value is related to household income and see whether higher house income means higher house value or vice versa. For whom that are unclear of what Zillow is: Zillow is the leading real estate and rental marketplace dedicated to empowering consumers with data, inspiration and knowledge around the place they call home, and connecting them with the best local professionals who can help (https://www.zillow.com/corp/About.htm).
        Extending our interest with the household income and the house value, we grew curiosity about whether GDP (Gross Domestic Product) of each US state is also related to the house value, whether the state with higher GDP value has the higher house cost. Gross Domestic Product (GDP) is the economic value of total market goods and services produced in one year. The GDP data and household income data is originally from https://www.census.gov/ but cleaned by users of Kaggle.
        By analyzing such relations between household income and the house value, we hope to give insight about which state best matches with the user's income and their demanding house cost. (We can also find out which state has the highest house hold income, but lowest house cost!) Further, by analyzing GDP and house cost relation, we seek to find out whether economic activity of each state decides the house value. This will help inform people in business about which state best matches their current economic activity and their demanding house cost if they were to start new business in another state."),
    p("We will be using median household income data because it is a better representation of central value than mean because mean tend to be much higher for subjects like income due to high 1% of people who earns much more than general people."),
    br(),
    h3("Details about the Application"),
    br(),
    p("For Median Household Income, Average House Value and Ratio tab, the user can use the slider 
        widget to only select the number he/she is interested in. User can also select a specific state 
        if wish so. The states that does not meet the requirements will be displayed gray. More red the state
        is, higher the value (income, house value, ratio) is. The data table for matching states is also displayed
        for the user's reference."),
    p("For Income vs House cost and GDP vs Hous cost tabs, the user can analyze the correlation between the variables in both poly fit and linear fit by selecting the fit options. Also, the user can select to remove or keep the outliers and see if there is any dependency of outliers.")

  )

page_two <- tabPanel(
  "Median Household Income",
    h1("Which states have the lowest and the highest household income to house value ratio?"),
    br(),
    h3("How does Median Household Income differ by state?"),
    br(),
    p("This page is intended for the user to explore the median household income of US States by using the Slider or selecting a state the user 
               if specifically interesd in. Hence, the user can find out the general trend of the household income in US as to relate with income and house value ration later on."),
    br(),
    p("The Top 3 States with the highest household income are: 1. District of Columbia ($206991.00), 2. Maryland ($172117.95), 3. Rhode Island ($168722.89). 
              The 3 lowest household income states are: 1. Mississippi ($83675.31), 2. Arkansas ($85795.29), 3. South Carolina ($91771.32) 
              As notes below the bar chart, the ultimate average of household income in US is $121588"),
    sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, 
               sliderInput(inputId = "slider", h3("Income Range"),
                           min = 0, max = 1000000, value = c(35000,700000))
        )
      ),
      fluidRow(
        column(12,
               selectInput(inputId = "select", h3("State"), 
                           choices = c("All", all_df$RegionName))
        )
      ),
      plotOutput(outputId = "income_bar"),
      textOutput(outputId = "income_mean"),
      width = 5,
      height = 4
    ),
    mainPanel(
      plotOutput(outputId = "income_map"),
      tableOutput(outputId = "income_table"),
      width = 7
    ),
  )
  
)
  

page_three <- tabPanel(
  "Average House Value",
  h1("Which states have the lowest and the highest household income to house value ratio?"),
  br(),
  h3("How does Average House Value differ by state?"),
  br(),
  p("In this page, the user can explore the house value of US States by using the Slider or selecting a state that the user 
             if specifically interesd in. This page will give insight of which states have the lowest and the highest average house value as to relate with their ratios."),
  br(),
  p(paste("The top 3 states with the highest house value are: 1. Hawaii ($518050.94), 2. District of Columbia ($507305.92), 3. California ($407016.06)
           The lowest 3 house value states are: 1. West Virginia ($92899.11), 2. Mississippi ($105690.75), 3. Arkansas ($106004.33). We can see that this list doesn't differ much from the listing of the previous tab with incomes. 
           In addition, the ultimate average of house values of US is ")),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "slide_hv",
        label = "House value range",
        min = 1000, 
        max = 600000, 
        value = c(1000, 600000)
      ), 
      
      selectInput(
        inputId = "select_hv",
        label = "Choose a State",
        choices = c("All", income_house_df$RegionName),
        multiple = FALSE
      ), 
      
      plotOutput(outputId = "housevalue_bar"), 
      textOutput(outputId = "housevalue_mean"),
      width = 5, 
      height = 4
      
    ),
    mainPanel(
      plotOutput(outputId = "housevalue_map"),
      tableOutput(outputId = "housevalue_table"),
      width = 7
    )
  )
  
)


page_four <- tabPanel(
  "Ratio",
  h1("Which states have the lowest and the highest housevalue to household ratio?"), #How does House Value to Median Household Income ratio differ by state?"),
  p("The user can explore the ratio between house value and household income of each US State by using the Slider or selecting a state that the user 
             is specifically interesd in. Thus, the user can find out which states have the lowest and highest ratio."),
  br(),
  p("The state with the highest housevalue to household is Hawaii with the ratio of 4.30. 
    The lowest value goes to West Virginia with the ratio of 0.89. 
    Having higher house value to househol dincome ratio means that the house value is exceptionally expensive than what people can afford. On the other hand, having lower ratio means that the house is cheaper than what people can afford.
    Thus, it is harder for people living in Hawaii to save money for new house than the people living in West Virginia."),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, 
               sliderInput(inputId = "ratio_slider", h3("Ratio Range"),
                           min = 0, max = 5, value = c(1,4))
        )
      ),
      fluidRow(
        column(12,
               selectInput(inputId = "ratio_select", h3("State"), 
                           choices = c("All", all_df$RegionName))
        )
      ),
      plotOutput(outputId = "ratio_bar"),
      textOutput(outputId = "ratio_cor"),
      width = 5,
      height = 4
    ),
    mainPanel(
      plotOutput(outputId = "map"),
      tableOutput(outputId = "table"),
      width = 7
    ),
  )
  
)



page_five <- tabPanel(
  "Income vs House cost",
  h1("How are Median household income and house value related?"),
  br(),
  p("In this page, with the correlation plot provided, the user can explore about how income and house cost are related. Does the states with higher income have more expensive houses?"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", h3("Outlier Options"),
                   choices = list("Keep Outliers" = 1, "Remove Outliers" = 2)
                   ,selected = 1),
      radioButtons("radio2", h3("Regression Line Options"),
                   choices = list("Linear Regression" = 1, "Polynomial Regression" = 2)
                   ,selected = 1)
    ),
    mainPanel(
      plotOutput(outputId = "income_relation"),
      textOutput(outputId = "income_r")
    )
  ),
  br(),
  h4("Summary:"),
  br(),
  p("To summarize, we can see from the correlation plot that the regression line takes positive slope. This means that as the house cost increases, the income also increased. 
    To solidify the result, take the R value above. The R value, the correlation coefficient between household income and house value is about 0.70 (including the outliers). This implies that those two are highly related, meaning that the states with higher income tend to have higher house costs or vice versa. 
    Also by selecting to keep or remove outliers, we can see that the regression line depends slightly on the outliers. In this case, the outliers are Hawaii, District of Columbia, and California; having excessive house price compared to people's income. The outlier deviates the correlation and by removing, the correlation between the variables gets strengthened to r = 0.74"),
  br(),
  p("The users can also view the correlation plot with polynomial regression line. This type of regression line more explicitly show the relationship between the two variables through nth polynomial line. 
    We can see that although there is a slight change in the form of regression line, the general trend between the two variables isn't deviated much. Hence, the correlation coefficient isn't affected and there is still a strong correlation between the household income and the house value.")
)



page_six <- tabPanel(
  "GDP vs House cost",
  h1("How are GDP and house value related?"),
  br(),
  p("In this page, with the correlation plot provided, the user can explore about how GDP and house cost are related. Does the states with higher GDP have more expensive houses?"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio_gdp", h3("Outlier Options"),
                   choices = list("Keep Outliers" = 1, "Remove Outliers" = 2)
                   ,selected = 1),
      radioButtons("radio2_gdp", h3("Regression Line Options"),
                   choices = list("Linear Regression" = 1, "Polynomial Regression" = 2)
                   ,selected = 1)
    ),
    mainPanel(
      plotOutput(outputId = "gdp_relation"),
      textOutput(outputId = "gdp_r")
    )
  ),
  br(),
  h4("Summary:"),
  br(),
  p("By looking at the correlation plot, we can see that the regression line takes positive slope. This means that as the gdp increases, the house value also increased. 
    To solidify the result, take the R value above. The R value, the correlation coefficient between gdp and house value is about 0.64 (including the outliers). This implies that those two are fairly related, meaning that the states with higher gdp tend to have higher house costs or vice versa. 
    Also by selecting to keep or remove outliers, we can see that the regression line depends slightly on the outliers. In this case, the outlier is District of Columbia, having excessive gdp and house value compared to any other states. Although the outlier deviates the correlation, suprisingly by removing the District of Columbia outlier, the correlation between the variables gets weakened to r = 0.53.
    From this we can infer that the correlation between the two variables were strengthened with the presence of the outlier and keep in mind that they might not be correlated confidently."),
  br(),
  p("The users can also view the correlation plot with polynomial regression line. This type of regression line more explicitly show the relationship between the two variables through nth polynomial line. 
    We can see that poly fit is very distorted with regression area spreading out towards the right side when the outlier is removed. This explains why the correlation coeffient decreased without the outlier.")
)


my_ui <- fluidPage(
  navbarPage(
    "Housing App",
    page_one,
    page_two,
    page_three,
    page_four,
    page_five,
    page_six

  )
)



# The server is a function that takes `input` and `output` arguments
my_server <- function(input, output) {
  
  #income & house cost ratio tab
  output$map <- renderPlot({
    range <- input$ratio_slider
    my_state <- input$ratio_select
    new_df <- income_house_df %>%
      filter(ratio >= range[1], ratio <= range[2])
    if (my_state != "All") {
      new_df <- new_df %>%
        filter(RegionName == my_state)
    }
    ratio_map <- plot_usmap(data = new_df, values = "ratio", color = "red") + 
      scale_fill_continuous(name = "House Value to Household Income Ratio", label = scales::comma,
                            low = 'white', high = 'red') + 
      labs(title = "House Value to Household Income Ratio") +
      theme(legend.position = "right")
    ratio_map
  })
  output$table <- renderTable({
    range <- input$ratio_slider
    my_state <- input$ratio_select
    new_df <- all_df %>%
      filter(ratio >= range[1], ratio <= range[2])
    if (my_state != "All") {
      new_df <- new_df %>%
        filter(RegionName == my_state)
    }
    new_df
  })
  output$ratio_bar <- renderPlot({
    ratio_bar + 
      theme(axis.text.x = element_text(size = 7))
  })
  
  #income Tab
  output$income_map <- renderPlot({
    range <- input$slider
    my_state <- input$select
    new_df <- income_house_df %>%
      filter(mean_median >= range[1], mean_median <= range[2])
    if (my_state != "All") {
      new_df <- new_df %>%
        filter(RegionName == my_state)
    }
    income_map <- plot_usmap(data = new_df, values = "mean_median", color = "red") + 
      scale_fill_continuous(name = "Median Household Income", label = scales::comma,
                            low = 'white', high = 'red') + 
      labs(title = "Median Household Income") +
      theme(legend.position = "right")
    income_map
  })
  output$income_table <- renderTable({
    range <- input$slider
    my_state <- input$select
    new_df <- all_df %>%
      filter(Median_household_income >= range[1], Median_household_income <= range[2])
    if (my_state != "All") {
      new_df <- new_df %>%
        filter(RegionName == my_state)
    }
    select(new_df, RegionName, Median_household_income)
  })
  output$income_bar <- renderPlot({
    income_bar +
      theme(axis.text.x = element_text(size = 7))
  })
  output$income_mean <- renderText({
    message <- paste("Mean income =", round(mean(all_df$Median_household_income)))
    message
  })
  
  # For GDP vs House cost Tab
  output$gdp_relation <- renderPlot({
    remove <- new_df
    if (input$radio_gdp == 2) {
      remove <- new_df %>%
        filter(State != "District of Columbia")
      house_gdp_plot <- ggplot(data = remove, mapping = aes(x = avg_household, y = avg_gdp)) +
        geom_point(shape = 18, size = 3) +
        geom_smooth(method = "lm") +
        labs(
          title = "House value - GDP", 
          x = "Average House value ($)", 
          y = "Average GDP ($)"
        ) 
    }
    if (input$radio2_gdp == 2) {
      house_gdp_plot <- ggplot(data = remove, mapping = aes(x = avg_household, y = avg_gdp)) +
        geom_point(shape = 18, size = 3) +
        geom_smooth() +
        labs(
          title = "House value - GDP", 
          x = "Average House value ($)", 
          y = "Average GDP ($)"
        ) 
    }
    house_gdp_plot  
  })
  
  output$gdp_r <- renderText({
    if (input$radio_gdp == 2) {
      remove <- new_df %>%
        filter(State != "District of Columbia")
      x <- cor.test(remove$avg_household, remove$avg_gdp)  
    } else {
      x <- cor.test(new_df$avg_household, new_df$avg_gdp)  
    }
    message <- paste("R Value =", x$estimate)
    message
  })
  
  #house value Tab
  output$housevalue_map <- renderPlot({
    input_range <- input$slide_hv
    input_state <- input$select_hv
    new_hv_df <- income_house_df %>% 
      filter(Ave_13.15 >= input_range[1], Ave_13.15 <= input_range[2])
    if(input_state != "All") {
      new_hv_df <- new_hv_df %>% 
        filter(state == input_state)
    }
    
    
    hv_map <- plot_usmap(data = new_hv_df, values = "Ave_13.15", color = "red") +
      scale_fill_continuous(name = "Average House Value per State", label = scales::comma, low = 'white', high = 'red') +
      labs(title = "House Value per US State") +
      theme(legend.position = "right")
    hv_map
  })
  
  output$housevalue_table <- renderTable({
    input_range <- input$slide_hv
    input_state <- input$select_hv
    new_hv_df <- all_df %>% 
      filter(Average_house_value >= input_range[1], Average_house_value <= input_range[2])
    if(input_state != "All") {
      new_hv_df <- new_hv_df %>% 
        filter(RegionName == input_state)
    }
    select(new_hv_df, RegionName, Average_house_value) 
  })
  output$housevalue_bar <- renderPlot({
    house_value_bar + 
      theme(axis.text.x = element_text(size = 7))
  })
  output$housevalue_mean <- renderText({
    message <- paste("Mean house value =", round(mean(all_df$Average_house_value)))
    message
  })
  
  
  #income vs house value tab
  output$income_relation <- renderPlot({
    remove <- combined_data
    if (input$radio == 2) {
      remove <- combined_data %>%
        filter(State_Name != "Hawaii", State_Name != "District of Columbia", State_Name != "California")
      relation_house_earning <- ggplot(data = remove, mapping = aes(x = average_median_earning, y = average_home_price)) +
          geom_point() +
          geom_smooth(method = "lm") +
          xlim(75000, 175000) +
          scale_color_brewer(palette = "Dark2") +
          labs(title = "Relation between Median Earning and House Price", x = "Median Earning", y = "House Price") 
    }
    if (input$radio2 == 2) {
      relation_house_earning <- ggplot(data = remove, mapping = aes(x = average_median_earning, y = average_home_price)) +
        geom_point() +
        geom_smooth() +
        xlim(75000, 175000) +
        scale_color_brewer(palette = "Dark2") +
        labs(title = "Relation between Median Earning and House Price", x = "Median Earning", y = "House Price") 
    }
    relation_house_earning
  })
  output$income_r <- renderText({
    if (input$radio == 2) {
      remove <- combined_data %>%
        filter(State_Name != "Hawaii", State_Name != "District of Columbia")
      x <- cor.test(remove$average_median_earning, remove$average_home_price)
    } else {
      x <- cor.test(combined_data$average_median_earning, combined_data$average_home_price)
    }
    message <- paste("R Value =", x$estimate)
    message
  })
}
