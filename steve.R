# Load packages used by the app
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)

# Create sample data since setup.R isn't available
industries <- c("Technology", "Healthcare", "Finance", "Retail")
propensities <- c("High", "Medium", "Low")
contracts <- c("Enterprise", "Professional", "Basic")

# Create sample data frame for expansions
set.seed(123)
expansions <- expand.grid(
  date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
  industry = industries,
  propensity = propensities,
  contract = contracts,
  evaluation = c("A", "B")
) %>%
  mutate(
    outcome = sample(c("Won", "Lost"), n(), replace = TRUE, prob = c(0.6, 0.4)),
    amount = round(rnorm(n(), mean = 10000, sd = 2000))
  )

# Create sample data frame for expansion groups
expansion_groups <- expand.grid(
  industry = industries,
  propensity = propensities,
  contract = contracts,
  evaluation = c("A", "B")
) %>%
  mutate(
    n = sample(100:1000, n(), replace = TRUE),
    success_rate = round(runif(n(), 20, 80))
  )

# Set the default theme for ggplot2 plots
ggplot2::theme_set(ggplot2::theme_minimal())

# Apply the CSS used by the Shiny app to the ggplot2 plots
thematic_shiny()

# Define the Shiny UI layout
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "darkly",
                   bg = "#222222",
                   fg = "#86C7ED",
                   success ="#86C7ED"),
  
  title = "Effectiveness of DemoCo App Free Trial by Customer Segment",
  
  sidebar = sidebar(
    title = "Select a segment of data to view",
    class ="bg-secondary",
    selectInput("industry", "Select industries", choices = industries, selected = "", multiple  = TRUE),
    selectInput("propensity", "Select propensities to buy", choices = propensities, selected = "", multiple  = TRUE),
    selectInput("contract", "Select contract types", choices = contracts, selected = "", multiple  = TRUE)
  ),
  
  # Layout non-sidebar elements
  layout_columns(
    card(card_header("Conversions over time"),
         plotOutput("line")),
    card(card_header("Conversion rates"),
         plotOutput("bar")),
    value_box(
      title = "Recommended Trial",
      value = textOutput("recommended_eval"),
      theme_color = "secondary"
    ),
    value_box(
      title = "Customers",
      value = textOutput("number_of_customers"),
      theme_color = "secondary"
    ),
    value_box(
      title = "Avg Spend",
      value = textOutput("average_spend"),
      theme_color = "secondary"
    ),
    value_box(
      title = "Filtered Rows",  # Changed from "Memory Usage"
      value = textOutput("memory_usage"),
      theme_color = "info"
    ),
    value_box(
      title = "Active Filters",  # Changed from "CPU Usage"
      value = textOutput("cpu_usage"),
      theme_color = "info"
    ),
    card(
      card_header("Conversion rates by subgroup"),
      tableOutput("table")
    ),
    col_widths = c(8, 4, 4, 4, 4, 4, 4, 12),
    row_heights = c(4, 1.5, 3)
  )
)

# Define the Shiny server function
server <- function(input, output) {
  selected_industries <- reactive({
    if (is.null(input$industry)) industries else input$industry
  })
  
  selected_propensities <- reactive({
    if (is.null(input$propensity)) propensities else input$propensity
  })
  
  selected_contracts <- reactive({
    if (is.null(input$contract)) contracts else input$contract
  })
  
  filtered_expansions <- reactive({
    expansions %>%
      filter(industry %in% selected_industries(),
             propensity %in% selected_propensities(),
             contract %in% selected_contracts())
  })
  
  conversions <- reactive({
    filtered_expansions() %>%
      mutate(date = floor_date(date, unit = "month")) %>%
      group_by(date, evaluation) %>%
      summarize(n = sum(outcome == "Won"), .groups = "drop")
  })
  
  groups <- reactive({
    expansion_groups %>%
      filter(industry %in% selected_industries(),
             propensity %in% selected_propensities(),
             contract %in% selected_contracts())
  })
  
  # Show number of filtered rows instead of memory usage
  output$memory_usage <- renderText({
    n_rows <- nrow(filtered_expansions())
    format(n_rows, big.mark=",")
  })

  # Show number of active filters instead of CPU usage
  output$cpu_usage <- renderText({
    n_filters <- sum(
      length(input$industry) > 0,
      length(input$propensity) > 0,
      length(input$contract) > 0
    )
    as.character(n_filters)
  })
  
  output$recommended_eval <- renderText({
    recommendation <-
      filtered_expansions() %>%
      group_by(evaluation) %>%
      summarise(rate = mean(outcome == "Won")) %>%
      filter(rate == max(rate)) %>%
      pull(evaluation)
    
    as.character(recommendation[1])
  })
  
  output$number_of_customers <- renderText({
    sum(filtered_expansions()$outcome == "Won") %>%
      format(big.mark = ",")
  })
  
  output$average_spend <- renderText({
    x <-
      filtered_expansions() %>%
      filter(outcome == "Won") %>%
      summarise(spend = round(mean(amount))) %>%
      pull(spend)
    
    str_glue("${x}")
  })
  
  output$line <- renderPlot({
    ggplot(conversions(), aes(x = date, y = n, color = evaluation)) +
      geom_line() +
      theme(axis.title = element_blank()) +
      labs(color = "Trial Type")
  })
  
  output$bar <- renderPlot({
    groups() %>%
      group_by(evaluation) %>%
      summarise(rate = round(sum(n * success_rate) / sum(n), 2)) %>%
      ggplot(aes(x = evaluation, y = rate, fill = evaluation)) +
      geom_col() +
      guides(fill = "none") +
      theme(axis.title = element_blank()) +
      scale_y_continuous(limits = c(0, 100))
  })
  
  output$table <- renderTable({
    groups() %>%
      select(industry, propensity, contract, evaluation, success_rate) %>%
      pivot_wider(names_from = evaluation, values_from = success_rate)
  },
  digits = 0)
}

# Create the Shiny app
shinyApp(ui = ui, server = server)
