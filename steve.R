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
      title = "Memory Usage",
      value = textOutput("memory_usage"),
      theme_color = "info"
    ),
    value_box(
      title = "CPU Usage",
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
  
  # System monitoring reactive timer
  autoInvalidate <- reactiveTimer(1000)  # Update every second

  # Memory usage monitor
  output$memory_usage <- renderText({
    autoInvalidate()
    tryCatch({
      # Get memory usage using base R
      mem_used <- sum(gc()[,2]) * 1024^2  # Convert to MB
      sprintf("%.1f MB", mem_used)
    }, error = function(e) {
      "N/A"
    })
  })

  # CPU usage using simple calculation
  output$cpu_usage <- renderText({
    autoInvalidate()
    tryCatch({
      # Create a simple CPU load estimate
      start_time <- Sys.time()
      start_stats <- gc.time()
      Sys.sleep(0.1)  # Short delay
      end_stats <- gc.time()
      end_time <- Sys.time()
      
      elapsed <- as.numeric(end_time - start_time)
      cpu_time <- (end_stats[1] - start_stats[1])
      
      cpu_percent <- min((cpu_time / elapsed) * 100, 100)
      sprintf("%.1f%%", cpu_percent)
    }, error = function(e) {
      "N/A"
    })
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
