# ============================================================
# Market Analysis Module
# ============================================================
market_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title = "Market Share Analysis", width = 12, status = "primary", solidHeader = TRUE,
        class = "glass-card",
        fluidRow(
          column(3, sliderInput(ns("top_n"), "Top N Companies", min = 5, max = 28, value = 10)),
          column(3, selectInput(ns("metric"), "Metric", choices = c("Market Share" = "market_share_percent", "Branch Network" = "branches_count", "Years Established" = "year_established"))),
          column(3, selectInput(ns("chart_type"), "Chart Type", choices = c("Bar Chart", "Pie Chart", "Treemap"))),
          column(3, selectInput(ns("color_by"), "Color By", choices = c("Financial Rating" = "financial_rating", "Category" = "category", "Claims Rating" = "claims_settlement_rating")))
        )
      )
    ),
    fluidRow(
      bs4Card(title = "Market Visualization", width = 8, status = "primary", solidHeader = TRUE,
              class = "glass-card",
              plotlyOutput(ns("main_plot"), height = "450px")),
      bs4Card(title = "Market Statistics", width = 4, status = "info", solidHeader = TRUE,
              class = "glass-card",
              uiOutput(ns("stats_ui")))
    ),
    fluidRow(
      bs4Card(title = "Product Distribution", width = 6, status = "primary", solidHeader = TRUE,
              class = "glass-card",
              plotlyOutput(ns("product_dist"), height = "350px")),
      bs4Card(title = "Rating Distribution", width = 6, status = "primary", solidHeader = TRUE,
              class = "glass-card",
              plotlyOutput(ns("rating_dist"), height = "350px"))
    )
  )
}

market_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$main_plot <- renderPlotly({
      metric <- input$metric
      df <- insurance_companies %>% arrange(desc(!!sym(metric))) %>% head(input$top_n)
      df$company_ordered <- factor(df$company_name, levels = df$company_name[order(df[[metric]])])
      colors <- c("A" = "#00C853", "A-" = "#64DD17", "B+" = "#FFB300", "B" = "#FF9100", "B-" = "#FF1744",
                  "Composite" = "#0066CC", "General" = "#00C853", "Life" = "#FFB300", "Islamic" = "#00B8D4",
                  "Excellent" = "#00C853", "Very Good" = "#64DD17", "Good" = "#FFB300", "Fair" = "#FF9100")
      used <- colors[names(colors) %in% unique(df[[input$color_by]])]

      if (input$chart_type == "Bar Chart") {
        plot_ly(df, x = ~company_ordered, y = df[[metric]], type = "bar",
                color = df[[input$color_by]], colors = used,
                text = ~scales::comma(round(!!sym(metric), 1)), textposition = "outside") %>%
          layout(xaxis = list(title = ""), yaxis = list(title = metric),
                 paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                 margin = list(b = 100))
      } else if (input$chart_type == "Pie Chart") {
        plot_ly(df, labels = ~company_name, values = df[[metric]], type = "pie", hole = 0.3, textinfo = "label+percent") %>%
          layout(showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)")
      } else {
        plot_ly(df, type = "treemap", labels = ~company_name, parents = "",
                values = df[[metric]], color = df[[input$color_by]], colors = used,
                textinfo = "label+value+percent root") %>%
          layout(paper_bgcolor = "rgba(0,0,0,0)")
      }
    })

    output$stats_ui <- renderUI({
      top3 <- sum(head(insurance_companies$market_share_percent[order(insurance_companies$market_share_percent, decreasing = TRUE)], 3))
      tagList(
        div(class = "comparison-wizard", h4("Market Concentration", style = "margin-top: 0;"),
            p("Top 3 Control: ", strong(paste0(round(top3, 1), "%"))),
            p("Avg Branches: ", strong(round(mean(insurance_companies$branches_count), 1))),
            p("A-Rated: ", strong(sum(insurance_companies$financial_rating %in% c("A", "A-"))))),
        div(class = "comparison-wizard",
            h4("Categories", style = "margin-top: 0;"),
            p("Composite: ", strong(sum(insurance_companies$category == "Composite"))),
            p("General: ", strong(sum(insurance_companies$category == "General"))),
            p("Life: ", strong(sum(insurance_companies$category == "Life"))))
      )
    })

    output$product_dist <- renderPlotly({
      prods <- insurance_products %>% group_by(category, sub_category) %>% summarise(count = n(), .groups = "drop")
      plot_ly(prods, x = ~category, y = ~count, color = ~sub_category, type = "bar") %>%
        layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Products"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               legend = list(orientation = "h", y = -0.3))
    })

    output$rating_dist <- renderPlotly({
      ratings <- insurance_companies %>% group_by(financial_rating) %>% summarise(count = n(), avg_share = mean(market_share_percent), .groups = "drop")
      plot_ly(ratings, x = ~financial_rating, y = ~count, type = "bar",
              marker = list(color = c("A" = "#00C853", "A-" = "#64DD17", "B+" = "#FFB300", "B" = "#FF9100", "B-" = "#FF1744")[ratings$financial_rating]),
              text = ~paste0("Avg: ", round(avg_share, 1), "%"), textposition = "outside") %>%
        layout(xaxis = list(title = "Rating"), yaxis = list(title = "Companies"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
    })
  })
}
