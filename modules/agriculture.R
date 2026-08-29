# ============================================================
# Agriculture Insurance Module
# ============================================================
agriculture_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title = "Agriculture Insurance Calculator", width = 12, status = "primary", solidHeader = TRUE,
        class = "glass-card",
        fluidRow(
          column(3, selectInput(ns("crop"), "Crop/Livestock", choices = c("Maize", "Wheat", "Vegetables/Fruits", "Cattle/Livestock"))),
          column(3, numericInput(ns("acres"), "Acres / Animals", value = 5, min = 0.5, max = 500)),
          column(3, selectInput(ns("coverage"), "Coverage Type", choices = c("All", "Multi-Peril", "Weather Index", "Mortality"))),
          column(3, div(style = "margin-top: 25px;", checkboxInput(ns("subsidy_only"), "Subsidized Only", value = FALSE),
                        actionButton(ns("calculate"), "Calculate", icon = icon("calculator"), class = "btn-primary btn-block")))
      )
      )
    ),
    fluidRow(
      bs4Card(title = "Premium with Subsidy Breakdown", width = 7, status = "primary", solidHeader = TRUE,
              class = "glass-card",
              plotlyOutput(ns("premium_plot"), height = "400px")),
      bs4Card(title = "Subsidy Impact", width = 5, status = "success", solidHeader = TRUE,
              class = "glass-card",
              uiOutput(ns("subsidy_ui")))
    ),
    fluidRow(
      bs4Card(title = "Agriculture Insurance Products", width = 12, status = "primary", solidHeader = TRUE, maximizable = TRUE,
              class = "glass-card",
              DTOutput(ns("products_table")))
    )
  )
}

agriculture_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    results <- eventReactive(input$calculate, {
      req(input$crop, input$acres)
      res <- agriculture_products %>% filter(grepl(tolower(input$crop), tolower(crop_type)))
      if (input$coverage != "All") res <- res %>% filter(coverage_type == input$coverage)
      if (input$subsidy_only) res <- res %>% filter(subsidy_available == "Yes")
      if (nrow(res) == 0) return(NULL)

      res$total_premium <- input$acres * res$sum_insured_per_acre_kes * res$premium_rate_percent / 100
      res$subsidy_amount <- res$total_premium * res$subsidy_percent / 100
      res$farmer_pays <- res$total_premium - res$subsidy_amount
      res %>% left_join(insurance_companies %>% select(company_id, company_name), by = "company_id")
    })

    output$premium_plot <- renderPlotly({
      res <- results()
      if (is.null(res) || nrow(res) == 0) return(plot_ly() %>% layout(title = "No products match your criteria"))
      plot_ly(res) %>%
        add_bars(x = ~reorder(company_name, farmer_pays), y = ~total_premium,
                 name = "Total Premium", marker = list(color = "#FF1744"),
                 text = ~format_kes(total_premium), textposition = "outside") %>%
        add_bars(x = ~reorder(company_name, farmer_pays), y = ~farmer_pays,
                 name = "You Pay (After Subsidy)", marker = list(color = "#00C853"),
                 text = ~format_kes(farmer_pays), textposition = "outside") %>%
        layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Premium (KES)"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               legend = list(orientation = "h", y = -0.2))
    })

    output$subsidy_ui <- renderUI({
      res <- results()
      if (is.null(res) || nrow(res) == 0) return(NULL)
      best <- res[which.min(res$farmer_pays), ]
      tagList(
        div(class = "premium-highlight",
            h4(icon("piggy-bank"), " Subsidy Savings", style = "margin-top: 0;"),
            h2(format_kes(best$subsidy_amount), style = "margin: 5px 0;"),
            p(paste0("You save ", best$subsidy_percent, "% through ", best$govt_program))),
        br(),
        div(class = "comparison-wizard best",
            h4("Best Value", style = "margin-top: 0; color: #00C853;"),
            p(strong(best$company_name)),
            p("Total: ", strong(format_kes(best$total_premium))),
            p("You Pay: ", strong(style = "color: #00C853;", format_kes(best$farmer_pays))))
      )
    })

    output$products_table <- renderDT({
      res <- results()
      if (is.null(res) || nrow(res) == 0) return(NULL)
      res %>%
        mutate(total_premium = format_kes(total_premium), subsidy_amount = format_kes(subsidy_amount),
               farmer_pays = format_kes(farmer_pays)) %>%
        select(company_name, product_name, crop_type, coverage_type, total_premium, subsidy_amount, farmer_pays, govt_program) %>%
        dt_export(colnames = c("Company", "Product", "Crop", "Coverage", "Total Premium", "Subsidy", "You Pay", "Program"))
    })
  })
}

