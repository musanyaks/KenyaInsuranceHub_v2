# ============================================================
# Life Insurance Module
# ============================================================
life_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title = "Life Insurance Calculator", width = 12, status = "primary", solidHeader = TRUE,
        class = "glass-card",
        fluidRow(
          column(2, selectInput(ns("policy_type"), "Policy Type", choices = c("Term Life", "Endowment", "Whole Life", "Education"))),
          column(2, textInput(ns("sum_assured"), "Sum Assured (KES)", value = "5000000", class = "kes-input"),
                 div(style = "margin-top: -10px; color: #0066CC; font-weight: bold; font-size: 12px;", textOutput(ns("sum_fmt")))),
          column(2, numericInput(ns("age"), "Your Age", value = 30, min = 18, max = 70)),
          column(2, sliderInput(ns("term"), "Policy Term (Years)", min = 5, max = 30, value = 20)),
          column(2, selectInput(ns("rider"), "Include Riders", choices = c("None", "Critical Illness", "Disability", "Both"))),
          column(2, div(style = "margin-top: 25px;", actionButton(ns("compare"), "Compare Products", icon = icon("magnifying-glass"), class = "btn-primary btn-block")))
      )
    ),
    fluidRow(
      bs4Card(title = "Premium Comparison", width = 8, status = "primary", solidHeader = TRUE,
              class = "glass-card",
              plotlyOutput(ns("comparison_plot"), height = "400px")),
      bs4Card(title = "Rider Options", width = 4, status = "info", solidHeader = TRUE,
              class = "glass-card",
              DTOutput(ns("rider_table")))
    ),
    fluidRow(
      bs4Card(title = "Life Insurance Products", width = 12, status = "primary", solidHeader = TRUE,
              class = "glass-card",
              DTOutput(ns("products_table")))
    )
  )
}

life_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    results <- eventReactive(input$compare, {
      req(input$policy_type, input$sum_assured, input$age, input$term)
      val <- as.numeric(gsub(",", "", input$sum_assured))
      req(!is.na(val))

      filtered <- life_premiums %>%
        filter(policy_type == input$policy_type,
               entry_age_min <= input$age, entry_age_max >= input$age,
               policy_term_min_years <= input$term, policy_term_max_years >= input$term,
               sum_assured_min_kes <= val, sum_assured_max_kes >= val)
      if (nrow(filtered) == 0) return(NULL)

      filtered$estimated_annual <- (val / 1000000) * filtered$premium_per_1m_kes_annual

      if (input$rider == "Critical Illness") filtered <- filtered %>% filter(critical_illness_rider_available == "Yes")
      else if (input$rider == "Disability") filtered <- filtered %>% filter(disability_rider_available == "Yes")
      else if (input$rider == "Both") filtered <- filtered %>% filter(critical_illness_rider_available == "Yes", disability_rider_available == "Yes")

      filtered %>% left_join(insurance_companies %>% select(company_id, company_name, financial_rating), by = "company_id")
    })

    output$sum_fmt <- renderText({
      val <- suppressWarnings(as.numeric(gsub(",", "", input$sum_assured)))
      if (!is.na(val) && val > 0) format_kes(val) else ""
    })

    output$comparison_plot <- renderPlotly({
      res <- results()
      if (is.null(res) || nrow(res) == 0) return(plot_ly() %>% layout(title = "No products match your criteria"))
      plot_ly(res, x = ~reorder(company_name, estimated_annual), y = ~estimated_annual, type = "bar",
              color = ~financial_rating, colors = c("A" = "#00C853", "A-" = "#64DD17", "B+" = "#FFB300", "B" = "#FF9100"),
              text = ~format_kes(estimated_annual), textposition = "outside") %>%
        layout(xaxis = list(title = ""), yaxis = list(title = "Estimated Annual Premium (KES)"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               legend = list(orientation = "h", y = -0.2))
    })

    output$rider_table <- renderDT({
      data.frame(
        Rider = c("Critical Illness", "Disability", "Accidental Death"),
        Description = c("Lump sum on diagnosis of specified critical illnesses", "Income replacement if unable to work due to disability", "Additional payout if death is due to accident"),
        Typical_Cost = c("+20-30%", "+15-25%", "+10-15%"),
        stringsAsFactors = FALSE
      ) %>% datatable(options = list(dom = 't', ordering = FALSE), rownames = FALSE,
                      colnames = c("Rider", "Description", "Additional Cost"))
    })

    output$products_table <- renderDT({
      res <- results()
      if (is.null(res) || nrow(res) == 0) return(NULL)
      res %>%
        mutate(estimated_annual = format_kes(estimated_annual)) %>%
        select(company_name, financial_rating, product_name, estimated_annual, premium_frequency, waiting_period_months, critical_illness_rider_available, disability_rider_available) %>%
        dt_export(colnames = c("Company", "Rating", "Product", "Est. Annual", "Frequency", "Wait (Mo)", "Critical Illness", "Disability"))
    })
  })
}
