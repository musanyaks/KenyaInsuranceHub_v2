# ============================================================
# Motor Insurance Module
# ============================================================
motor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title = "Motor Insurance Calculator", width = 12, status = "primary", solidHeader = TRUE,
        class = "glass-card",
        fluidRow(
          column(3, selectInput(ns("vehicle_type"), "Vehicle Type",
                                choices = c("Saloon Car", "SUV/4x4", "Pickup/Truck", "Matatu (14-seater)", "Matatu (33-seater)", "Motorcycle"))),
          column(3, tagAppendAttributes(textInput(ns("vehicle_value"), "Vehicle Value (KES)", value = "1500000"), class = "kes-input"),
                 div(style = "margin-top: -10px; color: #0066CC; font-weight: bold; font-size: 12px;", textOutput(ns("value_fmt")))),
          column(2, sliderInput(ns("vehicle_age"), "Vehicle Age (Years)", min = 0, max = 15, value = 2)),
          column(2, selectInput(ns("cover_type"), "Cover Type", choices = c("Comprehensive", "Third Party", "Both"))),
          column(2, div(style = "margin-top: 25px;", actionButton(ns("compare"), "Compare Quotes", icon = icon("magnifying-glass"), class = "btn-primary btn-block")))
      )
      )
    ),
    fluidRow(
      bs4Card(title = "Premium Comparison", width = 8, status = "primary", solidHeader = TRUE,
              class = "glass-card",
              plotlyOutput(ns("comparison_plot"), height = "400px")),
      bs4Card(title = "Cover Type Comparison", width = 4, status = "info", solidHeader = TRUE, maximizable = TRUE,
              class = "glass-card",
              DTOutput(ns("cover_comparison")))
    ),
    fluidRow(
      bs4Card(title = "Detailed Quotes", width = 12, status = "primary", solidHeader = TRUE, maximizable = TRUE,
              class = "glass-card",
              DTOutput(ns("quotes_table")))
    )
  )
}

motor_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    results <- eventReactive(input$compare, {
      req(input$vehicle_type, input$vehicle_value)
      val <- as.numeric(gsub(",", "", input$vehicle_value))
      req(!is.na(val))

      res <- motor_premiums %>%
        filter(vehicle_type == input$vehicle_type,
               vehicle_value_min_kes <= val, vehicle_value_max_kes >= val,
               age_min_years <= input$vehicle_age, age_max_years >= input$vehicle_age)
      if (nrow(res) == 0) return(NULL)

      res$calculated_comprehensive <- pmax(res$comprehensive_min_premium_kes, val * res$comprehensive_rate_percent / 100)
      res %>% left_join(insurance_companies %>% select(company_id, company_name, financial_rating), by = "company_id")
    })

    output$value_fmt <- renderText({
      val <- suppressWarnings(as.numeric(gsub(",", "", input$vehicle_value)))
      if (!is.na(val) && val > 0) format_kes(val) else ""
    })

    output$comparison_plot <- renderPlotly({
      res <- results()
      if (is.null(res) || nrow(res) == 0) return(plot_ly() %>% layout(title = "No data for selected criteria"))

      if (input$cover_type == "Both") {
        plot_ly(res) %>%
          add_bars(x = ~reorder(company_name, calculated_comprehensive), y = ~calculated_comprehensive,
                   name = "Comprehensive", marker = list(color = "#0066CC"),
                   text = ~format_kes(calculated_comprehensive), textposition = "outside") %>%
          add_bars(x = ~reorder(company_name, calculated_comprehensive), y = ~third_party_premium_kes,
                   name = "Third Party", marker = list(color = "#FFB300"),
                   text = ~format_kes(third_party_premium_kes), textposition = "outside") %>%
          layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Premium (KES)"),
                 paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                 legend = list(orientation = "h", y = -0.2))
      } else if (input$cover_type == "Comprehensive") {
        plot_ly(res, x = ~reorder(company_name, calculated_comprehensive), y = ~calculated_comprehensive, type = "bar",
                color = ~financial_rating, colors = c("A" = "#00C853", "A-" = "#64DD17", "B+" = "#FFB300", "B" = "#FF9100", "B-" = "#FF1744"),
                text = ~format_kes(calculated_comprehensive), textposition = "outside") %>%
          layout(xaxis = list(title = ""), yaxis = list(title = "Premium (KES)"),
                 paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
      } else {
        plot_ly(res, x = ~reorder(company_name, third_party_premium_kes), y = ~third_party_premium_kes, type = "bar",
                color = ~financial_rating, colors = c("A" = "#00C853", "A-" = "#64DD17", "B+" = "#FFB300", "B" = "#FF9100", "B-" = "#FF1744"),
                text = ~format_kes(third_party_premium_kes), textposition = "outside") %>%
          layout(xaxis = list(title = ""), yaxis = list(title = "Premium (KES)"),
                 paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
      }
    })

    output$cover_comparison <- renderDT({
      data.frame(
        Feature = c("Own Vehicle Damage", "Theft Cover", "Third Party Injury", "Third Party Property", "Fire Damage", "Natural Disasters", "Towing/Recovery", "Typical Premium"),
        Comprehensive = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "KES 22K - 72K"),
        Third_Party = c("No", "No", "Yes", "Yes (limit)", "No", "No", "No", "KES 7,500"),
        stringsAsFactors = FALSE
      ) %>% datatable(options = list(dom = 't', ordering = FALSE), rownames = FALSE,
                      colnames = c("Feature", "Comprehensive", "Third Party"))
    })

    output$quotes_table <- renderDT({
      res <- results()
      if (is.null(res) || nrow(res) == 0) return(NULL)
      res %>%
        mutate(calculated_comprehensive = format_kes(calculated_comprehensive),
               third_party_premium_kes = format_kes(third_party_premium_kes),
               excess_kes = format_kes(excess_kes)) %>%
        select(company_name, financial_rating, calculated_comprehensive, third_party_premium_kes, excess_kes, notes) %>%
        dt_export(colnames = c("Company", "Rating", "Comprehensive", "Third Party", "Excess", "Notes"))
    })
  })
}

