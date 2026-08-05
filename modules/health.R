# ============================================================
# Health Insurance Module
# ============================================================
health_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title = "Health Insurance Calculator", width = 12, status = "primary", solidHeader = TRUE,
        class = "glass-card",
        fluidRow(
          column(2, numericInput(ns("age"), "Your Age", value = 35, min = 18, max = 80)),
          column(2, selectInput(ns("cover_type"), "Cover Type", choices = c("Comprehensive", "Inpatient Only"))),
          column(2, selectInput(ns("plan_type"), "Plan Type", choices = c("Individual", "Family of 4", "Both"))),
          column(2, selectInput(ns("sort_by"), "Sort By", choices = c("Premium (Low-High)" = "premium", "Rating (High-Low)" = "rating", "Network Size" = "network"))),
          column(2, div(style = "margin-top: 25px;", actionButton(ns("compare"), "Compare Plans", icon = icon("magnifying-glass"), class = "btn-primary btn-block")))
      )
      )
    ),
    fluidRow(
      bs4Card(title = "Premium by Age Group", width = 6, status = "primary", solidHeader = TRUE,
              class = "glass-card",
              plotlyOutput(ns("age_plot"), height = "350px")),
      bs4Card(title = "Coverage Comparison", width = 6, status = "primary", solidHeader = TRUE,
              class = "glass-card",
              plotlyOutput(ns("coverage_plot"), height = "350px"))
    ),
    fluidRow(
      bs4Card(title = "Health Plan Details", width = 12, status = "primary", solidHeader = TRUE, maximizable = TRUE,
              class = "glass-card",
              DTOutput(ns("plans_table")))
    )
  )
}

health_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    results <- eventReactive(input$compare, {
      req(input$age, input$cover_type)
      filtered <- health_premiums %>% filter(cover_type == input$cover_type)
      age <- input$age
      age_col <- case_when(
        age <= 30 ~ "age_18_30_kes",
        age <= 40 ~ "age_31_40_kes",
        age <= 50 ~ "age_41_50_kes",
        age <= 60 ~ "age_51_60_kes",
        TRUE ~ "age_61_80_kes"
      )
      filtered$individual_premium <- filtered[[age_col]]
      filtered %>% left_join(insurance_companies %>% select(company_id, company_name, financial_rating), by = "company_id")
    })

    output$age_plot <- renderPlotly({
      req(input$compare)
      age_cols <- c("age_18_30_kes", "age_31_40_kes", "age_41_50_kes", "age_51_60_kes", "age_61_80_kes")
      age_labels <- c("18-30", "31-40", "41-50", "51-60", "61-80")
      hp <- health_premiums %>% filter(cover_type == input$cover_type) %>% left_join(insurance_companies %>% select(company_id, company_name), by = "company_id")

      plot_data <- do.call(rbind, lapply(1:length(age_cols), function(i) {
        tmp <- hp %>% select(company_name, premium = all_of(age_cols[i]))
        tmp$age_group <- age_labels[i]
        tmp
      }))

      plot_ly(plot_data, x = ~age_group, y = ~premium, color = ~company_name, type = "scatter", mode = "lines+markers") %>%
        layout(xaxis = list(title = "Age Group"), yaxis = list(title = "Premium (KES)"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               legend = list(orientation = "h", y = -0.3))
    })

    output$coverage_plot <- renderPlotly({
      req(input$compare)
      res <- results()
      if (is.null(res) || nrow(res) == 0) return(NULL)
      plot_ly(res, x = ~reorder(plan_name, individual_premium), y = ~inpatient_limit_kes, type = "bar",
              name = "Inpatient", marker = list(color = "#0066CC"),
              text = ~format_kes(inpatient_limit_kes), textposition = "outside") %>%
        add_trace(y = ~outpatient_limit_kes, name = "Outpatient", marker = list(color = "#00C853"),
                  text = ~format_kes(outpatient_limit_kes), textposition = "outside") %>%
        add_trace(y = ~maternity_limit_kes, name = "Maternity", marker = list(color = "#FFB300"),
                  text = ~format_kes(maternity_limit_kes), textposition = "outside") %>%
        layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Coverage Limit (KES)"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               legend = list(orientation = "h", y = -0.3))
    })

    output$plans_table <- renderDT({
      res <- results()
      if (is.null(res) || nrow(res) == 0) return(NULL)
      df <- res %>%
        mutate(individual_premium = format_kes(individual_premium),
               family_of_4_kes = format_kes(family_of_4_kes),
               inpatient_limit_kes = format_kes(inpatient_limit_kes),
               outpatient_limit_kes = ifelse(outpatient_limit_kes == 0, "Not Covered", format_kes(outpatient_limit_kes))) %>%
        select(company_name, financial_rating, plan_name, inpatient_limit_kes, outpatient_limit_kes, individual_premium, family_of_4_kes, network_hospitals_count, waiting_period_days)
      names(df) <- c("Company", "Rating", "Plan", "Inpatient", "Outpatient", "Individual", "Family of 4", "Hospitals", "Wait (Days)")
      if (input$sort_by == "premium") df <- df[order(as.numeric(gsub("[^0-9]", "", df$Individual))), ]
      else if (input$sort_by == "rating") df <- df[order(df$Rating, decreasing = TRUE), ]
      else df <- df[order(df$Hospitals, decreasing = TRUE), ]
      dt_export(df)
    })
  })
}
