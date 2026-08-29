# ============================================================
# Companies Module
# ============================================================
companies_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title = "Filter Companies", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        class = "glass-card",
        fluidRow(
          column(3, pickerInput(ns("comp_category"), "Category", 
                                choices = c("All", unique(insurance_companies$category)), selected = "All", multiple = TRUE)),
          column(3, pickerInput(ns("comp_rating"), "Financial Rating",
                                choices = c("All", sort(unique(insurance_companies$financial_rating))), selected = "All", multiple = TRUE)),
          column(3, sliderInput(ns("comp_share"), "Min Market Share (%)", min = 0, max = 20, value = 0, step = 0.5)),
          column(3, textInput(ns("comp_search"), "Search Company", placeholder = "Type company name..."))
        )
      )
    ),
    fluidRow(
      bs4Card(
        title = "Insurance Companies", width = 12, status = "primary", solidHeader = TRUE, maximizable = TRUE,
        class = "glass-card",
        DTOutput(ns("companies_table"))
      )
    ),
    fluidRow(
      bs4Card(
        id = ns("company_details_card"),
        title = "Company Details", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
        class = "glass-card",
        uiOutput(ns("company_detail"))
      )
    )
  )
}

companies_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    filtered <- reactive({
      df <- insurance_companies
      if (!is.null(input$comp_category) && !("All" %in% input$comp_category)) {
        df <- df %>% filter(category %in% input$comp_category)
      }
      if (!is.null(input$comp_rating) && !("All" %in% input$comp_rating)) {
        df <- df %>% filter(financial_rating %in% input$comp_rating)
      }
      df <- df %>% filter(market_share_percent >= input$comp_share)
      if (input$comp_search != "") {
        df <- df %>% filter(grepl(tolower(input$comp_search), tolower(company_name)))
      }
      df
    })

    observeEvent(input$companies_table_rows_selected, {
      sel <- input$companies_table_rows_selected
      if (!is.null(sel) && length(sel) == 1 && sel <= nrow(filtered())) {
        if (isTRUE(input$company_details_card$collapsed)) {
          updateBox("company_details_card", session = session, action = "toggle")
        }
      }
    })

    output$companies_table <- renderDT({
      filtered() %>%
        select(company_name, category, year_established, market_share_percent, 
               financial_rating, claims_settlement_rating, branches_count, phone) %>%
        mutate(market_share_percent = paste0(market_share_percent, "%")) %>%
        dt_export(colnames = c("Company", "Category", "Est.", "Market Share", "Fin. Rating", "Claims Rating", "Branches", "Phone"),
                  selection = "single")
    })

    output$company_detail <- renderUI({
      sel <- input$companies_table_rows_selected
      if (is.null(sel) || length(sel) != 1 || sel > nrow(filtered())) {
        return(div(style = "padding: 20px; text-align: center; color: #999;",
                   icon("hand-pointer", class = "fa-3x"), h4("Select a company to view details")))
      }
      comp <- filtered()[sel, ]
      prods <- insurance_products %>% filter(company_id == comp$company_id)

      tagList(
        fluidRow(
          column(12, div(style = "text-align: center; margin-bottom: 20px; padding: 20px; background: rgba(255,255,255,0.5); border-radius: 16px;",
                         company_logo(comp$company_id, comp$company_name, "80px"),
                         h2(comp$company_name, style = "color: #0066CC; margin-top: 15px; font-weight: bold;"),
                         tags$span(class = "badge badge-primary", style = "font-size: 14px; padding: 6px 12px;", comp$category)))
        ),
        fluidRow(
          column(4, div(class = "comparison-wizard",
                        h4(icon("building"), " Company Info", style = "margin-top: 0; color: #0066CC;"),
                        p(icon("calendar"), " Est: ", strong(comp$year_established)),
                        p(icon("location-dot"), " ", comp$headquarters),
                        p(icon("phone"), " ", comp$phone),
                        p(icon("envelope"), " ", comp$email),
                        p(icon("globe"), tags$a(href = paste0("https://", comp$website), target = "_blank", comp$website)))),
          column(4, div(class = "comparison-wizard",
                        h4(icon("chart-line"), " Financial Profile", style = "margin-top: 0; color: #0066CC;"),
                        p("Market Share: ", strong(paste0(comp$market_share_percent, "%"))),
                        p("Rating: ", strong(comp$financial_rating)),
                        p("Claims: ", strong(comp$claims_settlement_rating)),
                        p("Branches: ", strong(comp$branches_count)),
                        p("License: ", strong(comp$ira_license_number)))),
          column(4, div(class = "comparison-wizard",
                        h4(icon("boxes-stacked"), " Products", style = "margin-top: 0; color: #0066CC;"),
                        lapply(unique(prods$category), function(cat) {
                          div(style = "margin-bottom: 8px;",
                              tags$span(class = "badge badge-info", style = "margin-right: 8px;", sum(prods$category == cat)), cat)
                        })))
        ),
        fluidRow(column(12, DTOutput(session$ns("comp_prods"))))
      )
    })

    output$comp_prods <- renderDT({
      sel <- input$companies_table_rows_selected
      if (is.null(sel) || length(sel) != 1 || sel > nrow(filtered())) return(NULL)
      comp <- filtered()[sel, ]
      insurance_products %>%
        filter(company_id == comp$company_id) %>%
        select(product_name, category, sub_category, max_sum_insured_kes, popular_rating) %>%
        mutate(max_sum_insured_kes = ifelse(is.na(max_sum_insured_kes), "No Limit", format_kes(max_sum_insured_kes))) %>%
        datatable(options = list(dom = 't', pageLength = 10), colnames = c("Product", "Category", "Type", "Max Coverage", "Rating"))
    })
  })
}

