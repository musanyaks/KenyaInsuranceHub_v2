# Dashboard Module
dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4ValueBoxOutput(ns("vb_companies"), width = 3),
      bs4ValueBoxOutput(ns("vb_products"), width = 3),
      bs4ValueBoxOutput(ns("vb_motor"), width = 3),
      bs4ValueBoxOutput(ns("vb_health"), width = 3)
    ),
    fluidRow(
      glass_card(
        title = "Market Overview", width = 8, status = "primary", solidHeader = TRUE,
        plotlyOutput(ns("market_plot"), height = "350px")
      ),
      glass_card(
        title = "Quick Actions", width = 4, status = "info", solidHeader = TRUE,
        div(style = "padding: 10px;",
            actionButton(ns("qa_motor"), "Compare Motor", icon = icon("car"), class = "btn-primary btn-block mb-2"),
            actionButton(ns("qa_health"), "Compare Health", icon = icon("heart-pulse"), class = "btn-success btn-block mb-2"),
            actionButton(ns("qa_life"), "Compare Life", icon = icon("user-shield"), class = "btn-warning btn-block mb-2"),
            actionButton(ns("qa_agri"), "Agriculture", icon = icon("seedling"), class = "btn-info btn-block")
        )
      )
    ),
    fluidRow(
      glass_card(
        title = "Top Rated Companies", width = 6, status = "primary", solidHeader = TRUE, maximizable = TRUE,
        DTOutput(ns("top_companies"))
      ),
      glass_card(
        title = "Product Distribution", width = 6, status = "primary", solidHeader = TRUE,
        plotlyOutput(ns("category_pie"), height = "350px")
      )
    ),
    fluidRow(
      glass_card(
        title = tagList(icon("user"), " About the Developer"), width = 12, status = "primary", solidHeader = TRUE,
        class = "reveal",
        fluidRow(
          column(2, div(style = "text-align: center;",
                        img(src = "musa_rioba.jpg", class = "author-avatar", style = "width: 100%; max-width: 140px;"))),
          column(10, div(style = "padding-left: 20px;",
                         h2("Musa Rioba", style = "color: #0066CC; font-weight: bold;"),
                         p(icon("location-dot"), " Kenya"),
                         div(class = "mb-3",
                             tags$a(href = "tel:+254704059015", class = "btn btn-outline-primary mr-2", icon("phone"), " +254704059015"),
                             tags$a(href = "mailto:nyakerabachi@gmail.com", class = "btn btn-outline-danger mr-2", icon("envelope"), " Email")),
                         div(class = "mb-3",
                             tags$a(href = "https://musanyaks.github.io", target = "_blank", class = "btn btn-primary mr-2", icon("globe"), " Portfolio"),
                             tags$a(href = "https://www.linkedin.com/in/musarioba/", target = "_blank", class = "btn btn-info mr-2", icon("linkedin"), " LinkedIn"),
                             tags$a(href = "https://github.com/musanyaks", target = "_blank", class = "btn btn-dark", icon("github"), " GitHub")),
                         div(class = "glass-card", style = "padding: 15px;",
                             p(style = "margin: 0; font-style: italic;",
                               icon("quote-left", class = "text-primary mr-2"),
                               "I built this app to help Kenyans make informed insurance decisions. Data is power - use it wisely."))
          ))
        )
      )
    )
  )
}

dashboard_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {

    output$vb_companies <- renderbs4ValueBox({
      bs4ValueBox(
        value = tags$div(class = "counter-anim", `data-target` = nrow(insurance_companies), "0"),
        subtitle = paste0(sum(insurance_companies$license_status == "Active"), " Active"),
        icon = icon("building"), color = "primary", width = 3
      )
    })

    output$vb_products <- renderbs4ValueBox({
      bs4ValueBox(
        value = tags$div(class = "counter-anim", `data-target` = nrow(insurance_products), "0"),
        subtitle = paste0(length(unique(insurance_products$category)), " Categories"),
        icon = icon("shield-halved"), color = "success", width = 3
      )
    })

    output$vb_motor <- renderbs4ValueBox({
      bs4ValueBox(
        value = tags$div(class = "counter-anim", `data-target` = sum(insurance_products$category == "Motor"), "0"),
        subtitle = "Comprehensive & TPO",
        icon = icon("car"), color = "warning", width = 3
      )
    })

    output$vb_health <- renderbs4ValueBox({
      bs4ValueBox(
        value = tags$div(class = "counter-anim", `data-target` = sum(insurance_products$category == "Health"), "0"),
        subtitle = "Individual & Family",
        icon = icon("heart-pulse"), color = "danger", width = 3
      )
    })

    output$market_plot <- renderPlotly({
      top10 <- insurance_companies %>% arrange(desc(market_share_percent)) %>% head(10)
      plot_ly(top10, x = ~reorder(company_name, market_share_percent), 
              y = ~market_share_percent, type = "bar",
              color = ~financial_rating,
              colors = c("A" = "#00C853", "A-" = "#64DD17", "B+" = "#FFB300", "B" = "#FF9100", "B-" = "#FF1744"),
              text = ~paste0(market_share_percent, "%"), textposition = "outside") %>%
        layout(xaxis = list(title = ""), yaxis = list(title = "Market Share (%)"),
               showlegend = TRUE, margin = list(b = 100),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
    })

    output$top_companies <- renderDT({
      insurance_companies %>%
        arrange(desc(popular_rating)) %>%
        head(10) %>%
        select(company_name, category, financial_rating, popular_rating, market_share_percent, branches_count) %>%
        mutate(market_share_percent = paste0(market_share_percent, "%")) %>%
        dt_export(colnames = c("Company", "Category", "Fin. Rating", "Avg. Rating", "Market Share", "Branches"))
    })

    output$category_pie <- renderPlotly({
      cats <- insurance_products %>% group_by(category) %>% summarise(count = n(), .groups = "drop")
      plot_ly(cats, labels = ~category, values = ~count, type = "pie",
              hole = 0.4, textinfo = "label+percent",
              marker = list(colors = c("#0066CC", "#00C853", "#FFB300", "#FF1744", "#00B8D4"))) %>%
        layout(showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)",
               annotations = list(text = "Products", showarrow = FALSE, font = list(size = 14)))
    })

    observeEvent(input$qa_motor, { updateTabItems(parent_session, "sidebarMenu", "motor") })
    observeEvent(input$qa_health, { updateTabItems(parent_session, "sidebarMenu", "health") })
    observeEvent(input$qa_life, { updateTabItems(parent_session, "sidebarMenu", "life") })
    observeEvent(input$qa_agri, { updateTabItems(parent_session, "sidebarMenu", "agriculture") })
  })
}
