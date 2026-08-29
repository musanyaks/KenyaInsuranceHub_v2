# ============================================================
# Kenya Insurance Hub v2 - Main Application
# bs4Dash | Glassmorphism | Dark/Light | Modular
# ============================================================

source("global.R")

# Load all modules
module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
invisible(sapply(module_files, source))

ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  scrollToTop = TRUE,

  header = dashboardHeader(
    title = dashboardBrand(
      title = tags$span(
        tags$img(src = "logo.png", style = "height: 32px; margin-right: 8px; vertical-align: middle;"),
        "Kenya Insurance Hub"
      ),
      color = "primary"
    ),
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("calculator"),
    fixed = FALSE,
    rightUi = tags$li(
      class = "nav-item dropdown",
      tags$a(
        class = "nav-link theme-toggle",
        href = "#",
        onclick = "toggleTheme()",
        icon("moon")
      )
    ),
    leftUi = tags$li(
      class = "nav-item dropdown",
      tags$a(
        class = "nav-link",
        href = "https://www.ira.go.ke",
        target = "_blank",
        icon("globe"), " IRA Kenya"
      )
    )
  ),

  sidebar = dashboardSidebar(
    skin = "dark",
    status = "primary",
    elevation = 3,
    collapsed = FALSE,
    bs4SidebarMenu(
      id = "sidebarMenu",
      bs4SidebarHeader("Main Menu"),
      bs4SidebarMenuItem("Dashboard", tabName = "dashboard", icon = icon("gauge-high")),
      bs4SidebarMenuItem("Companies", tabName = "companies", icon = icon("building")),
      bs4SidebarMenuItem("Motor Insurance", tabName = "motor", icon = icon("car")),
      bs4SidebarMenuItem("Health Insurance", tabName = "health", icon = icon("heart-pulse")),
      bs4SidebarMenuItem("Life Insurance", tabName = "life", icon = icon("user-shield")),
      bs4SidebarMenuItem("Agriculture", tabName = "agriculture", icon = icon("seedling")),
      bs4SidebarMenuItem("Market Analysis", tabName = "market", icon = icon("chart-pie")),
      bs4SidebarMenuItem("Regulatory Info", tabName = "regulatory", icon = icon("scale-balanced")),
      bs4SidebarMenuItem("Insurance Tips", tabName = "tips", icon = icon("lightbulb")),
      bs4SidebarHeader("Developer"),
      bs4SidebarMenuItem("About the Author", tabName = "author", icon = icon("user"))
    )
  ),

  controlbar = dashboardControlbar(
    id = "controlbar",
    skin = "light",
    pinned = FALSE,
    overlay = TRUE,
    collapsed = TRUE,
    controlbarMenu(
      id = "controlbarMenu",
      controlbarItem(
        title = icon("calculator", "Premium Calculator"),
        icon = icon("calculator"),
        numericInput("calc_vehicle_value", "Vehicle Value (KES)", value = 1500000),
        selectInput("calc_vehicle_type", "Vehicle Type", choices = c("Saloon Car", "SUV/4x4", "Pickup/Truck")),
        sliderInput("calc_age", "Your Age", min = 18, max = 80, value = 35),
        hr(),
        h5(textOutput("calc_result"), style = "color: #0066CC; font-weight: bold;")
      )
    )
  ),

  footer = dashboardFooter(
    left = "Kenya Insurance Hub v2.0 | Data from IRA Kenya",
    right = "Built by Musa Rioba"
  ),

  body = dashboardBody(
    use_theme(app_theme),
    useShinyjs(),
    useWaiter(),

    # Background image layer
    tags$div(
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: -1; background: url('background.jpg') no-repeat center center fixed; background-size: cover; opacity: 0.15;"
    ),

    # Preloader
    waiter_show_on_load(
      html = tagList(
        spin_flower(),
        h3("Loading Kenya Insurance Hub...", style = "color: white; margin-top: 20px;"),
        p("Preparing your insurance data...", style = "color: rgba(255,255,255,0.7);")
      ),
      color = "#1a1a2e"
    ),

    # Custom assets
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),

    tabItems(
      tabItem(tabName = "dashboard", dashboard_ui("dashboard")),
      tabItem(tabName = "companies", companies_ui("companies")),
      tabItem(tabName = "motor", motor_ui("motor")),
      tabItem(tabName = "health", health_ui("health")),
      tabItem(tabName = "life", life_ui("life")),
      tabItem(tabName = "agriculture", agriculture_ui("agriculture")),
      tabItem(tabName = "market", market_ui("market")),
      tabItem(tabName = "regulatory", regulatory_ui("regulatory")),
      tabItem(tabName = "tips",
              fluidRow(
                bs4Card(title = "Insurance Tips by Category", width = 12, status = "primary", solidHeader = TRUE, class = "glass-card",
                        fluidRow(
                          column(2, actionButton("tip_motor", "Motor", icon = icon("car"), class = "btn-primary btn-block")),
                          column(2, actionButton("tip_health", "Health", icon = icon("heart-pulse"), class = "btn-success btn-block")),
                          column(2, actionButton("tip_life", "Life", icon = icon("user-shield"), class = "btn-warning btn-block")),
                          column(2, actionButton("tip_agri", "Agriculture", icon = icon("seedling"), class = "btn-info btn-block")),
                          column(2, actionButton("tip_general", "General", icon = icon("circle-info"), class = "btn-secondary btn-block"))
                        ))
              ),
              fluidRow(
                bs4Card(title = "Tips", width = 12, status = "primary", solidHeader = TRUE, class = "glass-card",
                        uiOutput("tips_content_ui"))
              ),
              fluidRow(
                bs4Card(title = "Glossary of Insurance Terms", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, class = "glass-card",
                        DTOutput("glossary_table"))
              )),
      tabItem(tabName = "author",
              fluidRow(
                bs4Card(title = tagList(icon("user"), " About the Author"), width = 12, status = "primary", solidHeader = TRUE, class = "glass-card",
                        fluidRow(
                          column(3, div(style = "text-align: center; padding: 20px;",
                                        img(src = "musa_rioba.jpg", class = "author-avatar", style = "width: 100%; max-width: 180px;"),
                                        h3("Musa Rioba", style = "color: #0066CC; margin-top: 15px; font-weight: bold;"),
                                        p(style = "color: #888; font-size: 14px;", icon("location-dot"), " Kenya"))),
                          column(9, div(style = "padding: 20px;",
                                        h3("Get In Touch", style = "color: #0066CC; margin-top: 0; border-bottom: 2px solid #0066CC; padding-bottom: 10px; display: inline-block;"),
                                        div(style = "margin: 20px 0;",
                                            div(style = "display: flex; align-items: center; margin-bottom: 12px; padding: 12px; background: #f8f9fa; border-radius: 8px;",
                                                div(style = "background: #0066CC; color: white; border-radius: 50%; width: 40px; height: 40px; display: flex; align-items: center; justify-content: center; margin-right: 15px;", icon("phone")),
                                                div(p(style = "margin: 0; color: #888; font-size: 12px;", "Phone"), p(style = "margin: 0; font-size: 16px; font-weight: bold;", "+254 704 059 015"))),
                                            div(style = "display: flex; align-items: center; margin-bottom: 12px; padding: 12px; background: #f8f9fa; border-radius: 8px;",
                                                div(style = "background: #dc3545; color: white; border-radius: 50%; width: 40px; height: 40px; display: flex; align-items: center; justify-content: center; margin-right: 15px;", icon("envelope")),
                                                div(p(style = "margin: 0; color: #888; font-size: 12px;", "Email"), p(style = "margin: 0; font-size: 16px; font-weight: bold;", tags$a(href = "mailto:nyakerabachi@gmail.com", style = "color: #333; text-decoration: none;", "nyakerabachi@gmail.com"))))),
                                        div(style = "background: linear-gradient(135deg, #0066CC, #00A3E0); color: white; padding: 20px; border-radius: 10px; margin-top: 20px;",
                                            h4(style = "margin-top: 0;", icon("quote-left"), " About This App"),
                                            p(style = "margin: 0; font-size: 15px; line-height: 1.6;", "I built this Kenya Insurance Hub to help everyday Kenyans make informed decisions about insurance. Whether you're comparing motor premiums, finding health coverage, exploring life policies, or checking agriculture subsidies — this app puts crucial data at your fingertips. Data is power. Use it wisely."))))
                        ))
              ),
              fluidRow(
                bs4Card(title = tagList(icon("handshake"), " Connect With Me"), width = 12, status = "success", solidHeader = TRUE, class = "glass-card",
                        div(style = "text-align: center; padding: 20px;",
                            tags$a(href = "https://musanyaks.github.io", target = "_blank", class = "btn btn-primary btn-lg", style = "margin: 5px;", icon("globe"), " Portfolio"),
                            tags$a(href = "https://www.linkedin.com/in/musarioba/", target = "_blank", class = "btn btn-info btn-lg", style = "margin: 5px;", icon("linkedin"), " LinkedIn"),
                            tags$a(href = "https://github.com/musanyaks", target = "_blank", class = "btn btn-dark btn-lg", style = "margin: 5px;", icon("github"), " GitHub"),
                            tags$a(href = "mailto:nyakerabachi@gmail.com", class = "btn btn-danger btn-lg", style = "margin: 5px;", icon("envelope"), " Email Me"),
                            tags$a(href = "tel:+254704059015", class = "btn btn-success btn-lg", style = "margin: 5px;", icon("phone"), " Call Me")))
              ))
    )
  )
)

server <- function(input, output, session) {

  # Hide preloader after init
  waiter_hide()

  # Welcome toast
  show_app_toast(session, "Welcome!", "Kenya Insurance Hub v2 is ready. Compare insurance products across all major categories.", "message", 5000)

  # Module servers
  dashboard_server("dashboard", session)
  companies_server("companies")
  motor_server("motor")
  health_server("health")
  life_server("life")
  agriculture_server("agriculture")
  market_server("market")
  regulatory_server("regulatory")

  # Tips reactive
  current_tips <- reactiveVal("general")
  observeEvent(input$tip_motor, { current_tips("motor") })
  observeEvent(input$tip_health, { current_tips("health") })
  observeEvent(input$tip_life, { current_tips("life") })
  observeEvent(input$tip_agri, { current_tips("agriculture") })
  observeEvent(input$tip_general, { current_tips("general") })

  output$tips_content_ui <- renderUI({
    tips <- tips_data[[current_tips()]]
    titles <- c(motor = "Motor Insurance Tips", health = "Health Insurance Tips",
                life = "Life Insurance Tips", agriculture = "Agriculture Insurance Tips",
                general = "General Insurance Tips")
    tagList(
      h3(titles[[current_tips()]], style = "color: #0066CC; margin-bottom: 20px;"),
      lapply(seq_along(tips), function(i) {
        div(class = "tip-card",
            div(style = "background: #0066CC; color: white; border-radius: 50%; width: 28px; height: 28px; display: flex; align-items: center; justify-content: center; margin-right: 12px; flex-shrink: 0;", strong(i)),
            div(style = "padding-top: 2px;", tips[i]))
      })
    )
  })

  output$glossary_table <- renderDT({
    dt_export(glossary_data, colnames = c("Term", "Definition"))
  })

  # Floating calculator result
  output$calc_result <- renderText({
    req(input$calc_vehicle_value, input$calc_vehicle_type)
    val <- as.numeric(gsub(",", "", as.character(input$calc_vehicle_value)))
    if (is.na(val)) return("Enter valid value")
    rate <- ifelse(input$calc_vehicle_type == "Saloon Car", 3.0, ifelse(input$calc_vehicle_type == "SUV/4x4", 3.2, 3.5))
    premium <- max(25000, val * rate / 100)
    paste0("Estimated Premium: KES ", scales::comma(round(premium, 0)), " / year")
  })
}

shinyApp(ui = ui, server = server)

