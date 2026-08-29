# ============================================================
# Regulatory Module
# ============================================================
regulatory_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title = "Insurance Regulatory Authority of Kenya", width = 12, status = "primary", solidHeader = TRUE,
        class = "glass-card",
        uiOutput(ns("main_ui"))
      )
    ),
    fluidRow(
      bs4Card(
        title = "How to File a Complaint", width = 6, status = "warning", solidHeader = TRUE,
        class = "glass-card",
        HTML("<ol><li><strong>Contact the insurer first</strong> - Try to resolve directly</li>
              <li><strong>Gather documentation</strong> - Policy docs, correspondence, claim forms</li>
              <li><strong>File with IRA</strong> - Use complaint hotline or online portal</li>
              <li><strong>Provide details</strong> - Company name, policy number, nature of complaint</li>
              <li><strong>Follow up</strong> - IRA will investigate and mediate</li></ol>
              <p><strong>Hotline:</strong> 0709 912 000 | <strong>Email:</strong> info@ira.go.ke</p>")
      ),
      bs4Card(
        title = "Verify Before You Buy", width = 6, status = "info", solidHeader = TRUE,
        class = "glass-card",
        HTML("<ul><li>Verify company license at <a href='https://www.ira.go.ke' target='_blank'>www.ira.go.ke</a></li>
              <li>Check agent's license and credentials</li>
              <li>Read policy document carefully</li>
              <li>Understand exclusions and waiting periods</li>
              <li>Get quotes from at least 3 companies</li>
              <li>Never pay premiums to an individual agent</li></ul>
              <div class='alert alert-danger'><strong>Warning:</strong> Unlicensed insurance schemes are illegal.</div>")
      )
    )
  )
}

regulatory_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$main_ui <- renderUI({
      reg <- regulatory_data[1, ]
      tagList(
        fluidRow(
          column(6, div(class = "comparison-wizard",
                        h3(reg$full_name, style = "color: #0066CC; margin-top: 0;"),
                        p(icon("calendar"), " Est: ", strong(reg$established_year)),
                        p(icon("location-dot"), " ", reg$physical_address),
                        p(icon("phone"), " ", reg$phone),
                        p(icon("envelope"), " ", reg$email),
                        p(icon("globe"), tags$a(href = paste0("https://", reg$website), target = "_blank", reg$website)))),
          column(6, div(class = "premium-highlight",
                        h4(icon("phone-volume"), " Complaint Hotline", style = "margin-top: 0;"),
                        h2(reg$complaint_hotline, style = "margin: 10px 0;"),
                        p("License Renewal: ", strong(format_kes(reg$license_renewal_fee_kes)))),
                   div(class = "comparison-wizard", h4("Key Functions", style = "margin-top: 0;"), p(reg$functions)))
        )
      )
    })
  })
}

