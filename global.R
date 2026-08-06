# ============================================================
# Kenya Insurance Hub v2 - Global Configuration & Data
# ============================================================

cat("global.R start:", Sys.time(), "\n"); flush.console()

# Required packages ----
# NOTE: these are literal library() calls (not a loop over a character vector)
# on purpose -- rsconnect's dependency scanner only detects packages this way
# when deciding what to bundle/install on shinyapps.io. A loop using
# library(p, character.only = TRUE) is invisible to that scanner and will
# cause packages to silently NOT be installed on deploy.
suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(shinyjs)
  library(waiter)
  library(DT)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(leaflet)
  library(fresh)
  library(htmltools)
})

packages <- c(
  "shiny", "bs4Dash", "shinyWidgets", "shinyjs", "waiter",
  "DT", "plotly", "ggplot2", "dplyr", "tidyr", "scales",
  "leaflet", "fresh", "htmltools"
)
cat("Packages loaded:", paste(packages, collapse = ", "), "\n"); flush.console()

# ============================================================
# THEME & BRAND COLORS
# ============================================================
brand_colors <- list(
  primary   = "#0066CC",
  secondary = "#00A3E0",
  success   = "#00C853",
  warning   = "#FFB300",
  danger    = "#FF1744",
  info      = "#00B8D4",
  dark      = "#1a1a2e",
  light     = "#f8f9fa",
  glass     = "rgba(255, 255, 255, 0.08)",
  glassBorder = "rgba(255, 255, 255, 0.18)"
)

# bs4Dash fresh theme ----
app_theme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#2c3e50",
    navbar_light_active_color = "#0066CC",
    navbar_light_hover_color = "#00A3E0"
  ),
  bs4dash_yiq(
    text_dark = "#2c3e50",
    text_light = "#ffffff"
  ),
  bs4dash_status(
    primary = "#0066CC",
    secondary = "#00A3E0",
    success = "#00C853",
    info = "#00B8D4",
    warning = "#FFB300",
    danger = "#FF1744",
    light = "#f8f9fa",
    dark = "#1a1a2e"
  )
)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

#' Format KES currency
format_kes <- function(x, suffix = "") {
  ifelse(is.na(x) | x == 0,
         "N/A",
         paste0("KES ", scales::comma(round(x, 0)), suffix))
}

#' Format percentage
format_pct <- function(x) {
  paste0(round(x, 1), "%")
}

#' Star rating HTML
star_rating <- function(rating, max = 5) {
  full <- floor(rating)
  half <- ifelse(rating - full >= 0.5, 1, 0)
  empty <- max - full - half
  
  stars <- paste0(
    strrep("<i class='fas fa-star text-warning'></i>", full),
    strrep("<i class='fas fa-star-half-alt text-warning'></i>", half),
    strrep("<i class='far fa-star text-muted'></i>", empty)
  )
  HTML(paste0(stars, " <span class='text-muted'>(", rating, ")</span>"))
}

#' Glassmorphism card wrapper
glass_card <- function(..., title = NULL, status = "primary", solidHeader = FALSE,
                       width = 12, collapsible = TRUE, collapsed = FALSE) {
  bs4Card(
    title = title,
    status = status,
    solidHeader = solidHeader,
    width = width,
    collapsible = collapsible,
    collapsed = collapsed,
    class = "glass-card",
    ...
  )
}

#' Animated value box
anim_value_box <- function(value, subtitle, icon = "chart-line", color = "primary",
                           href = NULL, width = 3) {
  bs4ValueBox(
    value = tags$div(
      class = "counter-anim",
      `data-target` = as.numeric(gsub("[^0-9.]", "", as.character(value))),
      "0"
    ),
    subtitle = subtitle,
    icon = icon,
    color = color,
    href = href,
    width = width,
    footer = NULL
  )
}

#' Notification toast
show_app_toast <- function(session, title, message, type = "info", duration = 4000) {
  showNotification(
    ui = tags$div(
      tags$strong(title),
      tags$br(),
      message
    ),
    type = type,
    duration = duration,
    closeButton = TRUE
  )
}

#' DT with export buttons
dt_export <- function(data, ...) {
  datatable(
    data,
    extensions = c("Buttons", "Responsive"),
    options = list(
      dom = "Blfrtip",
      buttons = list(
        list(extend = "copy", text = "<i class='fas fa-copy'></i> Copy", className = "btn-sm btn-outline-primary"),
        list(extend = "csv", text = "<i class='fas fa-file-csv'></i> CSV", className = "btn-sm btn-outline-primary"),
        list(extend = "excel", text = "<i class='fas fa-file-excel'></i> Excel", className = "btn-sm btn-outline-success"),
        list(extend = "pdf", text = "<i class='fas fa-file-pdf'></i> PDF", className = "btn-sm btn-outline-danger"),
        list(extend = "print", text = "<i class='fas fa-print'></i> Print", className = "btn-sm btn-outline-secondary")
      ),
      pageLength = 10,
      lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "All")),
      scrollX = TRUE,
      responsive = TRUE,
      language = list(
        search = "<i class='fas fa-search'></i>",
        lengthMenu = "Show _MENU_ entries"
      )
    ),
    rownames = FALSE,
    class = "display nowrap compact",
    ...
  )
}

# ============================================================
# EMBEDDED DATASETS
# ============================================================

cat("Loading embedded datasets...\n"); flush.console()

insurance_companies <- read.csv(textConnection(r"(
company_id,company_name,category,license_status,year_established,headquarters,website,phone,email,contact_person,branches_count,market_share_percent,financial_rating,claims_settlement_rating,regu[...]
IRA001,Jubilee Insurance Company Limited,Composite,Active,1937,Nairobi,www.jubileeinsurance.com,+254 20 3285000,info@jubileeinsurance.com,Julius Kipng'etich,38,15.2,A,Excellent,Compliant,IRA/01/0[...]
IRA002,Britam Insurance Company,Composite,Active,1965,Nairobi,www.britam.com,+254 20 2839000,info@britam.com,Tom Gitogo,25,12.8,A,Excellent,Compliant,IRA/01/002
IRA003,CIC Insurance Group Limited,Composite,Active,1984,Nairobi,www.cic.co.ke,+254 20 2823000,info@cic.co.ke,Nelson Kuria,28,10.5,A-,Very Good,Compliant,IRA/01/003
IRA004,APA Insurance Limited,Composite,Active,1979,Nairobi,www.apainsurance.org,+254 20 2860000,info@apainsurance.org,Ashok Shah,22,8.3,A-,Good,Compliant,IRA/01/004
IRA005,UAP Old Mutual Insurance,Composite,Active,1920,Nairobi,www.uapoldmutual.com,+254 20 2850000,info@uapoldmutual.com,Peter Mwangi,30,9.1,A,Excellent,Compliant,IRA/01/005
IRA006,Kenindia Assurance Company Limited,Composite,Active,1978,Nairobi,www.kenindia.co.ke,+254 20 2229861,info@kenindia.co.ke,Rajesh Shah,15,3.2,B+,Good,Compliant,IRA/01/006
IRA007,Pacis Insurance Company Limited,General,Active,1994,Nairobi,www.pacisinsurance.com,+254 20 2718860,info@pacisinsurance.com,John Mwangi,12,2.8,B+,Good,Compliant,IRA/01/007
IRA008,Madison Insurance Company Kenya Limited,Life,Active,1988,Nairobi,www.madison.co.ke,+254 20 2859000,info@madison.co.ke,John Mwangi,18,4.5,A-,Very Good,Compliant,IRA/01/008
IRA009,Heritage Insurance Company Limited,General,Active,1994,Nairobi,www.heritageinsurance.co.ke,+254 20 2741000,info@heritageinsurance.co.ke,Godfrey Kiptum,10,2.1,B,Good,Compliant,IRA/01/009
IRA010,GA Insurance Limited,Composite,Active,1979,Nairobi,www.gainsurance.com,+254 20 2228531,info@gainsurance.com,John Mwangi,14,3.8,B+,Good,Compliant,IRA/01/010
IRA011,AIG Kenya Insurance Company Limited,General,Active,1972,Nairobi,www.aig.co.ke,+254 20 2719922,info@aig.co.ke,John Mwangi,8,2.5,A-,Good,Compliant,IRA/01/011
IRA012,Canon Assurance Company Limited,Composite,Active,1976,Nairobi,www.canonassurance.com,+254 20 2222111,info@canonassurance.com,John Mwangi,10,1.9,B,Good,Compliant,IRA/01/012
IRA013,First Assurance Company Limited,Composite,Active,1933,Nairobi,www.firstassurance.co.ke,+254 20 3286000,info@firstassurance.co.ke,John Mwangi,16,3.1,B+,Good,Compliant,IRA/01/014
IRA014,ICEA Lion General Insurance,General,Active,2011,Nairobi,www.icealion.co.ke,+254 20 2888000,info@icealion.co.ke,John Mwangi,20,4.2,A-,Very Good,Compliant,IRA/01/015
IRA015,ICEA Lion Life Assurance,Life,Active,2011,Nairobi,www.icealion.co.ke,+254 20 2888000,info@icealion.co.ke,John Mwangi,20,3.9,A-,Very Good,Compliant,IRA/01/016
IRA016,Liberty Life Assurance Kenya Limited,Life,Active,1964,Nairobi,www.liberty.co.ke,+254 20 2859000,info@liberty.co.ke,John Mwangi,15,3.5,A,Excellent,Compliant,IRA/01/017
IRA017,Metropolitan Life Insurance Kenya,Life,Active,2015,Nairobi,www.metropolitan.co.ke,+254 20 2719000,info@metropolitan.co.ke,John Mwangi,8,1.8,B+,Good,Compliant,IRA/01/018
IRA018,Old Mutual Life Assurance Company Limited,Life,Active,1920,Nairobi,www.oldmutual.co.ke,+254 20 2850000,info@oldmutual.co.ke,Peter Mwangi,12,2.7,A-,Very Good,Compliant,IRA/01/019
IRA019,Phoenix of East Africa Assurance,Composite,Active,1984,Nairobi,www.phoenixofea.com,+254 20 2222000,info@phoenixofea.com,John Mwangi,9,1.5,B,Good,Compliant,IRA/01/020
IRA020,Takaful Insurance of Africa Limited,Islamic,Active,2008,Nairobi,www.takafulafrica.com,+254 20 2718000,info@takafulafrica.com,Hassan Bashir,6,1.2,B+,Good,Compliant,IRA/01/021
IRA021,Trident Insurance Company Limited,General,Active,1981,Nairobi,www.trident.co.ke,+254 20 2223333,info@trident.co.ke,John Mwangi,7,1.1,B,Good,Compliant,IRA/01/022
IRA022,Xplico Insurance Company Limited,General,Active,2015,Nairobi,www.xplico.co.ke,+254 20 2717000,info@xplico.co.ke,John Mwangi,5,0.8,B-,Fair,Compliant,IRA/01/023
IRA023,Sanlam Kenya PLC,Life,Active,1965,Nairobi,www.sanlam.co.ke,+254 20 2850000,info@sanlam.co.ke,John Mwangi,10,2.3,A-,Good,Compliant,IRA/01/024
IRA024,Mayfair Insurance Company Limited,General,Active,1992,Nairobi,www.mayfair.co.ke,+254 20 2224444,info@mayfair.co.ke,John Mwangi,6,0.9,B,Good,Compliant,IRA/01/025
IRA025,Kenya Orient Insurance Limited,General,Active,1982,Nairobi,www.kenyaorient.co.ke,+254 20 2855000,info@kenyaorient.co.ke,John Mwangi,11,1.6,B+,Good,Compliant,IRA/01/026
IRA026,Directline Assurance Company Limited,General,Active,1998,Nairobi,www.directline.co.ke,+254 20 2716000,info@directline.co.ke,John Mwangi,8,1.4,B,Good,Compliant,IRA/01/027
IRA027,Intra Africa Assurance Company Limited,Composite,Active,1981,Nairobi,www.intraafrica.co.ke,+254 20 2225555,info@intraafrica.co.ke,John Mwangi,7,1.0,B,Good,Compliant,IRA/01/028
IRA028,Co-operative Insurance Company of Kenya,Composite,Active,1973,Nairobi,www.cic.co.ke,+254 20 2823000,info@cic.co.ke,Nelson Kuria,20,5.1,A-,Very Good,Compliant,IRA/01/029
)"), stringsAsFactors = FALSE)
cat("Loaded insurance_companies (rows =", nrow(insurance_companies), ")\n"); flush.console()

insurance_products <- read.csv(textConnection(r"(
product_id,company_id,product_name,category,sub_category,description,coverage_type,min_age,max_age,waiting_period_days,max_sum_insured_kes,premium_frequency,optional_riders,target_market,popular_[...]
PROD001,IRA001,Jubilee Health Insurance,Health,Medical Cover,Comprehensive inpatient and outpatient medical cover,Inpatient/Outpatient,0,80,30,10000000,Monthly/Annual,Optical/Dental/Maternity,Ind[...]
PROD002,IRA001,Jubilee Motor Comprehensive,Motor,Comprehensive,Full cover for private vehicles including theft and third party,Comprehensive,18,75,0,5000000,Annual,Excess Waiver/Personal Accident[...]
PROD003,IRA001,Jubilee Motor Third Party,Motor,Third Party,Legal liability cover for third party injury and property damage,Third Party Only,18,75,0,5000000,Annual,None,Private Vehicle Owners,4.0
PROD004,IRA001,Jubilee Life Assurance,Life,Term Life,Financial protection for family in case of death,Death Benefit,18,65,0,50000000,Monthly/Annual/Quarterly,Critical Illness/Disability,Income Ea[...]
PROD005,IRA001,Jubilee Education Policy,Life,Education,Save for children's education with life cover,Endowment/Savings,0,18,0,5000000,Monthly/Annual,None,Parents,4.2
PROD006,IRA002,Britam Medical Insurance,Health,Medical Cover,Comprehensive health insurance for individuals and families,Inpatient/Outpatient,0,80,30,15000000,Monthly/Annual,Optical/Dental/Matern[...]
PROD007,IRA002,Britam Motor Comprehensive,Motor,Comprehensive,All-risk motor insurance for private and commercial vehicles,Comprehensive,18,75,0,10000000,Annual,Excess Waiver/Entertainment Unit,V[...]
PROD008,IRA002,Britam Third Party Motor,Motor,Third Party,Mandatory third party liability cover,Third Party Only,18,75,0,5000000,Annual,None,All Vehicle Owners,4.1
PROD009,IRA002,Britam Life Cover,Life,Term Life,Protection against death with optional critical illness rider,Death Benefit,18,70,0,100000000,Monthly/Annual/Quarterly,Critical Illness/TPD,All Adu[...]
PROD010,IRA002,Britam Pension Plan,Life,Retirement,Retirement savings with guaranteed returns,Pension/Annuity,18,65,0,999999999,Monthly/Annual,None,Employed/Self-employed,4.3
)"), stringsAsFactors = FALSE)
cat("Loaded insurance_products (rows =", nrow(insurance_products), ")\n"); flush.console()

insurance_products$max_sum_insured_kes <- as.numeric(insurance_products$max_sum_insured_kes)

company_ratings <- insurance_products %>%
  group_by(company_id) %>%
  summarise(popular_rating = round(mean(popular_rating, na.rm = TRUE), 1), .groups = "drop")

insurance_companies <- insurance_companies %>%
  left_join(company_ratings, by = "company_id")
cat("Processed company ratings and joined to insurance_companies\n"); flush.console()

motor_premiums <- read.csv(textConnection(r"(
premium_id,company_id,vehicle_type,vehicle_value_min_kes,vehicle_value_max_kes,age_min_years,age_max_years,comprehensive_rate_percent,comprehensive_min_premium_kes,third_party_premium_kes,excess_[...]
MOT001,IRA001,Saloon Car,500000,1000000,0,5,3.5,25000,7500,20000,Standard rate
MOT002,IRA001,Saloon Car,1000001,2000000,0,5,3.0,35000,7500,25000,Standard rate
MOT003,IRA001,Saloon Car,2000001,5000000,0,5,2.5,60000,7500,35000,Standard rate
MOT004,IRA001,SUV/4x4,1000000,3000000,0,5,3.2,45000,7500,30000,Standard rate
MOT005,IRA001,SUV/4x4,3000001,8000000,0,5,2.8,96000,7500,50000,Standard rate
MOT006,IRA001,Pickup/Truck,800000,2500000,0,5,3.0,35000,7500,25000,Standard rate
MOT007,IRA001,Matatu (14-seater),1500000,3000000,0,5,4.0,60000,7500,40000,PSV rate
MOT008,IRA001,Matatu (33-seater),2500000,5000000,0,5,4.5,112500,7500,50000,PSV rate
MOT009,IRA001,Motorcycle,100000,500000,0,5,5.0,5000,5000,10000,Commercial use
MOT010,IRA002,Saloon Car,500000,1000000,0,5,3.8,28000,7500,20000,Standard rate
)"), stringsAsFactors = FALSE)
cat("Loaded motor_premiums (rows =", nrow(motor_premiums), ")\n"); flush.console()

health_premiums <- read.csv(textConnection(r"(
premium_id,company_id,plan_name,cover_type,inpatient_limit_kes,outpatient_limit_kes,maternity_limit_kes,dental_limit_kes,optical_limit_kes,age_18_30_kes,age_31_40_kes,age_41_50_kes,age_51_60_kes,[...]
HEA001,IRA001,Jubilee Premier,Comprehensive,5000000,100000,150000,50000,50000,45000,55000,75000,120000,180000,180000,30,400
HEA002,IRA001,Jubilee Standard,Comprehensive,3000000,50000,100000,30000,30000,28000,35000,50000,80000,120000,120000,30,350
HEA003,IRA001,Jubilee Basic,Inpatient Only,2000000,0,0,0,0,15000,18000,25000,40000,60000,60000,30,300
)"), stringsAsFactors = FALSE)
cat("Loaded health_premiums (rows =", nrow(health_premiums), ")\n"); flush.console()

life_premiums <- read.csv(textConnection(r"(
premium_id,company_id,product_name,policy_type,sum_assured_min_kes,sum_assured_max_kes,entry_age_min,entry_age_max,policy_term_min_years,policy_term_max_years,premium_frequency,premium_per_1m_kes[...]
LIF001,IRA001,Jubilee Term Life,Term Life,1000000,50000000,18,65,5,30,Monthly/Annual,25000,0,0,Yes,Yes,Yes
LIF002,IRA001,Jubilee Endowment,Endowment,500000,10000000,0,60,10,25,Monthly/Annual,45000,0,3,Yes,Yes,Yes
)"), stringsAsFactors = FALSE)
cat("Loaded life_premiums (rows =", nrow(life_premiums), ")\n"); flush.console()

agriculture_products <- read.csv(textConnection(r"(
product_id,company_id,product_name,crop_type,coverage_type,sum_insured_per_acre_kes,premium_rate_percent,subsidy_available,subsidy_percent,govt_program,min_area_acres,max_area_acres,waiting_perio[...]
AGR001,IRA001,Jubilee Crop Insurance,Maize,Multi-Peril,30000,5,Yes,50,KAIP,1,100,14,2,No,Registered farmer
AGR002,IRA001,Jubilee Livestock Insurance,Cattle/Livestock,Mortality,50000,4,Yes,50,KLIP,5,500,30,1,No,Registered pastoralist
)"), stringsAsFactors = FALSE)
cat("Loaded agriculture_products (rows =", nrow(agriculture_products), ")\n"); flush.console()

nhif_data <- read.csv(textConnection(r"(
benefit_category,benefit_name,description,coverage_limit_kes,co_payment_required,co_payment_amount_kes,waiting_period_days,eligibility,notes
Inpatient,General Ward,Admission in general ward,999999999,No,0,0,All members,No co-payment for members
)"), stringsAsFactors = FALSE)
cat("Loaded nhif_data (rows =", nrow(nhif_data), ")\n"); flush.console()

regulatory_data <- read.csv(textConnection(r"(
authority_name,full_name,established_year,headquarters,website,phone,email,physical_address,functions,license_renewal_fee_kes,complaint_hotline,annual_report_url
Insurance Regulatory Authority,Insurance Regulatory Authority of Kenya,2006,Nairobi,www.ira.go.ke,+254 20 4996000,info@ira.go.ke,"Zep-Re Place 1st Floor Longonot Road Upper Hill Nairobi",Regulati[...]
)"), stringsAsFactors = FALSE)
cat("Loaded regulatory_data (rows =", nrow(regulatory_data), ")\n"); flush.console()

# ============================================================
# TIPS DATA
# ============================================================
cat("Loading tips data...\n"); flush.console()
tips_data <- list(
  motor = c(
    "Always compare at least 3 quotes before buying motor insurance.",
    "Check the insurer's claims settlement ratio - higher is better.",
    "Consider adding excess waiver to avoid paying out-of-pocket during claims.",
    "Third party is mandatory by law, but comprehensive protects your investment.",
    "Declare all modifications to your vehicle to avoid claim rejection.",
    "Install anti-theft devices to qualify for premium discounts.",
    "Renew your policy before expiry to avoid penalties and inspection.",
    "Verify that your preferred garage is in the insurer's approved network."
  ),
  health = c(
    "Check the hospital network - ensure your preferred hospitals are included.",
    "Understand waiting periods for pre-existing conditions (usually 12 months).",
    "Consider maternity cover if planning to start a family.",
    "Verify if the cover includes chronic disease management.",
    "Check co-payment requirements and limits.",
    "NHIF is mandatory for all employed persons - supplement with private cover.",
    "Declare all pre-existing conditions honestly to avoid claim rejection.",
    "Review your cover annually as health needs change with age."
  ),
  life = c(
    "Buy life insurance when young - premiums are significantly lower.",
    "Consider term life for pure protection - it's more affordable.",
    "Add critical illness rider for comprehensive protection.",
    "Ensure sum assured is at least 10x your annual income.",
    "Review beneficiaries regularly, especially after major life events.",
    "Understand surrender value and policy loan options.",
    "Choose a company with strong financial ratings (A or higher).",
    "Consider education plans early to benefit from longer accumulation."
  ),
  agriculture = c(
    "Register with your county agriculture office to qualify for subsidized premiums.",
    "KAIP subsidies cover 50% of premium for eligible farmers.",
    "Weather index insurance pays automatically based on weather data.",
    "Keep proper farm records for easier claims processing.",
    "Insure before planting season - most products have waiting periods.",
    "Consider group insurance through cooperatives for better rates."
  ),
  general = c(
    "Always verify the insurer is licensed by IRA (check www.ira.go.ke).",
    "Read the policy document carefully before signing.",
    "Understand exclusions - what is NOT covered is as important as what is.",
    "Keep all policy documents and payment receipts safely.",
    "Report claims immediately - delays can lead to rejection.",
    "Use licensed insurance agents and verify their credentials.",
    "Review your insurance needs annually and adjust coverage accordingly.",
    "Don't buy insurance based on price alone - consider service quality and claims history."
  )
)
cat("Loaded tips_data\n"); flush.console()

# Glossary
glossary_data <- data.frame(
  Term = c("Premium", "Sum Assured", "Excess / Deductible", "Waiting Period",
           "Rider", "Third Party", "Comprehensive", "No Claim Bonus", "Underwriting"),
  Definition = c(
    "The amount you pay for insurance coverage.",
    "The maximum amount the insurer will pay for a claim.",
    "The amount you pay out-of-pocket before insurance kicks in.",
    "Time you must wait before certain benefits become available.",
    "An add-on to a policy that provides additional benefits.",
    "Insurance covering damage/injury to others, not your own property.",
    "Insurance covering both your own property and third party liability.",
    "Discount given for not making claims during the policy period.",
    "Process of evaluating risk to determine premium and coverage."
  ),
  stringsAsFactors = FALSE
)
cat("Loaded glossary_data\n"); flush.console()

# Company brand colors for logo cards
company_brands <- list(
  IRA001 = list(color = "#0066CC", initials = "JI"),
  IRA002 = list(color = "#E31837", initials = "BR"),
  IRA003 = list(color = "#00A651", initials = "CI"),
  IRA004 = list(color = "#FF6B00", initials = "AP"),
  IRA005 = list(color = "#0047AB", initials = "UO"),
  IRA006 = list(color = "#C41E3A", initials = "KA"),
  IRA007 = list(color = "#2E8B57", initials = "PA"),
  IRA008 = list(color = "#4169E1", initials = "MA"),
  IRA009 = list(color = "#8B4513", initials = "HE"),
  IRA010 = list(color = "#DC143C", initials = "GA"),
  IRA011 = list(color = "#191970", initials = "AI"),
  IRA012 = list(color = "#228B22", initials = "CA"),
  IRA013 = list(color = "#B22222", initials = "FA"),
  IRA014 = list(color = "#006400", initials = "IL"),
  IRA015 = list(color = "#4B0082", initials = "IL"),
  IRA016 = list(color = "#FF4500", initials = "LI"),
  IRA017 = list(color = "#800080", initials = "ML"),
  IRA018 = list(color = "#008080", initials = "OM"),
  IRA019 = list(color = "#A0522D", initials = "PH"),
  IRA020 = list(color = "#006400", initials = "TA"),
  IRA021 = list(color = "#4682B4", initials = "TR"),
  IRA022 = list(color = "#708090", initials = "XP"),
  IRA023 = list(color = "#1E90FF", initials = "SA"),
  IRA024 = list(color = "#DAA520", initials = "MY"),
  IRA025 = list(color = "#556B2F", initials = "KO"),
  IRA026 = list(color = "#8B0000", initials = "DL"),
  IRA027 = list(color = "#483D8B", initials = "IA"),
  IRA028 = list(color = "#2F4F4F", initials = "CC")
)

# ============================================================
# COMPANY LOGO MAPPING (filename in www/logos/)
# ============================================================
company_logo_map <- list(
  IRA001 = "jubilee.png",
  IRA002 = "britam.png",
  IRA003 = "cic.jpg",
  IRA004 = "apa.png",
  IRA005 = "uap_oldmutual.jpg",
  IRA006 = "kenindia.png",
  IRA007 = "pacis.png",
  IRA008 = "madison.png",
  IRA009 = "heritage.png",
  IRA010 = "ga.jpg",
  IRA011 = "aig.jpg",
  IRA012 = "canon.png",
  IRA013 = "first_assurance.png",
  IRA014 = "icea_lion.png",
  IRA015 = "icea_lion.png",
  IRA016 = "liberty.jpg",
  IRA017 = "metropolitan.png",
  IRA018 = "oldmutual.jpg",
  IRA019 = "phoenix.jpg",
  IRA020 = "takaful.png",
  IRA021 = "trident.jpg",
  IRA022 = "xplico.jpg",
  IRA023 = "sanlam.png",
  IRA024 = "mayfair.jpg",
  IRA025 = "kenya_orient.jpg",
  IRA026 = "directline.jpg",
  IRA027 = "intra_africa.png",
  IRA028 = "coop.png"
)
company_logo <- function(company_id, company_name, height = "60px") {
  logo_file <- company_logo_map[[company_id]]
  if (!is.null(logo_file) && file.exists(paste0("www/logos/", logo_file))) {
    return(tags$img(src = paste0("logos/", logo_file), style = paste0("height: ", height, "; max-width: 180px; object-fit: contain;")))
  }
  # Fallback to gradient initials card
  brand <- company_brands[[company_id]]
  if (is.null(brand)) brand <- list(color = "#0066CC", initials = substr(company_name, 1, 2))
  h <- as.numeric(gsub("px", "", height))
  HTML(paste0(
    "<div style='background: linear-gradient(135deg, ", brand$color, ", ", adjustcolor(brand$color, alpha.f = 0.7), ");",
    "width: ", h, "px; height: ", h, "px; border-radius: 12px;",
    "display: flex; align-items: center; justify-content: center;",
    "color: white; font-weight: bold; font-size: ", max(14, h/3), "px;",
    "box-shadow: 0 4px 12px ", adjustcolor(brand$color, alpha.f = 0.3), ";'>",
    brand$initials, "</div>"
  ))
}

cat("global.R end:", Sys.time(), "\n"); flush.console()