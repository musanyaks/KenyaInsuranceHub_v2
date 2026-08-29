<div align="center">

# 🇰🇪 Kenya Insurance Hub

**Interactive Shiny Dashboard for Kenya's Insurance Market**

[![Live App](https://img.shields.io/badge/Live_App-276DC3?logo=r&logoColor=white&style=for-the-badge)](https://musanyaks.shinyapps.io/kenya-insurance-hub-v2/)
[![Shiny](https://img.shields.io/badge/Shiny-276DC3?logo=r&logoColor=white&style=for-the-badge)](https://shiny.rstudio.com/)

**[🚀 Open Live App](https://musanyaks.shinyapps.io/kenya-insurance-hub-v2/)**

</div>

---

## About This App

**Kenya Insurance Hub** is an interactive Shiny dashboard that puts Kenya's insurance market data at your fingertips. Whether you are a consumer shopping for insurance, a researcher analyzing market trends, a developer building insurance tools, or a policymaker tracking industry performance — this app gives you structured, comparable data for all licensed insurance companies in Kenya.

The app contains **28 licensed insurance companies**, **69 insurance products**, premium rate estimates, NHIF benefits, agriculture insurance subsidies, and regulatory contacts — all in one place.

**Developer:** Musa Rioba  
**Email:** nyakerabachi@gmail.com  
**Phone:** +254 704 059 015  
**Portfolio:** https://musanyaks.github.io  
**LinkedIn:** https://www.linkedin.com/in/musarioba/  
**GitHub:** https://github.com/musanyaks

---

## What This App Does

### For Consumers
- **Compare motor insurance** premiums across companies by vehicle type, value, and age
- **Find health insurance** plans that fit your budget and coverage needs
- **Explore life insurance** products (Term, Endowment, Whole Life, Education)
- **Calculate agriculture insurance** premiums with government subsidy breakdowns
- **Check NHIF benefits** to understand what is covered before buying private cover

### For Researchers & Analysts
- **Market share analysis** with interactive bar, pie, and treemap charts
- **Company ratings** by financial strength and claims settlement
- **Product distribution** across categories (Motor, Health, Life, Agriculture)
- **Regulatory compliance** tracking

### For Developers
- **Structured datasets** ready for integration into other apps or models
- **API-like functions** for querying insurance data programmatically
- **Clean, documented R code** as a reference implementation

---

## App Features by Tab

### 1. Dashboard
- KPI cards showing total companies, products, motor products, and health products
- Market share bar chart (top 10 companies)
- Top-rated companies table
- Product category distribution donut chart
- **Quick Action buttons** to jump to any calculator

### 2. Companies
- **Filterable directory** of all 28 licensed insurers
- Filter by: Category (Composite, General, Life, Islamic), Financial Rating, Market Share
- **Click any company** to see full profile:
  - Contact details (phone, email, website, address)
  - Financial rating & claims settlement rating
  - Number of branches
  - All products offered

### 3. Motor Insurance Calculator
- Select vehicle type: Saloon Car, SUV/4x4, Pickup/Truck, Matatu (14/33-seater), Motorcycle
- Enter vehicle value in KES (e.g., 1,500,000)
- Set vehicle age
- Choose cover type: **Comprehensive**, **Third Party**, or **Both**
- Get instant premium comparison across insurers
- See side-by-side comparison of what Comprehensive vs Third Party covers

### 4. Health Insurance Calculator
- Enter your age
- Select cover type: **Comprehensive** (inpatient + outpatient) or **Inpatient Only**
- Compare individual and family (4 members) premiums
- View coverage limits: inpatient, outpatient, maternity, dental, optical
- See network hospital counts per plan

### 5. Life Insurance Calculator
- Choose policy type: **Term Life**, **Endowment**, **Whole Life**, **Education**
- Enter desired sum assured (e.g., KES 5,000,000)
- Set your age and policy term
- Filter by available riders: Critical Illness, Disability, Accidental Death
- Compare estimated annual premiums across companies

### 6. Agriculture Insurance Calculator
- Select crop or livestock: Maize, Wheat, Vegetables/Fruits, Cattle/Livestock
- Enter acres or number of animals
- See **government subsidy breakdown** (KAIP/KLIP covers 50%)
- Know exactly what **you pay** vs what **government pays**
- Check eligibility requirements

### 7. NHIF Benefits
- Full list of **19 NHIF benefits** with coverage details
- Filter by category: Inpatient, Outpatient, Specialized
- See co-payment requirements and waiting periods
- Understand what NHIF covers before buying private insurance

### 8. Market Analysis
- **Interactive charts**: Bar, Pie, Treemap
- Configurable metrics: Market Share, Branch Network, Years Established
- Color by: Financial Rating, Category, Claims Rating
- Market concentration stats (Top 3 control, A-rated companies, etc.)
- Product distribution and rating distribution charts

### 9. Regulatory Info
- **Insurance Regulatory Authority (IRA)** contact details
- Complaint hotline: 0709 912 000
- Step-by-step guide to filing a complaint
- How to verify a company or agent before buying
- Warning about unlicensed insurance schemes

### 10. Insurance Tips
- Category-specific tips: Motor, Health, Life, Agriculture, General
- **Glossary of insurance terms**: Premium, Sum Assured, Excess, Rider, Waiting Period, etc.

### 11. About the Author
- Developer profile with photo
- Contact information and social links
- Motivation behind building the app

---

## How to Use the App

### Compare Motor Insurance
1. Go to **Motor Insurance** tab
2. Select your vehicle type (e.g., "Saloon Car")
3. Enter vehicle value (e.g., `1500000`)
4. Set vehicle age
5. Choose "Comprehensive" for full cover or "Third Party" for legal minimum
6. Click **Compare Quotes**
7. See bar chart and detailed table with premiums from all insurers

### Find Health Cover
1. Go to **Health Insurance** tab
2. Enter your age
3. Select "Comprehensive" for full cover or "Inpatient Only" for budget option
4. Choose "Family of 4" if covering dependents
5. Click **Compare Plans**
6. Sort by premium, rating, or network size

### Calculate Farm Insurance
1. Go to **Agriculture Insurance** tab
2. Select your crop (e.g., "Maize")
3. Enter acres (e.g., `5`)
4. Check "Show Subsidized Only" for KAIP-eligible products
5. Click **Calculate Premium**
6. See what you pay vs what government pays

---

## Data Included

| Dataset | Records | Description |
|---|---|---|
| Insurance Companies | 28 | Licensed insurers with contacts, ratings, market share |
| Insurance Products | 69 | Motor, Health, Life, Agriculture products |
| Motor Premiums | 41 | Rates by vehicle type & value across insurers |
| Health Premiums | 21 | Rates by age group (18–80) and coverage level |
| Life Premiums | 27 | Term, Endowment, Whole Life, Education plans |
| Agriculture Products | 12 | Crop/livestock with KAIP/KLIP subsidy info |
| NHIF Benefits | 19 | Full coverage details with limits |
| Regulatory Data | 1 | IRA contacts and complaint procedures |

---

## Data Sources

- Insurance Regulatory Authority of Kenya (www.ira.go.ke)
- National Hospital Insurance Fund (www.nhif.or.ke)
- Company websites and product brochures
- Kenya Agriculture Insurance Program (KAIP) reports

> ⚠️ **Disclaimer:** Premium rates are indicative (2026 data). Always verify current rates directly with insurance companies and the Insurance Regulatory Authority before making purchase decisions.

---

## License

MIT License

---

<div align="center">

**Connect With Me**

[Portfolio](https://musanyaks.github.io) · [LinkedIn](https://www.linkedin.com/in/musarioba/) · [GitHub](https://github.com/musanyaks) · [Email](mailto:nyakerabachi@gmail.com) · +254 704 059 015

Built by Musa Rioba in Nairobi, Kenya 🇰🇪

</div>
