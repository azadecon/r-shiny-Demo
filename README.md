# PDS Bihar API Data Visualizer

This Shiny app fetches and visualizes transaction data from the **Public Distribution System (PDS) Bihar** portal. It provides interactive plots and summary statistics for selected districts, focusing on **card transactions**, **portability**, and **portability as a share of total transactions**.

---
  
  ## Features
  
  - 🔢 Select a month range (MM format)
- 📥 Fetch real-time data from the PDS Bihar website
- 📊 View a clean data table with all fetched records
- 📈 Plot time series trends for:
  - Total Cards (A)
- Number of Transactions (B)
- Portability (C)
- 📉 Plot **Portability as a Share of Transactions** (C / B)
- 📋 See summary statistics by district (averages)

---
  
  ## How It Works
  
  1. **User selects a start and end month.**
  2. **Data is fetched** from the Bihar PDS API for each selected month.
3. **Parsed and cleaned** using `rvest` and `dplyr`.
4. **Visualized** using `ggplot2` in an interactive Shiny dashboard.

---
  
  ## Installation
  
  1. Clone the repository:
  
```{R}
git clone https://github.com/your-username/pds-bihar-api-visualizer.git
cd pds-bihar-api-visualizer
```

2. Install required R packages:
```{R}
install.packages(c("shiny", "httr", "rvest", "dplyr", "DT", "ggplot2", "tidyr"))
```

3. Run the app:
```{R}
shiny::runApp("app.R")
```


File Structure
```{R}
app.R         # Main Shiny app script
README.md     # This file
```