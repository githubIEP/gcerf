# Export charts, maps, and tables

### ---- Libraries and variables

f_LibraryLoader(tidyverse,
                countrycode,
                extrafont,
                iepg,
                dplyr,
                ggplot2,
                stringr,
                sf,
                purrr,
                scales,
                GGally,
                grid,
                openxlsx,
                gridExtra
)

# Charts-----------------------------------------------------------
source("03_scripts/subnational_hdi_map.R")
source("03_scripts/ppi_all_pillars_graph.R")
source("03_scripts/trends_raw_data_ukraine.R")
source("03_scripts/acled.R")
source("03_scripts/correlations.R")

#Export graphs----------------------------------------------------
wb_GCERF <- createWorkbook()

# List
GRAPHS_TABLES_EXPORT = c("CHART_AccGtiOecdNon", "CHART_PppiGtiOecdNon", "CHART_PpiGti", "pCHART_PPIscatter", "TABLE_PPIcorrelates.df", 
                         "MAP_subnat_hdi_ukr", "TABLE_AcledIncident", "CHART_PpiThemes_Indexed", "CHART_PpiPillarsUkraineInOne", 
                         "CHART_PlusRegionQualityInfo", "CHART_PlusRegionResearchers", "CHART_PlusRegionGroupExclusion", 
                         "CHART_PlusRegionPressFreedom", "CHART_PlusRegionCorruptionControl")

# Reset Counters
figure_count = 0
table_count = 0

# Function to create and export charts
create_chart <- function(chart_name) {
  plot <- ggplot()  # Replace with your actual plot creation code
  return(plot)
}


# Function to export charts or tables
f_ProjectExport <- function(item, wb) {
  figure_count <<- figure_count + 1
  sheet_name <- paste0("Chart_", figure_count, "_", item)
  addWorksheet(wb, sheet_name)
  plot <- create_chart(item)  # Replace with your actual plot creation function
  insertPlot(wb, sheet = sheet_name, startRow = 1, startCol = 1, width = 10, height = 5, plot = plot)
}

# Export Charts
for (item in GRAPHS_TABLES_EXPORT) {
  f_ProjectExport(item, wb_GCERF)
}

# Save the workbook
saveWorkbook(wb_GCERF, file = "GCERF_Output.xlsx", overwrite = TRUE)




