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
source("03_scripts/ppi_all_pillars.graph.R")
source("03_scripts/trends_raw_data_ukraine.R")
source("03_scripts/acled.R")
source("03_scripts/correlations.R")

#Export graphs----------------------------------------------------
wb_GCERF <- createWorkbook()

# List
GRAPHS_TABLES_EXPORT = c("CHART_AcceptanceGtiOecdNon", "CHART_PppiGtiOecdNon", "CHART_PpiGti", "pCHART_PPIscatter", "TABLE_PPIcorrelates.df", 
                         "MAP_subnat_hdi_ukr", "TABLE_AcledIncident", "CHART_PpiThemes_Indexed", "CHART_PpiPillarsUkraineInOne", 
                         "CHART_PlusRegionQualityInfo", "CHART_PlusRegionResearchers", "CHART_PlusRegionGroupExclusion", 
                         "CHART_PlusRegionPressFreedom", "CHART_PlusRegionCorruptionControl", "", "")

# Reset Counters
figure_count = 0
table_count = 0

# Export Data
f_ProjectExport("3", wb_GCERF, GRAPHS_TABLES_EXPORT)

# save the workbook
saveWorkbook(workbook, spreadsheet, overwrite = TRUE)




