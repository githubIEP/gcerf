# Export charts, maps, and tables

### ---- Libraries and variables

f_LibraryLoader(tidyverse,
                countrycode,
                extrafont,
                iepg,
                dplyr,
                ggplot2,
                stringr
)



# Charts-----------------------------------------------------------
wbCHARTS_GCERF <- createWorkbook()

# List
REPORT_EXPORT = c("CHART_GovtConflict", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "")

# Reset Counters
figure_count = 0
table_count = 0


# Export Data
f_ProjectExport("3", wbTRAINING_SECTION1, CHARTBOOK_1, SECTION1_EXPORT)


