
####-------------------------------------------------------- GCERF: VARIABLE FILE-----------------------------------
#' The purpose of this script is to have a single location to quickly change project settings
#' Project scripts are run from the ProjectControl.R file


### --- Filepaths --------------------------------------------------------------

# Key Locations
ONEDRIVE = paste0(IEP_USERPATH,"/GCERF/",REPORT_YEAR,"") # Location of one drive report folder

# Chartbooks
CHARTBOOK = paste0(ONEDRIVE,"/Layout/GCERF_Chartbook_",REPORT_YEAR,".xlsx") # Data for recreating charts or sharing with journalists
CHARTBOOK_APPENDICES = paste0(ONEDRIVE,"/Layout/GCERF_Chartbook_Appendices_",REPORT_YEAR,".xlsx") # Data for recreating charts or sharing with journalists

# Output Locations
o_MAPS = ("04_outputs/maps/")
o_CHARTS = ("04_outputs/charts/")
o_TABLES = ("04_outputs/tables/")
CHART_FILES = paste0(ONEDRIVE,"/Layout/Charts")
IMAGE_FILES = paste0(ONEDRIVE,"/Layout/Images")
TABLE_FILES = paste0(ONEDRIVE,"/Layout/Tables")
MAP_FILES = paste0(ONEDRIVE,"/Layout/Maps")
ANALYSIS_FILES = paste0(ONEDRIVE,"/Analysis")

####--------------------------------------------------------------------------------------------------------------=
# Chart Fonts
HEAVY_FONT = "Helvetica LT Pro" 
LIGHT_FONT = "Helvetica LT Pro Light" 

####------------------------------------------------------------------------------------------------------------
# OECD and non
IEP_NAMES = "~/GitHub/gcerf/02_data/gti-countrynames.xlsx" # Location of finalized list of country name spellings and associated data
THE_OECD = c("AUS","AUT","BEL","CAN","CHL", "COL", "CRI", "CZE", 
             "DNK", "EST","FIN","FRA","DEU", "GRC", "ISL","IRL", "ISR", "ITA","JPN", 
             "KOR", "LVA", "LTU", "LUX", "MEX", "NLD","NZL","NOR", "POL", "PRT", 
             "SVK", "SVN","ESP","SWE","CHE", "TUR ", "GBR", "USA")
####-----------------------------------------------------------------------------------------------------------
# Chart Sizes
CHARTS <- list(
  small = c(width = 8.45, height = 10),
  medium = c(width = 12, height = 10),
  large = c(width = 17.6, height = 10)
)

MAPS <- list(
  small = c(width = 12, height = 8),
  medium = c(width = 14, height = 10),
  large = c(width = 28, height = 14)
)

CHART_UNIT = "cm"
####----------------------------------------------------------------------------------------------------------




