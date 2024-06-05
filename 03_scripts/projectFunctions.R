###### ------------------------- PROJECT FUNCTIONS--------------------------------------------------------


#----------------------------------------------------------------------------------------------------------
# f_LibraryLoader: Checks to see if packages are loaded before loading them. Use at the start of every script.
f_LibraryLoader <- function(...) {
  args <- substitute(list(...))[-1] # Capture the unquoted arguments
  package_names <- sapply(args, function(arg) {
    if (is.character(arg)) {
      return(arg) # Return the argument if it's a string
    } else {
      return(as.character(arg)) # Convert to string if it's a symbol
    }
  })
  
  for (package in package_names) {
    if (!package %in% rownames(installed.packages())) {
      stop(paste("Package not installed:", package))
    }
    
    if (!package %in% .packages()) {
      library(package, character.only = TRUE)
    }
  }
}

f_LibraryLoader(tidyverse,
                countrycode,
                extrafont,
                iepg,
                dplyr,
                ggplot2,
                stringr
)


#---------------------------------------------------------------------------------------------------------

## -- f_DownloadIndicator ---------------------------------------------------------------------------------
#' What does this function do
f_DownloadIndicator <- function(url, destfile, method = "auto", quiet = FALSE, mode = "wb", ...) {
  
  ##### ----- Functions for the GPI project
  
  f_LibraryLoader(tidyverse, 
                  rlang,
                  extrafont)
}

#----------------------------------------------------------------------------------------------------------
## -- f_DownloadIndicator ---------------------------------------------------------------------------------
#' What does this function do
f_DownloadIndicator <- function(url, destfile, method = "auto", quiet = FALSE, mode = "wb", ...) {
  
  ##### ----- Functions for the GPI project
  
  f_LibraryLoader(tidyverse, 
                  rlang,
                  extrafont)
}


#----------------------------------------------------------------------------------------------------------
# get gti data

# get ppi data



#-----------------------------------------------------------------------------------------------------------
# - f_ConflictPriorities: Gives priority to tidyverse functions. Will remove issues with filter(), select() etc.
f_ConflictPriorities <- function() {
  
  # Check if 'conflicted' package is installed, install if not
  if (!requireNamespace("conflicted", quietly = TRUE)) {
    install.packages("conflicted")
  }
  library(conflicted)
  
  # Set conflict preferences using the global CONFLICT_PRIORITY
  for (conflict_func in names(CONFLICT_PRIORITY)) {
    conflict_prefer(conflict_func, CONFLICT_PRIORITY[[conflict_func]])
  }
}

#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
# - f_LibraryLoader: Checks to see if packages are loaded before loading them. Use at the start of every script.
f_LibraryLoader <- function(...) {
  args <- substitute(list(...))[-1] # Capture the unquoted arguments
  package_names <- sapply(args, function(arg) {
    if (is.character(arg)) {
      return(arg) # Return the argument if it's a string
    } else {
      return(as.character(arg)) # Convert to string if it's a symbol
    }
  })
  
  for (package in package_names) {
    if (!package %in% rownames(installed.packages())) {
      stop(paste("Package not installed:", package))
    }
    
    if (!package %in% .packages()) {
      library(package, character.only = TRUE)
    }
  }
}

#----------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# f_GPISavePlots: Saves the GPI charts in the correct format in three different sizes
f_GPISavePlots <- function(chart_title, plot_name) {
  
  if (chart_title["type"] == "Chart") {
    # Looping through the three chart sizes
    for (size_name in names(GPI_CHARTS)) {
      size <- GPI_CHARTS[[size_name]]
      file_base_name <- paste0(chart_title["sheet"], "_", size_name)
      
      # Save as PNG in CHARTS_PATH with transparency
      png_file <- paste0(IMAGE_FILES, "/", file_base_name, ".png")
      ggsave(png_file, plot_name, device = "png", width = size["width"], height = size["height"], units = CHART_UNIT, bg = "transparent")
      
      # Save as SVG in ONEDRIVE_PATH
      svg_file <- paste0(CHART_FILES, "/", file_base_name, ".svg")
      ggsave(svg_file, plot_name, device = "svg", width = size["width"], height = size["height"], units = CHART_UNIT)
    }
  }
  
  if (chart_title["type"] == "Map") {
    # Looping through the three chart sizes
    for (size_name in names(GPI_MAPS)) {
      size <- GPI_MAPS[[size_name]]
      file_base_name <- paste0(chart_title["sheet"], "_", size_name)
      
      # Save as PNG in CHARTS_PATH with transparency
      png_file <- paste0(IMAGE_FILES, "/", file_base_name, ".png")
      ggsave(png_file, plot_name, device = "png", width = size["width"], height = size["height"], units = CHART_UNIT, bg = "transparent")
      
      # Save as SVG in ONEDRIVE_PATH
      svg_file <- paste0(MAP_FILES, "/", file_base_name, ".svg")
      ggsave(svg_file, plot_name, device = "svg", width = size["width"], height = size["height"], units = CHART_UNIT)
    }
  }
  
  if (chart_title["type"] == "Diagram") {
    # Looping through the three chart sizes
    for (size_name in names(GPI_CHARTS)) {
      size <- GPI_CHARTS[[size_name]]
      file_base_name <- paste0(chart_title["sheet"], "_", size_name)
      
      # Save as SVG in ONEDRIVE_PATH
      svg_file <- paste0(CHART_FILES, "/", file_base_name, ".svg")
      svg_temp = DiagrammeRsvg::export_svg(plot_name)
      write_lines(svg_temp,svg_file)
      
      #convert to png
      rsvg_png(svg_file, paste0(IMAGE_FILES, "/", file_base_name, ".png"))
      
      
    }
  }
  
}

#------------------------------------------------------------------------------------------------------------------------
# f_LabelFormatter: Helper function for chart labelling
f_LabelFormatter <- function(x) {
  ifelse(x < 0, paste0("-", abs(x)), as.character(x))
}

#------------------------------------------------------------------------------------------------------------------------
# f_TopNCountries: Returns the top N countries for a given year and indicator
f_TopNCountries <- function(df, N, GPIyear, indicator) {
  
  # Convert indicator to symbol
  indicator <- sym(indicator)
  
  # Filter for selected year
  df <- df %>% 
    filter(year == GPIyear) 
  
  # Get top N countries
  df1 <- df %>%
    select(country, !!indicator) %>%
    filter(!is.na(!!indicator)) %>%
    top_n(N, !!indicator) %>%
    arrange(desc(!!indicator))
  
  # Get sum of all other countries
  df2 <- df %>%
    select(country, !!indicator) %>%
    filter(!is.na(!!indicator)) %>%
    anti_join(df1, by = "country") %>%
    summarise(country = "All other countries", !!indicator := sum(!!indicator))
  
  # Combine the two dataframes
  df3 <- df1 %>%
    bind_rows(df2) %>%
    mutate(year = GPIyear)
  
  return(df3)
}

#-----------------------------------------------------------------------------------------------------------------
#f_dbIEPCombine: Combines multiple IEP datasets into one dataframe
f_dbIEPCombine <- function(id_range, index_name) {
  # List to store each dataframe
  list_of_dfs <- lapply(id_range, function(id) {
    iepg_get(id)
  })
  
  # Combine all dataframes into one and then perform the additional manipulations
  combined_df <- bind_rows(list_of_dfs) %>%
    ungroup() %>%
    select(
      geocode,
      country = geoname,
      year,
      indicator = variablename,
      type = disaggregation,
      value
    ) %>%
    mutate(index = index_name)
  
  return(combined_df)
}


#-------------------------------------------------------------------------------------------------------------------
## -- f_ScatterLabels ----------------------------------------------------------

f_ScatterLabels <- function(pScatter, xaxis = "Include", yaxis = "Include", 
                            left_text = "", right_text = "", up_text = "", down_text = "", xposition = 0.07, yposition = 0.045) {
  
  ARROW_LENGTH = 0.05
  LEFT_ARROW_START = 0.12
  LEFT_ARROW_END = LEFT_ARROW_START + ARROW_LENGTH
  RIGHT_ARROW_END = 0.97
  RIGHT_ARROW_START = RIGHT_ARROW_END - ARROW_LENGTH
  UP_ARROW_END = 0.96
  UP_ARROW_START = UP_ARROW_END - ARROW_LENGTH
  DOWN_ARROW_START = 0.12
  DOWN_ARROW_END = DOWN_ARROW_START + ARROW_LENGTH
  LEFT_TEXT = LEFT_ARROW_END + 0.01
  RIGHT_TEXT = RIGHT_ARROW_START - 0.01
  UP_TEXT = UP_ARROW_START - 0.01
  DOWN_TEXT = DOWN_ARROW_END + 0.01
  
  
  # Using ggdraw to add annotations conditionally
  p <- ggdraw(pScatter)
  
  if (xaxis == "Include") {
    right_arrow <- linesGrob(x = unit(c(RIGHT_ARROW_START, RIGHT_ARROW_END), "npc"), y = unit(c(xposition, xposition), "npc"),
                             arrow = arrow(ends = "last", type = "closed", length = unit(2, "mm")))
    left_arrow <- linesGrob(x = unit(c(LEFT_ARROW_START, LEFT_ARROW_END), "npc"), y = unit(c(xposition, xposition), "npc"),
                            arrow = arrow(ends = "first", type = "closed", length = unit(2, "mm")))
    p <- p +
      draw_grob(right_arrow) +
      draw_grob(left_arrow) +
      draw_label(left_text, x = LEFT_TEXT, y = xposition, hjust = 0,vjust = 0.25, fontface = "bold", size = 6, fontfamily = HEAVY_FONT) +
      draw_label(right_text, x = RIGHT_TEXT, y = xposition, hjust = 1, vjust = 0.25, fontface = "bold", size = 6, fontfamily = HEAVY_FONT)
  }
  
  if (yaxis == "Include") {
    up_arrow <- linesGrob(x = unit(c(yposition, yposition), "npc"), y = unit(c(UP_ARROW_START, UP_ARROW_END), "npc"),
                          arrow = arrow(ends = "last", type = "closed", length = unit(2, "mm")))
    down_arrow <- linesGrob(x = unit(c(yposition, yposition), "npc"), y = unit(c(DOWN_ARROW_START, DOWN_ARROW_END), "npc"),
                            arrow = arrow(ends = "first", type = "closed", length = unit(2, "mm")))
    p <- p +
      draw_grob(up_arrow) +
      draw_grob(down_arrow) +
      draw_label(down_text, x = yposition, y = DOWN_TEXT, hjust = 0, vjust = 0.25, angle = 90, fontface = "bold", size = 6, fontfamily = HEAVY_FONT) +
      draw_label(up_text, x = yposition, y = UP_TEXT, hjust = 1, vjust = 0.25, angle = 90, fontface = "bold", size = 6, fontfamily = HEAVY_FONT)
  }
  
  return(p)
}


#------------------------------------------------------------------------------------------------------------------------
# f_GPIChartbook: Creates the GPI chartbook
f_GPIChartbook <- function(filepath) {
  {
    wb <- createWorkbook()
    addWorksheet(wb, "default")
    saveWorkbook(wb, filepath, overwrite = TRUE)
  }
}


#-------------------------------------------------------------------------------------------------------------------------






# -- SETUP FUNCTIONS -----------------------------------------------------------
f_ConflictPriorities() 










