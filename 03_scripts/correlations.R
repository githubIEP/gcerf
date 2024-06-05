##### ----- correlations GTI and PPI pillars, graph

##### ----- get indicators
#low levels of corruption
tmp1<- iepg_get(26)%>% select(geocode, geoname, value, year)%>% rename(low.levels.of.corruption = value)
# acceptance of the rights of others
tmp2<- iepg_get(1)%>% 
  select(geocode, geoname, value, year) %>% rename(acceptance_of_rigths_of_others = value)
# sound business environment
tmp3<- iepg_get(33)%>% 
  select(geocode, geoname, value, year) %>% rename(sounds.business.environment = value)
# well-functioning government
tmp4<- iepg_get(35)%>% 
  select(geocode, geoname, value, year) %>% rename(well.functioning.government = value)
# equitable distribution of resources
tmp5<- iepg_get(6)%>% 
  select(geocode, geoname, value, year) %>% rename(equitable.distribution.of.resources = value)
# free flow of information
tmp6<- iepg_get(11)%>% 
  select(geocode, geoname, value, year) %>% rename(free.flow.of.information = value)
# good relations with neighbors
tmp7<- iepg_get(15)%>% 
  select(geocode, geoname, value, year) %>% rename(good.relations.with.neighbours = value)
# high levels of human capital
tmp8<- iepg_get(20)%>% 
  select(geocode, geoname, value, year) %>% rename(high.levels.of.human.capital = value)
# ppi score
tmp9<- iepg_get(27)%>%
  select(geocode, geoname, value, year) %>% rename (ppi= value)

#gti
gti<- iepg_get(61)%>% 
  select(geocode, geoname, value, year) %>% rename(gti = value)

oecd_countries <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States")
gti <- gti %>%
  mutate(oecd = ifelse(geoname %in% oecd_countries, "oecd", "no"))


# Join data frames iteratively
merged_data <- Reduce(function(x, y) merge(x, y, by = c("geocode", "geoname", "year"), all = TRUE), list(gti, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9))


merged_data <- merged_data %>%
  select(unique(names(.)))%>%
  select(-c(muid.x, muid.y))

merged_data<- na.omit(merged_data)


# calculate correlation coefficient
correlation1 <- cor(merged_data$gti, merged_data$low.levels.of.corruption)
correlation2 <- cor(merged_data$gti, merged_data$acceptance_of_rigths_of_others)
correlation3 <- cor(merged_data$gti, merged_data$sounds.business.environment)
correlation4 <- cor(merged_data$gti, merged_data$well.functioning.government)
correlation5 <- cor(merged_data$gti, merged_data$equitable.distribution.of.resources)
correlation6 <- cor(merged_data$gti, merged_data$free.flow.of.information)
correlation7 <- cor(merged_data$gti, merged_data$good.relations.with.neighbours)
correlation8 <- cor(merged_data$gti, merged_data$high.levels.of.human.capital)
correlation9<- cor(merged_data$gti,  merged_data$ppi)

# two level graph (oecd, non-oecd countries)
ggplot(merged_data, aes(x = gti, y = acceptance_of_rigths_of_others, fill = oecd)) +
  geom_point(shape = 21, color = "black") + # Shape 21 is filled circle with black border
  geom_smooth(data = subset(merged_data, oecd == "oecd"), method = "lm", se = FALSE, aes(group = 1), color = "blue") + # Trend line for oecd
  geom_smooth(data = subset(merged_data, oecd == "no"), method = "lm", se = FALSE, aes(group = 1), color = "darkgreen") + # Trend line for no
  geom_text(aes(x = max(merged_data$gti), y = max(merged_data$acceptance_of_rigths_of_others)), # Annotation position
            label = paste("r =", round(correlation2, 2)), # Annotation text
            hjust = 1, vjust = 1, color = "red") + # Text alignment and color
  labs(x = "Global Terrorism Index score", y = "Acceptance of the rigths of others score", title = "") +
  scale_fill_manual(values = c("darkgreen", "blue"), labels = c("Rest of the World", "OECD"), name = NULL) + # Define colors for oecd and no
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) + # Format y-axis without decimal places
  theme_minimal()


pCHART_PPIscatter <- ggplot(merged_data, aes(x = gti, y = acceptance_of_rigths_of_others, fill = oecd)) +
  geom_point(shape = 21, color = "black") + # Shape 21 is filled circle with black border
  geom_smooth(data = subset(merged_data, oecd == "oecd"), method = "lm", se = FALSE, aes(group = 1), color = "blue") + # Trend line for oecd
  geom_smooth(data = subset(merged_data, oecd == "no"), method = "lm", se = FALSE, aes(group = 1), color = "darkgreen") + # Trend line for no
  geom_text(aes(x = max(merged_data$gti), y = max(merged_data$acceptance_of_rigths_of_others)), # Annotation position
            label = paste("r =", round(0.34, 2)), # Annotation text
            hjust = 1, vjust = 1, color = "red") + # Text alignment and color
  labs(x = "Global Terrorism Index score
Lower impact                          Higher impact", y = "Acceptance of the rights of others score
More peaceful                         Less peaceful", title = "") +
  scale_fill_manual(values = c("darkgreen", "blue"), labels = c("Rest of the World", "OECD"), name = NULL) + # Define colors for oecd and no
  scale_y_continuous(labels = number_format(accuracy = 1)) + # Format y-axis without decimal places
  theme_minimal() +
  theme(
    legend.position = c(1, 0.25), # Use normalized coordinates (x = 7.5/10, y = 1.5/2)
    legend.justification = c("right", "top"), # Align legend within specified position
    legend.box.background = element_rect(color = "white", fill = "white", size = 0.5, linetype = "solid") # Optional: add a box around the legend
  )

print(pCHART_PPIscatter)


##### ppi
pCHART_PPIscatter <- ggplot(merged_data, aes(x = gti, y = ppi, fill = oecd)) +
  geom_point(shape = 21, color = "black") + # Shape 21 is filled circle with black border
  geom_smooth(data = subset(merged_data, oecd == "oecd"), method = "lm", se = FALSE, aes(group = 1), color = "blue") + # Trend line for oecd
  geom_smooth(data = subset(merged_data, oecd == "no"), method = "lm", se = FALSE, aes(group = 1), color = "darkgreen") + # Trend line for no
  geom_text(aes(x = max(merged_data$gti), y = max(merged_data$ppi)), # Annotation position
            label = paste("r =", round(correlation2, 2)), # Annotation text
            hjust = 1, vjust = 1, color = "red") + # Text alignment and color
  labs(x = "Global Terrorism Index score
Lower impact                          Higher impact", y = "Positive Peace Index score
More peaceful                         Less peaceful", title = "") +
  scale_fill_manual(values = c("darkgreen", "blue"), labels = c("Rest of the World", "OECD"), name = NULL) + # Define colors for oecd and no
  scale_y_continuous(labels = number_format(accuracy = 1)) + # Format y-axis without decimal places
  theme_minimal() +
  theme(
    legend.position = c(1, 0.25), # Use normalized coordinates (x = 7.5/10, y = 1.5/2)
    legend.justification = c("right", "top"), # Align legend within specified position
    legend.box.background = element_rect(color = "white", fill = "white", size = 0.5, linetype = "solid") # Optional: add a box around the legend
  )

print(pCHART_PPIscatter)



# one level
ggplot(merged_data, aes(x = gti, y = acceptance_of_rigths_of_others)) +
  geom_point(shape = 21, color = "darkgreen", fill = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") + # Trend line for all data
  geom_text(aes(x = max(gti), y = max(acceptance_of_rigths_of_others)), # Annotation position
            label = paste("r =", round(correlation2, 2)), # Annotation text
            hjust = 1, vjust = 1, color = "black") + # Text alignment and color
  labs(x = "Acceptance of the rights of others", y = "GTI score", title = "Correlation between GTI and Acceptance of the rights of others") +
  theme_minimal()


########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # correlation results with PPI 2024

# load data from GTI 2024
corr_gti.df = readRDS("~/GitHub/gcerf/02_data/gti_banded_national.rds") 

corr_gti.df<- corr_gti.df%>%
  mutate(variablename = "GTI", value = banded_score) %>%
  select(ID_0, variablename, year, value) %>%
  drop_na(value)%>% rename (geocode = ID_0) 

# PPI Indicators 2022- from database
#corr_ppi.df = iepg_search("PPI 2023 Report") %>%
#  filter(disaggregation == "banded") %>% pull(muid)
#corr_ppi.df = iepg_get(uid = (corr_ppi.df)) %>% ungroup() %>%
# select(geocode, variablename, year, value) %>%
# drop_na(value)
#corr_ppi.list = corr_ppi.df %>% distinct(variablename) %>% pull(variablename)

# PPI Indicators 2024
ppi_scores_full <- rio::import("~/GitHub/gcerf/02_data/ppi_scores_full.xlsx")
ppi_scores_full<- ppi_scores_full%>%
  select(geocode, variablename, year, value)
corr_ppi.df<- ppi_scores_full


# Combine Data, divide countires into OECD and Non-OECD
IEP_NAMES = "~/GitHub/gcerf/02_data/gti-countrynames.xlsx" # Location of finalized list of country name spellings and associated data
THE_OECD = c("AUS","AUT","BEL","CAN","CHL", "COL", "CRI", "CZE", 
             "DNK", "EST","FIN","FRA","DEU", "GRC", "ISL","IRL", "ISR", "ITA","JPN", 
             "KOR", "LVA", "LTU", "LUX", "MEX", "NLD","NZL","NOR", "POL", "PRT", 
             "SVK", "SVN","ESP","SWE","CHE", "TUR ", "GBR", "USA")

# List of countries
countries.df = rio::import(IEP_NAMES) %>% 
  mutate(OECD = if_else(geocode %in% THE_OECD, "OECD", "Rest of the World"))


correlates.df <- rbind(corr_gti.df, corr_ppi.df) %>%
  left_join(countries.df)


# GPI Scatterplot
#CHART_GPIscatter.df = correlates.df %>%
#  dplyr::filter(variablename %in% c("GTI", "overall score")) %>%
#  pivot_wider(names_from = variablename, values_from = value) %>%
#  select(OECD, GTI, `overall score`) %>%
#  na.omit()

# Base Plot
#p <- ggplot(CHART_GPIscatter.df, aes(x = GTI, y = `overall score`, color = OECD)) +
#  geom_point(alpha = 0.5) +
#  scale_color_manual(values = c("OECD" = "#2D4F5E", "Rest of the World" = "#770A1E")) +
#  labs(x = "GTI Score", y = "GPI Overall Score", color = "") +
#  geom_smooth(method = "lm", se = FALSE, aes(group = OECD)) 
#pCHART_GPIscatter <- f_ThemeGTI(p,
#                                CHART_GPIscatter,
#                                plottitle = "", 
#                                xaxis = "Include", 
#                                yaxis = "Include", 
#                               xgridline = "Include", 
#                                ygridline = "Include") +
#  theme(legend.position = c(.9,.1))

# Add scatterplot labels
#pCHART_GPIscatter <- f_ScatterLabels(pCHART_GPIscatter, 
#                                     xaxis = "Include",
#                                     yaxis = "Include",
#                                    left_text = "Lower Impact",
#                                    right_text = "Higher Impact",
#                                    up_text = "Less Peaceful",
#                                     down_text = "More Peaceful")


# PPI Scatterplot
CHART_PPIscatter.df = correlates.df %>%
  dplyr::filter(variablename %in% c("GTI", "PPI Overall Score")) %>%
  pivot_wider(names_from = variablename, values_from = value) %>%
  select(OECD, GTI, `PPI Overall Score`) %>%
  na.omit()

print(CHART_PPIscatter.df)

# Base Plot
p <- ggplot(CHART_PPIscatter.df, aes(x = GTI, y = `PPI Overall Score`, color = OECD)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("OECD" = "#2D4F5E", "Rest of the World" = "#770A1E")) +
  labs(x = "GTI Score", y = "PPI Overall Score", color = "") +
  geom_smooth(method = "lm", se = FALSE, aes(group = OECD)) 

print(p)



# GTI Theme
# f_ThemeGTI: Sets themes for GTI charts. Allows for easy customisation
f_ThemeGTI <- function(plot, chart_info, plottitle, xaxis, yaxis, xgridline, ygridline) {
  finalcaption <- paste0("Source: ", chart_info[["source"]])
  
  plot_labels <- labs(
    title = chart_info[["title"]],
    x = chart_info[["xtext"]],
    y = chart_info[["ytext"]],
    caption = finalcaption
  )
  
  plot_base <- theme_minimal()
  
  plot_theme <- plot_base +
    theme(text = element_text(family = HEAVY_FONT),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 9),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, colour = "#888686", size = 7),
          axis.text = element_text(colour = "#444444", size = 6.5, family = LIGHT_FONT),
          axis.title = element_text(face = "bold", size = 7, family = HEAVY_FONT),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 7, family = LIGHT_FONT),
          axis.line.x = element_blank(),
          axis.line.y = element_blank()
    )
  
  if (plottitle == "Include") {
    plot_theme <- plot_theme + theme(plot.title = element_text(size = 13, family = HEAVY_FONT))
  } else {
    plot_theme <- plot_theme + theme(plot.title = element_blank())
  }
  
  if (xaxis == "Include") {
    plot_theme <- plot_theme + theme(axis.line.x.bottom = element_line(colour = "#444444"))
  }
  
  if (yaxis == "Include") {
    plot_theme <- plot_theme + theme(axis.line.y = element_line(colour = "#444444"))
  }
  
  if (ygridline == "Include") {
    plot_theme <- plot_theme + theme(panel.grid.major.y = element_line(colour = "lightgrey"))
  } else {
    plot_theme <- plot_theme + theme(panel.grid.major.y = element_blank())
  }
  
  if (xgridline == "Include") {
    plot_theme <- plot_theme + theme(panel.grid.major.x = element_line(colour = "lightgrey"))
  } else {
    plot_theme <- plot_theme + theme(panel.grid.major.x = element_blank())
  }
  
  # Apply themes and labels to the plot
  plot <- plot + plot_labels + plot_theme
  
  # Adjust y-axis to position x-axis line at y=0
  if (xaxis == "Include") {
    plot <- plot + scale_y_continuous(expand = c(0,0))
  }
  
  return(plot)
}

pCHART_PPIscatter <- f_ThemeGTI(p,
                                CHART_PPIscatter.df,
                                plottitle = "", 
                                xaxis = "Include", 
                                yaxis = "Include", 
                                xgridline = "Include", 
                                ygridline = "Include") +
  theme(legend.position = c(.9,.1))

pCHART_PPIscatter <- f_ScatterLabels(pCHART_PPIscatter.df, 
                                     xaxis = "Include",
                                     yaxis = "Include",
                                     left_text = "Lower Impact",
                                     right_text = "Higher Impact",
                                     up_text = "Less Peaceful",
                                     down_text = "More Peaceful",
                                     yposition = 0.02)    



## -- Correlation Tables
f_CalculateCorrelations <- function(df) {
  df %>%
    pivot_wider(names_from = variablename, values_from = value) %>%
    select(where(is.numeric)) %>%
    select(-year) %>%
    cor(use = "complete.obs") %>%
    .[, "GTI", drop = FALSE] %>%
    as_tibble(rownames = "Indicator") %>%
    rename(Correlation = GTI)
}



# Calculate correlations for OECD countries
OECD_correlations.df <- correlates.df %>%
  dplyr::filter(OECD == "OECD") %>%
  f_CalculateCorrelations()

# Calculate correlations for Rest of the World
ROW_correlations.df <- correlates.df %>%
  dplyr::filter(OECD == "Rest of the World") %>%
  f_CalculateCorrelations()

# Correlation for Both
all_correlations.df <- correlates.df %>%
  f_CalculateCorrelations()

# Combine the results
correlation_table.df <- full_join(OECD_correlations.df, ROW_correlations.df, by = "Indicator") %>%
  rename(OECD = Correlation.x, `Rest of the World` = Correlation.y)



# Tidy the Table
correlation_table.df <- full_join(correlation_table.df, all_correlations.df, by = "Indicator") %>%
  rename(`All Countries` = Correlation) %>%
  mutate(Index = case_when(
    Indicator %in% corr_ppi.list ~ "PPI"),
    OECD = round(OECD, 2),
    `Rest of the World` = round(`Rest of the World`, 2),
    `All Countries` = round(`All Countries`, 2)) %>% 
  dplyr::filter(Indicator != "terrorism impact", Indicator != "GTI") %>%
  arrange(desc(OECD)) %>% relocate(Index) %>%
  mutate(Indicator = str_to_title(Indicator))

#GPI_DOMAINS = c("Overall Score","Safety and Security", "Ongoing Conflict", "Militarisation")
# Create GPI Table
#correlation_tableGPI.df <- correlation_table.df %>%
#  filter(Index == "GPI") %>%
# select(Indicator, `All Countries`,`OECD`,`Rest of the World`)
#domains.table <- correlation_tableGPI.df %>%
#  filter(Indicator %in% GPI_DOMAINS) %>%
#  arrange(Indicator != "Overall Score", desc(`All Countries`))
#indicators.table <- correlation_tableGPI.df %>%
#  filter(!Indicator %in% GPI_DOMAINS) %>%
#  arrange(desc(`All Countries`))
#domains_header <- tibble(Indicator = "GLOBAL PEACE INDEX DOMAINS", `All Countries` = NA, OECD = NA, `Rest of the World` = NA)
#indicators_header <- tibble(Indicator = "GLOBAL PEACE INDEX INDICATORS", `All Countries` = NA, OECD = NA, `Rest of the World` = NA)
#TABLE_GPIcorrelates.df <- bind_rows(domains_header, domains.table, indicators_header, indicators.table)

# Create PPI Table

PPI_DOMAINS = c("PPI Overall Score","Well-Functioning Government", "Sound Business Environment", "Low Levels of Corruption", "High Levels of Human Capital",
                "Acceptance of the Rights of Others", "Good Relations with Neighbours", "Free Flow of Information","Equitable Distribution of Resources")
PPI_THEMES = c("Attitudes","Institutions","Structures")

correlation_tablePPI.df <- correlation_table.df %>%
  filter(Index == "PPI") %>%
  select(Indicator, `All Countries`,`OECD`,`Rest of the World`)

domains.table <- correlation_tablePPI.df %>%
  filter(Indicator %in% PPI_DOMAINS) %>%
  arrange(Indicator != "PPI Overall Score", desc(`All Countries`))

themes.table <- correlation_tableGPI.df %>%
  filter(Indicator %in% PPI_THEMES) %>%
  arrange(desc(`All Countries`))

domains_header <- tibble(Indicator = "POSITIVE PEACE INDEX DOMAINS", `All Countries` = NA, OECD = NA, `Rest of the World` = NA)
indicators_header <- tibble(Indicator = "POSITIVE PEACE INDEX THEMES", `All Countries` = NA, OECD = NA, `Rest of the World` = NA)

TABLE_PPIcorrelates.df <- bind_rows(domains_header, domains.table, indicators_header)

# one level
wide_data <- correlates.df %>%
  pivot_wider(names_from = variablename, values_from = value)

# one level
ggplot(wide_data, aes(x = GTI, y = `PPI Overall Score`)) +
  geom_point(alpha = 0.5, color = "#770A1E", fill = "#770A1E", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#000033", linetype = "dashed") + # Trend line for all data
  geom_text(x = 8.00, y = 2, # Annotation position
          label = "R = 0.24", # Annotation text
          hjust = 0, vjust = 0, color = "black") +
  labs(x = "GTI score", y = "PPI score", title = "Correlation between Terorrism and Positive Peace", caption = "Source: IEP") +
  theme_minimal()


# two level
ggplot(wide_data, aes(x = GTI, y = `PPI Overall Score`, color = OECD)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("OECD" = "#2D4F5E", "Rest of the World" = "#770A1E")) +
  labs(x = "GTI Score", y = "PPI Theme Attitudes", color = "") +
  geom_smooth(method = "lm", se = FALSE, aes(group = OECD))


# other
correlation <- cor(merged_data$gti, merged_data$high.levels.of.human.capital)

ggplot(wide_data, aes(x = GTI, y = `Factionalised elites`)) +
  geom_point(alpha = 0.5, color = "#333333", fill = "#666666", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#000033", linetype = "dashed") + # Trend line for all data
  geom_text(x = 8.00, y = 2, # Annotation position
            label = "R = 0.31", # Annotation text
            hjust = 0, vjust = 0, color = "black") +
  labs(x = "GTI score", y = "Group grievance", title = "Correlation between Terorrism and Factionalised elites", caption = "Source: IEP") +
  theme_minimal()




#############################################################################################################################
# calculate correlations for indicators- all countries
corr_gti.df = readRDS("~/GitHub/gcerf/02_data/gti_banded_national.rds") 

corr_gti.df<- corr_gti.df%>%
  mutate(variablename = "GTI", value = banded_score) %>%
  select(ID_0, variablename, year, value) %>%
  drop_na(value)%>% rename (geocode = ID_0) 

# PPI Indicators 2024
ppi_scores_full <- rio::import("~/GitHub/gcerf/02_data/ppi_scores_full.xlsx")
ppi_scores_full<- ppi_scores_full%>%
  select(geocode, variablename, year, value)
corr_ppi.df<- ppi_scores_full

# Combine Data, divide countires into OECD and Non-OECD
IEP_NAMES = "~/GitHub/gcerf/02_data/gti-countrynames.xlsx" # Location of finalized list of country name spellings and associated data
THE_OECD = c("AUS","AUT","BEL","CAN","CHL", "COL", "CRI", "CZE", 
             "DNK", "EST","FIN","FRA","DEU", "GRC", "ISL","IRL", "ISR", "ITA","JPN", 
             "KOR", "LVA", "LTU", "LUX", "MEX", "NLD","NZL","NOR", "POL", "PRT", 
             "SVK", "SVN","ESP","SWE","CHE", "TUR ", "GBR", "USA")

# List of countries
countries.df = rio::import(IEP_NAMES) %>% 
  mutate(OECD = if_else(geocode %in% THE_OECD, "OECD", "Rest of the World"))


correlates.df <- left_join(corr_ppi.df, corr_gti.df, by = c("year", "geocode")) %>%
  left_join(countries.df)
combined_df <- left_join(corr_ppi.df, corr_gti.df, by = c("year", "geocode"), suffix = c(".x", ".y")) %>%
  left_join(countries.df)


correlation_results <- combined_df %>%
  group_by(variablename.x) %>%
  summarize(correlation = cor(value.x, value.y, use = "complete.obs"))

# Print the correlation results for all countries
print(correlation_results)
