

training.df <- rio::import("Data/GPI_sample.csv")

CHART_GovtConflict = c(title = "Average level of conflict by government type",
                       sheet = "GovtConflict", source = "EIU, IEP Calculations", xtext = "", ytext = "ONGOING CONFLICT SCORE",
                       type = "Chart", position = "Normal")


CHART_GovtConflict.df <- training.df %>%
  dplyr::filter(year == 2024, indicator == "ongoing conflict", type == "banded") %>%
  mutate(government = str_to_lower(government)) %>%
  group_by(government) %>%
  summarise(value = mean(value)) %>%
  arrange(value) %>%
  ungroup()


p <- ggplot(data = CHART_GovtConflict.df, aes(x = government, y = value)) +
  geom_bar(stat = "identity")

pCHART_GovtConflict <- f_ThemeTraining(plot = p, 
                                       chart_info = CHART_GovtConflict, 
                                       plottitle = "Include", 
                                       xaxis = "Include", 
                                       yaxis = "Include", 
                                       xgridline = "", 
                                       ygridline = "Include")

pCHART_GovtConflict


