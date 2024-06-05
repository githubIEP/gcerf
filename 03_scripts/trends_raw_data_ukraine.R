# pillars- trends

ppi <- rio::import("~/GitHub/gcerf/02_data/ppi_scores_full.xlsx")

ukraine_data <- ppi[ppi$country == "Ukraine", ]
ukraineukraine_data<- ukraine_data[ukraine_data$variablename == "Quality of information",]

ukraine_data$year <- as.numeric(as.character(ukraine_data$year))
ukraine_data <- ukraine_data[order(ukraine_data$year), ]

# Create a ggplot object
p<- ggplot(ukraineukraine_data, aes(x = year, y = value)) +
  geom_line(color = "#FFA500", size = 3) +  # Orange points for Researchers R&D
  labs(x = "Year", y = "Quality of information", title = "Quality of information", caption = "Source: IEP") +
  theme_minimal()

# Print the plot
print(p)

# add regional trend
ppi<- ppi[ppi$variablename == "Quality of information",]
country <- rio::import("~/GitHub/gcerf/02_data/gti-countrynames.xlsx")

tmp <- merge(ppi, country, by = c("geocode", "country"))
tmp<- tmp[tmp$region == "Russia and Eurasia",]

tmp <- tmp %>%
  group_by(year) %>%
  mutate(avrg = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% select(year, avrg)

merged_data <- merge(ukraine_data, tmp, by = "year")

ggsave("~/GitHub/gcerf/04_outputs/researchersrd.png", p, width=12, height=8, units="in")

###############################################################################
###############################################################################
ppi <- rio::import("~/GitHub/gcerf/02_data/ppi_scores_full.xlsx")
ukraine_data <- ppi[ppi$country == "Ukraine", ]

filtered_data <- ukraine_data %>%
  filter(year %in% c(2009, 2022))

# Calculate whether value in year 2009 is higher than in year 2022 for each variablename
result <- filtered_data %>%
  group_by(variablename) %>%
  summarize(higher_2009 = value[year == 2009] > value[year == 2022])

# Print the result
print(result)


filtered_data <- ukraine_data %>%
  filter(year %in% c(2015, 2020))

# Calculate whether value in year 2009 is higher than in year 2022 for each variablename
result <- filtered_data %>%
  group_by(variablename) %>%
  summarize(higher_2020 = value[year == 2020] > value[year == 2015])

# Print the result
print(result)


###############################################################################
# trendline- raw data
ppi_data<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")
ppi_data<- ppi_data%>%
  filter(country == "Ukraine")

ppi_data<- ppi_data%>%
  pivot_wider(names_from = variablename, values_from = value)
ppi_data <- ppi_data[, -which(names(ppi_data) == "reportname")]

ppi_data <- ppi_data[ppi_data$type == "Raw", ]
ppi_data<- select(ppi_data, year, `Researchers in R&D`)
ppi_data<- na.omit(ppi_data)

# region
all<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")

all<- all%>%
  filter(type == "Raw")

names<- rio::import("~/GitHub/gcerf/02_data/gti-countrynames.xlsx")

tmp<- left_join(all, names, by = c("geocode", "country"))
tmp<- tmp%>% filter (region == "Russia and Eurasia") %>% filter(variablename == "Researchers in R&D" )

avg<- aggregate(value ~ year, data = tmp, FUN = mean)

ppi_data<- left_join(ppi_data, avg, by = c("year"))

p<- ggplot(ppi_data, aes(x = year)) +
  geom_line(aes(y = `Researchers in R&D`), color = "orange", size = 1) +  # Solid line for 'value', orange color
  geom_line(aes(y = value), linetype = "dashed", color = "darkgreen", size = 1) +  # Dashed line for 'avrg', green color
  labs(x = "Year", y = "Researchers in R&D (raw)", title = "R&D Over the Years", caption = "Source: IEP") +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$"Researchers in R&D", 1), label = "Ukraine", vjust = -0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value, 1), label = "Russia and Eurasia", vjust = -0.5, hjust = 1) +
  theme_minimal()

print(p)

############################################################################### quality of information- two regions
ppi_data<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")
ppi_data<- ppi_data%>%
  filter(country == "Ukraine")

ppi_data<- ppi_data%>%
  pivot_wider(names_from = variablename, values_from = value)
ppi_data <- ppi_data[, -which(names(ppi_data) == "reportname")]

ppi_data <- ppi_data[ppi_data$type == "Raw", ]
ppi_data<- select(ppi_data, year, `Quality of information`)
ppi_data<- na.omit(ppi_data)

# region
all<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")

all<- all%>%
  filter(type == "Raw")

names<- rio::import("~/GitHub/gcerf/02_data/gti-countrynames.xlsx")

tmp<- left_join(all, names, by = c("geocode", "country"))
tmp <- tmp %>% filter(variablename == "Quality of information")
tmp1<- tmp%>% filter (region == "Russia and Eurasia") %>% filter(variablename == "Quality of information" )

tmp2<- tmp%>% filter (region == "Europe") %>% filter(variablename == "Quality of information" )

avg1<- aggregate(value ~ year, data = tmp1, FUN = mean)
avg2<- aggregate(value ~ year, data = tmp2, FUN = mean)

ppi_data<- left_join(ppi_data, avg1, by = c("year"))
ppi_data<- left_join(ppi_data, avg2, by = c("year"))


p<- ggplot(ppi_data, aes(x = year)) +
  geom_line(aes(y = `Quality of information`), color = "orange", size = 1) +  # Solid line for 'value', orange color
  geom_line(aes(y = value.x), linetype = "dashed", color = "darkgreen", size = 1) +  # Dashed line for 'avrg', green color
  geom_line(aes(y = value.y), linetype = "dashed", color = "darkblue", size = 1) +  # Dashed line for 'avrg', green color
  labs(x = "Year", y = "Quality of information
  More disinformation                   Less disinformation", title = "", caption = "Source: v-dem, IEP Calculations") +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$"Quality of information", 1), label = "Ukraine", vjust = -0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value.x, 1), label = "Russia and Eurasia", vjust = 0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value.y, 1), label = "Europe", vjust = -0.5, hjust = 1) +
  theme_minimal()+
  ylim(-2.5, 2.5)

print(p)



####
tmp<- iepg_get(15299) 
tmp<- tmp%>% filter (geoname == "Ukraine")

p <- ggplot(tmp, aes(x = year, y = value)) +
  geom_line(color = "orange", size = 1) +  # Solid line for 'value', orange color
  labs(x = "Year", y = "Government dissemination of false information domestically
       0-Extremly Often, 4-Never",
       caption = "Source: vdem, IEP Calculations") +
  theme_minimal()

print(p)

#### improvements (control of corruption, freedom of the press, )
ppi_data<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")
ppi_data<- ppi_data%>%
  filter(country == "Ukraine")

ppi_data<- ppi_data%>%
  pivot_wider(names_from = variablename, values_from = value)
ppi_data <- ppi_data[, -which(names(ppi_data) == "reportname")]

ppi_data <- ppi_data[ppi_data$type == "Raw", ]
ppi_data<- select(ppi_data, year, `Researchers in R&D`)
ppi_data<- na.omit(ppi_data)

all<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")

all<- all%>%
  filter(type == "Raw")

names<- rio::import("~/GitHub/gcerf/02_data/gti-countrynames.xlsx")

tmp<- left_join(all, names, by = c("geocode", "country"))
tmp1<- tmp%>% filter (region == "Russia and Eurasia") %>% filter(variablename == "Researchers in R&D" )
tmp2<- tmp%>% filter (region == "Europe") %>% filter(variablename == "Researchers in R&D" )

avg1<- aggregate(value ~ year, data = tmp1, FUN = mean)
avg2<- aggregate(value ~ year, data = tmp2, FUN = mean)

ppi_data<- left_join(ppi_data, avg1, by = c("year"))
ppi_data<- left_join(ppi_data, avg2, by = c("year"))


p<- ggplot(ppi_data, aes(x = year)) +
  geom_line(aes(y = `Researchers in R&D`), color = "orange", size = 1) +  # Solid line for 'value', orange color
  geom_line(aes(y = value.x), linetype = "dashed", color = "darkgreen", size = 1) +  # Dashed line for 'avrg', green color
  geom_line(aes(y = value.y), linetype = "dashed", color = "darkblue", size = 1) +  # Dashed line for 'avrg', green color
  labs(x = "Year", y = "Researchers in R&D (per million people)", title = "", caption = "Source: UNESCO, IEP Calculations") +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$"Researchers in R&D", 1), label = "Ukraine", vjust = -0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value.x, 1), label = "Russia and Eurasia", vjust = -0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value.y, 1), label = "Europe", vjust = -0.5, hjust = 1) +
  theme_minimal()+
  scale_y_continuous(labels = comma_format())


print(p)


########################################### exclusion by socio-economic group
ppi_data<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")
ppi_data<- ppi_data%>%
  filter(country == "Ukraine")

ppi_data<- ppi_data%>%
  pivot_wider(names_from = variablename, values_from = value)
ppi_data <- ppi_data[, -which(names(ppi_data) == "reportname")]

ppi_data <- ppi_data[ppi_data$type == "Raw", ]
ppi_data<- select(ppi_data, year, `Exclusion by socio-economic group`)
ppi_data<- na.omit(ppi_data)

# region
all<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")

all<- all%>%
  filter(type == "Raw")

names<- rio::import("~/GitHub/gcerf/02_data/gti-countrynames.xlsx")

tmp<- left_join(all, names, by = c("geocode", "country"))
tmp <- tmp %>% filter(variablename == "Exclusion by socio-economic group")
tmp1<- tmp%>% filter (region == "Russia and Eurasia") %>% filter(variablename == "Exclusion by socio-economic group" )

tmp2<- tmp%>% filter (region == "Europe") %>% filter(variablename == "Exclusion by socio-economic group" )

avg1<- aggregate(value ~ year, data = tmp1, FUN = mean)
avg2<- aggregate(value ~ year, data = tmp2, FUN = mean)

ppi_data<- left_join(ppi_data, avg1, by = c("year"))
ppi_data<- left_join(ppi_data, avg2, by = c("year"))


p<- ggplot(ppi_data, aes(x = year)) +
  geom_line(aes(y = `Exclusion by socio-economic group`), color = "orange", size = 1) +  # Solid line for 'value', orange color
  geom_line(aes(y = value.x), linetype = "dashed", color = "darkgreen", size = 1) +  # Dashed line for 'avrg', green color
  geom_line(aes(y = value.y), linetype = "dashed", color = "darkblue", size = 1) +  # Dashed line for 'avrg', green color
  labs(x = "Year", y = "Exclusion by socio-economic group
  More exclusion                          Less exclusion", title = "", caption = "Source: v-dem, IEP Calculations") +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$"Exclusion by socio-economic group", 1), label = "Ukraine", vjust = -0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value.x, 1), label = "Russia and Eurasia", vjust = -0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value.y, 1), label = "Europe", vjust = -0.5, hjust = 1) +
  theme_minimal()

print(p)



########################################### freedom of the press
ppi_data<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")
ppi_data<- ppi_data%>%
  filter(country == "Ukraine")

ppi_data<- ppi_data%>%
  pivot_wider(names_from = variablename, values_from = value)
ppi_data <- ppi_data[, -which(names(ppi_data) == "reportname")]

ppi_data <- ppi_data[ppi_data$type == "Raw", ]
ppi_data<- select(ppi_data, year, `Freedom of the press`)
ppi_data<- na.omit(ppi_data)

# region
all<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")

all<- all%>%
  filter(type == "Raw")

names<- rio::import("~/GitHub/gcerf/02_data/gti-countrynames.xlsx")

tmp<- left_join(all, names, by = c("geocode", "country"))
tmp <- tmp %>% filter(variablename == "Freedom of the press")
tmp1<- tmp%>% filter (region == "Russia and Eurasia") %>% filter(variablename == "Freedom of the press" )

tmp2<- tmp%>% filter (region == "Europe") %>% filter(variablename == "Freedom of the press" )

avg1<- aggregate(value ~ year, data = tmp1, FUN = mean)
avg2<- aggregate(value ~ year, data = tmp2, FUN = mean)

ppi_data<- left_join(ppi_data, avg1, by = c("year"))
ppi_data<- left_join(ppi_data, avg2, by = c("year"))


p<- ggplot(ppi_data, aes(x = year)) +
  geom_line(aes(y = `Freedom of the press`), color = "orange", size = 1) +  # Solid line for 'value', orange color
  geom_line(aes(y = value.x), linetype = "dashed", color = "darkgreen", size = 1) +  # Dashed line for 'avrg', green color
  geom_line(aes(y = value.y), linetype = "dashed", color = "darkblue", size = 1) +  # Dashed line for 'avrg', green color
  labs(x = "Year", y = "Freedom of the Press
       (0=good, -100=very serious)", title = "", caption = "Source: RSF, IEP Calculations") +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$"Freedom of the press", 1), label = "Ukraine", vjust = -0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value.x, 1), label = "Russia and Eurasia", vjust = -0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value.y, 1), label = "Europe", vjust = -0.5, hjust = 1) +
  theme_minimal()+
  ylim(-60,0)

print(p)




########################################### control of corruption
ppi_data<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")
ppi_data<- ppi_data%>%
  filter(country == "Ukraine")

ppi_data<- ppi_data%>%
  pivot_wider(names_from = variablename, values_from = value)
ppi_data <- ppi_data[, -which(names(ppi_data) == "reportname")]

ppi_data <- ppi_data[ppi_data$type == "Raw", ]
ppi_data<- select(ppi_data, year, `Control of corruption`)
ppi_data<- na.omit(ppi_data)

# region
all<- rio::import("~/GitHub/gcerf/02_data/ppi_data_banded.xlsx")

all<- all%>%
  filter(type == "Raw")

names<- rio::import("~/GitHub/gcerf/02_data/gti-countrynames.xlsx")

tmp<- left_join(all, names, by = c("geocode", "country"))
tmp <- tmp %>% filter(variablename == "Control of corruption")
tmp1<- tmp%>% filter (region == "Russia and Eurasia") %>% filter(variablename == "Control of corruption" )

tmp2<- tmp%>% filter (region == "Europe") %>% filter(variablename == "Control of corruption" )

avg1<- aggregate(value ~ year, data = tmp1, FUN = mean)
avg2<- aggregate(value ~ year, data = tmp2, FUN = mean)

ppi_data<- left_join(ppi_data, avg1, by = c("year"))
ppi_data<- left_join(ppi_data, avg2, by = c("year"))


p<- ggplot(ppi_data, aes(x = year)) +
  geom_line(aes(y = `Control of corruption`), color = "orange", size = 1) +  # Solid line for 'value', orange color
  geom_line(aes(y = value.x), linetype = "dashed", color = "darkgreen", size = 1) +  # Dashed line for 'avrg', green color
  geom_line(aes(y = value.y), linetype = "dashed", color = "darkblue", size = 1) +  # Dashed line for 'avrg', green color
  labs(x = "Year", y = "Control of corruption
  Higher corruption                        Lower corruption", title = "", caption = "Source: World Bank, IEP Calculations") +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$"Control of corruption", 1), label = "Ukraine", vjust = -0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value.x, 1), label = "Russia and Eurasia", vjust = -0.5, hjust = 1) +
  annotate("text", x = max(ppi_data$year), y = tail(ppi_data$value.y, 1), label = "Europe", vjust = -0.5, hjust = 1) +
  theme_minimal()

print(p)






