# Set working directory to "BEB Working Directory"

library(tidyverse)
library(forcats)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(patchwork)
library(reshape2)
library(gt)
library(webshot)
library(maps)
library(mapproj)


# Data files
raw_veh <- read.csv("2019-Data/2019 Revenue Vehicle Inventory.csv",
                    na.strings = c("", "NA"),
                    stringsAsFactors = FALSE)
raw_veh_21 <- read.csv("2021-Data/2021 Revenue Vehicle Inventory.csv",
                       na.strings = c("", "NA"))
agency_info <- read.csv("2021-Data/2021 Agency Information.csv",
                        na.strings = c("", "NA"),
                        stringsAsFactors = FALSE)
service_info <- read.csv("2019-Data/2019 Service.csv",
                         na.strings = c("", "NA"),
                         stringsAsFactors = FALSE)
service_info_21 <- read.csv("2021-Data/2021 Service.csv",
                            na.strings = c("", "NA"),
                            stringsAsFactors = FALSE)
# "raw_energy_2" comes from the 2019 Fuel and Energy macro excel sheet titled "Fuel and Energy"
raw_energy_2 <- read.csv("2021-Data/2021 Fuel and Energy.csv",
                         na.strings = c("", "NA"), 
                         stringsAsFactors = FALSE)
egrid <- read.csv("Powergrid/egrid2020.csv",
                  na.strings = c("", "NA"), 
                  stringsAsFactors = FALSE)
egrid_zip <- read.csv("Powergrid/zipcode_tool.csv",
                      na.strings = c("", "NA"), 
                      stringsAsFactors = TRUE)
emfac <- read.csv("Emissions/EMFAC2021-ER-202xClass-Statewide-2023-Annual-20230117072304.csv",
                  skip = 8)

# Emfac comes with units of g/mi, this provides the mpg used in those calculataions so we may apply our own mileage
emfac_eco <- read.csv("Emissions/EMFAC2021-avg_fuel_eco.csv", skip = 8)

batt_lc_emissions <- read.csv("Batteries/chemistry_specs.csv",
                               na.strings = c("", "NA"), 
                               stringsAsFactors = TRUE)
# CNG in "welltotank" is calculated into grams per diesel gallon equivalent 
# (dge, it is assumed this is how most agencies reported mpg for CNG
# as they were only advised to report CNG mpg as "based on what type of 
# fuel the revenue vehicle would use if it were not powered by CNG")
welltotank <- read.csv("Emissions/welltotank.csv", 
                           na.strings = c("", "NA"), 
                           stringsAsFactors = TRUE)

# Battery Specifications of 10 most popular vehicle make and models
batt_specs <- read.csv("Batteries/Batt_Specs.csv",
                       na.strings = c("", "NA"), 
                       stringsAsFactors = TRUE)

summary(raw_veh_21)

colnames(raw_veh)==colnames(raw_veh_21)

colnames(raw_veh_21)[1] <- "NTD.ID"
raw_veh_21 = subset(raw_veh_21, select = -c(Group.Plan.Sponsor.NTDID, Group.Plan.Sponsor.Name))

#### General cleaning for raw vehicle data ####
dollarsign_to_numeric <- function(raw_data, list_numeric_fields) {
  temp_veh <- raw_data
  # Replace "NA" with zero
  temp_veh[is.na(temp_veh)] <- 0
  
  # Selecting numeric/integer fields
  
  # Remove special characters to make fields numeric
  for (i in list_numeric_fields) {
    temp_veh[, i] <- temp_veh[, i] %>% 
      as.character %>% 
      gsub(",","", .) %>% 
      gsub("-","", .) %>% 
      as.numeric()
  }
  rm(i, list_numeric_fields)
  
  temp_veh
}

x <- c(8,9,14,15,17,21,22,23,25,26,27,28,29)
temp_veh <- dollarsign_to_numeric(raw_veh_21, x)
summary(temp_veh)

# Separate mode (MB - Buses, DR - Demand Response, etc.) from type (Purchased or Directly operated)
temp_veh$Mode <- temp_veh$Modes %>% str_sub(end = 2L)
temp_veh$Mode %>% as.factor() %>% summary()

# How many miles driven on active vehicles by mode?
mode_share <- aggregate(Total.Miles.on.Active.Vehicles.During.Period ~ Mode, data = temp_veh, FUN = "sum")
total.mi <- sum(mode_share$Total.Miles.on.Active.Vehicles.During.Period)
mode_share$proportion <- (mode_share$Total.Miles.on.Active.Vehicles.During.Period / total.mi) * 100
mode_share$proportion <- mode_share$proportion %>% round(digits = 2)

# "MB" are transit buses generally, keeping directly operated (DO) and purchased transportation (PT)
veh_data_agg <- temp_veh[temp_veh$Modes=="MB/DO" | temp_veh$Modes=="MB/PT",]
veh_data_agg <- unique(veh_data_agg)

# Raw vehicles types
data_sum <- veh_data_agg %>% 
  group_by(Fuel.Type) %>% 
  summarize(Count.of.bus.fuel.type = sum(Total.Fleet.Vehicles))

data_sum$proportions <- data_sum$Count.of.bus.fuel.type/sum(data_sum$Count.of.bus.fuel.type)
sum(data_sum$Count.of.bus.fuel.type)

#### Some EDA ####
summary(veh_data_agg$Standing.Capacity)
summary(veh_data_agg$Seating.Capacity)
veh_data_agg$Total.Capacity <- veh_data_agg$Seating.Capacity + veh_data_agg$Standing.Capacity

# There are some vehicles with capacity of 2
summary(veh_data_agg$Total.Capacity)

# These vehicles are listed as "Automobiles"
veh_data_agg[veh_data_agg$Total.Capacity==2, "Vehicle.Type"]

# There is more than just bus data in this data set
veh_data_agg$Vehicle.Type <- factor(veh_data_agg$Vehicle.Type)
summary(veh_data_agg$Vehicle.Type)

# Keep only transit bus data
x <- sum(veh_data_agg$Total.Fleet.Vehicles)
veh_data_agg <- filter(veh_data_agg,
                       veh_data_agg$Vehicle.Type == "Bus" |
                         veh_data_agg$Vehicle.Type == "Articulated Bus" | 
                         veh_data_agg$Vehicle.Type == "Cutaway" |
                         veh_data_agg$Vehicle.Type == "Double Decker Bus")
y <- sum(veh_data_agg$Total.Fleet.Vehicles)

print(paste0(x-y, " vehicles not classified as buses removed"))

Total.US.BEB <- sum(veh_data_agg[veh_data_agg$Fuel.Type == "Electric Battery", "Total.Fleet.Vehicles"])
Active.US.BEB <- sum(veh_data_agg[veh_data_agg$Fuel.Type == "Electric Battery", "Active.Fleet.Vehicles"])
service.miles.2021 <- sum(veh_data_agg[, "Total.Miles.on.Active.Vehicles.During.Period"])

print(paste0("There are ", 
             Total.US.BEB, 
             " total electric buses and ", 
             Active.US.BEB, 
             " active electric buses in service as of the 2021 according to the FTA NTD dataset."))

print(paste0("In 2021, there were ",
             service.miles.2021,
             " service miles."))

# Includes AS (American Samoa), GU (Guam), PR (Puerto Rico), and VI (Virgin Islands) 
agency_info$State %>% as.factor() %>% summary()

# Merge agency locations to their bus data and remove territories
joined_veh_data <- left_join(veh_data_agg, agency_info, by = 'NTD.ID')

joined_veh_data[is.na(joined_veh_data$Zip.Code), "Total.Fleet.Vehicles"] %>% sum()
joined_veh_data <- joined_veh_data[!is.na(joined_veh_data$Zip.Code),]

x <- sum(joined_veh_data$Total.Fleet.Vehicles)
joined_veh_data <- filter(joined_veh_data,
                          joined_veh_data$State != "AS" &
                            joined_veh_data$State != "GU" & 
                            joined_veh_data$State != "PR" & 
                            joined_veh_data$State != "VI")
y <- sum(joined_veh_data$Total.Fleet.Vehicles)
print(paste0(x-y, " vehicles removed by excluding territories"))
print(paste0(y, " total transit buses in the U.S."))

## What organization types are there?
joined_veh_data$Organization.Type <- joined_veh_data$Organization.Type %>% as.factor()
summary(joined_veh_data$Organization.Type)

## Funding sources?
joined_veh_data$Funding.Source <- joined_veh_data$Funding.Source %>% as.factor()
summary(joined_veh_data$Funding.Source)

## Summarize by fuel type
x <- sum(joined_veh_data$Total.Fleet.Vehicles)
joined_veh_data <- filter(joined_veh_data, Fuel.Type == "Diesel Fuel"|
                            Fuel.Type == "Compressed Natural Gas"|
                            Fuel.Type == "Gasoline" |
                            Fuel.Type == "Electric Battery"|
                            Fuel.Type == "Hybrid Diesel"|
                            Fuel.Type == "Hybrid Gasoline")
y <- sum(joined_veh_data$Total.Fleet.Vehicles)
print(paste0(x-y, " buses removed for not being of listed fuel types"))

#### Plotting Manufacture year ####
x <- sum(joined_veh_data$Total.Fleet.Vehicles)
joined_veh_data <- filter(joined_veh_data, Manufacture.Year>0)
y <- sum(joined_veh_data$Total.Fleet.Vehicles)
print(paste0(x-y, " vehicles removed for having a manufacture year <= 0"))

x <- sum(joined_veh_data$Total.Fleet.Vehicles)
joined_veh_data <- joined_veh_data[joined_veh_data$Manufacture.Year>1990,]
y <- sum(joined_veh_data$Total.Fleet.Vehicles)
print(paste0(x-y, " vehicles removed for having a manufacture year <= 1990"))

#### Summarizing and Plotting Fuel Type ####
data_sum <- joined_veh_data %>% 
  group_by(Fuel.Type) %>% 
  summarize(Count.of.bus.fuel.type = sum(Total.Fleet.Vehicles))

fuel_type_plot <- ggplot(data = data_sum, aes(x=fct_reorder(Fuel.Type, Count.of.bus.fuel.type), y=Count.of.bus.fuel.type)) +
  geom_col() + 
  coord_flip() + 
  labs(y="Number of Buses", x="Fuel Type") + theme_bw()

fuel_type_plot

pct_diesel <- sum((data_sum[str_detect(data_sum$Fuel.Type, "Diesel"), "Count.of.bus.fuel.type"])) /
  sum(data_sum$Count.of.bus.fuel.type)

data_sum <- joined_veh_data %>% 
  group_by(Manufacture.Year) %>%
  summarize(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

#### FIGURE: Manufacture Year ####
manufacture_year <- ggplot(data = data_sum, aes(x=Manufacture.Year, y=Active.Fleet.Vehicles)) + 
  geom_col(fill="grey16") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11),
        text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 12, family = "sans"),
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 4.5),
        axis.title.y = element_text(margin = margin(r = 15), vjust = -0.5)) +
  scale_x_continuous(limits = c(1990, 2022),
                     breaks = 1990:2021) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  labs(x = "Manufacture Year", y = "Number of Active U.S. Transit Buses in 2021")

manufacture_year

# ggsave("Graphs/manufacture_year.png",
#        plot = manufacture_year,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

#### SI FIGURES: Manufacture Years for Big Three ####
nyc <- joined_veh_data[joined_veh_data$Agency.Name.x == "MTA New York City Transit",]
chicago <- joined_veh_data[joined_veh_data$Agency.Name.x == "Chicago Transit Authority",]
la_metro <- joined_veh_data[joined_veh_data$Agency.Name.x == "Los Angeles County Metropolitan Transportation Authority ",]

data_nyc <- nyc %>% 
  group_by(Manufacture.Year) %>%
  summarize(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

data_chicago <- chicago %>% 
  group_by(Manufacture.Year) %>%
  summarize(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

data_la <- la_metro %>% 
  group_by(Manufacture.Year) %>%
  summarize(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

manufacture_year_nyc <- ggplot(data = data_nyc, aes(x=Manufacture.Year, y=Active.Fleet.Vehicles)) + 
  geom_col(fill="grey16") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11),
        text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 12, family = "sans"),
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 4.5),
        axis.title.y = element_text(margin = margin(r = 15), vjust = -0.5)) +
  scale_x_continuous(limits = c(2000, 2022),
                     breaks = 2000:2021) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  labs(x = "Manufacture Year", y = str_wrap("MTA New York City Transit's Number of Active Transit Buses in 2021", 26))

manufacture_year_nyc

# ggsave("Graphs/manufacture_year_nyc.png",
#        plot = manufacture_year_nyc,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

manufacture_year_chicago <- ggplot(data = data_chicago, aes(x=Manufacture.Year, y=Active.Fleet.Vehicles)) + 
  geom_col(fill="grey16") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11),
        text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 12, family = "sans"),
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 4.5),
        axis.title.y = element_text(margin = margin(r = 15), vjust = -0.5)) +
  scale_x_continuous(limits = c(2000, 2022),
                     breaks = 2000:2021) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ","),
                     limits = c(0, 600)) +
  labs(x = "Manufacture Year", y = str_wrap("Chicago Transit Authority's Number of Active Transit Buses in 2021", 27))

manufacture_year_chicago

# ggsave("Graphs/manufacture_year_chicago.png",
#        plot = manufacture_year_chicago,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

manufacture_year_la <- ggplot(data = data_la, aes(x=Manufacture.Year, y=Active.Fleet.Vehicles)) + 
  geom_col(fill="grey16") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11),
        text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 12, family = "sans"),
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 4.5),
        axis.title.y = element_text(margin = margin(r = 15), vjust = -0.5)) +
  scale_x_continuous(limits = c(2000, 2022),
                     breaks = 2000:2021) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ","),
                     limits = c(0, 600)) +
  labs(x = "Manufacture Year", y = str_wrap("Los Angeles County MTA's Number of Active Transit Buses in 2021", 27))

manufacture_year_la

# ggsave("Graphs/manufacture_year_la.png",
#        plot = manufacture_year_la,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

#### SI FIGURE: Organization Types ####
data_sum = joined_veh_data %>%
  group_by(Organization.Type) %>%
  summarize(Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

organizations <- ggplot(data = data_sum, 
                       aes(x = fct_reorder(Organization.Type, Active.Fleet.Vehicles),
                           y = Active.Fleet.Vehicles)) + 
  theme_bw() +
  geom_col(fill = "grey16", position = position_dodge(0.1)) + 
  coord_flip() +
  labs(y = str_wrap("Number of U.S. Transit Buses owned by an Organization Type", width = 30), x = "Organization Type") +
  scale_y_continuous(breaks = seq(0, 40000, 5000),
                     labels = function(x) format(x, big.mark = ",")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 45)) +
  theme(axis.text.y = element_text(size = 10))

organizations

# ggsave("Graphs/organizations.png",
#        plot = organizations,
#        dpi = 700,
#        width = 6.5,
#        height = 5,
#        units = "in")


#### SI FIGURE: Funding Source ####
data_sum = joined_veh_data %>%
  group_by(Funding.Source) %>%
  summarize(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles))

funding <-  ggplot(data = data_sum,
                   aes(x=fct_reorder(Funding.Source, Total.Fleet.Vehicles), y=Total.Fleet.Vehicles)) +
  geom_col(fill = "grey16") + 
  coord_flip() +
  theme_bw() +
  labs(y = str_wrap("Number of U.S. Transit Buses Partially or Fully Funded by Source Type", width = 40),
       x = "Funding Source Type") +
  scale_y_continuous(breaks = seq(0, 40000, 5000),
                     labels = function(x) format(x, big.mark = ",")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 45)) 

funding

# ggsave("Graphs/funding.png",
#        plot = funding,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

#### EDA of service information ####

#number of conventional buses to serve a route
colnames(service_info) == colnames(service_info_21)
colnames(service_info_21)[colnames(service_info) != colnames(service_info_21)]
colnames(service_info)[colnames(service_info) != colnames(service_info_21)]
# Seems like "service_info" has the most correct names
colnames(service_info_21) <- colnames(service_info)

service_info_21 <- filter(service_info_21, Mode=="MB")
service_info_21 <- unique(service_info_21)
# We will obtain days operated for each agency to be able to calculate
# approximate daily mileage for each transit bus
summary(service_info_21)
# Divide agency information and vehicle information
agency_service <- service_info_21[,c(1:10)]
vehicle_service <- service_info_21[,c(11,12,14:21,33,34)]
# Any vehicle information that is NA will be replaced with 0
vehicle_service[is.na(vehicle_service)] <- 0
# Recombine vehicle and agency information
service <- cbind.data.frame(agency_service, vehicle_service)


# Distinguish between directly operated and purchased transportation
do_service <- service[service$TOS=="DO" & service$Mode == "MB",]
pt_service <- service[service$TOS=="PT" & service$Mode == "MB",]
rm(agency_service, vehicle_service)


# We want annual totals of the days operated
# Some agencies did not report annual totals but
# did report weekday and weekend data
for (j in 1:2) {
  if(j==1){
    service <- do_service
    service <- service[!is.na(service$NTD.ID),]
    agency_list <- unique(service$NTD.ID)
  } else {
    service <- pt_service
    service <- service[!is.na(service$NTD.ID),]
    agency_list <- unique(service$NTD.ID)
  }
  
  for (i in 1:length(agency_list)) {
    
    # x will be each agencies' individual service info
    x <- service[service$NTD.ID==agency_list[i],]
    # y will be the "annual total" field of x (one data point)
    y <- x[x$Time.Period=="Annual Total",] 
    
    # if the number of annual total days operated is zero, add the weekdays and weekends for the year
    if(y[1,22] == 0) {
      y[1,22] <- sum(x[x$Time.Period=="Average Typical Weekday",22], 
                     x[x$Time.Period=="Average Typical Saturday",22], 
                     x[x$Time.Period=="Average Typical Sunday",22], 
                     na.rm = TRUE)
    }else{}
    
    temp <- rbind.data.frame(x[x$Time.Period=="Average Typical Weekday",], 
                             x[x$Time.Period=="Average Typical Saturday",], 
                             x[x$Time.Period=="Average Typical Sunday",], 
                             y)
    
    if(i==1){
      if(j==1){
        dts_service <- temp
      }
      else{dts_service <- rbind.data.frame(dts_service, temp)
      }
    }else{
      dts_service <- rbind.data.frame(dts_service, temp)
    }
    
    rm(x, y)
    
  }
}

# Product is the originally given service info with annual service provided from by agency
dts_service$Passenger.Miles <- dts_service$Passenger.Miles %>% gsub("[$,]","", .) %>% as.numeric()
annual_service <- dts_service[dts_service$Time.Period=="Annual Total",]
rm(temp, i)

#### Group by make and model ####
electric_veh_data <- filter(joined_veh_data,
                            joined_veh_data$Fuel.Type == "Electric Battery")
electric_veh_data$Manufacturer <- as.factor(electric_veh_data$Manufacturer)
levels(electric_veh_data$Manufacturer)[levels(electric_veh_data$Manufacturer)=="Build Your Dreams, Inc."] <- "BYD Motors"
levels(electric_veh_data$Manufacturer)[levels(electric_veh_data$Manufacturer)=="Flyer Industries Ltd (aka New Flyer Industries)"] <- "New Flyer of America"

summary(electric_veh_data$Manufacturer)

# Most popular make and model
electric_veh_data$make_model <- str_c(electric_veh_data$Manufacturer," ", electric_veh_data$Model)
summary(electric_veh_data$make_model %>% as.factor())
electric_veh_data$make_model <- str_to_title(electric_veh_data$make_model, locale = "en")

electric_veh_data[str_detect(electric_veh_data$make_model, "Byd Motors K7"), "make_model"] <- "Byd Motors K7"
electric_veh_data[str_detect(electric_veh_data$make_model, "Byd Motors K9|Byd Motors k9|Byd Motors Byd K9"), "make_model"] <- "Byd Motors K9"
electric_veh_data[str_detect(electric_veh_data$make_model, "Gillig Corporation L"), "make_model"] <- "Gillig Corporation Low Floor Plus"
electric_veh_data[str_detect(electric_veh_data$make_model, "New Flyer Of America X"), "make_model"] <- "New Flyer of America Xcelsior"
electric_veh_data[str_detect(electric_veh_data$make_model, "Proterra Inc. Eco"), "make_model"] <- "Proterra Inc. Ecoride"
electric_veh_data[str_detect(electric_veh_data$make_model, "Zx5"), "make_model"] <- "Proterra Inc. Zx5"
electric_veh_data[str_detect(electric_veh_data$make_model, "Proterra Inc. X"), "make_model"] <- "Proterra Inc. Catalyst Xr"
electric_veh_data[str_detect(electric_veh_data$make_model, "Proterra Inc. Catalyst X"), "make_model"] <- "Proterra Inc. Catalyst Xr"
electric_veh_data[str_detect(electric_veh_data$make_model, "E2|C2"), "make_model"] <- "Proterra Inc. Catalyst E2"
electric_veh_data[str_detect(electric_veh_data$make_model, "Proterra Inc. E2"), "make_model"] <- "Proterra Inc. Catalyst E2"
electric_veh_data[electric_veh_data$make_model == "Proterra Inc. Catalyst" & electric_veh_data$Manufacture.Year>= 2015, "make_model"] <- "Proterra Inc. Catalyst Xr"
electric_veh_data[electric_veh_data$make_model == "Proterra Inc. Catalyst" & electric_veh_data$Manufacture.Year< 2015, "make_model"] <- "Proterra Inc. Catalyst FC"
electric_veh_data[str_detect(electric_veh_data$make_model, "fc"), "make_model"] <- "Proterra Inc. Catalyst FC"
electric_veh_data[str_detect(electric_veh_data$make_model, "Be"), "make_model"] <- "Proterra Inc. Ecoride"

known_models <- c("Byd Motors K7", 
                  "Byd Motors K9", 
                  "Byd Motors K11", 
                  "Gillig Corporation Low Floor Plus",
                  "New Flyer of America Xcelsior",
                  "Proterra Inc. Ecoride",
                  "Proterra Inc. Catalyst FC",
                  "Proterra Inc. Catalyst E2",
                  "Proterra Inc. Catalyst Xr",
                  "Proterra Inc. Zx5")

electric_veh_data[!(electric_veh_data$make_model %in% known_models), "make_model"] <- NA


data_sum <- electric_veh_data %>% group_by(make_model) %>% summarise(Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles),
                                                                     Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles))

data_sum$Active.Proportion <- data_sum$Active.Fleet.Vehicles/sum(data_sum$Active.Fleet.Vehicles)
data_sum$Total.Proportion <- data_sum$Total.Fleet.Vehicles/sum(data_sum$Total.Fleet.Vehicles)

data_sum <- left_join(data_sum, batt_specs, by="make_model")

batt_data_sum <- data_sum %>% group_by(batt_chem) %>% summarise(Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles),
                                                                Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
                                                                Active.Proportion = sum(Active.Proportion),
                                                                Total.Proportion = sum(Total.Proportion))

joined_veh_data <- left_join(joined_veh_data, electric_veh_data, by = colnames(joined_veh_data))

joined_veh_data[joined_veh_data$Fuel.Type == "Electric Battery", "Active.Fleet.Vehicles"] %>% sum()

# Replace unknown vehicles with most common vehicle
joined_veh_data[joined_veh_data$Fuel.Type == "Electric Battery" & is.na(joined_veh_data$make_model), "make_model"] <- "Proterra Inc. Catalyst E2"
joined_veh_data[joined_veh_data$Fuel.Type == "Electric Battery", "make_model"]%>% as.factor() %>% summary()


#### Bus Ranges ####

summary(annual_service)

# This filter removes agencies that still have no reported days open
# These agencies also report zero passenger miles
annual_service <- filter(annual_service,
                         Days.of.Service.Operated>0,
                         Vehicles.Passenger.Cars.Operated.in.Maximum.Service>0)

# Make mode field match the fta data for easier joining
annual_service$Modes <- paste0(annual_service$Mode, "/", annual_service$TOS)
joined_veh_data <- left_join(joined_veh_data, annual_service, by=c("NTD.ID", "Modes"))

x <- sum(joined_veh_data$Total.Fleet.Vehicles)
joined_veh_data <- joined_veh_data[joined_veh_data$Total.Miles.on.Active.Vehicles.During.Period>0,]
# Some of these vehicles are part of a contingency fleet or just seem to be missing data
y <- sum(joined_veh_data$Total.Fleet.Vehicles)
print(paste0(x-y, " vehicles removed due to inactivity."))

joined_veh_data$Average.Daily.Miles.on.Active.Vehicles.During.Period <- joined_veh_data$Total.Miles.on.Active.Vehicles.During.Period/joined_veh_data$Active.Fleet.Vehicles/joined_veh_data$Days.of.Service.Operated
joined_veh_data <- arrange(joined_veh_data, Average.Daily.Miles.on.Active.Vehicles.During.Period)
joined_veh_data <- joined_veh_data[!is.na(joined_veh_data$Average.Daily.Miles.on.Active.Vehicles.During.Period),]
summary(joined_veh_data$Average.Daily.Miles.on.Active.Vehicles.During.Period)

conv_fta <- filter(joined_veh_data, 
                   joined_veh_data$Fuel.Type != "Electric Battery" &
                     Fuel.Type!= "Hydrogen")

#### FIGURE: Range of conventional buses ####

data_count <- as.data.frame(lapply(conv_fta,rep,conv_fta$Active.Fleet.Vehicles))

nbins <- nclass.FD(data_count$Average.Daily.Miles.on.Active.Vehicles.During.Period)
conv_range <- ggplot(data_count, aes(Average.Daily.Miles.on.Active.Vehicles.During.Period)) + 
  geom_histogram(bins = nbins, fill = "grey16") + theme_classic() + 
  labs(x = "Average Daily Miles on Active Vehicles in 2021", 
       y = str_wrap("Number of Conventional U.S. Transit Buses in Service", width = 30)) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 20), hjust = 0.5, vjust = -0.5)) +
  scale_x_continuous(breaks = seq(0, 250, 50)) +
  scale_y_continuous(breaks = seq(0, 2000, 500),
                     limits = c(0, 2000),
                     labels = function(x) format(x, big.mark = ","))

conv_range

# ggsave("Graphs/average_daily_miles.png",
#        plot = conv_range,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

#### SI FIGURES: Daily Mileage range for big three ####
data_nyc <- data_count[data_count$Agency.Name.x == "MTA New York City Transit",]
data_chicago <- data_count[data_count$Agency.Name.x == "Chicago Transit Authority",]
data_la <- data_count[data_count$Agency.Name.x == "Los Angeles County Metropolitan Transportation Authority ",]

nbins <- nclass.FD(data_nyc$Average.Daily.Miles.on.Active.Vehicles.During.Period)
conv_nyc <- ggplot(data_nyc, aes(Average.Daily.Miles.on.Active.Vehicles.During.Period)) + 
  geom_histogram(bins = nbins, fill = "grey16") + theme_classic() + 
  labs(x = "Average Daily Miles on Active Vehicles in 2021", 
       y = str_wrap("Number of Conventional Transit Buses in Service for MTA New York City Transit", width = 30)) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 20), hjust = 0.5, vjust = -0.5)) 
  # scale_x_continuous(breaks = seq(0, 25, 5),
  #                    limits = c(5,25)) +
  # scale_y_continuous(breaks = seq(0, 20, 5),
  #                    limits = c(0, 20),
  #                    labels = function(x) format(x, big.mark = ","))

conv_nyc

# ggsave("Graphs/daily_miles_nyc.png",
#        plot = conv_nyc,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

nbins <- nclass.FD(data_chicago$Average.Daily.Miles.on.Active.Vehicles.During.Period)
conv_chicago <- ggplot(data_chicago, aes(Average.Daily.Miles.on.Active.Vehicles.During.Period)) + 
  geom_histogram(bins = nbins, fill = "grey16") + theme_classic() + 
  labs(x = "Average Daily Miles on Active Vehicles in 2021", 
       y = str_wrap("Number of Conventional Transit Buses in Service for Chicago Transit Authority", width = 30)) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 20), hjust = 0.5, vjust = -0.5))  
  # scale_y_continuous(breaks = seq(0, 10,5),
  #                    limits = c(0, 10))

conv_chicago

# ggsave("Graphs/daily_miles_chicago.png",
#        plot = conv_chicago,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

nbins <- nclass.FD(data_la$Average.Daily.Miles.on.Active.Vehicles.During.Period)
conv_la <- ggplot(data_la, aes(Average.Daily.Miles.on.Active.Vehicles.During.Period)) + 
  geom_histogram(bins = nbins, fill = "grey16") + theme_classic() + 
  labs(x = "Average Daily Miles on Active Vehicles in 2021", 
       y = str_wrap("Number of Conventional Transit Buses in Service for Los Angeles Country MTA", width = 30)) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 20), hjust = 0.5, vjust = -0.5)) 
  # scale_x_continuous(breaks = seq(0, 250, 50)) +
  # scale_y_continuous(breaks = seq(0, 2000, 500),
  #                    limits = c(0, 2000),
  #                    labels = function(x) format(x, big.mark = ","))

conv_la

# ggsave("Graphs/daily_miles_la.png",
#        plot = conv_la,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

conv_fta[conv_fta$Average.Daily.Miles.on.Active.Vehicles.During.Period <= 100,] %>% nrow()/nrow(conv_fta)
# 2019: 55% of the fleet has a daily range smaller than 100 mi.
# 2021: 62% of the fleet has a daily range smaller than 100 mi.
conv_fta[conv_fta$Average.Daily.Miles.on.Active.Vehicles.During.Period <= 200,] %>% nrow()/nrow(conv_fta)
# 2019: Almost 99% of the fleet has a daily range smaller than 200 miles
# 2021: 98.8% of the fleet has a daily range smaller than 200 miles

conv_fta$Average.Daily.Miles.on.Active.Vehicles.During.Period %>% mean()
# 2021: Average daily driving range of 85 mi

#comparing the current age of vehicles to expected lifespans
data_count$Vehicle.Age <- 2021 - data_count$Manufacture.Year
conv_fta$Vehicle.Age <- 2021 - conv_fta$Manufacture.Year
data_count <- data_count[data_count$Useful.Life.Benchmark>0,]
data_count$Life.Left <- data_count$Useful.Life.Benchmark-data_count$Vehicle.Age
data_count$life.past <- - data_count$Life.Left
summary(data_count$Life.Left)
summary(data_count$Useful.Life.Benchmark)
data_count[data_count$Useful.Life.Benchmark==25,] %>% unique()
# The buses with a ULB of 25 were reconditioned in past years

#### FIGURE: Buses past useful life benchmark ####
nbins <- nclass.FD(data_count$Life.Left)
colors.ulb <- c(rep("grey16", 34), rep("firebrick", 33))
annotation <- data.frame(
  x = 10,
  y = 3000,
  label = "Buses Eligible for Replacement"
)
past_ulb <- ggplot(data = data_count, aes(life.past)) + 
  geom_histogram(binwidth = 0.5, fill = colors.ulb) + 
  theme_classic() + 
  labs(x = "Years Past Replacement Eligibility", y = str_wrap("Number of Conventional U.S. Transit Buses in Service in 2021", width = 30)) + 
  coord_cartesian(xlim = c(-20, 20),
                  ylim = c(0, 4000)) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 15), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 15), vjust = 2.5, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 4000, 500),
                     labels = function(x) format(x, big.mark = ","),
                     limits = c(0,4000)) +
  scale_x_continuous(breaks = seq(-20, 20, 5)) +
  geom_label(data = annotation, 
             aes(x = x, y = y, label = label),
             color = "firebrick",
             fontface = "bold" )

past_ulb

# ggsave("Graphs/years_past_life.png",
#        plot = past_ulb,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

#### FIGURE: Range of BEBs ####
eb_range <- filter(joined_veh_data, 
                   joined_veh_data$Fuel.Type == "Electric Battery")

data_count <- as.data.frame(lapply(eb_range,rep,eb_range$Active.Fleet.Vehicles))
#data_count <- data_count[data_count$Average.Daily.Miles.on.Active.Vehicles.During.Period < 250,]

nbins <- nclass.FD(data_count$Average.Daily.Miles.on.Active.Vehicles.During.Period)
annotation <- data.frame(x = c(2, 35, 67),
                         y = c(115, 140, 120), 
                         label = c(str_wrap("25th Percentile", width = 10), "Mean", "75th Percentile"))
current_eb <- ggplot(data = data_count, aes(Average.Daily.Miles.on.Active.Vehicles.During.Period)) + 
  geom_histogram(fill = "grey16", color = "white", bins = nbins) + 
  theme_classic() + 
  labs(x = "Average Daily Miles on Active Battery Electric Buses in 2021", y = str_wrap("Number of Active U.S. Battery Electric Transit Buses", width = 30)) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 20), vjust = -0.5)) +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  scale_x_continuous(breaks = seq(0, 200, 20)) +
  geom_vline(aes(xintercept = mean(Average.Daily.Miles.on.Active.Vehicles.During.Period, na.rm = TRUE)),
             color = "#009E73",
             linetype = "dashed") +
  geom_vline(aes(xintercept = summary(Average.Daily.Miles.on.Active.Vehicles.During.Period)[5]), 
             color = "#E69F00",
             linetype = "dashed") +
  geom_vline(aes(xintercept = summary(Average.Daily.Miles.on.Active.Vehicles.During.Period)[2]), 
             color = "#CC79A7",
             linetype = "dashed") +
  geom_label(data = annotation, aes(x = x, y = y, label = label), color = c("#CC79A7", "#009E73", "#E69F00"))

current_eb

eb_range[eb_range$Average.Daily.Miles.on.Active.Vehicles.During.Period >100,] %>% view()
data_count$Average.Daily.Miles.on.Active.Vehicles.During.Period %>% summary()

sum(eb_range$Active.Fleet.Vehicles)

# ggsave("Graphs/average_daily_miles_eb.png",
#        plot = current_eb,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

#### Fuel Data Cleaning ####
summary(raw_energy_2)
#no lpg data#
mb_energy <- raw_energy_2[raw_energy_2$Mode == "MB",]
#EB mi/kwh outliers, no lpg data, gasoline mpg, cng mpg, and diesel mpg outliers

#next 2 lines changes columns of interest from character to numeric types
index <- c(12,14,16,18,20,22,24,26,28,31,33,35,37,39,41,43,46,48,50,52,54,56,58)
mb_energy <- dollarsign_to_numeric(mb_energy, index)

# Data has some fields for questionable data
# Next loop removes these fields
unq_energy <- mb_energy[,1:12]
for (j in 13:ncol(mb_energy)) {
  x <- names(mb_energy[j])
  t <- str_detect(x, "uestionable")
  if(t == FALSE){
    unq_energy <- cbind.data.frame(unq_energy, mb_energy[,j])
    col_num <- ncol(unq_energy)
    names(unq_energy)[col_num] <- x
  }
}
rm(index, j, t, x)

# Last 4 columns of unq_energy are empty
unq_energy <- unq_energy[,1:37]

# remove NAs from numeric fields of data
temp <- unq_energy[,31:37]
temp[is.na(temp)] <- 0
unq_energy <- cbind.data.frame(unq_energy[,1:30],temp)
rm(temp)


#Replace mpg of diesel > 30, gasoline > 30, cng > 15, and electric battery>20
unq_mpg_index <- c(31:34,37)
unq_mi_index <- c(23:26,29)
unq_gal_index <- c(14:17,21)

# Recalculate mpg fields just in case since we are given mileage and fuel gallon data
for (j in 1:length(unq_mpg_index)) {
  for (i in 1:length(unq_energy)) {
    if (unq_energy[i,unq_gal_index[j]]>0){
      unq_energy[i,unq_mpg_index[j]] <- unq_energy[i,unq_mi_index[j]]/unq_energy[i,unq_gal_index[j]]
    }else{unq_energy[i,unq_mpg_index[j]] <- 0}
  }
}

summary(unq_energy[,c(31:34, 37)])
# Uncomment below if Some data is INF, replace with zero
# index <- c(31:34, 37)
# temp <- data.frame()
# for (i in index) {
#   temp <- unq_energy[, i]
#   temp[which(is.infinite(temp))] <- 0
#   unq_energy[, i] <- temp
# }

#### Identify and replace outliers in mpg for each fuel type ####

# Diesel
temp <- unq_energy[unq_energy$Diesel..mpg.>0.001,31]
hist(temp)
# Mean diesel mpg with outliers
mean(temp, na.rm = T)
temp <- temp[temp<15]
diesel.mpg.replacement <- mean(temp)
unq_energy[unq_energy$Diesel..mpg.>15,31] <- diesel.mpg.replacement
unq_energy[unq_energy$Diesel..mpg. < 0.001, 31] <- diesel.mpg.replacement
unq_energy[is.na(unq_energy$Diesel..mpg.),31] <- diesel.mpg.replacement
# Mean diesel mpg without outliers
mean(unq_energy$Diesel..mpg., na.rm = TRUE)

# Gasoline
temp <- unq_energy[unq_energy$Gasoline..mpg.>0.001,32]
hist(temp)
# Mean gas mpg with outliers
mean(temp, na.rm = T)
temp <- temp[temp<20]
gas.mpg.replacement <- mean(temp)
unq_energy[unq_energy$Gasoline..mpg.>20,32] <- gas.mpg.replacement
unq_energy[unq_energy$Gasoline..mpg. < 0.001, 32] <- gas.mpg.replacement
unq_energy[is.na(unq_energy$Gasoline..mpg.),32] <- gas.mpg.replacement
# Mean gas mpg without outliers
mean(unq_energy$Gasoline..mpg., na.rm = T)

# CNG
temp <- unq_energy[unq_energy$Compressed.Natural.Gas..mpg.>0.001,34]
# Mean CNG mpg (diesel equivalent assumed) with outliers
mean(temp, na.rm = T)
temp <- temp[temp < 15]
cng.replacement <- mean(temp)
unq_energy[unq_energy$Compressed.Natural.Gas..mpg.>15,34] <- cng.replacement
unq_energy[unq_energy$Compressed.Natural.Gas..mpg. < 0.001, 34] <- cng.replacement
unq_energy[is.na(unq_energy$Compressed.Natural.Gas..mpg.),34] <- cng.replacement
# Mean cng mpg without outliers
mean(unq_energy$Compressed.Natural.Gas..mpg., na.rm = T)

# Electric Battery
temp <- unq_energy[unq_energy$Electric.Battery..mi.kwh.>0.001,37]
# Mean electric mi/kWh with outliers
mean(temp, na.rm = T)
temp <- temp[temp<20]
elec.mpkwh.replacement <- mean(temp)
unq_energy[unq_energy$Electric.Battery..mi.kwh.>20,37] <- elec.mpkwh.replacement
unq_energy[unq_energy$Electric.Battery..mi.kwh. < 0.001, 37] <- elec.mpkwh.replacement
unq_energy[is.na(unq_energy$Electric.Battery..mi.kwh.),37] <- elec.mpkwh.replacement
# Mean electric mi/kWh without outliers
mean(unq_energy$Electric.Battery..mi.kwh., na.rm = T)

#### Bus data for model ####

joined_veh_data$Final.Year <- joined_veh_data$Manufacture.Year + joined_veh_data$Useful.Life.Benchmark

# accounting for rebuild year
joined_veh_data[which(joined_veh_data$Rebuild.Year>0),"Final.Year"] <- joined_veh_data[which(joined_veh_data$Rebuild.Year>0),"Rebuild.Year"] + 
  joined_veh_data[which(joined_veh_data$Rebuild.Year>0),"Useful.Life.Benchmark"]

joined_veh_data$Life.Left <- joined_veh_data$Final.Year-2021
x <- nrow(joined_veh_data[joined_veh_data$Life.Left<=0,])
y <- nrow(joined_veh_data)
print(paste0(round(x/y*100, 2), " % of the fleet is eligible for replacement"))

colnames(joined_veh_data)

fta_2021 <- joined_veh_data[,c(1,2,5,9,14,15,17,21,24:29,52:57,80,86,100:104)]

fta_2021$Fuel.Type <- fta_2021$Fuel.Type %>% as.factor()

fta_2021$Fuel.Type %>% summary()
levels(fta_2021$Fuel.Type)[levels(fta_2021$Fuel.Type)=="Gasoline/Compressed Natural Gas"] <- "Compressed Natural Gas"
levels(fta_2021$Fuel.Type)[levels(fta_2021$Fuel.Type)=="Diesel Fuel"] <- "Diesel"
#fta_2021 %>% View()

#### Join Bus and MPG data for model ####
unq_energy[unq_energy$TOS == "0", "TOS"] <- "DO"
unq_energy$Modes <- paste0(unq_energy$Mode, "/",str_trim(unq_energy$TOS, side = "both"))
mpg_data <- unq_energy[,c(5,1,38,31,32,34,37)]
summary(mpg_data)

mpg_data$NTD.ID <- as.character(mpg_data$NTD.ID)
mpg_data$NTD.ID <- str_pad(mpg_data$NTD.ID, width = 5, side = "left", pad = "0")
mpg_data <- unique(mpg_data)
bus_mileage <- left_join(fta_2021, mpg_data, by = c("NTD.ID", "Modes"))
summary(bus_mileage)

# Fill in all NA's to assure fuel types have corresponding economies
bus_mileage[is.na(bus_mileage$Diesel..mpg.), "Diesel..mpg."] <- diesel.mpg.replacement
bus_mileage[is.na(bus_mileage$Gasoline..mpg.), "Gasoline..mpg."] <- gas.mpg.replacement
bus_mileage[is.na(bus_mileage$Compressed.Natural.Gas..mpg.), "Compressed.Natural.Gas..mpg."] <- cng.replacement
bus_mileage[is.na(bus_mileage$Electric.Battery..mi.kwh.), "Electric.Battery..mi.kwh."] <- elec.mpkwh.replacement

# Consolidate economies to one field
bus_mileage[bus_mileage$Fuel.Type == "Hybrid Diesel", "Fuel.Type"] <- "Diesel"
bus_mileage[bus_mileage$Fuel.Type == "Hybrid Gasoline", "Fuel.Type"] <- "Gasoline"
bus_mileage[bus_mileage$Fuel.Type != "Diesel", "Diesel..mpg."] <- NA
bus_mileage[bus_mileage$Fuel.Type != "Gasoline", "Gasoline..mpg."] <- NA
bus_mileage[bus_mileage$Fuel.Type != "Compressed Natural Gas", "Compressed.Natural.Gas..mpg."] <- NA
bus_mileage[bus_mileage$Fuel.Type != "Electric Battery", "Electric.Battery..mi.kwh."] <- NA

bus_mileage$bus.mpg <- rowSums(bus_mileage[, c("Diesel..mpg.", 
                                               "Gasoline..mpg.",
                                               "Compressed.Natural.Gas..mpg.", 
                                               "Electric.Battery..mi.kwh.")], na.rm = TRUE)

#### EGRID data: Convert lb/MWh to g/kWh ####

egrid_names <- colnames(egrid)[2:13]
str_sub(egrid_names, -7, -1) <- "g.kWh"

egrid <- dollarsign_to_numeric(egrid, c(4,7,10,13))
egrid[,2:13] <- egrid[,2:13] * 453.592 / 1000
colnames(egrid)[2:13] <- egrid_names

#### Battery price forecast ####

# all battery prices in $/kWh
# Method: Using Argonne National Lab BEAN Model
bean_batt <- data.frame(year = c(2021, 2027, 2035),
                        tech = c(rep("Pessimistic", 3), rep("Medium", 3), rep("Optimistic", 3)),
                        liion.price = c(302, 225, 150, 302, 175, 115, 302, 175, 80))

# Plot the battery price trajectories
bean_pred <- data.frame(year = 2023:2040)
b.value <- 0.858
bean_pred$Medium <- (-127/(b.value^2027 - b.value^2021))*(b.value^bean_pred$year - b.value^2021) + 302
b.pes <- 0.956
bean_pred$Pessimistic <- (-77/(b.pes^2027 - b.pes^2021))*(b.pes^bean_pred$year - b.pes^2021) + 302
b.opt <- 0.920
bean_pred$Optimistic <- (-127/(b.opt^2027 - b.opt^2021))*(b.opt^bean_pred$year - b.opt^2021) + 302

bean_melt <- melt(bean_pred, id.vars = "year")

#### FIGURE: Battery Forecast ####

batt_forcast <- ggplot(data = bean_melt, aes(x = year, y = value)) +
  geom_line(aes(color = variable)) +
  theme_classic() +
  labs(x = "Year", y = str_wrap("Estimated Price of Li-ion High Energy Battery ($/kWh)", width = 40)) +
  scale_x_continuous(breaks = 2023:2040) +
  scale_y_continuous(breaks = seq(0, 300, 25), limits = c(0, 302)) +
  scale_color_discrete(name = str_wrap("Technology Progress:", width = 10)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))

batt_forcast

# ggsave("Graphs/batt_forcast.png",
#        plot = batt_forcast,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

#### APTA BEB Prices ####
fleet <- read.csv("APTA Vehicle Database/fleet.csv",
                  na.strings = c("", "NA"),
                  stringsAsFactors = FALSE)

beb_fleet <- fleet %>% filter(Mode.Code == "MB",
                              Year.Built == 2021,
                              Power.Type.Code == "EB",
                              !is.na(Amount.Paid.per.Vehicle))

beb_fleet$Amount.Paid.per.Vehicle <-  as.numeric(gsub("\\$", "", as.factor(gsub(",", "", beb_fleet$Amount.Paid.per.Vehicle))))

beb.mean <- sum(beb_fleet$Amount.Paid.per.Vehicle * beb_fleet$Total.Vehicles..Number.of) / sum(beb_fleet$Total.Vehicles..Number.of)
beb.min <- min(beb_fleet$Amount.Paid.per.Vehicle)
beb.max <- max(beb_fleet$Amount.Paid.per.Vehicle)

beb_prices <- data.frame(Mean = beb.mean,
                         Max = beb.max,
                         Min = beb.min)

# 2021 mean value of approx $745,000
# Range from $618,000 to $952,000

#### Replacement, parameters ####
batt_specs$batt_mean <- (batt_specs$batt_high + batt_specs$batt_low)/2

# Establish an average price for the bus frames
beb_frame <- beb_prices$Mean - (batt_specs$batt_mean %>% mean())*0.7 * bean_pred[bean_pred$year == 2023, "Medium"]

# Electricity efficiency values
elec_transm_dist_eff <- 0.95
plug_eff <- 0.85 

# Add GHG and Air emission data
ch4_gwp <- 30
n2o_gwp <- 265

#### WTW Emissions ####
# Calculate CO2e for well to tank emissions
welltotank$CO2e..g.gal <- welltotank$CO2..g.gal + ch4_gwp * welltotank$CH4..g.gal + n2o_gwp * welltotank$N2O..g.gal
# Only want running, tire wear, and break wear emissions
emfac <- emfac[,c("Model.Year", "Fuel", colnames(emfac)[str_detect(colnames(emfac), "RUNEX|PMBW|PMTW")])]
# Calculate CO2e for tank to wheel emissions
emfac$CO2e_RUNEX <- emfac$CO2_RUNEX + ch4_gwp * emfac$CH4_RUNEX + n2o_gwp * emfac$N2O_RUNEX
# "ROG" is "VOC"
colnames(emfac)[colnames(emfac) == "ROG_RUNEX"] <- "VOC_RUNEX"

# Fuel consumption given in 1,000 gallons/year
emfac_eco <- emfac_eco[,c("Fuel", "Total.VMT", "Fuel.Consumption")]
emfac_eco$mpg <- emfac_eco$Total.VMT / (emfac_eco$Fuel.Consumption * 1000)

# Rename emfac factors to match data fuel type factors
emfac[emfac$Fuel == "Electricity","Fuel"] <- "Electric Battery"
emfac[emfac$Fuel == "Natural Gas","Fuel"] <- "Compressed Natural Gas"

# Adjust emfac to g/gal
# Do not adjust this for PMBW & PMTW
emfac[emfac$Fuel == "Gasoline", c(3,4,7,10:18)] <- emfac[emfac$Fuel == "Gasoline", c(3,4,7,10:18)] * emfac_eco[emfac_eco$Fuel == "Gasoline", "mpg"]
emfac[emfac$Fuel == "Diesel", c(3,4,7,10:18)] <- emfac[emfac$Fuel == "Diesel", c(3,4,7,10:18)] * emfac_eco[emfac_eco$Fuel == "Diesel", "mpg"]
emfac[emfac$Fuel == "Compressed Natural Gas", c(3,4,7,10:18)] <- emfac[emfac$Fuel == "Compressed Natural Gas", c(3,4,7,10:18)] * emfac_eco[emfac_eco$Fuel == "Natural Gas", "mpg"]
colnames(emfac)[c(3,4,7,10:18)] <- paste0(colnames(emfac)[c(3,4,7,10:18)], "..g.gal")
colnames(emfac)[c(5,6,8,9)] <- paste0(colnames(emfac)[c(5,6,8,9)], "..g.mi")

# Generate rows summarizing each fuel type/model year we have in both data frames
emfac_spread <- emfac %>% group_by(Fuel, Model.Year) %>% summarise(Fuel, Model.Year) %>% unique()
data_spread <- bus_mileage %>% group_by(Fuel.Type, Manufacture.Year) %>% summarise(Fuel.Type, Manufacture.Year) %>% unique()
# Compare the two to see what emfac data we are missing
missing.emfac <- anti_join(data_spread, emfac_spread, by=c("Fuel.Type" = "Fuel", "Manufacture.Year"="Model.Year"))
cng.emfac.replace <- emfac[emfac$Model.Year == 2009 & emfac$Fuel == "Compressed Natural Gas",]
cng.missing <- missing.emfac[missing.emfac$Fuel.Type == "Compressed Natural Gas", "Manufacture.Year"] %>% unlist() %>% as.numeric()
for (i in cng.missing){
  cng.emfac.replace$Model.Year <- i
  emfac <- rbind.data.frame(emfac, cng.emfac.replace)
}
diesel.emfac.replace <- emfac[emfac$Model.Year == 2009 & emfac$Fuel == "Diesel",]
diesel.missing <- missing.emfac[missing.emfac$Fuel.Type == "Diesel", "Manufacture.Year"] %>% unlist() %>% as.numeric()
for (i in diesel.missing){
  diesel.emfac.replace$Model.Year <- i
  emfac <- rbind.data.frame(emfac, diesel.emfac.replace)
}
eb.emfac.replace <- emfac[emfac$Model.Year == 2009 & emfac$Fuel == "Electric Battery",]
eb.missing <- missing.emfac[missing.emfac$Fuel.Type == "Electric Battery", "Manufacture.Year"] %>% unlist() %>% as.numeric()
for (i in eb.missing[eb.missing < 2009]){
  eb.emfac.replace$Model.Year <- i
  emfac <- rbind.data.frame(emfac, eb.emfac.replace)
}
eb.emfac.replace <- emfac[emfac$Model.Year == 2019 & emfac$Fuel == "Electric Battery",]
for (i in eb.missing[eb.missing > 2019]){
  eb.emfac.replace$Model.Year <- i
  emfac <- rbind.data.frame(emfac, eb.emfac.replace)
}
gas.emfac.replace <- emfac[emfac$Model.Year == 2009 & emfac$Fuel == "Gasoline",]
gas.missing <- missing.emfac[missing.emfac$Fuel.Type == "Gasoline", "Manufacture.Year"] %>% unlist() %>% as.numeric()
for (i in gas.missing){
  gas.emfac.replace$Model.Year <- i
  emfac <- rbind.data.frame(emfac, gas.emfac.replace)
}

#### Decreasing eGRID emissions ####
egrid_simp <- egrid[, c(1, 8, 9, 13)]

years <- seq(2023, 2035, 1)
minus_matrix <- egrid_simp[, 2:4]/14

for (i in 1:length(years)) {
  
  a <- egrid_simp
  a$year <- years[i]
  
  if (i == 1) {
    
    decrease_egrid <- a
    
    egrid_running_total <- decrease_egrid %>%  group_by(eGRID.subregion.acronym) %>% 
      summarise(eGRID.subregion.annual.NOx.non.baseload.output.emission.rate..g.kWh = sum(eGRID.subregion.annual.NOx.non.baseload.output.emission.rate..g.kWh),
                eGRID.subregion.annual.SO2.non.baseload.output.emission.rate..g.kWh = sum(eGRID.subregion.annual.SO2.non.baseload.output.emission.rate..g.kWh),
                eGRID.subregion.annual.CO2e.non.baseload.output.emission.rate..g.kWh = sum(eGRID.subregion.annual.CO2e.non.baseload.output.emission.rate..g.kWh))
    
    egrid_running_total$year.start <- years[i]
    
  } else {
    
    a[, 2:4] <- egrid_simp[, 2:4] - (i - 1) * minus_matrix
    
    decrease_egrid <- rbind.data.frame(decrease_egrid, a)
    
    b <- decrease_egrid %>%  group_by(eGRID.subregion.acronym) %>% 
      summarise(eGRID.subregion.annual.NOx.non.baseload.output.emission.rate..g.kWh = sum(eGRID.subregion.annual.NOx.non.baseload.output.emission.rate..g.kWh),
                eGRID.subregion.annual.SO2.non.baseload.output.emission.rate..g.kWh = sum(eGRID.subregion.annual.SO2.non.baseload.output.emission.rate..g.kWh),
                eGRID.subregion.annual.CO2e.non.baseload.output.emission.rate..g.kWh = sum(eGRID.subregion.annual.CO2e.non.baseload.output.emission.rate..g.kWh))
    
    b$year.start <- years[i] 
    
    egrid_running_total <- rbind.data.frame(egrid_running_total, b)
    
    
  }
  
}

total_decrease_egrid <- decrease_egrid %>%  group_by(eGRID.subregion.acronym) %>% 
  summarise(eGRID.subregion.annual.NOx.non.baseload.output.emission.rate..g.kWh = sum(eGRID.subregion.annual.NOx.non.baseload.output.emission.rate..g.kWh),
            eGRID.subregion.annual.SO2.non.baseload.output.emission.rate..g.kWh = sum(eGRID.subregion.annual.SO2.non.baseload.output.emission.rate..g.kWh),
            eGRID.subregion.annual.CO2e.non.baseload.output.emission.rate..g.kWh = sum(eGRID.subregion.annual.CO2e.non.baseload.output.emission.rate..g.kWh))

## eGRID EMISSIONS ARE SUMMED FROM 2023 UNTIL 2035

egrid_emissions <- left_join(total_decrease_egrid, egrid_zip[,c(2,4)], 
                             by = c ("eGRID.subregion.acronym" = "eGRID.Subregion..1"))


for (i in unique(egrid_running_total$year.start)) {
  
  egrid_running_total[egrid_running_total$year.start == i, 2:4] <- total_decrease_egrid[, 2:4] - 
    egrid_running_total[egrid_running_total$year.start == i, 2:4]
  
}

vary_deg <- left_join(egrid_running_total, egrid_zip[,c(2,4)], 
                      by = c ("eGRID.subregion.acronym" = "eGRID.Subregion..1"))

plot_data <- filter(egrid_running_total, eGRID.subregion.acronym == "SRVC")
ggplot(data = plot_data, aes(x = year.start, y = eGRID.subregion.annual.CO2e.non.baseload.output.emission.rate..g.kWh)) +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(breaks = 2023:2035) +
  labs(x = "Starting Year", y = str_wrap("CO2e emissions accumulated from electricity generation between start date to 2035 (grams/kWh)", width = 50))

#### Battery Lifecycle Emissions ####
batt_lc_emissions$Total.grams.per.lifetime.per.kWh <- batt_lc_emissions$Total.grams.per.lifetime.per.kWh %>% str_trim(side = "both")

#### Range of available BEB data ####
# DOD = Depth of Discharge
DOD <- 0.7
batt_specs$batt_low <- batt_specs$batt_low * DOD
batt_specs$batt_high <- batt_specs$batt_high * DOD
batt_specs$range_low <- round(batt_specs$batt_low / batt_specs$consumption_high, 0)
batt_specs$range_high <- round(batt_specs$batt_high / batt_specs$consumption_low, 0)

# Assign conventional vehicle replacement model here
bus_base <- bus_mileage
bus_mileage[bus_mileage$Fuel.Type != "Electric Battery", "make_model"] <- "Proterra Inc. Catalyst Xr"
bus_mileage[bus_mileage$Fuel.Type != "Electric Battery" & bus_mileage$Average.Daily.Miles.on.Active.Vehicles.During.Period > 155, "make_model"] <- "Proterra Inc. Zx5"
bus_mileage[bus_mileage$Fuel.Type != "Electric Battery" & bus_mileage$Average.Daily.Miles.on.Active.Vehicles.During.Period > 300, "Active.Fleet.Vehicles"] <- bus_mileage[bus_mileage$Fuel.Type != "Electric Battery" & bus_mileage$Average.Daily.Miles.on.Active.Vehicles.During.Period > 300, "Active.Fleet.Vehicles"] * 2
bus_mileage[bus_mileage$Fuel.Type != "Electric Battery" & bus_mileage$Average.Daily.Miles.on.Active.Vehicles.During.Period > 300, "Total.Fleet.Vehicles"] <- bus_mileage[bus_mileage$Fuel.Type != "Electric Battery" & bus_mileage$Average.Daily.Miles.on.Active.Vehicles.During.Period > 300, "Total.Fleet.Vehicles"] * 2
x1 <- bus_mileage$Active.Fleet.Vehicles %>% sum()
x2 <- bus_mileage$Total.Fleet.Vehicles %>% sum()
bus_mileage[bus_mileage$Fuel.Type != "Electric Battery" & bus_mileage$Seating.Capacity > 60, "Active.Fleet.Vehicles"] <- bus_mileage[bus_mileage$Fuel.Type != "Electric Battery" & bus_mileage$Seating.Capacity > 60, "Active.Fleet.Vehicles"] * 2
bus_mileage[bus_mileage$Fuel.Type != "Electric Battery" & bus_mileage$Seating.Capacity > 60, "Total.Fleet.Vehicles"] <- bus_mileage[bus_mileage$Fuel.Type != "Electric Battery" & bus_mileage$Seating.Capacity > 60, "Total.Fleet.Vehicles"] * 2
y1 <- bus_mileage$Active.Fleet.Vehicles %>% sum()
y2 <- bus_mileage$Total.Fleet.Vehicles %>% sum()

#### Emissions Model ####
elec_transm_dist_eff <- 0.95
plug_eff <- 0.85 

source("base_function.R")
source("scen1_function.R")
source("scen2_function.R")
source("scen3_function.R")
source("scen3_dirty_elec.R")
source("scen4_function.R")

scen_0 <- base_emissions_model(batt_replace = 1)
scen_1 <- scen1_emissions_model(batt_replace = 1)
scen_2 <- scen2_emissions_model(batt_replace = 1)
scen_3 <- scen3_emissions_model(batt_replace = 1)
scen_3_dirty <- scen3_dirty_model(batt_replace = 1)
scen_4 <- scen4_emissions_model(batt_replace = 1)

scen_0$Scenario <- 0
scen_1$Scenario <- 1
scen_2$Scenario <- 2
scen_3$Scenario <- 3
scen_3_dirty$Scenario <- 3.5
scen_4$Scenario <- 4

scen_0$cFuel.Type <- scen_0$Fuel.Type

total_scen <- rbind.data.frame(scen_0, scen_1)
total_scen$Final.Year <- 2022
total_scen <- rbind.data.frame(total_scen, scen_2)
total_scen <- rbind.data.frame(total_scen, scen_3)
total_scen <- rbind.data.frame(total_scen, scen_3_dirty)
total_scen <- rbind.data.frame(total_scen, scen_4[, !names(scen_4) %in% "top"])

# Melt the dataframe
total_scen_melt <- melt(total_scen, value.name = "Emissions in mt", id.vars = c("NTD.ID",                                       
                                                                                "Agency.Name.x",
                                                                                "Modes",                                        
                                                                                "Fuel.Type",
                                                                                "cFuel.Type",
                                                                                "Address.Line.1",
                                                                                "City",                                        
                                                                                "State",
                                                                                "Zip.Code",
                                                                                "batt_size",
                                                                                "Scenario",
                                                                                "Total.Fleet.Vehicles",
                                                                                "Active.Fleet.Vehicles",                        
                                                                                "Total.Miles.on.Active.Vehicles.During.Period",
                                                                                "Final.Year"))

total_scen_melt$Emission.Type <- str_extract(total_scen_melt$variable, "VOC|CO2e|NOx|PM10|PM2.5|SOx|BC|OC|CO")
total_scen_melt$Source <- str_extract(total_scen_melt$variable, "batt|fuel")

total_plot <- total_scen_melt %>% group_by(Scenario,
                                           Emission.Type) %>%
  summarise(Emissions.in.mt = sum(`Emissions in mt`, na.rm = TRUE))

total_plot$Emissions.in.mmt <- total_plot$Emissions.in.mt/1000000

total_plot <- total_plot[total_plot$Scenario != 3.5,]

# Low Estimate
scen_0_low <- base_emissions_model(assumed_bus_life = 14, 
                               elec_eff_total = (elec_transm_dist_eff * plug_eff),
                               consumption_proportion = 0,
                               batt_replace = 0,
                               largest_pack = FALSE)
scen_1_low <- scen1_emissions_model(assumed_bus_life = 14, 
                                elec_eff_total = (elec_transm_dist_eff * plug_eff),
                                consumption_proportion = 0,
                                batt_replace = 0,
                                largest_pack = FALSE)
scen_2_low <- scen2_emissions_model(assumed_bus_life = 14, 
                                elec_eff_total = (elec_transm_dist_eff * plug_eff),
                                consumption_proportion = 0,
                                batt_replace = 0,
                                largest_pack = FALSE)
scen_3_low <- scen3_emissions_model(assumed_bus_life = 14, 
                                elec_eff_total = (elec_transm_dist_eff * plug_eff),
                                consumption_proportion = 0,
                                batt_replace = 0,
                                largest_pack = FALSE)
scen_4_low <- scen4_emissions_model(assumed_bus_life = 14, 
                                elec_eff_total = (elec_transm_dist_eff * plug_eff),
                                consumption_proportion = 0,
                                batt_replace = 0,
                                largest_pack = FALSE)

scen_0_low$Scenario <- 0
scen_1_low$Scenario <- 1
scen_2_low$Scenario <- 2
scen_3_low$Scenario <- 3
scen_4_low$Scenario <- 4

scen_0_low$cFuel.Type <- NA

total_low_scen <- rbind.data.frame(scen_0, scen_1_low)
total_low_scen$Final.Year <- 2022
total_low_scen <- rbind.data.frame(total_low_scen, scen_2_low)
total_low_scen <- rbind.data.frame(total_low_scen, scen_3_low)
total_low_scen <- rbind.data.frame(total_low_scen, scen_4_low[, !names(scen_4) %in% "top"])

# Melt the dataframe
total_low_scen_melt <- melt(total_low_scen, value.name = "Emissions in mt", id.vars = c("NTD.ID",                                       
                                                                                "Agency.Name.x",
                                                                                "Modes",                                        
                                                                                "Fuel.Type",
                                                                                "cFuel.Type",
                                                                                "Address.Line.1",
                                                                                "City",                                        
                                                                                "State",
                                                                                "Zip.Code",
                                                                                "batt_size",
                                                                                "Scenario",
                                                                                "Total.Fleet.Vehicles",
                                                                                "Active.Fleet.Vehicles",                        
                                                                                "Total.Miles.on.Active.Vehicles.During.Period",
                                                                                "Final.Year"))

total_low_scen_melt$Emission.Type <- str_extract(total_low_scen_melt$variable, "VOC|CO2e|NOx|PM10|PM2.5|SOx|BC|OC|CO")
total_low_scen_melt$Source <- str_extract(total_low_scen_melt$variable, "batt|fuel")

total_low_plot <- total_low_scen_melt %>% group_by(Scenario,
                                           Emission.Type) %>%
  summarise(Emissions.in.mt.low = sum(`Emissions in mt`, na.rm = TRUE))

total_low_plot$Emissions.in.mmt.low <- total_low_plot$Emissions.in.mt.low/1000000

# High Estimate
scen_0_high <- base_emissions_model(assumed_bus_life = 14, 
                               elec_eff_total = (elec_transm_dist_eff * plug_eff),
                               consumption_proportion = 1,
                               batt_replace = 2,
                               largest_pack = TRUE)
scen_1_high <- scen1_emissions_model(assumed_bus_life = 14, 
                                elec_eff_total = (elec_transm_dist_eff * plug_eff),
                                consumption_proportion = 1,
                                batt_replace = 2,
                                largest_pack = TRUE)
scen_2_high <- scen2_emissions_model(assumed_bus_life = 14, 
                                elec_eff_total = (elec_transm_dist_eff * plug_eff),
                                consumption_proportion = 1,
                                batt_replace = 2,
                                largest_pack = TRUE)
scen_3_high <- scen3_emissions_model(assumed_bus_life = 14, 
                                elec_eff_total = (elec_transm_dist_eff * plug_eff),
                                consumption_proportion = 1,
                                batt_replace = 2,
                                largest_pack = TRUE)
scen_4_high <- scen4_emissions_model(assumed_bus_life = 14, 
                                elec_eff_total = (elec_transm_dist_eff * plug_eff),
                                consumption_proportion = 1,
                                batt_replace = 2,
                                largest_pack = TRUE)

scen_0_high$Scenario <- 0
scen_1_high$Scenario <- 1
scen_2_high$Scenario <- 2
scen_3_high$Scenario <- 3
scen_4_high$Scenario <- 4

scen_0_high$cFuel.Type <- scen_0_high$Fuel.Type

total_high_scen <- rbind.data.frame(scen_0, scen_1_high)
total_high_scen$Final.Year <- 2022
total_high_scen <- rbind.data.frame(total_high_scen, scen_2_high)
total_high_scen <- rbind.data.frame(total_high_scen, scen_3_high)
total_high_scen <- rbind.data.frame(total_high_scen, scen_4_high[, !names(scen_4) %in% "top"])

# Melt the dataframe
total_high_scen_melt <- melt(total_high_scen, value.name = "Emissions in mt", id.vars = c("NTD.ID",                                       
                                                                                        "Agency.Name.x",
                                                                                        "Modes",                                        
                                                                                        "Fuel.Type",
                                                                                        "cFuel.Type",
                                                                                        "Address.Line.1",
                                                                                        "City",                                        
                                                                                        "State",
                                                                                        "Zip.Code",
                                                                                        "batt_size",
                                                                                        "Scenario",
                                                                                        "Total.Fleet.Vehicles",
                                                                                        "Active.Fleet.Vehicles",                        
                                                                                        "Total.Miles.on.Active.Vehicles.During.Period",
                                                                                        "Final.Year"))

total_high_scen_melt$Emission.Type <- str_extract(total_high_scen_melt$variable, "VOC|CO2e|NOx|PM10|PM2.5|SOx|BC|OC|CO")
total_high_scen_melt$Source <- str_extract(total_high_scen_melt$variable, "batt|fuel")

total_high_plot <- total_high_scen_melt %>% group_by(Scenario,
                                                   Emission.Type) %>%
  summarise(Emissions.in.mt.high = sum(`Emissions in mt`, na.rm = TRUE))

total_high_plot$Emissions.in.mmt.high <- total_high_plot$Emissions.in.mt.high/1000000

total_plot <- left_join(total_plot, total_low_plot, by = c("Scenario",
                                                           "Emission.Type"))

total_plot <- left_join(total_plot, total_high_plot, by = c("Scenario",
                                                           "Emission.Type"))

ghg_plot <- total_plot[total_plot$Emission.Type == "CO2e",]
CO_plot <- total_plot[total_plot$Emission.Type == "CO",]
emiss_plot <- total_plot[total_plot$Emission.Type != "CO2e" & 
                           total_plot$Emission.Type!="CO" &
                           total_plot$Emission.Type!="BC" &
                           total_plot$Emission.Type!="OC",]

#### FIGURE: Emissions ####
scen_labels <- c("Base Case (Conventional Buses)", 
                 "Replace All Bus Now", 
                 "Natural Phase Out", 
                 "5% Yearly", 
                 "Top 100") %>% str_wrap(width = 6)

ghgs <- ggplot(data = ghg_plot, aes(x = factor(Scenario), y = Emissions.in.mmt)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "#56B4E9", width = 0.8) +
  geom_errorbar(aes(ymax = Emissions.in.mmt.high, ymin=Emissions.in.mmt.low, width = 0.2),
                position = position_dodge(0.8)) +
  theme_classic() +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"),
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        axis.title.y = element_text(hjust = .5)) +
  labs(x = "Scenario", y = str_wrap("US Transit Bus Fleet GHGs Over 14 Year Period (MMT)", width = 25)) +
  scale_x_discrete(labels = scen_labels) +
  scale_y_continuous(breaks = seq(0,70,10),
                     limits = c(0,70))

ghgs

# ggsave("Graphs/ghg_scenarios.png",
#        plot = ghgs,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")


CO <- ggplot(data = CO_plot, aes(x = factor(Scenario), y=Emissions.in.mmt, fill=Emission.Type)) +
  geom_bar(position = "dodge", stat = "identity", fill = "#E69F00", width = 0.8) +
  geom_errorbar(aes(ymax = Emissions.in.mmt.high, ymin=Emissions.in.mmt.low, width = 0.2),
                position = position_dodge(0.8)) +
  theme_classic() +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"),
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        axis.title.y = element_text(hjust = .5)) +
  labs(x = "Scenario", y = str_wrap("US Transit Bus Fleet CO Pollution Over 14 Year Period (MMT)", width = 35)) +
  scale_x_discrete(labels = scen_labels) +
  scale_y_continuous(breaks = seq(0,0.3,0.05),
                     limits = c(0,0.3))
CO

# ggsave("Graphs/CO_scenarios.png",
#        plot = CO,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

emissions <- ggplot(data = emiss_plot, aes(x = factor(Scenario), y=Emissions.in.mmt, fill=Emission.Type)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.8) +
  geom_errorbar(aes(ymax = Emissions.in.mmt.high, ymin=Emissions.in.mmt.low, width = 0.2),
                position = position_dodge(0.8)) +
  theme_classic() +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"),
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        legend.position = c(0.9, 0.8),
        axis.title.y = element_text(hjust = 0.5)) +
  labs(x = "Scenario", y = str_wrap("US Transit Bus Fleet Air Pollutants Over 14 Year Period (MMT)", width = 35)) +
  scale_fill_manual(values = c("NOx" = "#009E73", "PM10" = "#F0E442", "PM2.5" = "#0072B2", "SOx" = "#D55E00", "VOC" = "#CC79A7")) +
  scale_x_discrete(labels = scen_labels) +
  scale_y_continuous(breaks = seq(0,0.06,0.01),
                     limits = c(0,0.06))
emissions

CO + emissions

# ggsave("Graphs/Emissions_scenarios.png",
#        plot = CO + emissions,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

#### Capital Costs ####
# Cost of battery depends on when battery is made
## Average
scen_0_cost <- scen_0 %>% group_by(NTD.ID,
                                   Agency.Name.x,
                                   Modes,
                                   Zip.Code,
                                   batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_1_cost <- scen_1 %>% group_by(NTD.ID,
                                   Agency.Name.x,
                                   Modes,
                                   Zip.Code,
                                   batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_2_cost <- scen_2 %>% group_by(NTD.ID,
                                   Agency.Name.x,
                                   Modes,
                                   Zip.Code,
                                   Final.Year,
                                   batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_3_cost<- scen_3 %>% group_by(NTD.ID,
                                  Agency.Name.x,
                                  Modes,
                                  Zip.Code,
                                  Final.Year,
                                  batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_4_cost <- scen_4 %>% group_by(NTD.ID,
                                   Agency.Name.x,
                                   Modes,
                                   Zip.Code,
                                   Final.Year,
                                   batt_size,
                                   top) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_4_cost <- scen_4_cost[scen_4_cost$top == "1",]

## Min
scen_0_cost_low <- scen_0_low %>% group_by(NTD.ID,
                                           Agency.Name.x,
                                           Modes,
                                           Zip.Code,
                                           batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_1_cost_low <- scen_1_low %>% group_by(NTD.ID,
                                           Agency.Name.x,
                                           Modes,
                                           Zip.Code,
                                           batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_2_cost_low  <- scen_2_low %>% group_by(NTD.ID,
                                            Agency.Name.x,
                                            Modes,
                                            Zip.Code,
                                            Final.Year,
                                            batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_3_cost_low <- scen_3_low %>% group_by(NTD.ID,
                                           Agency.Name.x,
                                           Modes,
                                           Zip.Code,
                                           Final.Year,
                                           batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_4_cost_low <- scen_4_low %>% group_by(NTD.ID,
                                           Agency.Name.x,
                                           Modes,
                                           Zip.Code,
                                           Final.Year,
                                           batt_size,
                                           top) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_4_cost_low <- scen_4_cost_low[scen_4_cost_low$top == "1",]

## Max
scen_0_cost_high  <- scen_0_high %>% group_by(NTD.ID,
                                              Agency.Name.x,
                                              Modes,
                                              Zip.Code,
                                              batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_1_cost_high<- scen_1_high %>% group_by(NTD.ID,
                                            Agency.Name.x,
                                            Modes,
                                            Zip.Code,
                                            batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_2_cost_high<- scen_2_high %>% group_by(NTD.ID,
                                            Agency.Name.x,
                                            Modes,
                                            Zip.Code,
                                            Final.Year,
                                            batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_3_cost_high<- scen_3_high %>% group_by(NTD.ID,
                                            Agency.Name.x,
                                            Modes,
                                            Zip.Code,
                                            Final.Year,
                                            batt_size) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_4_cost_high<- scen_4_high %>% group_by(NTD.ID,
                                            Agency.Name.x,
                                            Modes,
                                            Zip.Code,
                                            Final.Year,
                                            batt_size,
                                            top) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))

scen_4_cost_high <- scen_4_cost_high[scen_4_cost_high$top == "1",]

# Group together
scen_0_cost_low$Variation <- "low"
scen_0_cost$Variation <- "avg"
scen_0_cost_high$Variation <- "high"
scen_0_cost <- rbind.data.frame(scen_0_cost_low, scen_0_cost)
scen_0_cost <- rbind.data.frame(scen_0_cost, scen_0_cost_high)

scen_1_cost_low$Variation <- "low"
scen_1_cost$Variation <- "avg"
scen_1_cost_high$Variation <- "high"
scen_1_cost <- rbind.data.frame(scen_1_cost_low, scen_1_cost)
scen_1_cost <- rbind.data.frame(scen_1_cost, scen_1_cost_high)

scen_2_cost_low$Variation <- "low"
scen_2_cost$Variation <- "avg"
scen_2_cost_high$Variation <- "high"
scen_2_cost <- rbind.data.frame(scen_2_cost_low, scen_2_cost)
scen_2_cost <- rbind.data.frame(scen_2_cost, scen_2_cost_high)

scen_3_cost_low$Variation <- "low"
scen_3_cost$Variation <- "avg"
scen_3_cost_high$Variation <- "high"
scen_3_cost <- rbind.data.frame(scen_3_cost_low, scen_3_cost)
scen_3_cost <- rbind.data.frame(scen_3_cost, scen_3_cost_high)

scen_4_cost_low$Variation <- "low"
scen_4_cost$Variation <- "avg"
scen_4_cost_high$Variation <- "high"
scen_4_cost <- rbind.data.frame(scen_4_cost_low, scen_4_cost)
scen_4_cost <- rbind.data.frame(scen_4_cost, scen_4_cost_high)


final_costs <- data.frame(Min=0, Max=0, Mean=0, Scenario = c(1, 2, 3, 4))
# No scenario 0 since that does not require purchase
final_costs[final_costs$Scenario == 1, "Min"] <- sum(scen_1_cost[scen_1_cost$Variation == "low", "Total.Fleet.Vehicles"] * 
                                                       scen_1_cost[scen_1_cost$Variation == "low", "batt_size"] * 
                                                       bean_pred[bean_pred$year == 2023, "Optimistic"], na.rm = TRUE) + 
  sum(scen_1_cost[scen_1_cost$Variation == "low", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)
final_costs[final_costs$Scenario == 1, "Mean"] <- sum(scen_1_cost[scen_1_cost$Variation == "avg", "Total.Fleet.Vehicles"] * 
                                                        scen_1_cost[scen_1_cost$Variation == "avg", "batt_size"] * 
                                                        bean_pred[bean_pred$year == 2023, "Medium"], na.rm = TRUE) + 
  sum(scen_1_cost[scen_1_cost$Variation == "avg", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)

final_costs[final_costs$Scenario == 1, "Max"] <- sum(scen_1_cost[scen_1_cost$Variation == "high", "Total.Fleet.Vehicles"] * 
                                                       scen_1_cost[scen_1_cost$Variation == "high", "batt_size"] * 
                                                       bean_pred[bean_pred$year == 2023, "Pessimistic"], na.rm = TRUE) + 
  sum(scen_1_cost[scen_1_cost$Variation == "high", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)

scen_2_cost$Replace.Year <- scen_2_cost$Final.Year + 1
scen_2_cost[scen_2_cost$Replace.Year > 2040, "Replace.Year"] <- 2040
scen_2_cost <- left_join(scen_2_cost, bean_pred, by=c("Replace.Year" = "year"))
final_costs[final_costs$Scenario == 2, "Min"] <- sum(scen_2_cost[scen_2_cost$Variation == "low", "Total.Fleet.Vehicles"] * 
                                                       scen_2_cost[scen_2_cost$Variation == "low", "batt_size"] * 
                                                       scen_2_cost[scen_2_cost$Variation == "low", "Optimistic"], na.rm = TRUE) + 
  sum(scen_2_cost[scen_2_cost$Variation == "low", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)
final_costs[final_costs$Scenario == 2, "Mean"] <- sum(scen_2_cost[scen_2_cost$Variation == "avg", "Total.Fleet.Vehicles"] * 
                                                        scen_2_cost[scen_2_cost$Variation == "avg", "batt_size"] * 
                                                        scen_2_cost[scen_2_cost$Variation == "avg", "Medium"], na.rm = TRUE) + 
  sum(scen_2_cost[scen_2_cost$Variation == "avg", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)

final_costs[final_costs$Scenario == 2, "Max"] <- sum(scen_2_cost[scen_2_cost$Variation == "high", "Total.Fleet.Vehicles"] * 
                                                       scen_2_cost[scen_2_cost$Variation == "high", "batt_size"] * 
                                                       scen_2_cost[scen_2_cost$Variation == "high", "Pessimistic"], na.rm = TRUE) + 
  sum(scen_2_cost[scen_2_cost$Variation == "high", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)


scen_2_cost[scen_2_cost$Variation == "avg", "Cost"] <- scen_2_cost[scen_2_cost$Variation == "avg", "Total.Fleet.Vehicles"] * 
  scen_2_cost[scen_2_cost$Variation == "avg", "batt_size"] * 
  scen_2_cost[scen_2_cost$Variation == "avg", "Medium"] + 
  scen_2_cost[scen_2_cost$Variation == "avg", "Total.Fleet.Vehicles"] * beb_frame

scen_2_cost[scen_2_cost$Variation == "low", "Cost"] <- scen_2_cost[scen_2_cost$Variation == "low", "Total.Fleet.Vehicles"] * 
  scen_2_cost[scen_2_cost$Variation == "low", "batt_size"] * 
  scen_2_cost[scen_2_cost$Variation == "low", "Optimistic"] + 
  scen_2_cost[scen_2_cost$Variation == "low", "Total.Fleet.Vehicles"] * beb_frame

scen_2_cost[scen_2_cost$Variation == "high", "Cost"] <- scen_2_cost[scen_2_cost$Variation == "high", "Total.Fleet.Vehicles"] * 
  scen_2_cost[scen_2_cost$Variation == "high", "batt_size"] * 
  scen_2_cost[scen_2_cost$Variation == "high", "Pessimistic"] + 
  scen_2_cost[scen_2_cost$Variation == "high", "Total.Fleet.Vehicles"] * beb_frame

s2_sched <- scen_2_cost %>% group_by(Replace.Year,
                                     Variation) %>% summarise(Cost = sum(round(Cost/1000000000, 3)))

s2_sched_avg <- s2_sched[s2_sched$Variation == "avg",]

scen_3_cost$Replace.Year <- scen_3_cost$Final.Year + 1
scen_3_cost <- left_join(scen_3_cost, bean_pred, by=c("Final.Year" = "year"))
final_costs[final_costs$Scenario == 3, "Min"] <- sum(scen_3_cost[scen_3_cost$Variation == "low", "Total.Fleet.Vehicles"] * 
                                                       scen_3_cost[scen_3_cost$Variation == "low", "batt_size"] * 
                                                       scen_3_cost[scen_3_cost$Variation == "low", "Optimistic"], na.rm = TRUE) + 
  sum(scen_3_cost[scen_3_cost$Variation == "low", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)
final_costs[final_costs$Scenario == 3, "Mean"] <- sum(scen_3_cost[scen_3_cost$Variation == "avg", "Total.Fleet.Vehicles"] * 
                                                        scen_3_cost[scen_3_cost$Variation == "avg", "batt_size"] * 
                                                        scen_3_cost[scen_3_cost$Variation == "avg", "Medium"], na.rm = TRUE) + 
  sum(scen_3_cost[scen_3_cost$Variation == "avg", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)

final_costs[final_costs$Scenario == 3, "Max"] <- sum(scen_3_cost[scen_3_cost$Variation == "high", "Total.Fleet.Vehicles"] * 
                                                       scen_3_cost[scen_3_cost$Variation == "high", "batt_size"] * 
                                                       scen_3_cost[scen_3_cost$Variation == "high", "Pessimistic"], na.rm = TRUE) + 
  sum(scen_3_cost[scen_3_cost$Variation == "high", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)



scen_4_cost$Replace.Year <- scen_4_cost$Final.Year + 1
scen_4_cost[scen_4_cost$Replace.Year > 2040, "Replace.Year"] <- 2040
scen_4_cost <- left_join(scen_4_cost, bean_pred, by=c("Final.Year" = "year"))
final_costs[final_costs$Scenario == 4, "Min"] <- sum(scen_4_cost[scen_4_cost$Variation == "low", "Total.Fleet.Vehicles"] * 
                                                       scen_4_cost[scen_4_cost$Variation == "low", "batt_size"] * 
                                                       scen_4_cost[scen_4_cost$Variation == "low", "Optimistic"], na.rm = TRUE) + 
  sum(scen_4_cost[scen_4_cost$Variation == "low", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)
final_costs[final_costs$Scenario == 4, "Mean"] <- sum(scen_4_cost[scen_4_cost$Variation == "avg", "Total.Fleet.Vehicles"] * 
                                                        scen_4_cost[scen_4_cost$Variation == "avg", "batt_size"] * 
                                                        scen_4_cost[scen_4_cost$Variation == "avg", "Medium"], na.rm = TRUE) + 
  sum(scen_4_cost[scen_4_cost$Variation == "avg", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)

final_costs[final_costs$Scenario == 4, "Max"] <- sum(scen_4_cost[scen_4_cost$Variation == "high", "Total.Fleet.Vehicles"] * 
                                                       scen_4_cost[scen_4_cost$Variation == "high", "batt_size"] * 
                                                       scen_4_cost[scen_4_cost$Variation == "high", "Pessimistic"], na.rm = TRUE) + 
  sum(scen_4_cost[scen_4_cost$Variation == "high", "Total.Fleet.Vehicles"] * 
        beb_frame, na.rm = TRUE)

final_costs[,1:3] <- final_costs[,1:3]/1000000000

#### LCOD ####

#### Values for Levelized Cost of Driving ####
mean.diesel <- diesel.mpg.replacement
mean.cng.mpgde <- cng.replacement
mean.eb.mpgde <- elec.mpkwh.replacement * (129488 / 3412) 
#1 kWh = 3412 BTU & 1 gal diesel = 129488 Btu from GREET 2022

diesel_vmt_data <- filter(scen_1, cFuel.Type == "Diesel" & Total.Miles.on.Active.Vehicles.During.Period > 0)
diesel_vmt_data$vmt.per.veh <- diesel_vmt_data$Total.Miles.on.Active.Vehicles.During.Period/diesel_vmt_data$Active.Fleet.Vehicles
d.vmt <- diesel_vmt_data$vmt.per.veh %>% median(na.rm = TRUE)

ng_vmt_data <- filter(scen_1, cFuel.Type == "Compressed Natural Gas" & Total.Miles.on.Active.Vehicles.During.Period > 0)
ng_vmt_data$vmt.per.veh <- ng_vmt_data$Total.Miles.on.Active.Vehicles.During.Period/ng_vmt_data$Active.Fleet.Vehicles
ng.vmt <- ng_vmt_data$vmt.per.veh %>% median(na.rm = TRUE)

eb_vmt_data <- filter(scen_1, cFuel.Type == "Electric Battery" & Total.Miles.on.Active.Vehicles.During.Period > 0)
eb_vmt_data$vmt.per.veh <- eb_vmt_data$Total.Miles.on.Active.Vehicles.During.Period/eb_vmt_data$Active.Fleet.Vehicles
eb.vmt <- eb_vmt_data$vmt.per.veh %>% median(na.rm = TRUE)

# Distribution of VMT of BEBs
hist(eb_vmt_data$Total.Miles.on.Active.Vehicles.During.Period)

all.vmt <- median(scen_1$Total.Miles.on.Active.Vehicles.During.Period/scen_1$Active.Fleet.Vehicles, na.rm = TRUE)

# LCOD RESULTS
# $/DGE <- Cents per kWh / (100 cents/$) / (1 kWh = 3412 BTU) * (1 DGE = 129488 BTU)
elec.avg <- round(12.7 / 100 / 3412 * 129488, 2)
elec.max <- round(23.07 / 100 / 3412 * 129488, 2)
elec.min <- round(6.93 /100 / 3412 * 129488, 2)

lcod <- data.frame(Fuel.Type = c("Diesel", "Compressed Natural Gas", "Electric.rc", "Electric.sc"),
                   Discount.Rate = 0.05,
                   Discount.Rate.pes = 0.07,
                   Discount.Rate.opt = 0.03,
                   Lifetime = 12,
                   Capital.Costs.dollar = c(557000, 649000, beb.mean, beb.mean),
                   Capital.Costs.dollar.pes = c(728000, 897000, beb.max, beb.max),
                   Infrastructure.dollar = c(0, 62000, 56000, 68000),
                   Maintenance.dollar..mi = c(0.94, 0.94, 0.66, 0.66),
                   Maintenance.dollar..mi.pes = c(1.83, 1.83, 1.58, 1.58),
                   Maintenance.dollar..mi.opt = c(0.4, 0.4, 0.19, 0.19),
                   ind.VMT = c(d.vmt, ng.vmt, eb.vmt, eb.vmt),
                   all.VMT = c(d.vmt, ng.vmt, 30000, 30000),
                   Fuel.Price.dollar..dge = c(4.58, 3.68, elec.avg, elec.avg),
                   Fuel.Price.dollar..dge.pes = c(5.75, 5.36, elec.max, elec.max),
                   Fuel.Price.dollar..dge.opt = c(4.03, 2.88, elec.min, elec.min),
                   MPGDE = c(4.7, 5.2, 22.0, 22.0))

# Calculate how electric fuel price will be affected by plug efficiency
lcod$Fuel.Price.dollar..dge[3:4] <- lcod$Fuel.Price.dollar..dge[3:4] / plug_eff
lcod$Fuel.Price.dollar..dge.pes[3:4] <- lcod$Fuel.Price.dollar..dge.pes[3:4] / plug_eff
lcod$Fuel.Price.dollar..dge.opt[3:4] <- lcod$Fuel.Price.dollar..dge.opt[3:4] / plug_eff

# First calculate CRF
lcod$CRF <- (lcod$Discount.Rate*(1+lcod$Discount.Rate)^lcod$Lifetime)/
  ((1+lcod$Discount.Rate)^lcod$Lifetime - 1)

# Calculate each parameter for a stacked chart
lcod$Capital <- (lcod$CRF*lcod$Capital.Costs.dollar)/(lcod$ind.VMT)
lcod$Infrastructure <- (lcod$CRF*lcod$Infrastructure.dollar)/(lcod$ind.VMT)
lcod$Maintenance <- lcod$Maintenance.dollar..mi
lcod$Fuel <- lcod$Fuel.Price.dollar..dge/lcod$MPGDE

# Calculate baseline total
lcod$final <- (lcod$CRF*(lcod$Capital.Costs.dollar + lcod$Infrastructure.dollar))/
  (lcod$ind.VMT) + lcod$Maintenance.dollar..mi + lcod$Fuel.Price.dollar..dge/lcod$MPGDE

# Calculate +/- 10% for infrastructure costs
lcod$Infrastructure.dollar.pes <- lcod$Infrastructure.dollar * 1.1
lcod$Infrastructure.dollar.opt <- lcod$Infrastructure.dollar * 0.9

# Calculate high and low estimates for sensitivity
lcod$CRF.pes <- (lcod$Discount.Rate.pes*(1+lcod$Discount.Rate.pes)^lcod$Lifetime)/
  ((1+lcod$Discount.Rate.pes)^lcod$Lifetime - 1)
lcod$Capital.pes <- (lcod$CRF.pes * lcod$Capital.Costs.dollar.pes)/(lcod$ind.VMT)
lcod$Infrastructure.pes <- (lcod$CRF.pes * lcod$Infrastructure.dollar.pes)/lcod$ind.VMT
lcod$Maintenance.pes <- lcod$Maintenance.dollar..mi.pes
lcod$Fuel.pes <- lcod$Fuel.Price.dollar..dge.pes/lcod$MPGDE
lcod$final.pes <- (lcod$CRF.pes*(lcod$Capital.Costs.dollar.pes + lcod$Infrastructure.dollar.pes))/
  (lcod$ind.VMT) + lcod$Maintenance.dollar..mi.pes + lcod$Fuel.Price.dollar..dge.pes/lcod$MPGDE

lcod$CRF.opt <- (lcod$Discount.Rate.opt*(1+lcod$Discount.Rate.opt)^lcod$Lifetime)/
  ((1+lcod$Discount.Rate.opt)^lcod$Lifetime - 1)
lcod$Capital.opt <- (lcod$CRF.opt * lcod$Capital.Costs.dollar)/(lcod$ind.VMT)
lcod$Infrastructure.opt <- (lcod$CRF.opt * lcod$Infrastructure.dollar.opt)/lcod$all.VMT
lcod$Maintenance.opt <- lcod$Maintenance.dollar..mi.opt
lcod$Fuel.opt <- lcod$Fuel.Price.dollar..dge.opt/lcod$MPGDE
lcod$final.opt <- (lcod$CRF.opt*(lcod$Capital.Costs.dollar + lcod$Infrastructure.dollar.opt))/
  (lcod$all.VMT) + lcod$Maintenance.dollar..mi.opt + lcod$Fuel.Price.dollar..dge.opt/lcod$MPGDE


lcod_melt <- lcod[, c(1, 19:22)]
lcod_plot <- melt(lcod_melt, id.vars = "Fuel.Type")
names(lcod_plot)[2] <- "Costs"
lcod_plot$Fuel.Type <- factor(lcod_plot$Fuel.Type, 
                              levels = c("Diesel", 
                                         "Compressed Natural Gas", 
                                         "Electric.rc", 
                                         "Electric.sc"))
axis_names <- c("Diesel", "CNG", "Electric rapid-charge", "Electric slow-charge")
axis_names <- str_wrap(axis_names, width = 13)

lcod_pes <- lcod[, c(1, 27:30)]
names(lcod_pes) <- names(lcod_melt)
lcod_pes_melt <- melt(lcod_pes, id.vars = "Fuel.Type")
names(lcod_pes_melt)[2:3] <- c("Costs", "pessimistic")

lcod_opt <- lcod[, c(1, 33:36)]
names(lcod_opt) <- names(lcod_melt)
lcod_opt_melt <- melt(lcod_opt, id.vars = "Fuel.Type")
names(lcod_opt_melt)[2:3] <- c("Costs", "optimistic")

sensitivity_lcod <- merge(lcod_pes_melt, lcod_opt_melt)
lcod_plot <- merge(lcod_plot, sensitivity_lcod)

lcod_plot[lcod_plot$Costs == "Maintenance", 4:5] <- lcod_plot[lcod_plot$Costs == "Maintenance", 4:5] + lcod_plot[lcod_plot$Costs == "Fuel", 4:5]
lcod_plot[lcod_plot$Costs == "Infrastructure", 4:5] <- lcod_plot[lcod_plot$Costs == "Maintenance", 4:5] + lcod_plot[lcod_plot$Costs == "Infrastructure", 4:5]
lcod_plot[lcod_plot$Costs == "Capital", 4:5] <- lcod_plot[lcod_plot$Costs == "Infrastructure", 4:5] + lcod_plot[lcod_plot$Costs == "Capital", 4:5]

lcod_plot[lcod_plot$Costs == "Capital", 4:5] <- c(lcod$final.pes, lcod$final.opt)
lcod_plot[lcod_plot$Costs != "Capital", 4:5] <- NA

# Calculate lcod for BEBs only assuming increased daily miles
lcod$final.beb <- (lcod$CRF*(lcod$Capital.Costs.dollar + lcod$Infrastructure.dollar))/
  (lcod$all.VMT) + lcod$Maintenance.dollar..mi + lcod$Fuel.Price.dollar..dge/lcod$MPGDE

#### FIGURE: LCOD Plot ####
lcod_save <- ggplot(data = lcod_plot, aes(fill = Costs, y=value, x=Fuel.Type)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymax = pessimistic, ymin = optimistic), position = "identity", width = 0.3) +
  theme_classic() +
  scale_x_discrete(labels=axis_names) + 
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  labs(x="", y="Levelized Cost of Driving (2023$/mi)") +
  annotate(geom = "label", x = 1, y = lcod$final[1] + 1, label = lcod$final[1] %>% round(digits = 2), family = "sans", size = 5) +
  annotate(geom = "label", x = 2, y = lcod$final[2] + 1, label = lcod$final[2] %>% round(digits = 2), family = "sans", size = 5) +
  annotate(geom = "label", x = 3, y = lcod$final[3] + 1, label = lcod$final[3] %>% round(digits = 2), family = "sans", size = 5) +
  annotate(geom = "label", x = 4, y = lcod$final[4] + 1, label = lcod$final[4] %>% round(digits = 2), family = "sans", size = 5) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 12, family = "sans"), 
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        legend.key.size = unit(4, "mm")) +
  ylim(0, 20)

lcod_save

# ggsave("Graphs/lcod_all.png",
#        plot = lcod_save,
#        dpi = 700,
#        width = 7,
#        height = 4,
#        units = "in")

#### LCOD Breakeven Point ####

# Capital cost of BEB to make slow charge
# cost competitive with diesel buses assuming no increase in yearly mileage
((lcod[lcod$Fuel.Type == "Diesel", "final"] -
    lcod[lcod$Fuel.Type == "Electric.sc", "Fuel"] -
    lcod[lcod$Fuel.Type == "Electric.sc", "Maintenance"]) *
   lcod[lcod$Fuel.Type == "Electric.sc", "ind.VMT"])/ 
  lcod[lcod$Fuel.Type == "Electric.sc", "CRF"] -
  lcod[lcod$Fuel.Type == "Electric.sc", "Infrastructure.dollar"]

# Capital cost of BEB to make slow charge
# cost competitive with diesel buses, assuming increase in yearly mileage
((lcod[lcod$Fuel.Type == "Diesel", "final"] -
    lcod[lcod$Fuel.Type == "Electric.sc", "Fuel"] -
    lcod[lcod$Fuel.Type == "Electric.sc", "Maintenance"]) *
    lcod[lcod$Fuel.Type == "Electric.sc", "all.VMT"])/ 
  lcod[lcod$Fuel.Type == "Electric.sc", "CRF"] -
  lcod[lcod$Fuel.Type == "Electric.sc", "Infrastructure.dollar"]

# Capital cost of BEB to make rapid charge
# cost competitive with diesel buses assuming no increase in yearly mileage
((lcod[lcod$Fuel.Type == "Diesel", "final"] -
    lcod[lcod$Fuel.Type == "Electric.rc", "Fuel"] -
    lcod[lcod$Fuel.Type == "Electric.rc", "Maintenance"]) *
    lcod[lcod$Fuel.Type == "Electric.rc", "ind.VMT"])/ 
  lcod[lcod$Fuel.Type == "Electric.rc", "CRF"] -
  lcod[lcod$Fuel.Type == "Electric.rc", "Infrastructure.dollar"]

# Capital cost of BEB to make rapid charge
# cost competitive with diesel buses, assuming increasing in yearly mileage
((lcod[lcod$Fuel.Type == "Diesel", "final"] -
    lcod[lcod$Fuel.Type == "Electric.rc", "Fuel"] -
    lcod[lcod$Fuel.Type == "Electric.rc", "Maintenance"]) *
    lcod[lcod$Fuel.Type == "Electric.rc", "all.VMT"])/ 
  lcod[lcod$Fuel.Type == "Electric.rc", "CRF"] -
  lcod[lcod$Fuel.Type == "Electric.rc", "Infrastructure.dollar"]

#### Social Cost of Carbon Calculation ####
# GHGs mitigated in "top 100 agencies" scenario
ghg_plot <- na.omit(ghg_plot)
(ghg_plot[ghg_plot$Scenario == 0, "Emissions.in.mmt"] - ghg_plot[ghg_plot$Scenario == 1, "Emissions.in.mmt"])/ghg_plot[ghg_plot$Scenario == 0, "Emissions.in.mmt"]
(ghg_plot[ghg_plot$Scenario == 0, "Emissions.in.mmt"] - ghg_plot[ghg_plot$Scenario == 3, "Emissions.in.mmt"])/ghg_plot[ghg_plot$Scenario == 0, "Emissions.in.mmt"]

# Price for parity
parity <- function(lcod,
                   discount.rate = 0.05, 
                   lifetime = 12, 
                   infrastructure, 
                   maintenance = 1.58,
                   vmt,
                   fuel.price = 4.82,
                   mpdge = 22){
  CRF = (discount.rate*(1+discount.rate)^lifetime)/((1+discount.rate)^lifetime - 1)
  capital_cost = ((lcod - (fuel.price/mpdge) - maintenance)*vmt/CRF) - infrastructure
  return(capital_cost)
}

# Slow Charge to diesel
parity(lcod = 4.61,
       infrastructure = 68000,
       vmt = 30000)
# Rapid Charge to diesel
parity(lcod = 4.61,
       infrastructure = 56000,
       vmt = 30000)

# Annual miles
lcod[lcod$Fuel.Type == "Electric.rc", "all.VMT"]
lcod[lcod$Fuel.Type == "Electric.rc", "ind.VMT"]

# LCOD of BEBs at 30,000 aVMT
lcod$final.beb[3]

# SCC to make electric rapid charge cost
# competitive with CNG

opt_infra <- 0.9 * lcod$Infrastructure.dollar
pes_infra <- 1.1 * lcod$Infrastructure.dollar

#### LCOD Individual Parameter Sensitivity Analysis ####

lcod_func <- function(discount.rate, 
                      lifetime, 
                      capital, 
                      infrastructure, 
                      maintenance,
                      vmt,
                      fuel.price,
                      mpdge) (((discount.rate * (1 + discount.rate)^lifetime) / ((1 + discount.rate)^lifetime - 1)) * (capital + infrastructure)) / vmt + maintenance + fuel.price/mpdge

## Discount Sensitivity
# Vary the discount rate between the pessimistic and optimistic values

lcod_base <- lcod[,c("Discount.Rate",
                     "Capital.Costs.dollar",
                     "Infrastructure.dollar",
                     "Maintenance.dollar..mi",
                     "ind.VMT",
                     "Fuel.Price.dollar..dge",
                     "MPGDE")]

lcod_opt <- lcod[,c("Discount.Rate.opt",
                    "Capital.Costs.dollar",
                    "Infrastructure.dollar.opt",
                    "Maintenance.dollar..mi.opt",
                    "all.VMT",
                    "Fuel.Price.dollar..dge.opt",
                    "MPGDE")]

lcod_pes <- lcod[,c("Discount.Rate.pes",
                    "Capital.Costs.dollar.pes",
                    "Infrastructure.dollar.pes",
                    "Maintenance.dollar..mi.pes",
                    "ind.VMT",
                    "Fuel.Price.dollar..dge.pes",
                    "MPGDE")]

lcod_opt_results <- lcod_opt
for (i in 1:7) {
  
  for (j in 1:4) {
    
    lcod_base <- lcod[,c("Discount.Rate",
                         "Capital.Costs.dollar",
                         "Infrastructure.dollar",
                         "Maintenance.dollar..mi",
                         "ind.VMT",
                         "Fuel.Price.dollar..dge",
                         "MPGDE")]
    
    lcod_base[j,i] <- lcod_opt[j,i]
    
    
    lcod_opt_results[j,i] <- lcod_func(lcod_base[j, "Discount.Rate"], 
                                       12, 
                                       lcod_base[j, "Capital.Costs.dollar"], 
                                       lcod_base[j, "Infrastructure.dollar"],
                                       lcod_base[j, "Maintenance.dollar..mi"], 
                                       lcod_base[j, "ind.VMT"],
                                       lcod_base[j, "Fuel.Price.dollar..dge"], 
                                       lcod_base[j, "MPGDE"]) %>% round(digits = 2)
    
  }
  
}

lcod_pes_results <- lcod_pes
for (i in 1:7) {
  
  for (j in 1:4) {
    
    lcod_base <- lcod[,c("Discount.Rate",
                         "Capital.Costs.dollar",
                         "Infrastructure.dollar",
                         "Maintenance.dollar..mi",
                         "ind.VMT",
                         "Fuel.Price.dollar..dge",
                         "MPGDE")]
    
    lcod_base[j,i] <- lcod_pes[j,i]
    
    
    lcod_pes_results[j,i] <- lcod_func(lcod_base[j, "Discount.Rate"], 
                                       12, 
                                       lcod_base[j, "Capital.Costs.dollar"], 
                                       lcod_base[j, "Infrastructure.dollar"],
                                       lcod_base[j, "Maintenance.dollar..mi"], 
                                       lcod_base[j, "ind.VMT"],
                                       lcod_base[j, "Fuel.Price.dollar..dge"], 
                                       lcod_base[j, "MPGDE"]) %>% round(digits = 2)
    
  }
  
}



