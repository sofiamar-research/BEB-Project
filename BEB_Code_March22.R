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

raw_veh <- read.csv("2019-Data/2019 Revenue Vehicle Inventory.csv",
                    na.strings = c("", "NA"),
                    stringsAsFactors = FALSE)
agency_info <- read.csv("2019-Data/2019 Agency Info.csv",
                        na.strings = c("", "NA"),
                        stringsAsFactors = FALSE)
service_info <- read.csv("2019-Data/2019 Service.csv",
                         na.strings = c("", "NA"),
                         stringsAsFactors = FALSE)
#"raw_energy_2" comes from the 2019 Fuel and Energy macro excel sheet titled "Fuel and Energy"
raw_energy_2 <- read.csv("2019-Data/Fuel and Energy.csv",
                         na.strings = c("", "NA"), 
                         stringsAsFactors = FALSE)
egrid <- read.csv("Powergrid/egrid2019.csv",
                  na.strings = c("", "NA"), 
                  stringsAsFactors = TRUE)
egrid_zip <- read.csv("Powergrid/zipcode_tool.csv",
                      na.strings = c("", "NA"), 
                      stringsAsFactors = TRUE)
emfac <- read.csv("Emissions/EMFAC2021-modelyr.csv")
batt_emissions_raw <- read.csv("Emissions/batt_emissions.csv",
                               na.strings = c("", "NA"), 
                               stringsAsFactors = TRUE,
                               skip = 3)
# CNG in "welltotank" is calculated into grams per diesel gallon equivalent 
# (dge, it is assumed this is how most agencies reported mpg for CNG
# as they were only advised to report CNG mpg as "based on what type of 
# fuel the revenue vehicle would use if it were not powered by CNG")
welltotank_raw <- read.csv("Emissions/welltotank.csv", 
                           na.strings = c("", "NA"), 
                           stringsAsFactors = TRUE)

fuel_eco <- read.csv("Emissions/EMFAC2021-avg_fuel_eco.csv", skip = 8)

summary(raw_veh)

#### General cleaning for raw vehicle data ####
temp_veh <- raw_veh
# Replace "NA" with zero
temp_veh[is.na(temp_veh)] <- 0

# Selecting numeric/integer fields
x <- c(8,9,14,15,17,21,22,23,25,26,27,28,29)
# Remove special characters to make fields numeric
for (i in x) {
  temp_veh[, i] <- temp_veh[, i] %>% 
    as.character %>% 
    gsub(",","", .) %>% 
    gsub("-","", .) %>% 
    as.numeric()
}
rm(i,x)
summary(temp_veh)

veh_exp <- temp_veh

# Separate mode (MB - Buses, DR - Demand Response, etc.) from type (Purchased or Directly operated)
veh_exp$Mode <- veh_exp$Modes %>% str_sub(end = 2L)
veh_exp$Mode %>% as.factor() %>% summary()

# How many miles driven on active vehicles by mode?
mode_share <- aggregate(Total.Miles.on.Active.Vehicles.During.Period ~ Mode, data = veh_exp, FUN = "sum")
total.mi <- sum(mode_share$Total.Miles.on.Active.Vehicles.During.Period)
mode_share$proportion <- (mode_share$Total.Miles.on.Active.Vehicles.During.Period / total.mi) * 100
mode_share$proportion <- mode_share$proportion %>% round(digits = 2)

# "MB" are transit buses generally, keeping directly operated (DO) and purchased transportations (PT)
temp_vehs <- temp_veh[temp_veh$Modes=="MB/DO" | temp_veh$Modes=="MB/PT",]
temp_vehs <- unique(temp_vehs)

# Claiming this as a dataframe for the aggregated data
veh_data_agg <- temp_vehs
rm(temp_veh)

## Make a single entry for each bus, nrow of dataframe is number of buses
## This database will make it easier for some plotting
ind_veh_data <- as.data.frame(lapply(veh_data_agg,rep,veh_data_agg$Total.Fleet.Vehicles))                                                     
nrow(ind_veh_data)
ind_veh_data$Fuel.Type <- as.factor(ind_veh_data$Fuel.Type)

#### Some EDA ####
summary(ind_veh_data$Standing.Capacity)
summary(ind_veh_data$Seating.Capacity)
ind_veh_data$Total.Capacity <- ind_veh_data$Seating.Capacity + ind_veh_data$Standing.Capacity

# There are some vehicles with capacity of 2
summary(ind_veh_data$Total.Capacity)

# These vehicles are listed as "Automobiles"
ind_veh_data[ind_veh_data$Total.Capacity==2, "Vehicle.Type"]

# There is more than just bus data in this data set
ind_veh_data$Vehicle.Type <- factor(ind_veh_data$Vehicle.Type)
summary(ind_veh_data$Vehicle.Type)

# Keep only transit bus data
nrow(ind_veh_data)
ind_veh_data <- filter(ind_veh_data, 
                       ind_veh_data$Vehicle.Type =="Bus" | 
                         ind_veh_data$Vehicle.Type == "Articulated Bus" | 
                         ind_veh_data$Vehicle.Type == "Cutaway" | 
                         ind_veh_data$Vehicle.Type == "Double Decker Bus")
nrow(ind_veh_data)

# Doing the same thing to the aggregated dataset
sum(veh_data_agg$Total.Fleet.Vehicles)
veh_data_agg <- filter(veh_data_agg,
                       veh_data_agg$Vehicle.Type == "Bus" |
                         veh_data_agg$Vehicle.Type == "Articulated Bus" | 
                         veh_data_agg$Vehicle.Type == "Cutaway" |
                         veh_data_agg$Vehicle.Type == "Double Decker Bus")
sum(veh_data_agg$Total.Fleet.Vehicles)
#3,054 vehicles removed from both sets

electric_veh_data <- filter(ind_veh_data, Fuel.Type == "Electric Battery")
summary(electric_veh_data)
nrow(electric_veh_data)
#541 electric buses listed in 2019 FTA data

# Includes AS (American Samoa), GU (Guam), PR (Puerto Rico), and VI (Virgin Islands) 
agency_info$State %>% as.factor() %>% summary()

# Merge agency locations to their bus data and remove territories
joined_veh_data <- left_join(veh_data_agg, agency_info, by = 'NTD.ID')
ind_veh_data <- left_join(ind_veh_data, agency_info, by = "NTD.ID")
joined_veh_data <- joined_veh_data[!is.na(joined_veh_data$Zip.Code),]

nrow(ind_veh_data)
ind_veh_data <- filter(ind_veh_data, 
                       ind_veh_data$State != "AS" & 
                         ind_veh_data$State != "GU" & 
                         ind_veh_data$State != "PR" & 
                         ind_veh_data$State != "VI")
nrow(ind_veh_data)

sum(joined_veh_data$Total.Fleet.Vehicles)
joined_veh_data <- filter(joined_veh_data,
                          joined_veh_data$State != "AS" &
                            joined_veh_data$State != "GU" & 
                            joined_veh_data$State != "PR" & 
                            joined_veh_data$State != "VI")
sum(joined_veh_data$Total.Fleet.Vehicles)
# 429 vehicles removed
# This is the number used for "total transit buses in the U.S."

## What organization types are there?
joined_veh_data$Organization.Type <- joined_veh_data$Organization.Type %>% as.factor()
summary(joined_veh_data$Organization.Type)

## Funding sources?
joined_veh_data$Funding.Source <- joined_veh_data$Funding.Source %>% as.factor()
summary(joined_veh_data$Funding.Source)
  

# Plotting Fuel Types
x <- nrow(ind_veh_data)
ind_veh_data <- ind_veh_data[ind_veh_data$Fuel.Type=="Diesel Fuel"| 
                                 ind_veh_data$Fuel.Type=="Compressed Natural Gas"| 
                                 ind_veh_data$Fuel.Type=="Gasoline"|
                                 ind_veh_data$Fuel.Type=="Electric Battery"|
                                 ind_veh_data$Fuel.Type=="Hybrid Diesel"|
                                 ind_veh_data$Fuel.Type=="Hybrid Gasoline",]
y <- nrow(ind_veh_data)
y/x*100

sum(joined_veh_data$Total.Fleet.Vehicles)
joined_veh_data <- filter(joined_veh_data, Fuel.Type == "Diesel Fuel"|
                          Fuel.Type == "Compressed Natural Gas"|
                          Fuel.Type == "Gasoline" |
                          Fuel.Type == "Electric Battery"|
                          Fuel.Type == "Hybrid Diesel"|
                          Fuel.Type == "Hybrid Gasoline")
sum(joined_veh_data$Total.Fleet.Vehicles)


ind_veh_data$Fuel.Type <- factor(ind_veh_data$Fuel.Type, 
                                   levels = c("Hybrid Gasoline",
                                              "Electric Battery",
                                              "Gasoline", 
                                              "Hybrid Diesel", 
                                              "Compressed Natural Gas", 
                                              "Diesel Fuel"))


ggplot(data = ind_veh_data, aes(Fuel.Type)) + 
  geom_bar() + 
  coord_flip() + 
  labs(y="Number of Buses", x="Fuel Type") + theme_bw()

# ~72% of buses are diesel powered
(nrow(ind_veh_data[ind_veh_data$Fuel.Type == "Diesel Fuel",]) + nrow(ind_veh_data[ind_veh_data$Fuel.Type == "Hybrid Diesel",]))*100/nrow(ind_veh_data)

#Plotting manufacture year
summary(ind_veh_data$Manufacture.Year)
nrow(ind_veh_data)
ind_veh_data <- ind_veh_data[ind_veh_data$Manufacture.Year>0,]
nrow(ind_veh_data)
# 144 vehicles removed

sum(joined_veh_data$Total.Fleet.Vehicles)
joined_veh_data <- filter(joined_veh_data, Manufacture.Year>0)
sum(joined_veh_data$Total.Fleet.Vehicles)
# 144 vehicles removed

summary(ind_veh_data$Manufacture.Year)
qplot(Manufacture.Year, data = ind_veh_data)
ind_veh_data[ind_veh_data$Manufacture.Year<0,] %>% nrow()
ind_veh_data[ind_veh_data$Manufacture.Year<1990,] %>% nrow()
ind_veh_data <- ind_veh_data[ind_veh_data$Manufacture.Year>1990,]
# 7 vehicles removed

#### SI FIGURE: Organization Types ####
organizations <- ggplot(data = ind_veh_data, 
                        aes(x = reorder(Organization.Type, 
                                        Organization.Type, 
                                        function(x)length(x)))) + 
  theme_bw() +
  geom_bar(stat = "count", fill = "grey16", position = position_dodge(0.1)) + 
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

funding <-  ggplot(data = ind_veh_data, 
       aes(reorder(Funding.Source, 
                   Funding.Source, 
                   function(x) length(x)))) + 
  geom_bar(fill = "grey16",) + 
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

#### FIGURE: Manufacture Year ####
manufacture_year <- ggplot(data = ind_veh_data, aes(Manufacture.Year)) + 
  geom_histogram(binwidth = 0.5, fill = "grey16") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11),
        text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 12, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 4.5),
        axis.title.y = element_text(margin = margin(r = 15), vjust = -0.5)) +
  scale_x_continuous(breaks = 1990:2019) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  labs(x = "Manufacture Year", y = "Number of U.S. Transit Buses")

manufacture_year

summary(ind_veh_data[ind_veh_data$Fuel.Type != "Electric Battery", "Manufacture.Year"])

# ggsave("Graphs/manufacture_year.png",
#        plot = manufacture_year,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")


#number of conventional buses to serve a route
service_info <- filter(service_info, Mode=="MB")
service_info <- unique(service_info)
# We will obtain days operated for each agency to be able to calculate
# approximate daily mileage for each transit bus
summary(service_info)
# Divide agency information and vehicle information
agency_service <- service_info[,c(1:10)]
vehicle_service <- service_info[,c(11,12,14:21,33,34)]
# Any vehicle information that is NA will be replaced with 0
vehicle_service[is.na(vehicle_service)] <- 0
# Recombine vehicle and agency information
service <- cbind.data.frame(agency_service, vehicle_service)
# Distinguish between directly operated and purchased transportation
do_service <- service[service$TOS=="DO",]
pt_service <- service[service$TOS=="PT",]
rm(agency_service, vehicle_service)

# Create a list of agency names
agency_list <- unique(service$Agency.Name)

# We want annual totals of the days operated
# Some agencies did not report annual totals but
# did report weekday and weekend data
for (j in 1:2) {
  if(j==1){
    service <- do_service
    agency_list <- unique(service$Agency.Name) 
  } else {
    service <- pt_service
    agency_list <- unique(service$Agency.Name)
  }

  for (i in 1:length(agency_list)) {
  
    # x will be each agencies' individual service info
    x <- service[service$Agency.Name==agency_list[i],]
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

# Who is making the BEBs?
electric_veh_data$Manufacturer <- as.factor(electric_veh_data$Manufacturer)
levels(electric_veh_data$Manufacturer)[levels(electric_veh_data$Manufacturer)=="Build Your Dreams, Inc."] <- "BYD Motors"
levels(electric_veh_data$Manufacturer)[levels(electric_veh_data$Manufacturer)=="Flyer Industries Ltd (aka New Flyer Industries)"] <- "New Flyer of America"

summary(electric_veh_data$Manufacturer)

# Double checking if we only have full length transit buses
electric_veh_data$Vehicle.Type <- as.factor(electric_veh_data$Vehicle.Type)
summary(electric_veh_data$Vehicle.Type)

# Now need conventional buses mileage data
conv_fta <- filter(ind_veh_data, 
                   ind_veh_data$Fuel.Type != "Electric Battery" &
                     Fuel.Type!= "Hydrogen")

summary(annual_service)

# This filter removes agencies that still have no reported days open
# These agencies also report zero passenger miles
annual_service <- filter(annual_service,
                         Days.of.Service.Operated>0,
                         Vehicles.Passenger.Cars.Operated.in.Maximum.Service>0)

# Make mode field match the fta data for easier joining
annual_service$Modes <- paste0(annual_service$Mode, "/", annual_service$TOS)
conv_fta <- left_join(conv_fta, annual_service, by=c("NTD.ID", "Modes"))

nrow(conv_fta)
conv_fta <- conv_fta[conv_fta$Total.Miles.on.Active.Vehicles.During.Period>0,]
# Some of these vehicles are part of a contingency fleet or just seem to be missing data
nrow(conv_fta)
# 393 vehicles removed for inactivity

conv_fta$Average.Daily.Miles.on.Active.Vehicles.During.Period <- conv_fta$Total.Miles.on.Active.Vehicles.During.Period/conv_fta$Total.Fleet.Vehicles/conv_fta$Days.of.Service.Operated
conv_fta <- arrange(conv_fta, Average.Daily.Miles.on.Active.Vehicles.During.Period)
conv_fta <- conv_fta[!is.na(conv_fta$Average.Daily.Miles.on.Active.Vehicles.During.Period),]
summary(conv_fta$Average.Daily.Miles.on.Active.Vehicles.During.Period)

#### FIGURE: Range of conventional buses ####
nbins <- nclass.FD(conv_fta$Average.Daily.Miles.on.Active.Vehicles.During.Period)
conv_range <- ggplot(conv_fta, aes(Average.Daily.Miles.on.Active.Vehicles.During.Period)) + 
  geom_histogram(bins = nbins, fill = "grey16") + theme_classic() + 
  labs(x = "Average Daily Miles on Active Vehicles in 2019", y = str_wrap("Number of Conventional U.S. Transit Buses in Service", width = 30)) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 20), hjust = 0.5, vjust = -0.5)) +
  scale_x_continuous(breaks = seq(0, 350, 50)) +
  scale_y_continuous(breaks = seq(0, 2500, 500),
                     limits = c(0, 2500),
                     labels = function(x) format(x, big.mark = ","))

conv_range

# ggsave("Graphs/average_daily_miles.png",
#        plot = conv_range,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

conv_fta[conv_fta$Average.Daily.Miles.on.Active.Vehicles.During.Period <= 100,] %>% nrow()/nrow(conv_fta)
# 55% of the fleet has a daily range smaller than 100 mi.
conv_fta[conv_fta$Average.Daily.Miles.on.Active.Vehicles.During.Period <= 200,] %>% nrow()/nrow(conv_fta)
# Almost 99% of the fleet has a daily range smaller than 200 miles

conv_fta$Average.Daily.Miles.on.Active.Vehicles.During.Period %>% mean()

#comparing the current age of vehicles to expected lifespans
conv_fta$Vehicle.Age <- (2019-conv_fta$Manufacture.Year)
ages <- conv_fta[conv_fta$Useful.Life.Benchmark>0,]
ages$Life.Left <- ages$Useful.Life.Benchmark-ages$Vehicle.Age
ages$life.past <- -ages$Life.Left
summary(ages$Life.Left)
summary(conv_fta$Useful.Life.Benchmark)
conv_fta[conv_fta$Useful.Life.Benchmark==25,] %>% unique()
# The buses with a ULB of 25 were reconditioned in past years

#### FIGURE: Buses past useful life benchmark ####
nbins <- nclass.FD(ages$Life.Left)
colors.ulb <- c(rep("grey16", 31), rep("firebrick", 30))
annotation <- data.frame(
  x = 10,
  y = 3000,
  label = "Buses Eligible for Replacement"
)
past_ulb <- ggplot(data = ages, aes(life.past)) + 
  geom_histogram(binwidth = 0.5, fill = colors.ulb) + 
  theme_classic() + 
  labs(x = "Years Past Useful Life", y = str_wrap("Number of Conventional U.S. Transit Buses in Service", width = 30)) + 
  coord_cartesian(xlim = c(-20, 20)) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 15), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 15), vjust = 2.5, hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
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


# Aggregate number of buses (total, electric, ratio) by agency and mode
# Aggregate total fleet numbers by agency and mode
joined_veh_data <- filter(joined_veh_data, Manufacture.Year>=1990)
bus_data_fta_2019 <- joined_veh_data
agency_agg_data <- aggregate(bus_data_fta_2019$Total.Fleet.Vehicles ~
                               bus_data_fta_2019$Agency.Name.x + bus_data_fta_2019$Modes,
                             FUN = sum)
names(agency_agg_data) <- c("Agency.Name", "Mode", "Total.Fleet.Vehicles")

# Aggregate BEBs by agency and modes
eveh_data_agg <- unique(electric_veh_data)
agency_agg_edata <- aggregate(eveh_data_agg$Total.Fleet.Vehicles ~
                                eveh_data_agg$Agency.Name + eveh_data_agg$Modes,
                              FUN = sum)
names(agency_agg_edata) <- c("Agency.Name", "Mode", "Total.Fleet.E.Vehicles")

# Join the two data sets
agency_agg_data <- left_join(agency_agg_data,
                             agency_agg_edata,
                             by=c("Agency.Name", "Mode"))

# Make a table of proportions
agency_agg_data$Electric.Vehicle.Ratio <- agency_agg_data$Total.Fleet.E.Vehicles/agency_agg_data$Total.Fleet.Vehicles
agency_agg_data$Electric.Vehicle.Ratio <- round(agency_agg_data$Electric.Vehicle.Ratio, digits = 3)
summary(agency_agg_data)

# Fleet size distribution
qplot(agency_agg_data$Total.Fleet.Vehicles)
Agency.Fleet.Vehicles <- cut(x = agency_agg_data$Total.Fleet.Vehicles,
                             breaks = c(0, 50, 200, 1000, 5000))
qplot(Agency.Fleet.Vehicles %>% na.omit()) + 
  theme_bw() + 
  labs(x="Agency Bus Fleet Size",
       y="Number of Agencies",
       caption = "*Blue numbers indicate how many agencies of each size bracket have at least one BEB.") + 
  annotate(geom = "text", x=4, y=65, label="5", color="Blue") + 
  annotate(geom = "text", x=3, y=130, label="15", color="Blue") + 
  annotate(geom = "text", x=2, y=200, label="30", color="Blue") + 
  annotate(geom = "text", x=1, y=460, label="16", color="Blue") + 
  scale_x_discrete(labels=c("(0,50]" = "Small", 
                            "(50,200]" = "Medium", 
                            "(200,1e+03]" = "Large", 
                            "(1e+03,5e+03]"="Extra Large"))
# This plot shows the number of agencies that fall into qualitative size categories
# The blue numbers show the number of agencies that have attempted or bought at least one BEB
# This is to show that larger agencies have more resources to test out the new tech

agency_agg_data$Total.Fleet.E.Vehicles[is.na(agency_agg_data$Total.Fleet.E.Vehicles)] <- 0

# Generate all the numbers from the previous plot
agency_agg_data_small <- agency_agg_data[agency_agg_data$Total.Fleet.Vehicles <= 50 & 
                                           agency_agg_data$Total.Fleet.Vehicles>0,]
agency_agg_data_med <- agency_agg_data[(agency_agg_data$Total.Fleet.Vehicles <= 200 & 
                                          agency_agg_data$Total.Fleet.Vehicles>50),]
agency_agg_data_lg <- agency_agg_data[(agency_agg_data$Total.Fleet.Vehicles <= 1000 & 
                                         agency_agg_data$Total.Fleet.Vehicles>200),]
agency_agg_data_xl <- agency_agg_data[agency_agg_data$Total.Fleet.Vehicles >1000,]

nrow(agency_agg_data_small[agency_agg_data_small$Total.Fleet.E.Vehicles>0,])
mean(agency_agg_data_small[agency_agg_data_small$Total.Fleet.E.Vehicles>0, "Total.Fleet.E.Vehicles"])

nrow(agency_agg_data_med[agency_agg_data_med$Total.Fleet.E.Vehicles>0,])
mean(agency_agg_data_med[agency_agg_data_med$Total.Fleet.E.Vehicles>0, "Total.Fleet.E.Vehicles"])

nrow(agency_agg_data_lg[agency_agg_data_lg$Total.Fleet.E.Vehicles>0,])
mean(agency_agg_data_lg[agency_agg_data_lg$Total.Fleet.E.Vehicles>0, "Total.Fleet.E.Vehicles"])

nrow(agency_agg_data_xl[agency_agg_data_xl$Total.Fleet.E.Vehicles>0,])
mean(agency_agg_data_xl[agency_agg_data_xl$Total.Fleet.E.Vehicles>0, "Total.Fleet.E.Vehicles"])

rm(agency_agg_data_small, agency_agg_data_med, agency_agg_data_lg, agency_agg_data_xl)

# Number of reporting agencies
agencies <- agency_info[unique(agency_info$Agency.Name),]
#2,904 data entries

#fuel data
summary(raw_energy_2)
#no lpg data#
mb_energy <- raw_energy_2[raw_energy_2$Mode == "MB",]
summary(mb_energy)
#EB mi/kwh outliers, no lpg data, gasoline mpg, cng mpg, and diesel mpg outliers

#next 7 lines changes columns of interest from character to numeric types
i <- c(8,9,12,14,16,18,20,22,24,26,28,31,33,35,37,39,41,43)
for (j in i) {
  temp <- mb_energy[,j]
  temp[is.na(temp)] <- 0
  temp <- temp %>% gsub("[$,]","", .) %>% as.numeric()
  mb_energy[,j] <- temp
}

# Data has some fields for questionable data
# Next loop removes these fields
unq_energy <- mb_energy[,1:14]
for (j in 15:ncol(mb_energy)) {
  x <- names(mb_energy[j])
  t <- str_detect(x, "uestionable")
  if(t == FALSE){
    unq_energy <- cbind.data.frame(unq_energy, mb_energy[,j])
    col_num <- ncol(unq_energy)
    names(unq_energy)[col_num] <- x
  }
}
rm(i, j, t, x)

# Last three columns of unq_energy are empty
unq_energy <- unq_energy[,1:37]

# remove NAs from numeric fields of data
temp <- unq_energy[,31:37]
temp[is.na(temp)] <- 0
unq_energy <- cbind.data.frame(unq_energy[,1:30],temp)
rm(temp)


#Replace mpg of diesel > 100, gasoline > 100, cng > 15, and electric battery>20
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

# Some data is INF, replace with zero
index <- c(31:34, 37)
temp <- data.frame()
for (i in index) {
  temp <- unq_energy[, i]
  temp[which(is.infinite(temp))] <- 0
  unq_energy[, i] <- temp
}

# Identify and replace outliers in mpg for each fuel type

# Diesel
temp <- unq_energy[unq_energy$Diesel..mpg.>0.001,31]
# Mean diesel mpg with outliers
mean(temp, na.rm = T)
temp <- temp[temp<30]
diesel.mpg.replacement <- mean(temp)
unq_energy[unq_energy$Diesel..mpg.>30,31] <- diesel.mpg.replacement
unq_energy[unq_energy$Diesel..mpg. < 0.001, 31] <- NA
# Mean diesel mpg without outliers
mean(unq_energy$Diesel..mpg., na.rm = TRUE)

# Gasoline
temp <- unq_energy[unq_energy$Gasoline..mpg.>0.001,32]
# Mean gas mpg with outliers
mean(temp, na.rm = T)
temp <- temp[temp<30]
gas.mpg.replacement <- mean(temp)
unq_energy[unq_energy$Gasoline..mpg.>30,32] <- gas.mpg.replacement
unq_energy[unq_energy$Gasoline..mpg. < 0.001, 32] <- NA
# Mean gas mpg without outliers
mean(unq_energy$Gasoline..mpg., na.rm = T)

# CNG
temp <- unq_energy[unq_energy$Compressed.Natural.Gas..mpg.>0.001,34]
# Mean CNG mpg (diesel equivalent assumed) with outliers
mean(temp, na.rm = T)
temp <- temp[temp < 15]
cng.replacement <- mean(temp)
unq_energy[unq_energy$Compressed.Natural.Gas..mpg.>15,34] <- cng.replacement
unq_energy[unq_energy$Compressed.Natural.Gas..mpg. < 0.001, 34] <- NA
# Mean cng mpg without outliers
mean(unq_energy$Compressed.Natural.Gas..mpg., na.rm = T)

# Electric Battery
temp <- unq_energy[unq_energy$Electric.Battery..mi.kwh.>0.001,37]
# Mean electric mi/kWh with outliers
mean(temp, na.rm = T)
temp <- temp[temp<20]
elec.mpkwh.replacement <- mean(temp)
unq_energy[unq_energy$Electric.Battery..mi.kwh.>20,37] <- elec.mpkwh.replacement
unq_energy[unq_energy$Electric.Battery..mi.kwh. < 0.001, 37] <- NA
# Mean electric mi/kWh without outliers
mean(unq_energy$Electric.Battery..mi.kwh., na.rm = T)

#sum total of gals(or equivalent) from reporting agencies (303)
total_gals_extract <- unq_energy[,c(8,12,14:21)]
total_gals <- as.data.frame(matrix(nrow = 1, ncol = ncol(total_gals_extract)))
for (i in 1:ncol(total_gals_extract)) {
  total_gals[1,i] <- sum(total_gals_extract[,i])
}
names(total_gals) <- names(total_gals_extract)
rm(total_gals_extract)

## Include both full and reduced reporters
full_reporters <- joined_veh_data[joined_veh_data$Reporter.Type.x=="Full Reporter" |
                                    joined_veh_data$Reporter.Type.x=="Reduced Reporter",]
full_reporter_agencies <- unique(full_reporters$NTD.ID) %>% as.numeric()
unq_energy_agencies <- unique(unq_energy$NTD.ID) %>% as.numeric()
agency_comp <- c(full_reporter_agencies, unq_energy_agencies)
agency_comp <- unique(agency_comp)
unq_energy$NTD.ID <- unq_energy$NTD.ID %>% as.integer()
unq_energy$TOS <- unq_energy$TOS %>% str_trim(side = "both")
unq_energy$Modes <- paste0(unq_energy$Mode, "/", unq_energy$TOS)
full_reporters$NTD.ID <- full_reporters$NTD.ID %>% as.integer()
# Merge vehicle data to energy data
bus_energy <- left_join(full_reporters, unq_energy, by=c("NTD.ID", "Modes"))

# Create a table to compare the number of vehicles reported by agencies in different datasets
for (i in 1:length(unq_energy_agencies)) {
  
  temp_table <- bus_energy[bus_energy$NTD.ID==unq_energy_agencies[i],]
  temp_sum <- temp_table$Total.Fleet.Vehicles %>% sum()
  temp_active_sum <- temp_table$Active.Fleet.Vehicles %>% sum()
  x <- cbind.data.frame(temp_table[1,1], 
                        temp_table[1,"ModeTOS.Vehicles.Operated.in.Maximum.Service"], 
                        temp_sum, 
                        temp_active_sum)
  if (i==1){
    comparison_table <- x
  } else{
    comparison_table <- rbind.data.frame(comparison_table, x)
  }
  rm(temp_table, temp_sum, temp_active_sum, x)
}

# Provide names to the table of comparison
names(comparison_table) <- c("NTD.ID", 
                             "Mode.VOMS", 
                             "sum.of.Total.Fleet.Veh", 
                             "sum.of.Active.Fleet.Veh")

# Optional additional analyses for the comparison table
#comparison_table$Total.Difference <- comparison_table$Mode.VOMS-comparison_table$sum.of.Total.Fleet.Veh
#comparison_table$Active.Difference <- comparison_table$Mode.VOMS-comparison_table$sum.of.Active.Fleet.Veh
#mta <- bus_energy[bus_energy$Agency.Name=="MTA New York City Transit",]
#sum(mta$Total.Fleet.Vehicles, na.rm = TRUE)

# number of buses do not add up
# switch to applying given mileage to bus/fuel types

bus_data_fta_2019$Final.Year <- bus_data_fta_2019$Manufacture.Year + bus_data_fta_2019$Useful.Life.Benchmark

# accounting for rebuild year
bus_data_fta_2019[which(bus_data_fta_2019$Rebuild.Year>0),"Final.Year"] <- bus_data_fta_2019[which(bus_data_fta_2019$Rebuild.Year>0),"Rebuild.Year"] + 
  bus_data_fta_2019[which(bus_data_fta_2019$Rebuild.Year>0),"Useful.Life.Benchmark"]

bus_data_fta_2019$Life.Left <- bus_data_fta_2019$Final.Year-2019
x <- nrow(bus_data_fta_2019[bus_data_fta_2019$Life.Left<=0,])
y <- nrow(bus_data_fta_2019)
x/y*100
rm(x, y)
#23% of the fleet is past its life

bus_data_fta_2019_temp <- bus_data_fta_2019[,c(1,2,5,9,14:15,17,21,24:29,54:56,76:77)]

bus_data_fta_2019_temp$Fuel.Type <- bus_data_fta_2019_temp$Fuel.Type %>% as.factor()

bus_data_fta_2019_temp$Fuel.Type %>% summary()
levels(bus_data_fta_2019_temp$Fuel.Type)[levels(bus_data_fta_2019_temp$Fuel.Type)=="Gasoline/Compressed Natural Gas"] <- "Compressed Natural Gas"
levels(bus_data_fta_2019_temp$Fuel.Type)[levels(bus_data_fta_2019_temp$Fuel.Type)=="Diesel Fuel"] <- "Diesel"
#bus_data_fta_2019_temp %>% View()

mpg_data <- unq_energy[,c(1,5,31:34,37:38)]
summary(mpg_data)

# Join bus data with the agency's average mileage data
mpg_data$NTD.ID <- as.character(mpg_data$NTD.ID)
mpg_data$NTD.ID <- str_pad(mpg_data$NTD.ID, width = 5, side = "left", pad = "0")
mpg_data <- unique(mpg_data)
bus_data_fta_2019_temp_1 <- left_join(bus_data_fta_2019_temp, mpg_data, by = c("NTD.ID", "Modes"))
summary(bus_data_fta_2019_temp_1)

# Data before cleaning
hist(bus_data_fta_2019_temp_1$Diesel..mpg., breaks = "FD")
hist(bus_data_fta_2019_temp_1$Electric.Battery..mi.kwh., breaks = "FD")
hist(bus_data_fta_2019_temp_1$Gasoline..mpg., breaks = "FD")
hist(bus_data_fta_2019_temp_1$Compressed.Natural.Gas..mpg., breaks = "FD")
# There seems to be some outliers

# Check to see if distribution of mpg is different between agencies that have hybrids
# versus those who don't
hybrids <- bus_data_fta_2019_temp_1[bus_data_fta_2019_temp_1$Fuel.Type == "Hybrid Diesel", "NTD.ID"] %>% unique()
hybrid_mpg <- mpg_data[mpg_data$NTD.ID %in% hybrids, ]
non_mpg <- mpg_data[!(mpg_data$NTD.ID %in% hybrids), ]
summary(hybrid_mpg$Diesel..mpg.)
summary(non_mpg$Diesel..mpg.)
# The interquartile range of non-hybrid diesel fleets' mpg 
# is greater than those fleets that do have hybrids
# Those that do have them have a higher overall maximum

# Aggregate data to begin replacement cases on
agg_replacement_case <- bus_data_fta_2019_temp_1 %>% 
  group_by(NTD.ID, 
           Agency.Name.x, 
           Modes, 
           Manufacture.Year, 
           Final.Year, 
           Useful.Life.Benchmark, 
           Fuel.Type, 
           City, 
           State, 
           Zip.Code) %>% 
  summarise(Total.Fleet.Vehicles=sum(Total.Fleet.Vehicles, na.rm = TRUE), 
            Active.Fleet.Vehicles=sum(Active.Fleet.Vehicles, na.rm = TRUE), 
            Total.Miles.on.Active.Vehicles.During.Period = sum(Total.Miles.on.Active.Vehicles.During.Period, na.rm = TRUE), 
            Average.Number.of.Seats = mean(Seating.Capacity, na.rm = TRUE), 
            Average.Diesel.mpg = mean(Diesel..mpg., na.rm = TRUE), 
            Average.Gasoline.mpg = mean(Gasoline..mpg., na.rm = TRUE), 
            Average.CNG.mpg = mean(Compressed.Natural.Gas..mpg., na.rm = TRUE), 
            Average.EB.mpkWh = mean(Electric.Battery..mi.kwh., na.rm = TRUE))

agg_replacement_case$Total.Miles.on.Active.Vehicles.During.Period <- agg_replacement_case$Total.Miles.on.Active.Vehicles.During.Period %>% 
  as.numeric()

#length should be 51
summary(agg_replacement_case$State %>% as.factor()) %>% length()
summary(agg_replacement_case$Fuel.Type)
# Will need to get rid of fuel types outside the scope of the analysis

#### Battery price forcast ####

# all battery prices in $/kWh
# Method: Using Argonne National Lab BEAN Model
bean_batt <- data.frame(year = c(2021, 2027, 2035),
                        tech = c(rep("Pessimistic", 3), rep("Medium", 3), rep("Optimistic", 3)),
                        liion.price = c(302, 225, 150, 302, 175, 115, 302, 175, 80))

# Plot the battery price trajectories
bean_pred <- data.frame(year = 2022:2040)
b.value <- 0.858
bean_pred$Medium <- (-127/(b.value^2027 - b.value^2021))*(b.value^bean_pred$year - b.value^2021) + 302
b.pes <- 0.956
bean_pred$Pessimistic <- (-77/(b.pes^2027 - b.pes^2021))*(b.pes^bean_pred$year - b.pes^2021) + 302
b.opt <- 0.920
bean_pred$Optimistic <- (-127/(b.opt^2027 - b.opt^2021))*(b.opt^bean_pred$year - b.opt^2021) + 302

bean_melt <- melt(bean_pred, id.vars = "year")

#### FIGURE: Battery Forcast ####

batt_forcast <- ggplot(data = bean_melt, aes(x = year, y = value)) +
  geom_line(aes(color = variable)) +
  theme_classic() +
  labs(x = "Year", y = str_wrap("Estimated Price of Li-ion High Energy Battery ($/kWh)", width = 40)) +
  scale_x_continuous(breaks = 2021:2040) +
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
                              Power.Type.Code == "EB")

beb_fleet$Amount.Paid.per.Vehicle <-  as.numeric(gsub("\\$", "", as.factor(gsub(",", "", beb_fleet$Amount.Paid.per.Vehicle))))

beb_fleet <- beb_fleet[!is.na(beb_fleet$Amount.Paid.per.Vehicle),]

sum(beb_fleet$Amount.Paid.per.Vehicle * beb_fleet$Total.Vehicles..Number.of) / sum(beb_fleet$Total.Vehicles..Number.of)
# 2021 mean value of approx $745,000
# Range from $618,000 to $952,000

#### Cost of Replacement, parameters ####
# 40' BEB price trend
batt_cap <- 450
beb_2021 <- 745000
no_of_seats <- 40

beb_frame <- beb_2021 - batt_cap * bean_pred[bean_pred$year == 2022, "Medium"]

bus_costs <- cbind.data.frame(year = bean_pred[,1],
                             beb_frame + bean_pred[,2:4] * batt_cap)

seat_cost <- cbind.data.frame(year = bean_pred[,1],
                              bus_costs[,2:4]/no_of_seats)

#### Cost of replacement, bus-method ####
# Include full bus replacement along with seat replacement

full_replace <- agg_replacement_case %>% group_by(Final.Year = Final.Year,
                                                  Agency.Name = Agency.Name.x) %>%
  summarise(total.replace = sum(Total.Fleet.Vehicles),
            active.replace = sum(Active.Fleet.Vehicles),
            avg.no.seats = mean(Average.Number.of.Seats))

# Assign year final year of buses as the year they are to be replaced
full_replace$year.replace <- full_replace$Final.Year

# If the buses are old, let them be replaced in 2022
full_replace[full_replace$year.replace < 2022, "year.replace"] <- 2022

# Join number of buses to the price of the buses the year they are replaced
full_replace <- full_join(full_replace, bus_costs, by=c("year.replace" = "year"))
full_replace <- full_join(full_replace, seat_cost, by=c("year.replace" = "year"))

# Total cost of replacement, bus method
full_replace$total.bus.med <- full_replace$total.replace * full_replace$Medium.x
full_replace$total.bus.pes <- full_replace$total.replace * full_replace$Pessimistic.x
full_replace$total.bus.opt <- full_replace$total.replace * full_replace$Optimistic.x

# Active cost of replacement, bus method
full_replace$active.bus.med <- full_replace$active.replace * full_replace$Medium.x
full_replace$active.bus.pes <- full_replace$active.replace * full_replace$Pessimistic.x
full_replace$active.bus.opt <- full_replace$active.replace * full_replace$Optimistic.x

# Total cost of replacement, seat method
full_replace$total.seat.med <- full_replace$total.replace * full_replace$Medium.y * full_replace$avg.no.seats
full_replace$total.seat.pes <- full_replace$total.replace * full_replace$Pessimistic.y * full_replace$avg.no.seats
full_replace$total.seat.opt <- full_replace$total.replace * full_replace$Optimistic.y * full_replace$avg.no.seats

# Active cost of replacement, seat method
full_replace$active.seat.med <- full_replace$active.replace * full_replace$Medium.y * full_replace$avg.no.seats
full_replace$active.seat.pes <- full_replace$active.replace * full_replace$Pessimistic.y * full_replace$avg.no.seats
full_replace$active.seat.opt <- full_replace$active.replace * full_replace$Optimistic.y * full_replace$avg.no.seats

# Cost of replacement by year
year_summary_replace <- full_replace %>% 
  group_by(year.replace) %>% 
  summarise(total.replace = sum(total.replace), 
            active.replace = sum(active.replace),
            avg.no.seats = mean(avg.no.seats),
            total.bus.med = sum(total.bus.med), 
            total.bus.opt = sum(total.bus.opt), 
            total.bus.pes = sum(total.bus.pes), 
            active.bus.med = sum(active.bus.med), 
            active.bus.opt = sum(active.bus.opt), 
            active.bus.pes = sum(active.bus.pes),
            total.seat.med = sum(total.seat.med), 
            total.seat.opt = sum(total.seat.opt), 
            total.seat.pes = sum(total.seat.pes), 
            active.seat.med = sum(active.seat.med), 
            active.seat.opt = sum(active.seat.opt), 
            active.seat.pes = sum(active.seat.pes))

year_summary_replace$total.bus.immed <- year_summary_replace$total.replace * bus_costs$Medium[bus_costs$year == 2022]
year_summary_replace$active.bus.immed <- year_summary_replace$active.replace * bus_costs$Medium[bus_costs$year == 2022]
year_summary_replace$total.seat.immed <- year_summary_replace$total.replace * seat_cost$Medium[seat_cost$year == 2022] * year_summary_replace$avg.no.seats
year_summary_replace$active.seat.immed <- year_summary_replace$active.replace * seat_cost$Medium[seat_cost$year == 2022] * year_summary_replace$avg.no.seats

summary_replace <- colSums(year_summary_replace[,c(2:3, 5:20)], na.rm = TRUE) %>% as.data.frame()
# Cost of replacement in $Billion
summary_replace[3:18,1] <- summary_replace[3:18,1]/1000000000

#### Fuel Data ####

# Check to see how many agencies are missing fuel mileage data
fuel_values <- rowSums(agg_replacement_case[, 15:18], na.rm = T)
agency_fuel_check <- cbind.data.frame(agg_replacement_case$Agency.Name.x, fuel_values)
names(agency_fuel_check) <- c("Agency.name", "fuel.check")
agency_fuel_check <- agency_fuel_check %>% 
  group_by(Agency.name) %>% 
  summarise(fuel.check = sum(fuel.check))
agency_fuel_check[agency_fuel_check$fuel.check==0,] %>% nrow()
#319 agencies missing all fuel data

#create average values while ignoring NA and zeros
temp <- agg_replacement_case[agg_replacement_case$Average.Diesel.mpg>0,
                               "Average.Diesel.mpg"]
summary(temp)
diesel.replace <- colMeans(temp, na.rm = T)
agg_replacement_case[is.na(agg_replacement_case$Average.Diesel.mpg),
                       "Average.Diesel.mpg"] <- diesel.replace


temp <- agg_replacement_case[agg_replacement_case$Average.Gasoline.mpg>0,
                               "Average.Gasoline.mpg"]
summary(temp)
gas.replace <- colMeans(temp, na.rm = T)
agg_replacement_case[is.na(agg_replacement_case$Average.Gasoline.mpg),
                       "Average.Gasoline.mpg"] <- gas.replace


#LPG has no mileage data#
temp <- agg_replacement_case[agg_replacement_case$Average.CNG.mpg>0,
                               "Average.CNG.mpg"]
summary(temp)
cng.replace <- colMeans(temp, na.rm = T)
agg_replacement_case[is.na(agg_replacement_case$Average.CNG.mpg),
                       "Average.CNG.mpg"] <- cng.replace


temp <- agg_replacement_case[agg_replacement_case$Average.EB.mpkWh>0,
                               "Average.EB.mpkWh"]
summary(temp)
eb.replace <- colMeans(temp, na.rm = T)
agg_replacement_case[is.na(agg_replacement_case$Average.EB.mpkWh),
                       "Average.EB.mpkWh"] <- eb.replace

summary(agg_replacement_case[, 15:18])

#### eGRID Data ####

# Double check that the zip code is in a proper form for joining
summary(agg_replacement_case$Zip.Code)

#View(egrid)
#View(egrid_zip)

# We will only focus on subregion 1 for each zip code area
# so we can omit regions 2 and 3 (columns 5 & 6)
egrid_zip_simple <- egrid_zip[,2:4]
egrid_zip_simple$ZIP..numeric. <- str_pad(egrid_zip_simple$ZIP..numeric., 
                                          width = 5, 
                                          side = "left", 
                                          pad = 0)

# "egrid" is actual subregion emission data
# "egrid_zip_simple" is to join the subregions to zip codes
egrid_emissions <- left_join(egrid, egrid_zip_simple, 
                             by = c ("ï..eGRID.subregion.acronym" = "eGRID.Subregion..1"))
egrid_emissions$ZIP..numeric. <- egrid_emissions$ZIP..numeric. %>% as.double()

agg_replacement_case$Zip.Code <- str_pad(agg_replacement_case$Zip.Code, 
                                         width = 5,
                                         side = "left", 
                                         pad = 0) %>% 
  as.double()

agg_replacement_case <- left_join(agg_replacement_case,
                                  egrid_emissions,
                                  by = c("Zip.Code" = "ZIP..numeric."))
summary(agg_replacement_case$Fuel.Type)
#We only have fuel emission data for Gasoline, Diesel, and CNG

replacement_case_1 <- agg_replacement_case[, c(1:18, 26:31)]
summary(replacement_case_1)

#### Beginning of adding emission data ####
# Electricity efficiency values
elec_transm_dist_eff <- 0.95
plug_eff <- 0.85 
  
# Add GHG and Air emission data
ch4_gwp <- 30
n2o_gwp <- 265

#### Add EMFAC data ####
emfac$Fuel <- as.factor(emfac$Fuel)
levels(emfac$Fuel)[levels(emfac$Fuel) == "Natural Gas"] <- "Compressed Natural Gas"

emfac_compare <- replacement_case_1 %>% 
  group_by(Manufacture.Year, Fuel.Type) %>% 
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles))
# Rough estimates:
# Need 1999 cng, 
#      1998 cng, 
#      1997 diesel, 
#      1996 electric, 
#      1990:1995 diesel emfacs

emfac_melt <- melt(emfac, id.vars = c("Model.Year", "Fuel"))

variables <- unique(emfac_melt$variable)

# Plot the EMFAC data so we can see trends over time
emfac_plot_names <- data.frame()
for (i in 1:length(variables)) {
  
  name_assign <- paste0("plot_", i)
  temp_table <- emfac_melt[emfac_melt$variable == variables[i],]
  
  a <- ggplot(data = temp_table, 
              mapping = aes(x = Model.Year, y = value)) + 
    geom_line(mapping = aes(color = Fuel)) + 
    theme_classic() +
    labs(x = "Model Year", y = paste0(variables[i], " (grams/mi)"))
  
  assign(name_assign, a)
  
  emfac_plot_names <- rbind.data.frame(emfac_plot_names, name_assign)
  
}

# We are considering a lot of emission types
# Plots are split up to see more easily
#(plot_1 + plot_2)/(plot_3 +plot_4)

#(plot_5 + plot_6)/(plot_7 + plot_8)

#(plot_9 + plot_10)/(plot_11 + plot_12 + plot_13)
# Seems like there is no data for BEBs made in 2000 or 2001.


# Since it seems like there is more data missing than we originally thought,
# We will create a list of where emission rates are zero
emfac_compare <- as.data.frame(emfac_compare)

levels(emfac_compare$Fuel.Type)[levels(emfac_compare$Fuel.Type) == "Electric Battery"] <- "Electricity"

# Initialize a dataset
replace_needed <- tibble(Manufacture.Year = 0,
                         Fuel.Type = NA,
                         Emission.Missing = as.character(0))


for (i in 1:nrow(emfac_compare)) {
  # Consider one row at a time by looking at fuel type and manufacture year
  fuel <- emfac_compare[i, "Fuel.Type"] %>% as.character()
  print(fuel)
  year <- emfac_compare[i, "Manufacture.Year"] %>% as.numeric()
  print(year)
  
  # Check if all emission types for each year and fuel type are present in the emfac dataset
  emfac_subset <- filter(emfac, Fuel == fuel & Model.Year == year)
  print("subset complete")
  print(nrow(emfac_subset))
  
  # If this subset is empty, then all emissions must be added to missing dataset
  if (nrow(emfac_subset) == 0) {
    
    print("zero rows identified")
    replace_needed <- add_row(replace_needed, Manufacture.Year = year, Fuel.Type = fuel, Emission.Missing = "all")
    
  } else {
    
    print("more than zero rows identified")
    
    if((which(emfac_subset[3:15] == 0) %>% sum()) > 0) {
      
      missing_emissions <- variables[which(emfac_subset[3:15] == 0)]
      
      # Otherwise, just add individual emission names to missing dataset
      for (j in 1:length(missing_emissions)) {
        
        print("Some missing values identified")
        
        replace_needed <- add_row(replace_needed, 
                                  Manufacture.Year = year, 
                                  Fuel.Type = fuel, 
                                  Emission.Missing = (missing_emissions[j] %>% as.character()))
        
      }
      
    } else { 
      
      # Otherwise, all data necessary is present
      print("No zeros identified") 
      
    }
  }
  
}

# Place data in a new table incase an error occurs so we don't need to rerun the loops
replace_needed_1 <- replace_needed

# Some data points are supposed to be zero
# Such as any tailpipe emission from BEB's
replace_needed_1 <- filter(replace_needed_1, !Fuel.Type == "Electricity" | !str_detect(Emission.Missing, "RUNEX"))
# And SOx emissions from CNG
replace_needed_1 <- filter(replace_needed_1, !Fuel.Type == "Compressed Natural Gas" | !Emission.Missing == "SOx_RUNEX")

# Attempt to estimate missing diesel values with an lm model since there are so many years of data missing
#emfac_diesel <- filter(emfac, Fuel == "Diesel")

# Years of data missing
diesel_missing <- c(1990, 1992:1995, 1997)
# for (i in 3:15) {
#   
#   # Print name of emission we are estimating
#   print(names(emfac_diesel)[i])
#   
#   # Estimate the linear model for each emission type
#   temp_lm <- lm(emfac_diesel[,i] ~ emfac_diesel[, 1])
#   
#   for (j in diesel_missing) {
#     
#     # Estimate the missing data points
#     add_diesel <- temp_lm$coefficients[1] + temp_lm$coefficients[2] * j
#     
#     # Create a partial entry for each year
#     temp <- cbind.data.frame(j, "Diesel", add_diesel)
#     
#     if (j == 1990) {
#       
#       
#       temp_col <- temp
#       
#     } else {
#       
#       # Merge all years' worth of data
#       temp_col <- rbind.data.frame(temp_col, temp)
#       
#     }
#     
#   }
#   
#   names(temp_col) <- c("Model.Year", "Fuel", names(emfac_diesel)[i])
#   
#   if (i == 3) {
#     
#     temp_full <- temp_col
#     
#   } else {
#     
#     # Merge all emission types by model year and fuel type
#     temp_full <- full_join(temp_full, temp_col, by = c("Model.Year", "Fuel"))
#     
#   }
# }

# Add all generated data to given diesel data
#emfac_diesel <- rbind.data.frame(emfac_diesel, temp_full)


# Going to estimate missing data points in a second way
emfac_diesel_2 <- filter(emfac, Fuel == "Diesel")

# Replace all missing diesel data with those from 1996 model year
emfac_diesel_replace <- filter(emfac_diesel_2, Model.Year == 1996)

for (i in diesel_missing) {
  
  # Using the same emission data, replace with a model year we are missing
  emfac_diesel_replace$Model.Year <- i
  
  # Add this new data point
  emfac_diesel_2 <- rbind.data.frame(emfac_diesel_2, emfac_diesel_replace)
  
}


# Fill in missing Natural Gas
emfac_natgas <- filter(emfac, Fuel == "Compressed Natural Gas")

# Use model year 1999 to replace the emissions for the 1998 vehicles
emfac_natgas_add <- filter(emfac_natgas, Model.Year == 1999)

emfac_natgas_add$Model.Year <- 1998

# Add this new data point to the dataset
emfac_natgas <- rbind.data.frame(emfac_natgas, emfac_natgas_add)

# Fill in missing gasoline data
emfac_gas <- filter(emfac, Fuel == "Gasoline")

# Use model year 2000 to replace the emission for the 1999 vehicles
emfac_gas_add <- filter(emfac_gas, Model.Year == 2000)

emfac_gas_add$Model.Year <- 1999

# Add this new data point to the gas data
emfac_gas <- rbind.data.frame(emfac_gas, emfac_gas_add)

# Fill in missing electricity data
emfac_elec <- filter(emfac, Fuel == "Electricity")

# Missing electricity emission data for 1996 & 2001, replace with 1999
emfac_elec_add <- filter(emfac_elec, Model.Year == 1999)

emfac_elec_add$Model.Year <- 1996

emfac_elec <- rbind.data.frame(emfac_elec, emfac_elec_add)

emfac_elec_add$Model.Year <- 2001

emfac_elec <- rbind.data.frame(emfac_elec, emfac_elec_add)

# Recombine all emfac fuel types
# This is the version of the emfac where all the missing diesel
# is replaced by estimates from a linear regression model
#emfac_fixed <- rbind.data.frame(emfac_diesel, emfac_natgas, emfac_gas, emfac_elec)

# This is the version of the emfac where all the missing diesel
# is replaced by the data from model year 1996
emfac_fixed <- rbind.data.frame(emfac_diesel_2, emfac_natgas, emfac_gas, emfac_elec)
# Tailpipe emissions from electric vehicles are zero but dataset includes PM from tire and break wear
# Units: g/mile for RUNEX, PMBW and PMTW
# ttw = tank to wheel (emissions)

# Since EMFAC Emissions are given as grams/mi
# we will use the average CA transit bus fuel economy to 
#convert this to grams/gallon so we can apply it to hybrid vehicles as well

#### Calculate average CARB fuel economy####

# Fuel.Consuption in 1,000 gallons/year
carb_eco <- fuel_eco[, c("Fuel", "Total.VMT", "Fuel.Consumption")]
carb_eco$miles.p.gal <- carb_eco$Total.VMT / (carb_eco$Fuel.Consumption * 1000)
carb_eco[carb_eco$Fuel == "Natural Gas", "Fuel"] <- "Compressed Natural Gas"
summary(carb_eco$miles.p.gal)

emfac_fixed %>% names()

# Multiply "running exhaust emssions" by fuel economy to get units of grams/gallon
emfac_fixed_temp <- left_join(emfac_fixed, carb_eco, by="Fuel")
emfac_fixed_temp[,str_detect(names(emfac_fixed_temp), "RUNEX")] <- emfac_fixed_temp[, str_detect(names(emfac_fixed_temp), "RUNEX")] * 
  emfac_fixed_temp[, "miles.p.gal"]
emfac_fixed <- emfac_fixed_temp[,1:15]
rm(emfac_fixed_temp)
# NA's resulted for EB fuel type, replace with zeros
emfac_fixed[is.na(emfac_fixed)] <- 0
emfac_fixed[emfac_fixed$Fuel == "Electricity", "Fuel"] <- "Electric Battery" 

# ..g.gal = grams/gallon
# ..g.mi = grams/mile
names(emfac_fixed)[str_detect(names(emfac_fixed), "RUNEX")] <- paste0("fuel.ttw..", names(emfac_fixed)[str_detect(names(emfac_fixed), "RUNEX")], "..g.gal")
names(emfac_fixed)[str_detect(names(emfac_fixed), "PMTW|PMBW")] <- paste0("fuel.ttw..", names(emfac_fixed)[str_detect(names(emfac_fixed), "PMTW|PMBW")], "..g.mi")
summary(emfac_fixed)

summary(batt_emissions_raw)
batt_emissions <- t(batt_emissions_raw) %>% as.data.frame()
names(batt_emissions) <- batt_emissions[1,]
batt_emissions <- batt_emissions[3,]
names(batt_emissions) <- names(batt_emissions) %>% str_trim()
# ..g.kWh = grams/kWh
names(batt_emissions)[1:13] <- paste0("batt..", names(batt_emissions)[1:13], "..g.kWh")
batt_emissions[,1:13] <- lapply(batt_emissions[,1:13], as.numeric)

welltotank <- welltotank_raw %>% na.omit()
# wtt = well to tank (emissions)
names(welltotank)[2:12] <- paste0("fuel.wtt..", names(welltotank)[2:12])

# Need a field for the combusted fuel source in 
# hybrid vehicles so we can join their emission factors
replacement_case_1$cFuel.Type <- replacement_case_1$Fuel.Type
levels(replacement_case_1$cFuel.Type)[levels(replacement_case_1$cFuel.Type) == "Hybrid Diesel"] <- "Diesel"
levels(replacement_case_1$cFuel.Type)[levels(replacement_case_1$cFuel.Type) == "Hybrid Gasoline"] <- "Gasoline"

replace_case_1 <- left_join(replacement_case_1, emfac_fixed, by = c("cFuel.Type" = "Fuel", "Manufacture.Year" = "Model.Year"))
replace_case_1 <- left_join(replace_case_1, welltotank, by = c("cFuel.Type" = "ï..Fuel.Type"))
replace_case_1 <- cbind(replace_case_1, batt_emissions)

#### First Scenario: Immediate replacement, bus-to-bus method ####

#Change egrid variables to g/kwh
replace_case_1$eGRID.non.baseload.NOx..g.kWh <- replace_case_1$eGRID.subregion.annual.NOx.non.baseload.output.emission.rate..lb.MWh. * 453.592 / 1000
replace_case_1$eGRID.non.baseload.SO2..g.kWh <- replace_case_1$eGRID.subregion.annual.SO2.non.baseload.output.emission.rate..lb.MWh. * 453.592 / 1000
replace_case_1$eGRID.non.baseload.CO2e..g.kWh <- replace_case_1$eGRID.subregion.annual.CO2e.non.baseload.output.emission.rate..lb.MWh. * 453.592 / 1000

#Combine fuel emissions to make them "well-to-wheel" emissions
replace_case_1$fuel.wtt..VOC..g.mi <- replace_case_1$fuel.wtt..VOC..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.wtt..SOx..g.mi <- replace_case_1$fuel.wtt..SOx..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.wtt..PM2.5..g.mi <- replace_case_1$fuel.wtt..PM2.5..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.wtt..PM10..g.mi <- replace_case_1$fuel.wtt..PM10..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.wtt..OC..g.mi <- replace_case_1$fuel.wtt..OC..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.wtt..BC..g.mi <- replace_case_1$fuel.wtt..BC..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.wtt..CO..g.mi <- replace_case_1$fuel.wtt..CO..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.wtt..NOx..g.mi <- replace_case_1$fuel.wtt..NOx..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.wtt..CH4..g.mi <- replace_case_1$fuel.wtt..CH4..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.wtt..N2O..g.mi <- replace_case_1$fuel.wtt..N2O..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.wtt..CO2..g.mi <- replace_case_1$fuel.wtt..CO2..g.gal / replace_case_1$Average.Diesel.mpg

replace_case_1$fuel.ttw..NOx_RUNEX..g.mi <- replace_case_1$fuel.ttw..NOx_RUNEX..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.ttw..CH4_RUNEX..g.mi <- replace_case_1$fuel.ttw..CH4_RUNEX..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.ttw..CO2_RUNEX..g.mi <- replace_case_1$fuel.ttw..CO2_RUNEX..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.ttw..CO_RUNEX..g.mi <- replace_case_1$fuel.ttw..CO_RUNEX..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.ttw..N2O_RUNEX..g.mi <- replace_case_1$fuel.ttw..N2O_RUNEX..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.ttw..PM10_RUNEX..g.mi <- replace_case_1$fuel.ttw..PM10_RUNEX..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.ttw..PM2.5_RUNEX..g.mi <- replace_case_1$fuel.ttw..PM2.5_RUNEX..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.ttw..ROG_RUNEX..g.mi <- replace_case_1$fuel.ttw..ROG_RUNEX..g.gal / replace_case_1$Average.Diesel.mpg
replace_case_1$fuel.ttw..SOx_RUNEX..g.mi <- replace_case_1$fuel.ttw..SOx_RUNEX..g.gal / replace_case_1$Average.Diesel.mpg

for (i in 1:nrow(replace_case_1)) {
  
  if (replace_case_1$cFuel.Type[i] == "Gasoline"){
    
    replace_case_1$fuel.wtt..VOC..g.mi[i] <- replace_case_1$fuel.wtt..VOC..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.wtt..SOx..g.mi[i] <- replace_case_1$fuel.wtt..SOx..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.wtt..PM2.5..g.mi[i] <- replace_case_1$fuel.wtt..PM2.5..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.wtt..PM10..g.mi[i] <- replace_case_1$fuel.wtt..PM10..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.wtt..OC..g.mi[i] <- replace_case_1$fuel.wtt..OC..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.wtt..BC..g.mi[i] <- replace_case_1$fuel.wtt..BC..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.wtt..CO..g.mi[i] <- replace_case_1$fuel.wtt..CO..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.wtt..NOx..g.mi[i] <- replace_case_1$fuel.wtt..NOx..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.wtt..CH4..g.mi[i] <- replace_case_1$fuel.wtt..CH4..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.wtt..N2O..g.mi[i] <- replace_case_1$fuel.wtt..N2O..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.wtt..CO2..g.mi[i] <- replace_case_1$fuel.wtt..CO2..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    
    replace_case_1$fuel.ttw..NOx_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..NOx_RUNEX..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.ttw..CH4_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..CH4_RUNEX..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.ttw..CO2_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..CO2_RUNEX..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.ttw..CO_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..CO_RUNEX..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.ttw..N2O_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..N2O_RUNEX..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.ttw..PM10_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..PM10_RUNEX..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.ttw..PM2.5_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..PM2.5_RUNEX..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.ttw..ROG_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..ROG_RUNEX..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    replace_case_1$fuel.ttw..SOx_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..SOx_RUNEX..g.gal[i] / replace_case_1$Average.Gasoline.mpg[i]
    
  } else if (replace_case_1$cFuel.Type[i] == "Compressed Natural Gas") {
    
    replace_case_1$fuel.wtt..VOC..g.mi[i] <- replace_case_1$fuel.wtt..VOC..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.wtt..SOx..g.mi[i] <- replace_case_1$fuel.wtt..SOx..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.wtt..PM2.5..g.mi[i] <- replace_case_1$fuel.wtt..PM2.5..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.wtt..PM10..g.mi[i] <- replace_case_1$fuel.wtt..PM10..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.wtt..OC..g.mi[i] <- replace_case_1$fuel.wtt..OC..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.wtt..BC..g.mi[i] <- replace_case_1$fuel.wtt..BC..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.wtt..CO..g.mi[i] <- replace_case_1$fuel.wtt..CO..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.wtt..NOx..g.mi[i] <- replace_case_1$fuel.wtt..NOx..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.wtt..CH4..g.mi[i] <- replace_case_1$fuel.wtt..CH4..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.wtt..N2O..g.mi[i] <- replace_case_1$fuel.wtt..N2O..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.wtt..CO2..g.mi[i] <- replace_case_1$fuel.wtt..CO2..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    
    replace_case_1$fuel.ttw..NOx_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..NOx_RUNEX..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.ttw..CH4_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..CH4_RUNEX..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.ttw..CO2_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..CO2_RUNEX..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.ttw..CO_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..CO_RUNEX..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.ttw..N2O_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..N2O_RUNEX..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.ttw..PM10_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..PM10_RUNEX..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.ttw..PM2.5_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..PM2.5_RUNEX..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.ttw..ROG_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..ROG_RUNEX..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    replace_case_1$fuel.ttw..SOx_RUNEX..g.mi[i] <- replace_case_1$fuel.ttw..SOx_RUNEX..g.gal[i] / replace_case_1$Average.CNG.mpg[i]
    
  } else if (replace_case_1$cFuel.Type[i] == "Electric Battery"){
    
    # electric equivalent is the emissions from electricity generation
    # encapsulated in the elecgen emission below
    # Make all zeros to avoid error from adding NAs
    
    replace_case_1$fuel.wtt..VOC..g.mi[i] <- 0
    replace_case_1$fuel.wtt..SOx..g.mi[i] <- 0
    replace_case_1$fuel.wtt..PM2.5..g.mi[i] <- 0
    replace_case_1$fuel.wtt..PM10..g.mi[i] <- 0
    replace_case_1$fuel.wtt..OC..g.mi[i] <- 0
    replace_case_1$fuel.wtt..BC..g.mi[i] <- 0
    replace_case_1$fuel.wtt..CO..g.mi[i] <- 0
    replace_case_1$fuel.wtt..NOx..g.mi[i] <- 0
    replace_case_1$fuel.wtt..CH4..g.mi[i] <- 0
    replace_case_1$fuel.wtt..N2O..g.mi[i] <- 0
    replace_case_1$fuel.wtt..CO2..g.mi[i] <- 0
    
    replace_case_1$fuel.ttw..NOx_RUNEX..g.mi[i] <- 0
    replace_case_1$fuel.ttw..CH4_RUNEX..g.mi[i] <- 0
    replace_case_1$fuel.ttw..CO2_RUNEX..g.mi[i] <- 0
    replace_case_1$fuel.ttw..CO_RUNEX..g.mi[i] <- 0
    replace_case_1$fuel.ttw..N2O_RUNEX..g.mi[i] <- 0
    replace_case_1$fuel.ttw..PM10_RUNEX..g.mi[i] <- 0
    replace_case_1$fuel.ttw..PM2.5_RUNEX..g.mi[i] <- 0
    replace_case_1$fuel.ttw..ROG_RUNEX..g.mi[i] <- 0
    replace_case_1$fuel.ttw..SOx_RUNEX..g.mi[i] <- 0
    
    
  }
}

replace_case_1$fuel..NOx..g.mi <- replace_case_1$fuel.wtt..NOx..g.mi + replace_case_1$fuel.ttw..NOx_RUNEX..g.mi
replace_case_1$fuel..CH4..g.mi <- replace_case_1$fuel.ttw..CH4_RUNEX..g.mi + replace_case_1$fuel.wtt..CH4..g.mi
replace_case_1$fuel..CO2..g.mi <- replace_case_1$fuel.ttw..CO2_RUNEX..g.mi + replace_case_1$fuel.wtt..CO2..g.mi
replace_case_1$fuel..CO..g.mi <- replace_case_1$fuel.ttw..CO_RUNEX..g.mi + replace_case_1$fuel.wtt..CO..g.mi
replace_case_1$fuel..N2O..g.mi <- replace_case_1$fuel.ttw..N2O_RUNEX..g.mi + replace_case_1$fuel.wtt..N2O..g.mi
replace_case_1$fuel..PM10..g.mi <- replace_case_1$fuel.ttw..PM10_RUNEX..g.mi + replace_case_1$fuel.ttw..PM10_PMTW..g.mi + replace_case_1$fuel.ttw..PM10_PMBW..g.mi + replace_case_1$fuel.wtt..PM10..g.mi
replace_case_1$fuel..PM2.5..g.mi <- replace_case_1$fuel.ttw..PM2.5_RUNEX..g.mi + replace_case_1$fuel.ttw..PM2.5_PMTW..g.mi + replace_case_1$fuel.ttw..PM2.5_PMBW..g.mi + replace_case_1$fuel.wtt..PM2.5..g.mi
replace_case_1$fuel..VOC..g.mi <- replace_case_1$fuel.wtt..VOC..g.mi + replace_case_1$fuel.ttw..ROG_RUNEX..g.mi
replace_case_1$fuel..SOx..g.mi <- replace_case_1$fuel.ttw..SOx_RUNEX..g.mi + replace_case_1$fuel.wtt..SOx..g.mi
replace_case_1$fuel..BC..g.mi <- replace_case_1$fuel.wtt..BC..g.mi
replace_case_1$fuel..OC..g.mi <- replace_case_1$fuel.wtt..OC..g.mi

beb_replace <- replace_case_1[replace_case_1$Fuel.Type == "Electric Battery", c("fuel..NOx..g.mi",
                                                                                "fuel..CH4..g.mi",
                                                                                "fuel..CO2..g.mi",
                                                                                "fuel..CO..g.mi",
                                                                                "fuel..N2O..g.mi",
                                                                                "fuel..PM10..g.mi",
                                                                                "fuel..PM2.5..g.mi",
                                                                                "fuel..VOC..g.mi",
                                                                                "fuel..SOx..g.mi",
                                                                                "fuel..BC..g.mi",
                                                                                "fuel..OC..g.mi")]
beb_replace <- unique(beb_replace)
beb_replace <- filter(beb_replace, fuel..PM10..g.mi > 0)
beb_replace <- colMeans(beb_replace) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame()


# Reintroduce NA's into average EB mileage to make calculations easier
for (i in 1:nrow(replace_case_1)) {
  if (replace_case_1$Fuel.Type[i] != "Electric Battery") {
    replace_case_1$Average.EB.mpkWh[i] <- NA
  }
}

# Rearrage dataframe
replace_case_1 <- replace_case_1[, c("NTD.ID",
                                     "Agency.Name.x",
                                     "Modes",
                                     "Manufacture.Year",
                                     "Final.Year",
                                     "Useful.Life.Benchmark",
                                     "Fuel.Type",
                                     "cFuel.Type",
                                     "City",
                                     "State",
                                     "Zip.Code",
                                     "Total.Fleet.Vehicles",
                                     "Active.Fleet.Vehicles",
                                     "Total.Miles.on.Active.Vehicles.During.Period",
                                     "Average.Number.of.Seats",
                                     "Average.Diesel.mpg",
                                     "Average.Gasoline.mpg",
                                     "Average.CNG.mpg",
                                     "Average.EB.mpkWh",
                                     "batt..VOC..g.kWh",
                                     "batt..CO..g.kWh",
                                     "batt..NOx..g.kWh",
                                     "batt..PM10..g.kWh",
                                     "batt..PM2.5..g.kWh",
                                     "batt..SOx..g.kWh",
                                     "batt..BC..g.kWh",
                                     "batt..OC..g.kWh",
                                     "batt..GHGs..g.kWh",
                                     "eGRID.non.baseload.NOx..g.kWh",
                                     "eGRID.non.baseload.SO2..g.kWh",
                                     "eGRID.non.baseload.CO2e..g.kWh",
                                     "fuel.wtt..VOC..g.mi",
                                     "fuel.wtt..SOx..g.mi",
                                     "fuel.wtt..PM2.5..g.mi",
                                     "fuel.wtt..PM10..g.mi",
                                     "fuel.wtt..OC..g.mi",
                                     "fuel.wtt..BC..g.mi",
                                     "fuel.wtt..CO..g.mi",
                                     "fuel.wtt..NOx..g.mi",
                                     "fuel.wtt..CH4..g.mi",
                                     "fuel.wtt..N2O..g.mi",
                                     "fuel.wtt..CO2..g.mi",
                                     "fuel.ttw..NOx_RUNEX..g.mi",
                                     "fuel.ttw..CH4_RUNEX..g.mi",
                                     "fuel.ttw..CO2_RUNEX..g.mi",
                                     "fuel.ttw..CO_RUNEX..g.mi",
                                     "fuel.ttw..N2O_RUNEX..g.mi",
                                     "fuel.ttw..PM10_RUNEX..g.mi",
                                     "fuel.ttw..PM2.5_RUNEX..g.mi",
                                     "fuel.ttw..ROG_RUNEX..g.mi",
                                     "fuel.ttw..SOx_RUNEX..g.mi",
                                     "fuel.ttw..PM2.5_PMTW..g.mi",
                                     "fuel.ttw..PM2.5_PMBW..g.mi",
                                     "fuel.ttw..PM10_PMTW..g.mi",
                                     "fuel.ttw..PM10_PMBW..g.mi",
                                     "fuel..NOx..g.mi",
                                     "fuel..CH4..g.mi",
                                     "fuel..CO2..g.mi",
                                     "fuel..CO..g.mi",
                                     "fuel..N2O..g.mi",
                                     "fuel..PM10..g.mi",
                                     "fuel..PM2.5..g.mi",
                                     "fuel..VOC..g.mi",
                                     "fuel..SOx..g.mi",
                                     "fuel..BC..g.mi",
                                     "fuel..OC..g.mi")]

# Assume a battery capacity (kWh) for all (Assigned in "Cost of Replacement, parameters")
# current BEBs and future replacement BEBs
batt_cap
hybrid_cap <- 10

# Hybrid batteries are not powered by the power grid
elec_eff_total <- elec_transm_dist_eff * plug_eff

# Assume how many years the fossil fuel buses will be used for
assum_bus_life <- 14

# How many times will batteries need to be 
# replaced throughout the BEB's lifetime
batt_replace <- 1

replace_case_1$elecgen_NOx..mt <- (replace_case_1$eGRID.non.baseload.NOx..g.kWh / 
                                     replace_case_1$Average.EB.mpkWh * 
                                     replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * 
                                     assum_bus_life /
                                     elec_eff_total) / 1000000
replace_case_1$elecgen_SOx..mt <- (replace_case_1$eGRID.non.baseload.SO2..g.kWh / 
                                     replace_case_1$Average.EB.mpkWh * 
                                     replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * 
                                     assum_bus_life /
                                     elec_eff_total) / 1000000
replace_case_1$elecgen_CO2e..mt <- (replace_case_1$eGRID.non.baseload.CO2e..g.kWh / 
                                      replace_case_1$Average.EB.mpkWh * 
                                      replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * 
                                      assum_bus_life /
                                      elec_eff_total) / 1000000

replace_case_1$batt_VOC..mt <- 0
replace_case_1$batt_CO..mt <- 0
replace_case_1$batt_NOx..mt <- 0
replace_case_1$batt_PM10..mt <- 0
replace_case_1$batt_PM2.5..mt <- 0
replace_case_1$batt_SOx..mt <- 0
replace_case_1$batt_BC..mt <- 0
replace_case_1$batt_OC..mt <- 0
replace_case_1$batt_CO2e..mt <- 0

for (i in 1:nrow(replace_case_1)) {
  if (replace_case_1[i, "Fuel.Type"] == "Electric Battery"){
    
    replace_case_1[i, "batt_VOC..mt"] <- ((replace_case_1[i, "batt..VOC..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_CO..mt"] <- ((replace_case_1[i, "batt..CO..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_NOx..mt"] <- ((replace_case_1[i, "batt..NOx..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_PM10..mt"] <- ((replace_case_1[i, "batt..PM10..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_PM2.5..mt"] <- ((replace_case_1[i, "batt..PM2.5..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_SOx..mt"] <- ((replace_case_1[i, "batt..SOx..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_BC..mt"] <- ((replace_case_1[i, "batt..BC..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_OC..mt"] <- ((replace_case_1[i, "batt..OC..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_CO2e..mt"] <- ((replace_case_1[i, "batt..GHGs..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
 
  } else if (replace_case_1[i, "Fuel.Type"] == "Hybrid Gasoline") {
    
    replace_case_1[i, "batt_VOC..mt"] <- (replace_case_1[i, "batt..VOC..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_CO..mt"] <- (replace_case_1[i, "batt..CO..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_NOx..mt"] <- (replace_case_1[i, "batt..NOx..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_PM10..mt"] <- (replace_case_1[i, "batt..PM10..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_PM2.5..mt"] <- (replace_case_1[i, "batt..PM2.5..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_SOx..mt"] <- (replace_case_1[i, "batt..SOx..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_BC..mt"] <- (replace_case_1[i, "batt..BC..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_OC..mt"] <- (replace_case_1[i, "batt..OC..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_CO2e..mt"] <- (replace_case_1[i, "batt..GHGs..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    
  } else if (replace_case_1[i, "Fuel.Type"] == "Hybrid Diesel"){
    
    replace_case_1[i, "batt_VOC..mt"] <- (replace_case_1[i, "batt..VOC..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_CO..mt"] <- (replace_case_1[i, "batt..CO..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_NOx..mt"] <- (replace_case_1[i, "batt..NOx..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_PM10..mt"] <- (replace_case_1[i, "batt..PM10..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_PM2.5..mt"] <- (replace_case_1[i, "batt..PM2.5..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_SOx..mt"] <- (replace_case_1[i, "batt..SOx..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_BC..mt"] <- (replace_case_1[i, "batt..BC..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_OC..mt"] <- (replace_case_1[i, "batt..OC..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    replace_case_1[i, "batt_CO2e..mt"] <- (replace_case_1[i, "batt..GHGs..g.kWh"] * hybrid_cap / 1000000) * replace_case_1[i, "Active.Fleet.Vehicles"]
    
  }
}

replace_case_1$mobile_CO2e..mt <- ((replace_case_1$fuel..CO2..g.mi + ch4_gwp*replace_case_1$fuel..CH4..g.mi + n2o_gwp*replace_case_1$fuel..N2O..g.mi) * replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life)/1000000
replace_case_1$mobile_CO..mt <- (replace_case_1$fuel..CO..g.mi * replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
replace_case_1$mobile_NOx..mt <- (replace_case_1$fuel..NOx..g.mi * replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
replace_case_1$mobile_PM10..mt <- (replace_case_1$fuel..PM10..g.mi * replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
replace_case_1$mobile_PM2.5..mt <- (replace_case_1$fuel..PM2.5..g.mi * replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
replace_case_1$mobile_VOC..mt <- (replace_case_1$fuel..VOC..g.mi * replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
replace_case_1$mobile_SOx..mt <- (replace_case_1$fuel..SOx..g.mi * replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000

# Sum totals
replace_case_1$total_CO2e..mt <- rowSums(replace_case_1[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
replace_case_1$total_NOx..mt <- rowSums(replace_case_1[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
replace_case_1$total_SOx..mt <- rowSums(replace_case_1[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
replace_case_1$total_PM10..mt <- rowSums(replace_case_1[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
replace_case_1$total_PM2.5..mt <- rowSums(replace_case_1[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
replace_case_1$total_VOC..mt <- rowSums(replace_case_1[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
replace_case_1$total_CO..mt <- rowSums(replace_case_1[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)

summary(replace_case_1[,86:92])

## Electric bus range
eb_range <- replace_case_1[replace_case_1$Fuel.Type=="Electric Battery" ,c(1:5, 12:14)]
annual_service$Modes <- paste0(annual_service$Mode, "/", annual_service$TOS)
eb_range <- left_join(eb_range, annual_service, by = c("NTD.ID", "Modes"))
eb_range$Average.Daily.Miles.on.Active.Vehicles.EB <- {eb_range$Total.Miles.on.Active.Vehicles.During.Period / 
    eb_range$Active.Fleet.Vehicles / 
    eb_range$Days.of.Service.Operated}

#### FIGURE: BEB daily range ####
nbins <- nclass.FD(eb_range$Average.Daily.Miles.on.Active.Vehicles.EB %>% na.omit())
annotation <- data.frame(x = c(2, 33, 67),
                         y = c(26, 31, 25), 
                         label = c(str_wrap("25th Percentile", width = 10), "Mean", "75th Percentile"))
current_eb <- ggplot(data = eb_range, aes(Average.Daily.Miles.on.Active.Vehicles.EB)) + 
  geom_histogram(fill = "grey16", color = "white", bins = nbins) + 
  theme_classic() + 
  labs(x = "Average Daily Miles on Active Electric Buses in 2019", y = str_wrap("Number of Active U.S. Electric Transit Buses", width = 30)) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 20), vjust = -0.5)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  scale_x_continuous(breaks = seq(0, 200, 25)) +
  geom_vline(aes(xintercept = mean(Average.Daily.Miles.on.Active.Vehicles.EB, na.rm = TRUE)),
             color = "red",
             linetype = "dashed") +
  geom_vline(aes(xintercept = summary(Average.Daily.Miles.on.Active.Vehicles.EB)[5]), 
             color = "red",
             linetype = "dashed") +
  geom_vline(aes(xintercept = summary(Average.Daily.Miles.on.Active.Vehicles.EB)[2]), 
             color = "red",
             linetype = "dashed") +
  geom_label(data = annotation, aes(x = x, y = y, label = label), color = "firebrick")
  
current_eb

eb_range[eb_range$Average.Daily.Miles.on.Active.Vehicles.EB >100,] %>% view()

# ggsave("Graphs/average_daily_miles_eb.png",
#        plot = current_eb,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

sum(eb_range$Total.Fleet.Vehicles)
# Still 549

# If current fleet makeup persists for the next 14 years:
baseline <- replace_case_1 %>% 
  group_by(Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
si_baseline <- replace_case_1 %>% 
  group_by(Agency.Name.x, Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
baseline$Scenario <- 0
si_baseline$Scenario <- 0

ggplot(data = baseline, 
       aes(x = Fuel.Type, 
           y = total_PM2.5..mt, 
           fill = Modes)) +
  geom_bar(position = "stack", stat = "identity")
# Electric battery should not be zero in this figure

# Switch fossil fuel vehicles to Electric Battery and redo analysis to simulate an immediate replacement
flip_1 <- as.data.frame(replace_case_1)
flip_1$Fuel.Type <- "Electric Battery"
flip_1[is.na(flip_1[, "Average.EB.mpkWh"]), "Average.EB.mpkWh"] <- eb.replace

# Find "assum_bus_life", "batt_cap" earlier to adjust parameters

# Emissions from electricity generation
flip_1$elecgen_NOx..mt <- (flip_1$eGRID.non.baseload.NOx..g.kWh / 
                             flip_1$Average.EB.mpkWh * 
                             flip_1$Total.Miles.on.Active.Vehicles.During.Period * 
                             assum_bus_life /
                             elec_eff_total) / 1000000
flip_1$elecgen_SOx..mt <- (flip_1$eGRID.non.baseload.SO2..g.kWh / 
                             flip_1$Average.EB.mpkWh *
                             flip_1$Total.Miles.on.Active.Vehicles.During.Period * 
                             assum_bus_life) / 1000000
flip_1$elecgen_CO2e..mt <- (flip_1$eGRID.non.baseload.CO2e..g.kWh / 
                              flip_1$Average.EB.mpkWh * 
                              flip_1$Total.Miles.on.Active.Vehicles.During.Period * 
                              assum_bus_life /
                              elec_eff_total) / 1000000

# Initialize fields for battery production emission
flip_1$batt_VOC..mt <- 0
flip_1$batt_CO..mt <- 0
flip_1$batt_NOx..mt <- 0
flip_1$batt_PM10..mt <- 0
flip_1$batt_PM2.5..mt <- 0
flip_1$batt_SOx..mt <- 0
flip_1$batt_BC..mt <- 0
flip_1$batt_OC..mt <- 0
flip_1$batt_CO2e..mt <- 0

# Caluclate battery production and fill emission
flip_1$batt_VOC..mt <- ((flip_1$batt..VOC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_1$Active.Fleet.Vehicles
flip_1$batt_CO..mt <- ((flip_1$batt..CO..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_1$Active.Fleet.Vehicles
flip_1$batt_NOx..mt <- ((flip_1$batt..NOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_1$Active.Fleet.Vehicles
flip_1$batt_PM10..mt <- ((flip_1$batt..PM10..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_1$Active.Fleet.Vehicles
flip_1$batt_PM2.5..mt <- ((flip_1$batt..PM2.5..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_1$Active.Fleet.Vehicles
flip_1$batt_SOx..mt <- ((flip_1$batt..SOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_1$Active.Fleet.Vehicles
flip_1$batt_BC..mt <- ((flip_1$batt..BC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_1$Active.Fleet.Vehicles
flip_1$batt_OC..mt <- ((flip_1$batt..OC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_1$Active.Fleet.Vehicles
flip_1$batt_CO2e..mt <- ((flip_1$batt..GHGs..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_1$Active.Fleet.Vehicles

# Mobile (tailpipe) emissions from 
flip_1$mobile_CO2e..mt <- 0
flip_1$mobile_CO..mt <- 0
flip_1$mobile_NOx..mt <- 0
flip_1$mobile_PM10..mt <- (beb_replace$fuel..PM10..g.mi * flip_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
flip_1$mobile_PM2.5..mt <- (beb_replace$fuel..PM2.5..g.mi * flip_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
flip_1$mobile_VOC..mt <- 0
flip_1$mobile_SOx..mt <- 0

# Sum totals
flip_1$total_CO2e..mt <- rowSums(flip_1[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
flip_1$total_NOx..mt <- rowSums(flip_1[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
flip_1$total_SOx..mt <- rowSums(flip_1[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
flip_1$total_PM10..mt <- rowSums(flip_1[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
flip_1$total_PM2.5..mt <- rowSums(flip_1[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
flip_1$total_VOC..mt <- rowSums(flip_1[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
flip_1$total_CO..mt <- rowSums(flip_1[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)

si_flip_1 <- flip_1 %>% 
  group_by(Agency.Name.x, Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE),  
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
flip_1_sum <- flip_1 %>% 
  group_by(Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
flip_1_sum$Scenario <- 1
si_flip_1$Scenario <- 1

running_total <- rbind.data.frame(baseline, flip_1_sum)
si_running_total <- rbind.data.frame(si_baseline, si_flip_1)

#### Second Scenario: Replacement on daily miles ####

# A small portion of the annual service data
# Contains NTD.ID, Mode, Passenger Miles, and Days Operated in 2019
service_clip <- annual_service[,c(1, 23, 21:22)]
replace_case_2 <- left_join(replace_case_1, service_clip,
                            by = c("NTD.ID", "Modes")) %>% as.data.frame()

# Initialize some attributes for additional buses
# needed for the replacement
replace_case_2$add.total.bus <- 0
replace_case_2$add.active.bus <- 0
# Calculate how many average miles a day the current buses travel
replace_case_2$Daily.Miles.per.Vehicle <- 0
# Used to round down, For example:
# if Daily.Miles.per.Vehicle = 250
# rep.factor will equal 2 
# given a mile_cutoff of 100
replace_case_2$rep.factor <- 0
# This is the range of the "replacement BEBs"
# If the range improves you will need less 
# additional BEBs for replacement and the cost will decrease
mile_cutoff <- 100

for (i in 1:nrow(replace_case_2)) {
  if(replace_case_2[i, "Days.of.Service.Operated"] == 0){
    # If agencies do not report their number of days of service operated in a year, give them 5 days/week out of 52 weeks/year
    replace_case_2[i, "Days.of.Service.Operated"] <- 260
  }
  
  if(replace_case_2[i, "Active.Fleet.Vehicles"] > 0){
    # Calculate how many average miles a day the current buses travel
    replace_case_2$Daily.Miles.per.Vehicle[i] <- replace_case_2$Total.Miles.on.Active.Vehicles.During.Period[i]/replace_case_2$Active.Fleet.Vehicles[i]/replace_case_2$Days.of.Service.Operated[i]
  } else{
    # If this value equals 0, we want to avoid an error
    replace_case_2$Daily.Miles.per.Vehicle[i] <- 0
  }
  
  replace_case_2[is.na(replace_case_2)] <- 0
  # Calculate replace factor
  replace_case_2$rep.factor[i] <- (replace_case_2[i, "Daily.Miles.per.Vehicle"]/mile_cutoff) %>% trunc()
  # Calculate how many BEBs are needed for the replacement in total
  replace_case_2$replace.total.no.bus[i] <- replace_case_2$Total.Fleet.Vehicles[i] * (1 + replace_case_2$rep.factor[i])
  replace_case_2$replace.active.no.bus[i] <- replace_case_2$Active.Fleet.Vehicles[i] * (1 + replace_case_2$rep.factor[i])
  # How many BEBs extra were needed due to a small daily mileage
  replace_case_2$add.total.bus[i] <- (replace_case_2$replace.total.no.bus[i] - replace_case_2$Total.Fleet.Vehicles[i])
  replace_case_2$add.active.bus[i] <- (replace_case_2$replace.active.no.bus[i] - replace_case_2$Active.Fleet.Vehicles[i])

}

#### Cost of Replacement by miles ####
replace_case_2$replace.year <- replace_case_2$Final.Year
replace_case_2[replace_case_2$Final.Year < 2022, "replace.year"] <- 2022
mileage_replacement <- replace_case_2 %>% group_by(replace.year) %>% summarise(avg.no.seats = mean(Average.Number.of.Seats),
                                                                             total.veh = sum(replace.total.no.bus),
                                                                             active.veh = sum(replace.active.no.bus))

mileage_replacement <- left_join(mileage_replacement, bus_costs, by = c("replace.year" = "year"))
mileage_replacement$total.bus.med <- mileage_replacement$total.veh * mileage_replacement$Medium
mileage_replacement$total.bus.opt <- mileage_replacement$total.veh * mileage_replacement$Optimistic
mileage_replacement$total.bus.pes <- mileage_replacement$total.veh * mileage_replacement$Pessimistic

mileage_replacement$active.bus.med <- mileage_replacement$active.veh * mileage_replacement$Medium
mileage_replacement$active.bus.opt <- mileage_replacement$active.veh * mileage_replacement$Optimistic
mileage_replacement$active.bus.pes <- mileage_replacement$active.veh * mileage_replacement$Pessimistic

mileage_replacement <- left_join(mileage_replacement, seat_cost, by = c("replace.year" = "year"))
mileage_replacement$total.seat.med <- mileage_replacement$avg.no.seats * mileage_replacement$total.veh * mileage_replacement$Medium.y
mileage_replacement$total.seat.opt <- mileage_replacement$avg.no.seats * mileage_replacement$total.veh * mileage_replacement$Optimistic.y
mileage_replacement$total.seat.pes <- mileage_replacement$avg.no.seats * mileage_replacement$total.veh * mileage_replacement$Pessimistic.y

mileage_replacement$active.seat.med <- mileage_replacement$avg.no.seats * mileage_replacement$active.veh * mileage_replacement$Medium.y
mileage_replacement$active.seat.opt <- mileage_replacement$avg.no.seats * mileage_replacement$active.veh * mileage_replacement$Optimistic.y
mileage_replacement$active.seat.pes <- mileage_replacement$avg.no.seats * mileage_replacement$active.veh * mileage_replacement$Pessimistic.y

mileage_summary <- colSums(mileage_replacement[,c(8:13, 17:22)]) %>% as.data.frame()/1000000000

# Recalculate Emissions to account for the extra production in batteries
# Switch fossil fuel vehicles to Electric Battery and redo analysis to simulate an immediate replacement
flip_2 <- replace_case_2
flip_2$Fuel.Type <- "Electric Battery"
flip_2[is.na(flip_2[, "Average.EB.mpkWh"]), "Average.EB.mpkWh"] <- eb.replace
flip_2[flip_2$Average.EB.mpkWh <= 0.001, "Average.EB.mpkWh"] <- eb.replace

# Emissions from electricity generation
flip_2$elecgen_NOx..mt <- (flip_2$eGRID.non.baseload.NOx..g.kWh / 
                             flip_2$Average.EB.mpkWh * 
                             flip_2$Total.Miles.on.Active.Vehicles.During.Period * 
                             assum_bus_life /
                             elec_eff_total) / 1000000
flip_2$elecgen_SOx..mt <- (flip_2$eGRID.non.baseload.SO2..g.kWh / 
                             flip_2$Average.EB.mpkWh * 
                             flip_2$Total.Miles.on.Active.Vehicles.During.Period * 
                             assum_bus_life /
                             elec_eff_total) / 1000000
flip_2$elecgen_CO2e..mt <- (flip_2$eGRID.non.baseload.CO2e..g.kWh / 
                              flip_2$Average.EB.mpkWh * 
                              flip_2$Total.Miles.on.Active.Vehicles.During.Period * 
                              assum_bus_life /
                              elec_eff_total) / 1000000

# Emissions from battery production
flip_2$batt_VOC..mt <- 0
flip_2$batt_CO..mt <- 0
flip_2$batt_NOx..mt <- 0
flip_2$batt_PM10..mt <- 0
flip_2$batt_PM2.5..mt <- 0
flip_2$batt_SOx..mt <- 0
flip_2$batt_BC..mt <- 0
flip_2$batt_OC..mt <- 0
flip_2$batt_CO2e..mt <- 0

# Differs from flip 1 in that the number of 
# buses to flip is based on the mile cutoff
flip_2$batt_VOC..mt <- ((flip_2$batt..VOC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_2$replace.active.no.bus
flip_2$batt_CO..mt <- ((flip_2$batt..CO..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_2$replace.active.no.bus
flip_2$batt_NOx..mt <- ((flip_2$batt..NOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_2$replace.active.no.bus
flip_2$batt_PM10..mt <- ((flip_2$batt..PM10..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_2$replace.active.no.bus
flip_2$batt_PM2.5..mt <- ((flip_2$batt..PM2.5..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_2$replace.active.no.bus
flip_2$batt_SOx..mt <- ((flip_2$batt..SOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_2$replace.active.no.bus
flip_2$batt_BC..mt <- ((flip_2$batt..BC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_2$replace.active.no.bus
flip_2$batt_OC..mt <- ((flip_2$batt..OC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_2$replace.active.no.bus
flip_2$batt_CO2e..mt <- ((flip_2$batt..GHGs..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_2$replace.active.no.bus

# All mobile emissions (tailpipe (aka "fuel") which includes tire- & break-wear)
flip_2$mobile_CO2e..mt <- 0
flip_2$mobile_CO..mt <- 0
flip_2$mobile_NOx..mt <- 0
flip_2$mobile_PM10..mt <- (beb_replace$fuel..PM10..g.mi * flip_2$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
flip_2$mobile_PM2.5..mt <- (beb_replace$fuel..PM2.5..g.mi * flip_2$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
flip_2$mobile_VOC..mt <- 0
flip_2$mobile_SOx..mt <- 0

# Sum totals
flip_2$total_CO2e..mt <- rowSums(flip_2[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
flip_2$total_NOx..mt <- rowSums(flip_2[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
flip_2$total_SOx..mt <- rowSums(flip_2[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
flip_2$total_PM10..mt <- rowSums(flip_2[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
flip_2$total_PM2.5..mt <- rowSums(flip_2[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
flip_2$total_VOC..mt <- rowSums(flip_2[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
flip_2$total_CO..mt <- rowSums(flip_2[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)

# Summarize and append results to running tables
si_flip_2 <- flip_2 %>% 
  group_by(Agency.Name.x, 
           Modes, 
           Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
flip_2_sum <- flip_2 %>% 
  group_by(Modes, 
           Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
flip_2_sum$Scenario <- 2
si_flip_2$Scenario <- 2

running_total <- rbind.data.frame(running_total, flip_2_sum)
si_running_total <- rbind.data.frame(si_running_total, si_flip_2)

#### Varying Scenario 2 with replacement range ####

replace_costs <- left_join(replace_case_2, bus_costs, by = c("replace.year" = "year"))
replace_costs <- left_join(replace_costs, seat_cost, by = c("replace.year" = "year"))

cutoff_plot <- replace_costs[, c("Total.Fleet.Vehicles",
                                 "Active.Fleet.Vehicles",
                                 "Average.Number.of.Seats",
                                 "Total.Miles.on.Active.Vehicles.During.Period",
                                 "rep.factor",
                                 "Daily.Miles.per.Vehicle",
                                 "replace.total.no.bus",
                                 "replace.active.no.bus",
                                 "add.total.bus",
                                 "add.active.bus",
                                 "Medium.x",
                                 "Pessimistic.x",
                                 "Optimistic.x",
                                 "Medium.y",
                                 "Pessimistic.y",
                                 "Optimistic.y")]

cutoff_plot[is.na(cutoff_plot)] <- 0
cutoff <- seq(20, 200, 1)

range_vary <- data.frame(eb.range.replace = cutoff)

for (j in 1:length(cutoff)) {
  
  mile_cutoff <- cutoff[j]
  
  cutoff_plot$rep.factor <- trunc(cutoff_plot$Daily.Miles.per.Vehicle/mile_cutoff)
  cutoff_plot$replace.total.no.bus <- cutoff_plot$Total.Fleet.Vehicles * (1 + cutoff_plot$rep.factor)
  cutoff_plot$replace.active.no.bus <- cutoff_plot$Active.Fleet.Vehicles * (1 + cutoff_plot$rep.factor)
  cutoff_plot$add.total.bus <- cutoff_plot$replace.total.no.bus - cutoff_plot$Total.Fleet.Vehicles
  cutoff_plot$add.active.bus <- cutoff_plot$replace.active.no.bus - cutoff_plot$Active.Fleet.Vehicles
  
  cutoff_plot$total.bus.med <- cutoff_plot$replace.total.no.bus * cutoff_plot$Medium.x
  cutoff_plot$total.bus.pes <- cutoff_plot$replace.total.no.bus * cutoff_plot$Pessimistic.x
  cutoff_plot$total.bus.opt <- cutoff_plot$replace.total.no.bus * cutoff_plot$Optimistic.x
  
  cutoff_plot$active.bus.med <- cutoff_plot$replace.active.no.bus * cutoff_plot$Medium.x
  cutoff_plot$active.bus.pes <- cutoff_plot$replace.active.no.bus * cutoff_plot$Pessimistic.x
  cutoff_plot$active.bus.opt <- cutoff_plot$replace.active.no.bus * cutoff_plot$Optimistic.x
  
  cutoff_plot$total.seat.med <- cutoff_plot$replace.total.no.bus * cutoff_plot$Medium.y * cutoff_plot$Average.Number.of.Seats
  cutoff_plot$total.seat.pes <- cutoff_plot$replace.total.no.bus * cutoff_plot$Pessimistic.y * cutoff_plot$Average.Number.of.Seats
  cutoff_plot$total.seat.opt <- cutoff_plot$replace.total.no.bus * cutoff_plot$Optimistic.y * cutoff_plot$Average.Number.of.Seats
  
  cutoff_plot$active.seat.med <- cutoff_plot$replace.active.no.bus * cutoff_plot$Medium.y * cutoff_plot$Average.Number.of.Seats
  cutoff_plot$active.seat.pes <- cutoff_plot$replace.active.no.bus * cutoff_plot$Pessimistic.y * cutoff_plot$Average.Number.of.Seats
  cutoff_plot$active.seat.opt <- cutoff_plot$replace.active.no.bus * cutoff_plot$Optimistic.y * cutoff_plot$Average.Number.of.Seats
  
  range_vary$total.bus.med[j] <- sum(cutoff_plot$total.bus.med)
  range_vary$total.bus.pes[j] <- sum(cutoff_plot$total.bus.pes)
  range_vary$total.bus.opt[j] <- sum(cutoff_plot$total.bus.opt)
  
  range_vary$active.bus.med[j] <- sum(cutoff_plot$active.bus.med)
  range_vary$active.bus.pes[j] <- sum(cutoff_plot$active.bus.pes)
  range_vary$active.bus.opt[j] <- sum(cutoff_plot$active.bus.opt)
  
  range_vary$total.seat.med[j] <- sum(cutoff_plot$total.seat.med)
  range_vary$total.seat.pes[j] <- sum(cutoff_plot$total.seat.pes)
  range_vary$total.seat.opt[j] <- sum(cutoff_plot$total.seat.opt)
  
  range_vary$active.seat.med[j] <- sum(cutoff_plot$active.seat.med)
  range_vary$active.seat.pes[j] <- sum(cutoff_plot$active.seat.pes)
  range_vary$active.seat.opt[j] <- sum(cutoff_plot$active.seat.opt)

  range_vary$no.additional.total.buses[j] <- sum(cutoff_plot$add.total.bus)
  range_vary$no.additional.active.buses[j] <- sum(cutoff_plot$add.active.bus)
  
  print(paste0("Finished ", j, "/", length(cutoff)))
}

range_vary[,3:14] <- range_vary[,4:14]/1000000000

interest_points <- c(35, 45, 100, 200)
label_gen <- function(x) {
  return(
    paste0("$",
           (range_vary[range_vary$eb.range.replace == x, "total.seat.opt"]
            %>% round(0)),
           " to ",
           (range_vary[range_vary$eb.range.replace == x, "total.bus.pes"]
            %>% round(0))))
}

#### FIGURE: Total Cost vs BEB range #####
eb_range_vary <- ggplot(data = range_vary) + 
  geom_ribbon(aes(ymin = total.seat.opt, ymax = total.bus.pes, x = eb.range.replace), fill = "steelblue3") +
  scale_y_continuous(limits = c(0, 250), expand = c(0,0)) + 
  scale_x_continuous(limits = c(0, 225), expand = c(0,0)) + 
  labs(x = "Average BEB range on a single over-night charge (miles)", y = str_wrap("Cost of Total Replacement ($Billion)", width = 30)) + 
  theme_classic() +
  annotate(geom = "text", x = (interest_points[1] + 25), y = range_vary[range_vary$eb.range.replace == interest_points[1], "total.bus.pes"], label = label_gen(interest_points[1])) + 
  geom_segment(aes(x = interest_points[1], y = range_vary[range_vary$eb.range.replace == interest_points[1], "total.seat.opt"], xend = interest_points[1], yend = range_vary[range_vary$eb.range.replace == interest_points[1], "total.bus.pes"])) + 
  geom_segment(aes(x = interest_points[1], y = range_vary[range_vary$eb.range.replace == interest_points[1], "total.bus.pes"], xend = (interest_points[1] + 8), yend = range_vary[range_vary$eb.range.replace == interest_points[1], "total.bus.pes"]), linetype = "dashed") + 
  geom_segment(aes(x = interest_points[1], y = range_vary[range_vary$eb.range.replace == interest_points[1], "total.seat.opt"], xend = interest_points[1], yend = 0), linetype = "dashed") +
  annotate(geom = "text", x = (interest_points[2] + 25), y = range_vary[range_vary$eb.range.replace == interest_points[2], "total.bus.pes"], label = label_gen(interest_points[2])) + 
  geom_segment(aes(x = interest_points[2], y = range_vary[range_vary$eb.range.replace == interest_points[2], "total.seat.opt"], xend = interest_points[2], yend = range_vary[range_vary$eb.range.replace == interest_points[2], "total.bus.pes"])) + 
  geom_segment(aes(x = interest_points[2], y = range_vary[range_vary$eb.range.replace == interest_points[2], "total.bus.pes"], xend = (interest_points[2] + 8), yend = range_vary[range_vary$eb.range.replace == interest_points[2], "total.bus.pes"]), linetype = "dashed") +
  geom_segment(aes(x = interest_points[2], y = range_vary[range_vary$eb.range.replace == interest_points[2], "total.seat.opt"], xend = interest_points[2], yend = 0), linetype = "dashed") +
  annotate(geom = "text", x = (interest_points[3] + 20), y = (range_vary[range_vary$eb.range.replace == interest_points[3], "total.bus.pes"] + 10), label = label_gen(interest_points[3])) + 
  geom_segment(aes(x = interest_points[3], y = range_vary[range_vary$eb.range.replace == interest_points[3], "total.seat.opt"], xend = interest_points[3], yend = range_vary[range_vary$eb.range.replace == interest_points[3], "total.bus.pes"])) + 
  geom_segment(aes(x = interest_points[3], y = range_vary[range_vary$eb.range.replace == interest_points[3], "total.bus.pes"], xend = (interest_points[3] + 8), yend = range_vary[range_vary$eb.range.replace == interest_points[3], "total.bus.pes"] + 6), linetype = "dashed") + 
  geom_segment(aes(x = interest_points[3], y = range_vary[range_vary$eb.range.replace == interest_points[3], "total.seat.opt"], xend = interest_points[3], yend = 0), linetype = "dashed") +
  annotate(geom = "text", x = interest_points[4] + 5, y = range_vary[range_vary$eb.range.replace == interest_points[4], "total.bus.pes"] + 10, label = label_gen(interest_points[4])) + 
  geom_segment(aes(x = interest_points[4], y = range_vary[range_vary$eb.range.replace == interest_points[4], "total.seat.opt"], xend = interest_points[4], yend = range_vary[range_vary$eb.range.replace == interest_points[4], "total.bus.pes"])) + 
  geom_segment(aes(x = interest_points[4], y = range_vary[range_vary$eb.range.replace == interest_points[4], "total.bus.pes"], xend = (interest_points[4] + 5), yend = range_vary[range_vary$eb.range.replace == interest_points[4], "total.bus.pes"] + 5), linetype = "dashed") +
  geom_segment(aes(x = interest_points[4], y = range_vary[range_vary$eb.range.replace == interest_points[4], "total.seat.opt"], xend = interest_points[4], yend = 0), linetype = "dashed") +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 20), vjust = -0.5),
        legend.position = c(0.75, 0.8))

eb_range_vary

# ggsave("Graphs/price_vs_range.png",
#        plot = eb_range_vary,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")


#### Third Scenario: Natural Transition on Daily Miles ####

# Also dependent on daily range of BEB replacements
# To change this, adjust mile_cutoff in scen 2 and redo chunk (~line 1858)

flip_3 <- replace_case_2

#ice.years <12 will have (12 - ice.years) number of years as a BEB
flip_3$ice.years <- flip_3$Final.Year - 2022
flip_3[flip_3$ice.years < 0, "ice.years"] <- 0
flip_3[flip_3$ice.years > assum_bus_life, "ice.years"] <- assum_bus_life
flip_3[flip_3$Average.EB.mpkWh <= 0.002, "Average.EB.mpkWh"] <- eb.replace

# Account for current BEBs in service
flip_3[flip_3$Fuel.Type == "Electric Battery", "ice.years"] <- 0


flip_3$elecgen_NOx..mt <- (flip_3$eGRID.non.baseload.NOx..g.kWh / 
                             flip_3$Average.EB.mpkWh * 
                             flip_3$Total.Miles.on.Active.Vehicles.During.Period *
                             (assum_bus_life - flip_3$ice.years) /
                             elec_eff_total) / 1000000
flip_3$elecgen_SOx..mt <- (flip_3$eGRID.non.baseload.SO2..g.kWh / 
                             flip_3$Average.EB.mpkWh * 
                             flip_3$Total.Miles.on.Active.Vehicles.During.Period *
                             (assum_bus_life - flip_3$ice.years) /
                             elec_eff_total) / 1000000
flip_3$elecgen_CO2e..mt <- (flip_3$eGRID.non.baseload.CO2e..g.kWh / 
                              flip_3$Average.EB.mpkWh * 
                              flip_3$Total.Miles.on.Active.Vehicles.During.Period * 
                              (assum_bus_life - flip_3$ice.years) /
                              elec_eff_total) / 1000000
flip_3$batt_VOC..mt <- 0
flip_3$batt_CO..mt <- 0
flip_3$batt_NOx..mt <- 0
flip_3$batt_PM10..mt <- 0
flip_3$batt_PM2.5..mt <- 0
flip_3$batt_SOx..mt <- 0
flip_3$batt_BC..mt <- 0
flip_3$batt_OC..mt <- 0
flip_3$batt_CO2e..mt <- 0

flip_3$batt_VOC..mt <- ((flip_3$batt..VOC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_3$replace.active.no.bus
flip_3$batt_CO..mt <- ((flip_3$batt..CO..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_3$replace.active.no.bus
flip_3$batt_NOx..mt <- ((flip_3$batt..NOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_3$replace.active.no.bus
flip_3$batt_PM10..mt <- ((flip_3$batt..PM10..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_3$replace.active.no.bus
flip_3$batt_PM2.5..mt <- ((flip_3$batt..PM2.5..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_3$replace.active.no.bus
flip_3$batt_SOx..mt <- ((flip_3$batt..SOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_3$replace.active.no.bus
flip_3$batt_BC..mt <- ((flip_3$batt..BC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_3$replace.active.no.bus
flip_3$batt_OC..mt <- ((flip_3$batt..OC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_3$replace.active.no.bus
flip_3$batt_CO2e..mt <- ((flip_3$batt..GHGs..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_3$replace.active.no.bus

flip_3$beb.mobile_CO2e..mt <- 0
flip_3$beb.mobile_CO..mt <- 0
flip_3$beb.mobile_NOx..mt <- 0
flip_3$beb.mobile_PM10..mt <- (beb_replace$fuel..PM10..g.mi * flip_3$Total.Miles.on.Active.Vehicles.During.Period * (assum_bus_life - flip_3$ice.years)) / 1000000
flip_3$beb.mobile_PM2.5..mt <- (beb_replace$fuel..PM2.5..g.mi * flip_3$Total.Miles.on.Active.Vehicles.During.Period * (assum_bus_life - flip_3$ice.years)) / 1000000
flip_3$beb.mobile_VOC..mt <- 0
flip_3$beb.mobile_SOx..mt <- 0
                              
flip_3$ice.mobile_CO2e..mt <- ((flip_3$fuel..CO2..g.mi + ch4_gwp*flip_3$fuel..CH4..g.mi + n2o_gwp*flip_3$fuel..N2O..g.mi) * flip_3$Total.Miles.on.Active.Vehicles.During.Period * flip_3$ice.years)/1000000
flip_3$ice.mobile_CO..mt <- (flip_3$fuel..CO..g.mi * flip_3$Total.Miles.on.Active.Vehicles.During.Period * flip_3$ice.years) / 1000000
flip_3$ice.mobile_NOx..mt <- (flip_3$fuel..NOx..g.mi * flip_3$Total.Miles.on.Active.Vehicles.During.Period * flip_3$ice.years) / 1000000
flip_3$ice.mobile_PM10..mt <- (flip_3$fuel..PM10..g.mi * flip_3$Total.Miles.on.Active.Vehicles.During.Period * flip_3$ice.years) / 1000000
flip_3$ice.mobile_PM2.5..mt <- (flip_3$fuel..PM2.5..g.mi * flip_3$Total.Miles.on.Active.Vehicles.During.Period * flip_3$ice.years) / 1000000
flip_3$ice.mobile_VOC..mt <- (flip_3$fuel..VOC..g.mi * flip_3$Total.Miles.on.Active.Vehicles.During.Period * flip_3$ice.years) / 1000000
flip_3$ice.mobile_SOx..mt <- (flip_3$fuel..SOx..g.mi * flip_3$Total.Miles.on.Active.Vehicles.During.Period * flip_3$ice.years) / 1000000

flip_3$mobile_CO2e..mt <- flip_3$beb.mobile_CO2e..mt + flip_3$ice.mobile_CO2e..mt
flip_3$mobile_CO..mt <- flip_3$beb.mobile_CO..mt + flip_3$ice.mobile_CO..mt
flip_3$mobile_NOx..mt <- flip_3$beb.mobile_NOx..mt + flip_3$ice.mobile_NOx..mt
flip_3$mobile_PM10..mt <- flip_3$beb.mobile_PM10..mt + flip_3$ice.mobile_PM10..mt
flip_3$mobile_PM2.5..mt <- flip_3$beb.mobile_PM2.5..mt + flip_3$ice.mobile_PM2.5..mt
flip_3$mobile_VOC..mt <- flip_3$beb.mobile_VOC..mt + flip_3$ice.mobile_VOC..mt
flip_3$mobile_SOx..mt <- flip_3$beb.mobile_SOx..mt + flip_3$ice.mobile_SOx..mt

# Sum totals
flip_3$total_CO2e..mt <- rowSums(flip_3[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
flip_3$total_NOx..mt <- rowSums(flip_3[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
flip_3$total_SOx..mt <- rowSums(flip_3[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
flip_3$total_PM10..mt <- rowSums(flip_3[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
flip_3$total_PM2.5..mt <- rowSums(flip_3[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
flip_3$total_VOC..mt <- rowSums(flip_3[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
flip_3$total_CO..mt <- rowSums(flip_3[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)

si_flip_3 <- flip_3 %>% 
  group_by(Agency.Name.x, Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
flip_3_sum <- flip_3 %>% 
  group_by(Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))


flip_3_sum$Scenario <- 3
si_flip_3$Scenario <- 3

# Add results to running sum tables
running_total <- rbind.data.frame(running_total, flip_3_sum)
si_running_total <- rbind.data.frame(si_running_total, si_flip_3)

#### FIGURE: Natural phase out cost per year ####
year_summary_replace[,5:20] <- year_summary_replace[,5:20]/1000000000
phase_out <- ggplot(data = year_summary_replace, 
                    aes(x = year.replace, y = total.bus.med)) +
  geom_col(fill = "steelblue3") + 
  geom_errorbar(data = year_summary_replace, aes(ymin = total.seat.opt, ymax = total.bus.pes, width = 0.4)) +
  theme_classic() + 
  labs(x = "Year", y = str_wrap("Annual BEB Total Replacement Cost under Natural Transition ($billion)", width = 25)) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 14, family = "sans"),
        axis.title.x = element_text(margin = margin(t = 20), vjust = 3),
        axis.title.y = element_text(margin = margin(r = 20), vjust = -1),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = 2022:2040, limits = c(2021.5, 2040)) +
  scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0,16))

phase_out

# ggsave("Graphs/phase_out.png",
#        plot = phase_out,
#        dpi = 700,
#        width = 6.5,
#        height = 3.5,
#        units = "in")

year_summary_replace$total.seat.med
year_summary_replace$total.seat.med[1]
mean(year_summary_replace$total.seat.med[2:11])


#### Fourth Scenario: 5% yearly transition ####

# Find out how many buses is 5% of the fleet
five_percent <- (sum(flip_3$Total.Fleet.Vehicles)*0.05) %>% round()
# Order the data by phase out year
replace_case_4 <- flip_3[order(flip_3$Final.Year),]
# Initialize a field that will serve as a running sum of bus counts
replace_case_4$total.veh.running.sum <- replace_case_4$Total.Fleet.Vehicles

# The first row's running total is just itself so we can start on row two
i <- 2
while (i <= nrow(replace_case_4)) {
  
  # Calculate the running total
  replace_case_4[i, "total.veh.running.sum"] <- replace_case_4[i-1, "total.veh.running.sum"] + replace_case_4[i, "total.veh.running.sum"]
  
  # Progress Monitor
  print(paste0(i, "/", nrow(replace_case_4)))
  i <- i+1
  
}

# Calculate the bus counts that represent 5% (10%, 15%, ...) of the fleet
number_replace <- seq(0, 
                      sum(replace_case_4$Total.Fleet.Vehicles) + 
                        five_percent, 
                      five_percent)
# Create the vector of years the replacements will span
year_replace <- 2022:(2021 + length(number_replace))

# Initialize a column that assigns each fleet a replacement year
replace_case_4$year.replace <- 0
# year.replace is the replacement year for the 5% transition
# replace.year is the replacement year for the natural transition

for (i in 1:21) {
  
  j <- i + 1
  
  for (k in 1:nrow(replace_case_4)) {
    
    if ((number_replace[i] <= replace_case_4[k, "total.veh.running.sum"]) && 
        (replace_case_4[k, "total.veh.running.sum"] <= number_replace[j])){
     
      # If a row's running total falls between a year_replace bounds
      # then assign that portion of the fleet to that year 
      replace_case_4[k, "year.replace"] <- year_replace[i]
       
    }
  }
  print(paste0(i, "/21"))
}
# Does not allow for splitting the agency's 
# fleets so the 5% is a rough calculation

# Now we can merge the years that these buses need to be replaced with their battery prices
transition <- left_join(replace_case_4, bus_costs, by = c("year.replace" = "year"))
transition <- left_join(transition, seat_cost, by = c("year.replace" = "year"))

transition$total.bus.med <- transition$replace.total.no.bus * transition$Medium.x
transition$total.bus.pes <- transition$replace.total.no.bus * transition$Pessimistic.x
transition$total.bus.opt <- transition$replace.total.no.bus * transition$Optimistic.x

transition$active.bus.med <- transition$replace.active.no.bus * transition$Medium.x
transition$active.bus.pes <- transition$replace.active.no.bus * transition$Pessimistic.x
transition$active.bus.opt <- transition$replace.active.no.bus * transition$Optimistic.x

transition$total.seat.med <- transition$replace.total.no.bus * transition$Medium.y * transition$Average.Number.of.Seats
transition$total.seat.pes <- transition$replace.total.no.bus * transition$Pessimistic.y * transition$Average.Number.of.Seats
transition$total.seat.opt <- transition$replace.total.no.bus * transition$Optimistic.y * transition$Average.Number.of.Seats

transition$active.seat.med <- transition$replace.active.no.bus * transition$Medium.y * transition$Average.Number.of.Seats
transition$active.seat.pes <- transition$replace.active.no.bus * transition$Pessimistic.y * transition$Average.Number.of.Seats
transition$active.seat.opt <- transition$replace.active.no.bus * transition$Optimistic.y * transition$Average.Number.of.Seats


scen_4_plot <- transition %>% 
  group_by(year.replace, Final.Year) %>% 
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles, na.rm = TRUE), 
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles, na.rm = TRUE), 
            add.total.bus = sum(add.total.bus, na.rm = TRUE), 
            add.active.bus = sum(add.active.bus, na.rm = TRUE),
            total.bus.med = sum(total.bus.med),
            total.bus.pes = sum(total.bus.pes),
            total.bus.opt = sum(total.bus.opt),
            active.bus.med = sum(active.bus.med),
            active.bus.pes = sum(active.bus.pes),
            active.bus.opt = sum(active.bus.opt),
            total.seat.med = sum(total.seat.med),
            total.seat.pes = sum(total.seat.pes),
            total.seat.opt = sum(total.seat.opt),
            active.seat.med = sum(active.seat.med),
            active.seat.pes = sum(active.seat.pes),
            active.seat.opt = sum(active.seat.opt))

scen_4_plot$year.behind <- scen_4_plot$year.replace - scen_4_plot$Final.Year
scen_4_plot[7:19] <- scen_4_plot[7:19]/1000000000
scen_4_stat <- scen_4_plot %>% 
  group_by(year.replace) %>% 
  summarise(total.bus.med = sum(total.bus.med),
            total.bus.pes = sum(total.bus.pes),
            total.bus.opt = sum(total.bus.opt),
            active.bus.med = sum(active.bus.med),
            active.bus.pes = sum(active.bus.pes),
            active.bus.opt = sum(active.bus.opt),
            total.seat.med = sum(total.seat.med),
            total.seat.pes = sum(total.seat.pes),
            total.seat.opt = sum(total.seat.opt),
            active.seat.med = sum(active.seat.med),
            active.seat.pes = sum(active.seat.pes),
            active.seat.opt = sum(active.seat.opt))

mean(scen_4_stat$total.bus.med, na.rm = TRUE) # in $billion

flip_4 <- replace_case_4

# How many years will the buses run on fossil fuel
flip_4$ice.years <- flip_4$year.replace - 2022
# Minimum is zero years
flip_4[flip_4$ice.years < 0, "ice.years"] <- 0
# If this is greater than the assumed bus life of 14 yrs
# replace with 14 years
flip_4[flip_4$ice.years > assum_bus_life, "ice.years"] <- assum_bus_life
# Give the buses an average EB mpkWh for their flip
flip_4[flip_4$Average.EB.mpkWh <= 0.001, "Average.EB.mpkWh"] <- eb.replace

#Account for current BEBs in service
flip_4[flip_4$Fuel.Type == "Electric Battery", "ice.years"] <- 0


flip_4$elecgen_NOx..mt <- (flip_4$eGRID.non.baseload.NOx..g.kWh / 
                             flip_4$Average.EB.mpkWh * 
                             flip_4$Total.Miles.on.Active.Vehicles.During.Period * 
                             (assum_bus_life - flip_4$ice.years) /
                             elec_eff_total) / 1000000
flip_4$elecgen_SOx..mt <- (flip_4$eGRID.non.baseload.SO2..g.kWh / 
                             flip_4$Average.EB.mpkWh * 
                             flip_4$Total.Miles.on.Active.Vehicles.During.Period * 
                             (assum_bus_life - flip_4$ice.years) /
                             elec_eff_total) / 1000000
flip_4$elecgen_CO2e..mt <- (flip_4$eGRID.non.baseload.CO2e..g.kWh / 
                              flip_4$Average.EB.mpkWh * 
                              flip_4$Total.Miles.on.Active.Vehicles.During.Period * 
                              (assum_bus_life - flip_4$ice.years) /
                              elec_eff_total) / 1000000
flip_4$batt_VOC..mt <- 0
flip_4$batt_CO..mt <- 0
flip_4$batt_NOx..mt <- 0
flip_4$batt_PM10..mt <- 0
flip_4$batt_PM2.5..mt <- 0
flip_4$batt_SOx..mt <- 0
flip_4$batt_BC..mt <- 0
flip_4$batt_OC..mt <- 0
flip_4$batt_CO2e..mt <- 0

flip_4$batt_VOC..mt <- ((flip_4$batt..VOC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_4$replace.active.no.bus
flip_4$batt_CO..mt <- ((flip_4$batt..CO..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_4$replace.active.no.bus
flip_4$batt_NOx..mt <- ((flip_4$batt..NOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_4$replace.active.no.bus
flip_4$batt_PM10..mt <- ((flip_4$batt..PM10..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_4$replace.active.no.bus
flip_4$batt_PM2.5..mt <- ((flip_4$batt..PM2.5..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_4$replace.active.no.bus
flip_4$batt_SOx..mt <- ((flip_4$batt..SOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_4$replace.active.no.bus
flip_4$batt_BC..mt <- ((flip_4$batt..BC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_4$replace.active.no.bus
flip_4$batt_OC..mt <- ((flip_4$batt..OC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_4$replace.active.no.bus
flip_4$batt_CO2e..mt <- ((flip_4$batt..GHGs..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * flip_4$replace.active.no.bus

flip_4$beb.mobile_CO2e..mt <- 0
flip_4$beb.mobile_CO..mt <- 0
flip_4$beb.mobile_NOx..mt <- 0
flip_4$beb.mobile_PM10..mt <- (beb_replace$fuel..PM10..g.mi * flip_4$Total.Miles.on.Active.Vehicles.During.Period * (assum_bus_life - flip_4$ice.years)) / 1000000
flip_4$beb.mobile_PM2.5..mt <- (beb_replace$fuel..PM2.5..g.mi * flip_4$Total.Miles.on.Active.Vehicles.During.Period * (assum_bus_life - flip_4$ice.years)) / 1000000
flip_4$beb.mobile_VOC..mt <- 0
flip_4$beb.mobile_SOx..mt <- 0

flip_4$ice.mobile_CO2e..mt <- ((flip_4$fuel..CO2..g.mi + ch4_gwp*flip_4$fuel..CH4..g.mi + n2o_gwp*flip_4$fuel..N2O..g.mi) * flip_4$Total.Miles.on.Active.Vehicles.During.Period * flip_4$ice.years)/1000000
flip_4$ice.mobile_CO..mt <- (flip_4$fuel..CO..g.mi * flip_4$Total.Miles.on.Active.Vehicles.During.Period * flip_4$ice.years) / 1000000
flip_4$ice.mobile_NOx..mt <- (flip_4$fuel..NOx..g.mi * flip_4$Total.Miles.on.Active.Vehicles.During.Period * flip_4$ice.years) / 1000000
flip_4$ice.mobile_PM10..mt <- (flip_4$fuel..PM10..g.mi * flip_4$Total.Miles.on.Active.Vehicles.During.Period * flip_4$ice.years) / 1000000
flip_4$ice.mobile_PM2.5..mt <- (flip_4$fuel..PM2.5..g.mi * flip_4$Total.Miles.on.Active.Vehicles.During.Period * flip_4$ice.years) / 1000000
flip_4$ice.mobile_VOC..mt <- (flip_4$fuel..VOC..g.mi * flip_4$Total.Miles.on.Active.Vehicles.During.Period * flip_4$ice.years) / 1000000
flip_4$ice.mobile_SOx..mt <- (flip_4$fuel..SOx..g.mi * flip_4$Total.Miles.on.Active.Vehicles.During.Period * flip_4$ice.years) / 1000000

flip_4$mobile_CO2e..mt <- flip_4$beb.mobile_CO2e..mt + flip_4$ice.mobile_CO2e..mt
flip_4$mobile_CO..mt <- flip_4$beb.mobile_CO..mt + flip_4$ice.mobile_CO..mt
flip_4$mobile_NOx..mt <- flip_4$beb.mobile_NOx..mt + flip_4$ice.mobile_NOx..mt
flip_4$mobile_PM10..mt <- flip_4$beb.mobile_PM10..mt + flip_4$ice.mobile_PM10..mt
flip_4$mobile_PM2.5..mt <- flip_4$beb.mobile_PM2.5..mt + flip_4$ice.mobile_PM2.5..mt
flip_4$mobile_VOC..mt <- flip_4$beb.mobile_VOC..mt + flip_4$ice.mobile_VOC..mt
flip_4$mobile_SOx..mt <- flip_4$beb.mobile_SOx..mt + flip_4$ice.mobile_SOx..mt

# Sum totals
flip_4$total_CO2e..mt <- rowSums(flip_4[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
flip_4$total_NOx..mt <- rowSums(flip_4[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
flip_4$total_SOx..mt <- rowSums(flip_4[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
flip_4$total_PM10..mt <- rowSums(flip_4[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
flip_4$total_PM2.5..mt <- rowSums(flip_4[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
flip_4$total_VOC..mt <- rowSums(flip_4[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
flip_4$total_CO..mt <- rowSums(flip_4[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)

si_flip_4 <- flip_4 %>% 
  group_by(Agency.Name.x, Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE),
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
flip_4_sum <- flip_4 %>% 
  group_by(Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE),
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
flip_4_sum$Scenario <- 4
si_flip_4$Scenario <- 4

running_total <- rbind.data.frame(running_total, flip_4_sum)
si_running_total <- rbind.data.frame(si_running_total, si_flip_4)

#### Fifth Scenario: Top 100 agency transition ####

# Identify which are the largeset 100 agencies by total fleet count
top_agency <- replace_case_1 %>%
  group_by(Agency.Name.x, NTD.ID) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles))
top_agency <- unique(top_agency)
top_agency <- top_agency[order(top_agency$Total.Fleet.Vehicles, decreasing = TRUE),]
top_agency_replace <- top_agency[1:100, 2]
top_agency_replace$flip.yn <- 1

# Join this list to the data set
replace_case_5 <- right_join(top_agency_replace, transition, by = "NTD.ID")

# If the join did not designate an agency as one of the top 100
# Assign it a 0 signifying that it will not be transitioned
replace_case_5[is.na(replace_case_5$flip.yn), "flip.yn"] <- 0
replace_case_5$flip.yn <- as.factor(replace_case_5$flip.yn)
summary(replace_case_5$flip.yn)
#only 1,523/4853 data rows will be transitioned in this scenario
#Calculate what percent of the fleet this is
sum(replace_case_5[replace_case_5$flip.yn == 1, "Total.Fleet.Vehicles"])
#45,702 vehicles to be transitioned
sum(replace_case_5[replace_case_5$flip.yn == 0, "Total.Fleet.Vehicles"])
#14,960 vehicles not to be transitioned in this scenario

# What percentage of the fleet gets transitioned?
sum(replace_case_5[replace_case_5$flip.yn == 1, "Total.Fleet.Vehicles"])/sum(replace_case_5[, "Total.Fleet.Vehicles"]) * 100

x <- sum(replace_case_5[replace_case_5$flip.yn == 1, "total.bus.med"], na.rm = TRUE)/1000000000
x
# $44 billion replacement by seat
y <- sum(transition$total.bus.med, na.rm = TRUE)/1000000000
y
# compared to ~59B
x/y*100

flip_5 <- replace_case_5

#ice.years <14 will have (14 - ice.years) number of years as a BEB
flip_5$ice.years <- assum_bus_life
# Only fleets from top 100 agencies will have modified ice years
for (i in 1:nrow(flip_5)) {
  if (flip_5$flip.yn[i] == 1) {
    
    flip_5$ice.years[i] <- flip_5$Final.Year[i] - 2022
    
  }
}
# Give the ice years a lower and upper bound
flip_5[flip_5$ice.years < 0, "ice.years"] <- 0
flip_5[flip_5$ice.years > assum_bus_life, "ice.years"] <- assum_bus_life
flip_5[flip_5$Average.EB.mpkWh <= 0.002, "Average.EB.mpkWh"] <- eb.replace
 
# Account for current BEBs in service
flip_5[flip_5$Fuel.Type == "Electric Battery", "ice.years"] <- 0


flip_5$elecgen_NOx..mt <- (flip_5$eGRID.non.baseload.NOx..g.kWh / 
                             flip_5$Average.EB.mpkWh * 
                             flip_5$Total.Miles.on.Active.Vehicles.During.Period *
                             (assum_bus_life - flip_5$ice.years) /
                             elec_eff_total) / 1000000
flip_5$elecgen_SOx..mt <- (flip_5$eGRID.non.baseload.SO2..g.kWh / 
                             flip_5$Average.EB.mpkWh * 
                             flip_5$Total.Miles.on.Active.Vehicles.During.Period *
                             (assum_bus_life - flip_5$ice.years) /
                             elec_eff_total) / 1000000
flip_5$elecgen_CO2e..mt <- (flip_5$eGRID.non.baseload.CO2e..g.kWh / 
                              flip_5$Average.EB.mpkWh * 
                              flip_5$Total.Miles.on.Active.Vehicles.During.Period * 
                              (assum_bus_life - flip_5$ice.years) /
                              elec_eff_total) / 1000000
flip_5$batt_VOC..mt <- 0
flip_5$batt_CO..mt <- 0
flip_5$batt_NOx..mt <- 0
flip_5$batt_PM10..mt <- 0
flip_5$batt_PM2.5..mt <- 0
flip_5$batt_SOx..mt <- 0
flip_5$batt_BC..mt <- 0
flip_5$batt_OC..mt <- 0
flip_5$batt_CO2e..mt <- 0

flip_5$beb.mobile_CO2e..mt <- 0
flip_5$beb.mobile_CO..mt <- 0
flip_5$beb.mobile_NOx..mt <- 0
flip_5$beb.mobile_PM10..mt <- 0
flip_5$beb.mobile_PM2.5..mt <- 0
flip_5$beb.mobile_VOC..mt <- 0
flip_5$beb.mobile_SOx..mt <- 0


for (i in 1:nrow(flip_5)) {
  if (flip_5$flip.yn[i] == 1) {
    flip_5$batt_VOC..mt[i] <- ((flip_5$batt..VOC..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * flip_5$replace.active.no.bus[i]
    flip_5$batt_CO..mt[i] <- ((flip_5$batt..CO..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * flip_5$replace.active.no.bus[i]
    flip_5$batt_NOx..mt[i] <- ((flip_5$batt..NOx..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * flip_5$replace.active.no.bus[i]
    flip_5$batt_PM10..mt[i] <- ((flip_5$batt..PM10..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * flip_5$replace.active.no.bus[i]
    flip_5$batt_PM2.5..mt[i] <- ((flip_5$batt..PM2.5..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * flip_5$replace.active.no.bus[i]
    flip_5$batt_SOx..mt[i] <- ((flip_5$batt..SOx..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * flip_5$replace.active.no.bus[i]
    flip_5$batt_BC..mt[i] <- ((flip_5$batt..BC..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * flip_5$replace.active.no.bus[i]
    flip_5$batt_OC..mt[i] <- ((flip_5$batt..OC..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * flip_5$replace.active.no.bus[i]
    flip_5$batt_CO2e..mt[i] <- ((flip_5$batt..GHGs..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * flip_5$replace.active.no.bus[i]

    flip_5$beb.mobile_PM10..mt[i] <- (beb_replace$fuel..PM10..g.mi * flip_5$Total.Miles.on.Active.Vehicles.During.Period[i] * (assum_bus_life - flip_5$ice.years[i])) / 1000000
    flip_5$beb.mobile_PM2.5..mt[i] <- (beb_replace$fuel..PM2.5..g.mi * flip_5$Total.Miles.on.Active.Vehicles.During.Period[i] * (assum_bus_life - flip_5$ice.years[i])) / 1000000

  }
}

flip_5$ice.mobile_CO2e..mt <- ((flip_5$fuel..CO2..g.mi + ch4_gwp*flip_5$fuel..CH4..g.mi + n2o_gwp*flip_5$fuel..N2O..g.mi) * flip_5$Total.Miles.on.Active.Vehicles.During.Period * flip_5$ice.years)/1000000
flip_5$ice.mobile_CO..mt <- (flip_5$fuel..CO..g.mi * flip_5$Total.Miles.on.Active.Vehicles.During.Period * flip_5$ice.years) / 1000000
flip_5$ice.mobile_NOx..mt <- (flip_5$fuel..NOx..g.mi * flip_5$Total.Miles.on.Active.Vehicles.During.Period * flip_5$ice.years) / 1000000
flip_5$ice.mobile_PM10..mt <- (flip_5$fuel..PM10..g.mi * flip_5$Total.Miles.on.Active.Vehicles.During.Period * flip_5$ice.years) / 1000000
flip_5$ice.mobile_PM2.5..mt <- (flip_5$fuel..PM2.5..g.mi * flip_5$Total.Miles.on.Active.Vehicles.During.Period * flip_5$ice.years) / 1000000
flip_5$ice.mobile_VOC..mt <- (flip_5$fuel..VOC..g.mi * flip_5$Total.Miles.on.Active.Vehicles.During.Period * flip_5$ice.years) / 1000000
flip_5$ice.mobile_SOx..mt <- (flip_5$fuel..SOx..g.mi * flip_5$Total.Miles.on.Active.Vehicles.During.Period * flip_5$ice.years) / 1000000

flip_5$mobile_CO2e..mt <- flip_5$beb.mobile_CO2e..mt + flip_5$ice.mobile_CO2e..mt
flip_5$mobile_CO..mt <- flip_5$beb.mobile_CO..mt + flip_5$ice.mobile_CO..mt
flip_5$mobile_NOx..mt <- flip_5$beb.mobile_NOx..mt + flip_5$ice.mobile_NOx..mt
flip_5$mobile_PM10..mt <- flip_5$beb.mobile_PM10..mt + flip_5$ice.mobile_PM10..mt
flip_5$mobile_PM2.5..mt <- flip_5$beb.mobile_PM2.5..mt + flip_5$ice.mobile_PM2.5..mt
flip_5$mobile_VOC..mt <- flip_5$beb.mobile_VOC..mt + flip_5$ice.mobile_VOC..mt
flip_5$mobile_SOx..mt <- flip_5$beb.mobile_SOx..mt + flip_5$ice.mobile_SOx..mt

# Sum totals
flip_5$total_CO2e..mt <- rowSums(flip_5[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
flip_5$total_NOx..mt <- rowSums(flip_5[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
flip_5$total_SOx..mt <- rowSums(flip_5[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
flip_5$total_PM10..mt <- rowSums(flip_5[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
flip_5$total_PM2.5..mt <- rowSums(flip_5[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
flip_5$total_VOC..mt <- rowSums(flip_5[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
flip_5$total_CO..mt <- rowSums(flip_5[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)


si_flip_5 <- flip_5 %>% group_by(Agency.Name.x, Modes, Fuel.Type) %>%
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE),
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE),
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE),
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE),
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE),
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE),
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
flip_5_sum <- flip_5 %>% group_by(Modes, Fuel.Type) %>%
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE),
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE),
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE),
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE),
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE),
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE),
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
flip_5_sum$Scenario <- 5
si_flip_5$Scenario <- 5

running_total <- rbind.data.frame(running_total, flip_5_sum)
si_running_total <- rbind.data.frame(si_running_total, si_flip_5)


total_plot <- running_total %>% 
  group_by(Scenario) %>% 
  summarise(CO2e = sum(total_CO2e..mt), 
            NOx = sum(total_NOx..mt), 
            SOx = sum(total_SOx..mt), 
            PM10 = sum(total_PM10..mt), 
            PM2.5 = sum(total_PM2.5..mt), 
            VOC = sum(total_VOC..mt), 
            CO = sum(total_CO..mt))
total_plot_melt <- melt(total_plot, id.vars = "Scenario")
total_plot_melt$value <- total_plot_melt$value/1000000
# Divide the plot to see see two axes
total_plot_ghg <- filter(total_plot_melt, variable == "CO2e")
total_plot_ls <- filter(total_plot_melt, variable == "CO" | variable == "NOx" | variable == "SOx")
total_plot_ss <- filter(total_plot_melt, variable != "CO2e" & variable != "CO" & variable != "NOx" & variable != "SOx")
total_plot_ls$variable <- factor(total_plot_ls$variable)
total_plot_ss$variable <- factor(total_plot_ss$variable)
scen_labels <- c("Base Case", "Replace All Bus Now", "Replace All Miles Now", "Natural Phase Out", "5% Yearly", "Top 100") %>% str_wrap(width = 6)

#### (Not Used) FIGURE: Emissions ####
# This version does not account for the electricty mix becoming 
# cleaner over time

# This will be used as whiskers in the used figure by the same
# title later on
ghgs <- ggplot(data = total_plot_ghg, aes(x = factor(Scenario), y = value, fill = variable)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "steelblue3") +
  theme_classic() +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        legend.key.size = unit(4, "mm")) + 
  labs(x = "Scenario", y = "Amount of Emissions in MMT") +
  scale_fill_discrete(name = "Emission Type:") +
  scale_x_discrete(labels = scen_labels)

ghgs

# ggsave("Graphs/all_ghgs.png",
#        plot = a/b,
#        dpi = 700,
#        width = 6.5,
#        height = 4,
#        units = "in")

a <- ggplot(data = total_plot_ls, aes(x = factor(Scenario), y = value, fill = variable)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        legend.key.size = unit(4, "mm")) + 
  labs(x = "", y = "") +
  scale_fill_manual(name = "Emission Type:", 
                    values = c("CO" = "#E69F00", 
                               "NOx" = "#56B4E9", 
                               "SOx" = "#009E73")) +
  scale_x_discrete(labels = scen_labels)
  
b <- ggplot(data = total_plot_ss, aes(x = factor(Scenario), y = value, fill = variable)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        legend.key.size = unit(4, "mm"),
        axis.title.y = element_text(hjust = -2.2)) + 
  labs(x = "Scenario", y = "Amount of Emissions in MMT") + 
  scale_fill_manual(name = "Emission Type:", 
                    values = c("PM10" = "#F0E442", 
                               "PM2.5" = "#0072B2", 
                               "VOC" = "#D55E00")) +
  scale_x_discrete(labels = scen_labels)

a/b

# ggsave("Graphs/all_pollutants.png",
#        plot = a/b,
#        dpi = 700,
#        width = 6.5,
#        height = 4,
#        units = "in")


#### Check MOVES3 Data against CARB Data ####

moves_body <- read.csv("MOVES3/SummaryReportBody.csv",
                       na.strings = c("", "NA"),
                       stringsAsFactors = FALSE)

# moves_body units-
# Mass Units: ton
# Energy Units: MMBTU
# Distance Units: miles
# Time Units: years

# Icludes only Gasoline, Diesel, and CNG fuel types for transit buses
# Does not include EB

moves_2019sum <- (moves_body[, 7:14] %>% colSums()) %>% as.data.frame()

carb <- replace_case_1[replace_case_1$Fuel.Type != "Electric Battery", c(13, 65:71)]
carb_2019sum <- (carb %>% colSums()) %>% as.data.frame()
carb_2019sum <- carb_2019sum/12

#FTA mileage for 2019 = 169,123,976 mi
#MOVES3 mileage for 2019 = 	3,443,029,482 mi

sum(temp_vehs$Total.Miles.on.Active.Vehicles.During.Period)
# For reference, this number from an unpolished dataset includes Automobiles, 
# minivans, school buses, SUVs and vans, and more as "buses" and only reaches 
# 2 million miles driven in 2019


#### Decreasing eGRID emissions ####
egrid_simp <- egrid[, c(1, 8, 9, 13)]

egrid_simp$eGRID.non.baseload.NOx..g.kWh. <- egrid_simp$eGRID.subregion.annual.NOx.non.baseload.output.emission.rate..lb.MWh. * 453.592 / 1000
egrid_simp$eGRID.non.baseload.SO2..g.kWh. <- egrid_simp$eGRID.subregion.annual.SO2.non.baseload.output.emission.rate..lb.MWh. * 453.592 / 1000
egrid_simp$eGRID.non.baseload.CO2e..g.kWh. <- egrid_simp$eGRID.subregion.annual.CO2e.non.baseload.output.emission.rate..lb.MWh. * 453.592 / 1000

egrid_simp <- egrid_simp[, c(1, 5:7)]

#view(egrid_simp)

years <- seq(2022, 2035, 1)
minus_matrix <- egrid_simp[, 2:4]/14

for (i in 1:length(years)) {
  
  a <- egrid_simp
  a$year <- years[i]
  
  if (i == 1) {
    
    decrease_egrid <- a
    
    egrid_running_total <- decrease_egrid %>%  group_by(ï..eGRID.subregion.acronym) %>% 
      summarise(eGRID.non.baseload.NOx..g.kWh. = sum(eGRID.non.baseload.NOx..g.kWh.),
                eGRID.non.baseload.SO2..g.kWh. = sum(eGRID.non.baseload.SO2..g.kWh.),
                eGRID.non.baseload.CO2e..g.kWh. = sum(eGRID.non.baseload.CO2e..g.kWh.))
    
    egrid_running_total$year.start <- years[i]
    
  } else {
    
    a[, 2:4] <- egrid_simp[, 2:4] - (i - 1) * minus_matrix
    
    decrease_egrid <- rbind.data.frame(decrease_egrid, a)
    
    b <- decrease_egrid %>%  group_by(ï..eGRID.subregion.acronym) %>% 
      summarise(eGRID.non.baseload.NOx..g.kWh. = sum(eGRID.non.baseload.NOx..g.kWh.),
                eGRID.non.baseload.SO2..g.kWh. = sum(eGRID.non.baseload.SO2..g.kWh.),
                eGRID.non.baseload.CO2e..g.kWh. = sum(eGRID.non.baseload.CO2e..g.kWh.))
    
    b$year.start <- years[i] 
    
    egrid_running_total <- rbind.data.frame(egrid_running_total, b)
    
    
  }
  
}

total_decrease_egrid <- decrease_egrid %>%  group_by(ï..eGRID.subregion.acronym) %>% 
  summarise(eGRID.non.baseload.NOx..g.kWh. = sum(eGRID.non.baseload.NOx..g.kWh.),
            eGRID.non.baseload.SO2..g.kWh. = sum(eGRID.non.baseload.SO2..g.kWh.),
            eGRID.non.baseload.CO2e..g.kWh. = sum(eGRID.non.baseload.CO2e..g.kWh.))

## eGRID EMISSIONS ARE SUMMED FROM 2022 UNTIL 2035

egrid_emissions <- left_join(total_decrease_egrid, egrid_zip_simple, 
                             by = c ("ï..eGRID.subregion.acronym" = "eGRID.Subregion..1"))


for (i in unique(egrid_running_total$year.start)) {
  
  egrid_running_total[egrid_running_total$year.start == i, 2:4] <- total_decrease_egrid[, 2:4] - 
    egrid_running_total[egrid_running_total$year.start == i, 2:4]
  
}

vary_deg <- left_join(egrid_running_total, egrid_zip_simple, 
                      by = c ("ï..eGRID.subregion.acronym" = "eGRID.Subregion..1"))

plot_data <- filter(egrid_running_total, ï..eGRID.subregion.acronym == "SRVC")
qplot(x = year.start, y = eGRID.non.baseload.CO2e..g.kWh., data = plot_data)


deg_rc_1 <- replacement_case_1[, c(1:20, 24, 25)]

egrid_emissions$ZIP..numeric. <- as.numeric(egrid_emissions$ZIP..numeric.)

deg_replace_case_1 <- left_join(deg_rc_1, egrid_emissions, by = c("Zip.Code" = "ZIP..numeric."))

deg_replace_case_1 <- left_join(deg_replace_case_1, emfac_fixed, by = c("cFuel.Type" = "Fuel", "Manufacture.Year" = "Model.Year"))
deg_replace_case_1 <- left_join(deg_replace_case_1, welltotank, by = c("cFuel.Type" = "ï..Fuel.Type"))
deg_replace_case_1 <- cbind(deg_replace_case_1, batt_emissions)

#### Decreasing eGRID: First Scenario ####

#Combine fuel emissions to make them from well to wheel emissions
deg_replace_case_1$fuel.wtt..VOC..g.mi <- deg_replace_case_1$fuel.wtt..VOC..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.wtt..SOx..g.mi <- deg_replace_case_1$fuel.wtt..SOx..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.wtt..PM2.5..g.mi <- deg_replace_case_1$fuel.wtt..PM2.5..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.wtt..PM10..g.mi <- deg_replace_case_1$fuel.wtt..PM10..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.wtt..OC..g.mi <- deg_replace_case_1$fuel.wtt..OC..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.wtt..BC..g.mi <- deg_replace_case_1$fuel.wtt..BC..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.wtt..CO..g.mi <- deg_replace_case_1$fuel.wtt..CO..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.wtt..NOx..g.mi <- deg_replace_case_1$fuel.wtt..NOx..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.wtt..CH4..g.mi <- deg_replace_case_1$fuel.wtt..CH4..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.wtt..N2O..g.mi <- deg_replace_case_1$fuel.wtt..N2O..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.wtt..CO2..g.mi <- deg_replace_case_1$fuel.wtt..CO2..g.gal / deg_replace_case_1$Average.Diesel.mpg

deg_replace_case_1$fuel.ttw..NOx_RUNEX..g.mi <- deg_replace_case_1$fuel.ttw..NOx_RUNEX..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.ttw..CH4_RUNEX..g.mi <- deg_replace_case_1$fuel.ttw..CH4_RUNEX..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.ttw..CO2_RUNEX..g.mi <- deg_replace_case_1$fuel.ttw..CO2_RUNEX..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.ttw..CO_RUNEX..g.mi <- deg_replace_case_1$fuel.ttw..CO_RUNEX..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.ttw..N2O_RUNEX..g.mi <- deg_replace_case_1$fuel.ttw..N2O_RUNEX..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.ttw..PM10_RUNEX..g.mi <- deg_replace_case_1$fuel.ttw..PM10_RUNEX..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.ttw..PM2.5_RUNEX..g.mi <- deg_replace_case_1$fuel.ttw..PM2.5_RUNEX..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.ttw..ROG_RUNEX..g.mi <- deg_replace_case_1$fuel.ttw..ROG_RUNEX..g.gal / deg_replace_case_1$Average.Diesel.mpg
deg_replace_case_1$fuel.ttw..SOx_RUNEX..g.mi <- deg_replace_case_1$fuel.ttw..SOx_RUNEX..g.gal / deg_replace_case_1$Average.Diesel.mpg

for (i in 1:nrow(deg_replace_case_1)) {
  
  if (deg_replace_case_1$cFuel.Type[i] == "Gasoline"){
    
    deg_replace_case_1$fuel.wtt..VOC..g.mi[i] <- deg_replace_case_1$fuel.wtt..VOC..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.wtt..SOx..g.mi[i] <- deg_replace_case_1$fuel.wtt..SOx..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.wtt..PM2.5..g.mi[i] <- deg_replace_case_1$fuel.wtt..PM2.5..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.wtt..PM10..g.mi[i] <- deg_replace_case_1$fuel.wtt..PM10..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.wtt..OC..g.mi[i] <- deg_replace_case_1$fuel.wtt..OC..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.wtt..BC..g.mi[i] <- deg_replace_case_1$fuel.wtt..BC..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.wtt..CO..g.mi[i] <- deg_replace_case_1$fuel.wtt..CO..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.wtt..NOx..g.mi[i] <- deg_replace_case_1$fuel.wtt..NOx..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.wtt..CH4..g.mi[i] <- deg_replace_case_1$fuel.wtt..CH4..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.wtt..N2O..g.mi[i] <- deg_replace_case_1$fuel.wtt..N2O..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.wtt..CO2..g.mi[i] <- deg_replace_case_1$fuel.wtt..CO2..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    
    deg_replace_case_1$fuel.ttw..NOx_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..NOx_RUNEX..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.ttw..CH4_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..CH4_RUNEX..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.ttw..CO2_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..CO2_RUNEX..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.ttw..CO_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..CO_RUNEX..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.ttw..N2O_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..N2O_RUNEX..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.ttw..PM10_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..PM10_RUNEX..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.ttw..PM2.5_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..PM2.5_RUNEX..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.ttw..ROG_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..ROG_RUNEX..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    deg_replace_case_1$fuel.ttw..SOx_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..SOx_RUNEX..g.gal[i] / deg_replace_case_1$Average.Gasoline.mpg[i]
    
  } else if (deg_replace_case_1$cFuel.Type[i] == "Compressed Natural Gas") {
    
    deg_replace_case_1$fuel.wtt..VOC..g.mi[i] <- deg_replace_case_1$fuel.wtt..VOC..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.wtt..SOx..g.mi[i] <- deg_replace_case_1$fuel.wtt..SOx..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.wtt..PM2.5..g.mi[i] <- deg_replace_case_1$fuel.wtt..PM2.5..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.wtt..PM10..g.mi[i] <- deg_replace_case_1$fuel.wtt..PM10..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.wtt..OC..g.mi[i] <- deg_replace_case_1$fuel.wtt..OC..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.wtt..BC..g.mi[i] <- deg_replace_case_1$fuel.wtt..BC..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.wtt..CO..g.mi[i] <- deg_replace_case_1$fuel.wtt..CO..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.wtt..NOx..g.mi[i] <- deg_replace_case_1$fuel.wtt..NOx..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.wtt..CH4..g.mi[i] <- deg_replace_case_1$fuel.wtt..CH4..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.wtt..N2O..g.mi[i] <- deg_replace_case_1$fuel.wtt..N2O..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.wtt..CO2..g.mi[i] <- deg_replace_case_1$fuel.wtt..CO2..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    
    deg_replace_case_1$fuel.ttw..NOx_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..NOx_RUNEX..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.ttw..CH4_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..CH4_RUNEX..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.ttw..CO2_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..CO2_RUNEX..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.ttw..CO_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..CO_RUNEX..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.ttw..N2O_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..N2O_RUNEX..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.ttw..PM10_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..PM10_RUNEX..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.ttw..PM2.5_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..PM2.5_RUNEX..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.ttw..ROG_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..ROG_RUNEX..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    deg_replace_case_1$fuel.ttw..SOx_RUNEX..g.mi[i] <- deg_replace_case_1$fuel.ttw..SOx_RUNEX..g.gal[i] / deg_replace_case_1$Average.CNG.mpg[i]
    
  } else if (deg_replace_case_1$cFuel.Type[i] == "Electric Battery"){
    
    # electric equivalent is the emissions from electricity generation
    # encapsulated in the elecgen emission below
    
    deg_replace_case_1$fuel.wtt..VOC..g.mi[i] <- 0
    deg_replace_case_1$fuel.wtt..SOx..g.mi[i] <- 0
    deg_replace_case_1$fuel.wtt..PM2.5..g.mi[i] <- 0
    deg_replace_case_1$fuel.wtt..PM10..g.mi[i] <- 0
    deg_replace_case_1$fuel.wtt..OC..g.mi[i] <- 0
    deg_replace_case_1$fuel.wtt..BC..g.mi[i] <- 0
    deg_replace_case_1$fuel.wtt..CO..g.mi[i] <- 0
    deg_replace_case_1$fuel.wtt..NOx..g.mi[i] <- 0
    deg_replace_case_1$fuel.wtt..CH4..g.mi[i] <- 0
    deg_replace_case_1$fuel.wtt..N2O..g.mi[i] <- 0
    deg_replace_case_1$fuel.wtt..CO2..g.mi[i] <- 0
    
    deg_replace_case_1$fuel.ttw..NOx_RUNEX..g.mi[i] <- 0
    deg_replace_case_1$fuel.ttw..CH4_RUNEX..g.mi[i] <- 0
    deg_replace_case_1$fuel.ttw..CO2_RUNEX..g.mi[i] <- 0
    deg_replace_case_1$fuel.ttw..CO_RUNEX..g.mi[i] <- 0
    deg_replace_case_1$fuel.ttw..N2O_RUNEX..g.mi[i] <- 0
    deg_replace_case_1$fuel.ttw..PM10_RUNEX..g.mi[i] <- 0
    deg_replace_case_1$fuel.ttw..PM2.5_RUNEX..g.mi[i] <- 0
    deg_replace_case_1$fuel.ttw..ROG_RUNEX..g.mi[i] <- 0
    deg_replace_case_1$fuel.ttw..SOx_RUNEX..g.mi[i] <- 0
    
  }
}

deg_replace_case_1$fuel..NOx..g.mi <- deg_replace_case_1$fuel.wtt..NOx..g.mi + deg_replace_case_1$fuel.ttw..NOx_RUNEX..g.mi
deg_replace_case_1$fuel..CH4..g.mi <- deg_replace_case_1$fuel.ttw..CH4_RUNEX..g.mi + deg_replace_case_1$fuel.wtt..CH4..g.mi
deg_replace_case_1$fuel..CO2..g.mi <- deg_replace_case_1$fuel.ttw..CO2_RUNEX..g.mi + deg_replace_case_1$fuel.wtt..CO2..g.mi
deg_replace_case_1$fuel..CO..g.mi <- deg_replace_case_1$fuel.ttw..CO_RUNEX..g.mi + deg_replace_case_1$fuel.wtt..CO..g.mi
deg_replace_case_1$fuel..N2O..g.mi <- deg_replace_case_1$fuel.ttw..N2O_RUNEX..g.mi + deg_replace_case_1$fuel.wtt..N2O..g.mi
deg_replace_case_1$fuel..PM10..g.mi <- deg_replace_case_1$fuel.ttw..PM10_RUNEX..g.mi + deg_replace_case_1$fuel.ttw..PM10_PMTW..g.mi + deg_replace_case_1$fuel.ttw..PM10_PMBW..g.mi + deg_replace_case_1$fuel.wtt..PM10..g.mi
deg_replace_case_1$fuel..PM2.5..g.mi <- deg_replace_case_1$fuel.ttw..PM2.5_RUNEX..g.mi + deg_replace_case_1$fuel.ttw..PM2.5_PMTW..g.mi + deg_replace_case_1$fuel.ttw..PM2.5_PMBW..g.mi + deg_replace_case_1$fuel.wtt..PM2.5..g.mi
deg_replace_case_1$fuel..VOC..g.mi <- deg_replace_case_1$fuel.wtt..VOC..g.mi + deg_replace_case_1$fuel.ttw..ROG_RUNEX..g.mi
deg_replace_case_1$fuel..SOx..g.mi <- deg_replace_case_1$fuel.ttw..SOx_RUNEX..g.mi + deg_replace_case_1$fuel.wtt..SOx..g.mi


#Reintroduce NA's into average EB mileage to make calculations easier
deg_replace_case_1[which(deg_replace_case_1$Fuel.Type != "Electric Battery"), "Average.EB.mpkWh"] <- NA

summary(deg_replace_case_1$Average.EB.mpkWh)

#Rearrage dataframe
deg_replace_case_1 <- deg_replace_case_1[, c("NTD.ID",
                                             "Agency.Name.x",
                                             "Modes",
                                             "Manufacture.Year",
                                             "Final.Year",
                                             "Useful.Life.Benchmark",
                                             "Fuel.Type",
                                             "cFuel.Type",
                                             "City",
                                             "State",
                                             "Zip.Code",
                                             "Total.Fleet.Vehicles",
                                             "Active.Fleet.Vehicles",
                                             "Total.Miles.on.Active.Vehicles.During.Period",
                                             "Average.Number.of.Seats",
                                             "Average.Diesel.mpg",
                                             "Average.Gasoline.mpg",
                                             "Average.CNG.mpg",
                                             "Average.EB.mpkWh",
                                             "batt..VOC..g.kWh",
                                             "batt..CO..g.kWh",
                                             "batt..NOx..g.kWh",
                                             "batt..PM10..g.kWh",
                                             "batt..PM2.5..g.kWh",
                                             "batt..SOx..g.kWh",
                                             "batt..BC..g.kWh",
                                             "batt..OC..g.kWh",
                                             "batt..GHGs..g.kWh",
                                             "eGRID.non.baseload.NOx..g.kWh.",
                                             "eGRID.non.baseload.SO2..g.kWh.",
                                             "eGRID.non.baseload.CO2e..g.kWh.",
                                             "fuel.wtt..VOC..g.mi",
                                             "fuel.wtt..SOx..g.mi",
                                             "fuel.wtt..PM2.5..g.mi",
                                             "fuel.wtt..PM10..g.mi",
                                             "fuel.wtt..OC..g.mi",
                                             "fuel.wtt..BC..g.mi",
                                             "fuel.wtt..CO..g.mi",
                                             "fuel.wtt..NOx..g.mi",
                                             "fuel.wtt..CH4..g.mi",
                                             "fuel.wtt..N2O..g.mi",
                                             "fuel.wtt..CO2..g.mi",
                                             "fuel.ttw..NOx_RUNEX..g.mi",
                                             "fuel.ttw..CH4_RUNEX..g.mi",
                                             "fuel.ttw..CO2_RUNEX..g.mi",
                                             "fuel.ttw..CO_RUNEX..g.mi",
                                             "fuel.ttw..N2O_RUNEX..g.mi",
                                             "fuel.ttw..PM10_RUNEX..g.mi",
                                             "fuel.ttw..PM2.5_RUNEX..g.mi",
                                             "fuel.ttw..ROG_RUNEX..g.mi",
                                             "fuel.ttw..SOx_RUNEX..g.mi",
                                             "fuel.ttw..PM2.5_PMTW..g.mi",
                                             "fuel.ttw..PM2.5_PMBW..g.mi",
                                             "fuel.ttw..PM10_PMTW..g.mi",
                                             "fuel.ttw..PM10_PMBW..g.mi",
                                             "fuel..NOx..g.mi",
                                             "fuel..CH4..g.mi",
                                             "fuel..CO2..g.mi",
                                             "fuel..CO..g.mi",
                                             "fuel..N2O..g.mi",
                                             "fuel..PM10..g.mi",
                                             "fuel..PM2.5..g.mi",
                                             "fuel..VOC..g.mi",
                                             "fuel..SOx..g.mi")]

#Assume a battery capacity (kWh) for all current BEBs anf future replacement BEBs
batt_cap <- 450
hybrid_cap <- 10

elec_eff_total <- elec_transm_dist_eff * plug_eff

#Assume how many years the fossil fuel buses will be used for
assum_bus_life <- 14
# The new BEB life in this scenario will be from 2022 until 2035
# The values for the 3 eGRID parameters are already summed over this time. (14 years)

#How many batteries will be needed throughout the BEB's lifetime
batt_replace <- 1

deg_replace_case_1$elecgen_NOx..mt <- (deg_replace_case_1$eGRID.non.baseload.NOx..g.kWh. / 
                                         deg_replace_case_1$Average.EB.mpkWh * 
                                         deg_replace_case_1$Total.Miles.on.Active.Vehicles.During.Period /
                                         elec_eff_total) / 1000000
deg_replace_case_1$elecgen_SOx..mt <- (deg_replace_case_1$eGRID.non.baseload.SO2..g.kWh. / 
                                         deg_replace_case_1$Average.EB.mpkWh * 
                                         deg_replace_case_1$Total.Miles.on.Active.Vehicles.During.Period /
                                         elec_eff_total) / 1000000
deg_replace_case_1$elecgen_CO2e..mt <- (deg_replace_case_1$eGRID.non.baseload.CO2e..g.kWh. /
                                          deg_replace_case_1$Average.EB.mpkWh * 
                                          deg_replace_case_1$Total.Miles.on.Active.Vehicles.During.Period /
                                          elec_eff_total) / 1000000

deg_replace_case_1$batt_VOC..mt <- 0
deg_replace_case_1$batt_CO..mt <- 0
deg_replace_case_1$batt_NOx..mt <- 0
deg_replace_case_1$batt_PM10..mt <- 0
deg_replace_case_1$batt_PM2.5..mt <- 0
deg_replace_case_1$batt_SOx..mt <- 0
deg_replace_case_1$batt_BC..mt <- 0
deg_replace_case_1$batt_OC..mt <- 0
deg_replace_case_1$batt_CO2e..mt <- 0

for (i in 1:nrow(deg_replace_case_1)) {
  if (deg_replace_case_1[i, "Fuel.Type"] == "Electric Battery"){
    
    deg_replace_case_1[i, "batt_VOC..mt"] <- ((deg_replace_case_1[i, "batt..VOC..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_CO..mt"] <- ((deg_replace_case_1[i, "batt..CO..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_NOx..mt"] <- ((deg_replace_case_1[i, "batt..NOx..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_PM10..mt"] <- ((deg_replace_case_1[i, "batt..PM10..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_PM2.5..mt"] <- ((deg_replace_case_1[i, "batt..PM2.5..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_SOx..mt"] <- ((deg_replace_case_1[i, "batt..SOx..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_BC..mt"] <- ((deg_replace_case_1[i, "batt..BC..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_OC..mt"] <- ((deg_replace_case_1[i, "batt..OC..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_CO2e..mt"] <- ((deg_replace_case_1[i, "batt..GHGs..g.kWh"] * batt_cap * (1 + batt_replace))/ 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
  
    } else if (deg_replace_case_1[i, "Fuel.Type"] == "Hybrid Gasoline") {
    
    deg_replace_case_1[i, "batt_VOC..mt"] <- (deg_replace_case_1[i, "batt..VOC..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_CO..mt"] <- (deg_replace_case_1[i, "batt..CO..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_NOx..mt"] <- (deg_replace_case_1[i, "batt..NOx..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_PM10..mt"] <- (deg_replace_case_1[i, "batt..PM10..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_PM2.5..mt"] <- (deg_replace_case_1[i, "batt..PM2.5..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_SOx..mt"] <- (deg_replace_case_1[i, "batt..SOx..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_BC..mt"] <- (deg_replace_case_1[i, "batt..BC..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_OC..mt"] <- (deg_replace_case_1[i, "batt..OC..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_CO2e..mt"] <- (deg_replace_case_1[i, "batt..GHGs..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    
  } else if (deg_replace_case_1[i, "Fuel.Type"] == "Hybrid Diesel"){
    
    deg_replace_case_1[i, "batt_VOC..mt"] <- (deg_replace_case_1[i, "batt..VOC..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_CO..mt"] <- (deg_replace_case_1[i, "batt..CO..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_NOx..mt"] <- (deg_replace_case_1[i, "batt..NOx..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_PM10..mt"] <- (deg_replace_case_1[i, "batt..PM10..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_PM2.5..mt"] <- (deg_replace_case_1[i, "batt..PM2.5..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_SOx..mt"] <- (deg_replace_case_1[i, "batt..SOx..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_BC..mt"] <- (deg_replace_case_1[i, "batt..BC..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_OC..mt"] <- (deg_replace_case_1[i, "batt..OC..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    deg_replace_case_1[i, "batt_CO2e..mt"] <- (deg_replace_case_1[i, "batt..GHGs..g.kWh"] * hybrid_cap / 1000000) * deg_replace_case_1[i, "Active.Fleet.Vehicles"]
    
  }
}

deg_replace_case_1$mobile_CO2e..mt <- ((deg_replace_case_1$fuel..CO2..g.mi + ch4_gwp*deg_replace_case_1$fuel..CH4..g.mi + n2o_gwp*deg_replace_case_1$fuel..N2O..g.mi) * deg_replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life)/1000000
deg_replace_case_1$mobile_CO..mt <- (deg_replace_case_1$fuel..CO..g.mi * deg_replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
deg_replace_case_1$mobile_NOx..mt <- (deg_replace_case_1$fuel..NOx..g.mi * deg_replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
deg_replace_case_1$mobile_PM10..mt <- (deg_replace_case_1$fuel..PM10..g.mi * deg_replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
deg_replace_case_1$mobile_PM2.5..mt <- (deg_replace_case_1$fuel..PM2.5..g.mi * deg_replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
deg_replace_case_1$mobile_VOC..mt <- (deg_replace_case_1$fuel..VOC..g.mi * deg_replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
deg_replace_case_1$mobile_SOx..mt <- (deg_replace_case_1$fuel..SOx..g.mi * deg_replace_case_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000

# Sum totals
deg_replace_case_1$total_CO2e..mt <- rowSums(deg_replace_case_1[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
deg_replace_case_1$total_NOx..mt <- rowSums(deg_replace_case_1[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
deg_replace_case_1$total_SOx..mt <- rowSums(deg_replace_case_1[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
deg_replace_case_1$total_PM10..mt <- rowSums(deg_replace_case_1[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
deg_replace_case_1$total_PM2.5..mt <- rowSums(deg_replace_case_1[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
deg_replace_case_1$total_VOC..mt <- rowSums(deg_replace_case_1[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
deg_replace_case_1$total_CO..mt <- rowSums(deg_replace_case_1[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)

summary(deg_replace_case_1[, 84:90])

# If current fleet makeup persists for the next 12 years:
deg_baseline <- deg_replace_case_1 %>% 
  group_by(Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_si_baseline <- deg_replace_case_1 %>% 
  group_by(Agency.Name.x, Zip.Code, State, Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_baseline$Scenario <- 0
deg_si_baseline$Scenario <- 0

# Switch fossil fuel vehicles to Electric Battery and redo analysis to simulate an immediate replacement
deg_flip_1 <- as.data.frame(deg_replace_case_1)
deg_flip_1$Fuel.Type <- "Electric Battery"
deg_flip_1[is.na(deg_flip_1[, "Average.EB.mpkWh"]), "Average.EB.mpkWh"] <- eb.replace

deg_flip_1$elecgen_NOx..mt <- (deg_flip_1$eGRID.non.baseload.NOx..g.kWh /
                                 deg_flip_1$Average.EB.mpkWh * 
                                 deg_flip_1$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
deg_flip_1$elecgen_SOx..mt <- (deg_flip_1$eGRID.non.baseload.SO2..g.kWh /
                                 deg_flip_1$Average.EB.mpkWh * 
                                 deg_flip_1$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
deg_flip_1$elecgen_CO2e..mt <- (deg_flip_1$eGRID.non.baseload.CO2e..g.kWh /
                                  deg_flip_1$Average.EB.mpkWh * 
                                  deg_flip_1$Total.Miles.on.Active.Vehicles.During.Period /
                                  elec_eff_total) / 1000000
deg_flip_1$batt_VOC..mt <- 0
deg_flip_1$batt_CO..mt <- 0
deg_flip_1$batt_NOx..mt <- 0
deg_flip_1$batt_PM10..mt <- 0
deg_flip_1$batt_PM2.5..mt <- 0
deg_flip_1$batt_SOx..mt <- 0
deg_flip_1$batt_BC..mt <- 0
deg_flip_1$batt_OC..mt <- 0
deg_flip_1$batt_CO2e..mt <- 0

deg_flip_1$batt_VOC..mt <- ((deg_flip_1$batt..VOC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_1$Active.Fleet.Vehicles
deg_flip_1$batt_CO..mt <- ((deg_flip_1$batt..CO..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_1$Active.Fleet.Vehicles
deg_flip_1$batt_NOx..mt <- ((deg_flip_1$batt..NOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_1$Active.Fleet.Vehicles
deg_flip_1$batt_PM10..mt <- ((deg_flip_1$batt..PM10..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_1$Active.Fleet.Vehicles
deg_flip_1$batt_PM2.5..mt <- ((deg_flip_1$batt..PM2.5..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_1$Active.Fleet.Vehicles
deg_flip_1$batt_SOx..mt <- ((deg_flip_1$batt..SOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_1$Active.Fleet.Vehicles
deg_flip_1$batt_BC..mt <- ((deg_flip_1$batt..BC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_1$Active.Fleet.Vehicles
deg_flip_1$batt_OC..mt <- ((deg_flip_1$batt..OC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_1$Active.Fleet.Vehicles
deg_flip_1$batt_CO2e..mt <- ((deg_flip_1$batt..GHGs..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_1$Active.Fleet.Vehicles

deg_flip_1$mobile_CO2e..mt <- 0
deg_flip_1$mobile_CO..mt <- 0
deg_flip_1$mobile_NOx..mt <- 0
deg_flip_1$mobile_PM10..mt <- (beb_replace$fuel..PM10..g.mi * deg_flip_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
deg_flip_1$mobile_PM2.5..mt <- (beb_replace$fuel..PM2.5..g.mi * deg_flip_1$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
deg_flip_1$mobile_VOC..mt <- 0
deg_flip_1$mobile_SOx..mt <- 0

# Sum totals
deg_flip_1$total_CO2e..mt <- rowSums(deg_flip_1[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
deg_flip_1$total_NOx..mt <- rowSums(deg_flip_1[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
deg_flip_1$total_SOx..mt <- rowSums(deg_flip_1[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
deg_flip_1$total_PM10..mt <- rowSums(deg_flip_1[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
deg_flip_1$total_PM2.5..mt <- rowSums(deg_flip_1[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
deg_flip_1$total_VOC..mt <- rowSums(deg_flip_1[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
deg_flip_1$total_CO..mt <- rowSums(deg_flip_1[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)

deg_si_flip_1 <- deg_flip_1 %>% 
  group_by(Agency.Name.x, Zip.Code, State, Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE),  
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_flip_1_sum <- deg_flip_1 %>% 
  group_by(Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_flip_1_sum$Scenario <- 1
deg_si_flip_1$Scenario <- 1

deg_running_total <- rbind.data.frame(deg_baseline, deg_flip_1_sum)
deg_si_running_total <- rbind.data.frame(deg_si_baseline, deg_si_flip_1)

#### Decreasing eGRID: Second Scenario ####

service_clip <- annual_service[,c(1, 23, 21:22)]
deg_replace_case_2 <- left_join(deg_replace_case_1, service_clip,
                            by = c("NTD.ID", "Modes")) %>% as.data.frame()
deg_replace_case_2$add.total.bus <- 0
deg_replace_case_2$add.active.bus <- 0
deg_replace_case_2$Daily.Miles.per.Vehicle <- 0
deg_replace_case_2$rep.factor <- 0
mile_cutoff <- 100

for (i in 1:nrow(deg_replace_case_2)) {
  if(deg_replace_case_2[i, "Days.of.Service.Operated"] == 0){
    # If agencies do not report their number of days of service operated in a year, give them 5 days/week out of 52 weeks/year
    deg_replace_case_2[i, "Days.of.Service.Operated"] <- 260
  }
  
  if(deg_replace_case_2[i, "Active.Fleet.Vehicles"] > 0){
    deg_replace_case_2$Daily.Miles.per.Vehicle[i] <- deg_replace_case_2$Total.Miles.on.Active.Vehicles.During.Period[i]/deg_replace_case_2$Active.Fleet.Vehicles[i]/deg_replace_case_2$Days.of.Service.Operated[i]
  } else{
    deg_replace_case_2$Daily.Miles.per.Vehicle[i] <- 0
  }
  
  deg_replace_case_2[is.na(deg_replace_case_2)] <- 0
  deg_replace_case_2$rep.factor[i] <- (deg_replace_case_2[i, "Daily.Miles.per.Vehicle"]/mile_cutoff) %>% trunc()
  deg_replace_case_2$replace.total.no.bus[i] <- deg_replace_case_2$Total.Fleet.Vehicles[i] * (1 + deg_replace_case_2$rep.factor[i])
  deg_replace_case_2$replace.active.no.bus[i] <- deg_replace_case_2$Active.Fleet.Vehicles[i] * (1 + deg_replace_case_2$rep.factor[i])
  deg_replace_case_2$add.total.bus[i] <- (deg_replace_case_2$replace.total.no.bus[i] - deg_replace_case_2$Total.Fleet.Vehicles[i])
  deg_replace_case_2$add.active.bus[i] <- (deg_replace_case_2$replace.active.no.bus[i] - deg_replace_case_2$Active.Fleet.Vehicles[i])
  
}

# See "Second Scenario: Replacement on daily miles" for cost estimates

# Recalculate Emissions to account for the extra production in batteries
# Switch fossil fuel vehicles to Electric Battery and redo analysis to simulate an immediate deg_replacement
deg_flip_2 <- deg_replace_case_2
deg_flip_2$Fuel.Type <- "Electric Battery"
deg_flip_2[is.na(deg_flip_2[, "Average.EB.mpkWh"]), "Average.EB.mpkWh"] <- eb.replace
deg_flip_2[deg_flip_2$Average.EB.mpkWh <= 0.001, "Average.EB.mpkWh"] <- eb.replace

deg_flip_2$elecgen_NOx..mt <- (deg_flip_2$eGRID.non.baseload.NOx..g.kWh. / 
                                 deg_flip_2$Average.EB.mpkWh * 
                                 deg_flip_2$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
deg_flip_2$elecgen_SOx..mt <- (deg_flip_2$eGRID.non.baseload.SO2..g.kWh. / 
                                 deg_flip_2$Average.EB.mpkWh * 
                                 deg_flip_2$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
deg_flip_2$elecgen_CO2e..mt <- (deg_flip_2$eGRID.non.baseload.CO2e..g.kWh. / 
                                  deg_flip_2$Average.EB.mpkWh * 
                                  deg_flip_2$Total.Miles.on.Active.Vehicles.During.Period /
                                  elec_eff_total) / 1000000

deg_flip_2$batt_VOC..mt <- 0
deg_flip_2$batt_CO..mt <- 0
deg_flip_2$batt_NOx..mt <- 0
deg_flip_2$batt_PM10..mt <- 0
deg_flip_2$batt_PM2.5..mt <- 0
deg_flip_2$batt_SOx..mt <- 0
deg_flip_2$batt_BC..mt <- 0
deg_flip_2$batt_OC..mt <- 0
deg_flip_2$batt_CO2e..mt <- 0

deg_flip_2$batt_VOC..mt <- ((deg_flip_2$batt..VOC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_2$replace.active.no.bus
deg_flip_2$batt_CO..mt <- ((deg_flip_2$batt..CO..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_2$replace.active.no.bus
deg_flip_2$batt_NOx..mt <- ((deg_flip_2$batt..NOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_2$replace.active.no.bus
deg_flip_2$batt_PM10..mt <- ((deg_flip_2$batt..PM10..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_2$replace.active.no.bus
deg_flip_2$batt_PM2.5..mt <- ((deg_flip_2$batt..PM2.5..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_2$replace.active.no.bus
deg_flip_2$batt_SOx..mt <- ((deg_flip_2$batt..SOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_2$replace.active.no.bus
deg_flip_2$batt_BC..mt <- ((deg_flip_2$batt..BC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_2$replace.active.no.bus
deg_flip_2$batt_OC..mt <- ((deg_flip_2$batt..OC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_2$replace.active.no.bus
deg_flip_2$batt_CO2e..mt <- ((deg_flip_2$batt..GHGs..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_2$replace.active.no.bus

deg_flip_2$mobile_CO2e..mt <- 0
deg_flip_2$mobile_CO..mt <- 0
deg_flip_2$mobile_NOx..mt <- 0
deg_flip_2$mobile_PM10..mt <- (beb_replace$fuel..PM10..g.mi * deg_flip_2$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
deg_flip_2$mobile_PM2.5..mt <- (beb_replace$fuel..PM2.5..g.mi * deg_flip_2$Total.Miles.on.Active.Vehicles.During.Period * assum_bus_life) / 1000000
deg_flip_2$mobile_VOC..mt <- 0
deg_flip_2$mobile_SOx..mt <- 0

# Sum totals
deg_flip_2$total_CO2e..mt <- rowSums(deg_flip_2[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
deg_flip_2$total_NOx..mt <- rowSums(deg_flip_2[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
deg_flip_2$total_SOx..mt <- rowSums(deg_flip_2[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
deg_flip_2$total_PM10..mt <- rowSums(deg_flip_2[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
deg_flip_2$total_PM2.5..mt <- rowSums(deg_flip_2[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
deg_flip_2$total_VOC..mt <- rowSums(deg_flip_2[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
deg_flip_2$total_CO..mt <- rowSums(deg_flip_2[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)

deg_si_flip_2 <- deg_flip_2 %>% 
  group_by(Agency.Name.x, Zip.Code, State, Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_flip_2_sum <- deg_flip_2 %>% 
  group_by(Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_flip_2_sum$Scenario <- 2
deg_si_flip_2$Scenario <- 2

deg_running_total <- rbind.data.frame(deg_running_total, deg_flip_2_sum)
deg_si_running_total <- rbind.data.frame(deg_si_running_total, deg_si_flip_2)

#### Decreasing eGRID: Third Scenario ####

# Also dependent on daily range of BEB replacements
# To change this, adjust mile_cutoff in scen 2 and redo chunk (line 713)

deg_flip_3 <- deg_replace_case_2

#ice.years <14 will have (14 - ice.years) number of years as a BEB
vary_deg$ZIP..numeric. <- as.numeric(vary_deg$ZIP..numeric.)

# Calculate how many years of ice usage
deg_flip_3$ice.years <- deg_flip_3$Final.Year - 2022
# Give it upper and lower bounds
deg_flip_3[deg_flip_3$ice.years < 0, "ice.years"] <- 0
deg_flip_3[deg_flip_3$Final.Year < 2022, "Final.Year"] <- 2022
deg_flip_3[deg_flip_3$ice.years > assum_bus_life, "ice.years"] <- assum_bus_life
deg_flip_3[deg_flip_3$Average.EB.mpkWh <= 0.002, "Average.EB.mpkWh"] <- eb.replace

deg_flip_3 <- left_join(deg_flip_3, vary_deg, by = c("Final.Year" = "year.start", "Zip.Code" = "ZIP..numeric."))
# Account for current BEBs in service
deg_flip_3[deg_flip_3$Fuel.Type == "Electric Battery", "ice.years"] <- 0

deg_flip_3$elecgen_NOx..mt <- (deg_flip_3$eGRID.non.baseload.NOx..g.kWh..y / 
                                 deg_flip_3$Average.EB.mpkWh * 
                                 deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
deg_flip_3$elecgen_SOx..mt <- (deg_flip_3$eGRID.non.baseload.SO2..g.kWh..y / 
                                 deg_flip_3$Average.EB.mpkWh * 
                                 deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
deg_flip_3$elecgen_CO2e..mt <- (deg_flip_3$eGRID.non.baseload.CO2e..g.kWh..y / 
                                  deg_flip_3$Average.EB.mpkWh * 
                                  deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period /
                                  elec_eff_total) / 1000000
deg_flip_3$batt_VOC..mt <- 0
deg_flip_3$batt_CO..mt <- 0
deg_flip_3$batt_NOx..mt <- 0
deg_flip_3$batt_PM10..mt <- 0
deg_flip_3$batt_PM2.5..mt <- 0
deg_flip_3$batt_SOx..mt <- 0
deg_flip_3$batt_BC..mt <- 0
deg_flip_3$batt_OC..mt <- 0
deg_flip_3$batt_CO2e..mt <- 0

deg_flip_3$batt_VOC..mt <- ((deg_flip_3$batt..VOC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_3$replace.active.no.bus
deg_flip_3$batt_CO..mt <- ((deg_flip_3$batt..CO..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_3$replace.active.no.bus
deg_flip_3$batt_NOx..mt <- ((deg_flip_3$batt..NOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_3$replace.active.no.bus
deg_flip_3$batt_PM10..mt <- ((deg_flip_3$batt..PM10..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_3$replace.active.no.bus
deg_flip_3$batt_PM2.5..mt <- ((deg_flip_3$batt..PM2.5..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_3$replace.active.no.bus
deg_flip_3$batt_SOx..mt <- ((deg_flip_3$batt..SOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_3$replace.active.no.bus
deg_flip_3$batt_BC..mt <- ((deg_flip_3$batt..BC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_3$replace.active.no.bus
deg_flip_3$batt_OC..mt <- ((deg_flip_3$batt..OC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_3$replace.active.no.bus
deg_flip_3$batt_CO2e..mt <- ((deg_flip_3$batt..GHGs..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_3$replace.active.no.bus

deg_flip_3$beb.mobile_CO2e..mt <- 0
deg_flip_3$beb.mobile_CO..mt <- 0
deg_flip_3$beb.mobile_NOx..mt <- 0
deg_flip_3$beb.mobile_PM10..mt <- (beb_replace$fuel..PM10..g.mi * deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period * (assum_bus_life - deg_flip_3$ice.years)) / 1000000
deg_flip_3$beb.mobile_PM2.5..mt <- (beb_replace$fuel..PM2.5..g.mi * deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period * (assum_bus_life - deg_flip_3$ice.years)) / 1000000
deg_flip_3$beb.mobile_VOC..mt <- 0
deg_flip_3$beb.mobile_SOx..mt <- 0

deg_flip_3$ice.mobile_CO2e..mt <- ((deg_flip_3$fuel..CO2..g.mi + ch4_gwp*deg_flip_3$fuel..CH4..g.mi + n2o_gwp*deg_flip_3$fuel..N2O..g.mi) * deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_3$ice.years)/1000000
deg_flip_3$ice.mobile_CO..mt <- (deg_flip_3$fuel..CO..g.mi * deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_3$ice.years) / 1000000
deg_flip_3$ice.mobile_NOx..mt <- (deg_flip_3$fuel..NOx..g.mi * deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_3$ice.years) / 1000000
deg_flip_3$ice.mobile_PM10..mt <- (deg_flip_3$fuel..PM10..g.mi * deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_3$ice.years) / 1000000
deg_flip_3$ice.mobile_PM2.5..mt <- (deg_flip_3$fuel..PM2.5..g.mi * deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_3$ice.years) / 1000000
deg_flip_3$ice.mobile_VOC..mt <- (deg_flip_3$fuel..VOC..g.mi * deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_3$ice.years) / 1000000
deg_flip_3$ice.mobile_SOx..mt <- (deg_flip_3$fuel..SOx..g.mi * deg_flip_3$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_3$ice.years) / 1000000

deg_flip_3$mobile_CO2e..mt <- deg_flip_3$beb.mobile_CO2e..mt + deg_flip_3$ice.mobile_CO2e..mt
deg_flip_3$mobile_CO..mt <- deg_flip_3$beb.mobile_CO..mt + deg_flip_3$ice.mobile_CO..mt
deg_flip_3$mobile_NOx..mt <- deg_flip_3$beb.mobile_NOx..mt + deg_flip_3$ice.mobile_NOx..mt
deg_flip_3$mobile_PM10..mt <- deg_flip_3$beb.mobile_PM10..mt + deg_flip_3$ice.mobile_PM10..mt
deg_flip_3$mobile_PM2.5..mt <- deg_flip_3$beb.mobile_PM2.5..mt + deg_flip_3$ice.mobile_PM2.5..mt
deg_flip_3$mobile_VOC..mt <- deg_flip_3$beb.mobile_VOC..mt + deg_flip_3$ice.mobile_VOC..mt
deg_flip_3$mobile_SOx..mt <- deg_flip_3$beb.mobile_SOx..mt + deg_flip_3$ice.mobile_SOx..mt

# Sum totals
deg_flip_3$total_CO2e..mt <- rowSums(deg_flip_3[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
deg_flip_3$total_NOx..mt <- rowSums(deg_flip_3[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
deg_flip_3$total_SOx..mt <- rowSums(deg_flip_3[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
deg_flip_3$total_PM10..mt <- rowSums(deg_flip_3[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
deg_flip_3$total_PM2.5..mt <- rowSums(deg_flip_3[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
deg_flip_3$total_VOC..mt <- rowSums(deg_flip_3[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
deg_flip_3$total_CO..mt <- rowSums(deg_flip_3[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)

deg_si_flip_3 <- deg_flip_3 %>% 
  group_by(Agency.Name.x, Zip.Code, State, Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_flip_3_sum <- deg_flip_3 %>% 
  group_by(Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE), 
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))

deg_flip_3_sum$Scenario <- 3
deg_si_flip_3$Scenario <- 3

deg_running_total <- rbind.data.frame(deg_running_total, deg_flip_3_sum)
deg_si_running_total <- rbind.data.frame(deg_si_running_total, deg_si_flip_3)

#### Decreacing eGRID: Fourth Scenario ####

five_percent <- (sum(deg_flip_3$Total.Fleet.Vehicles)*0.05) %>% round()
deg_replace_case_4 <- deg_flip_3[order(deg_flip_3$Final.Year),]
deg_replace_case_4$total.veh.running.sum <- deg_replace_case_4$Total.Fleet.Vehicles

i <- 2
while (i <= nrow(deg_replace_case_4)) {
  
  deg_replace_case_4[i, "total.veh.running.sum"] <- deg_replace_case_4[i-1, "total.veh.running.sum"] + deg_replace_case_4[i, "total.veh.running.sum"]
  print(paste0(i, "/", nrow(deg_replace_case_4)))
  i <- i+1

}

number_replace <- seq(0, sum(deg_replace_case_4$Total.Fleet.Vehicles) + five_percent, five_percent)
year_replace <- 2022:(2021 + length(number_replace))

deg_replace_case_4$year.replace <- 0

for (i in 1:21) {
  j <- i + 1
  for (k in 1:nrow(deg_replace_case_4)) {
    if ((number_replace[i] <= deg_replace_case_4[k, "total.veh.running.sum"]) && (deg_replace_case_4[k, "total.veh.running.sum"] <= number_replace[j])){
      deg_replace_case_4[k, "year.replace"] <- year_replace[i]
    }
  }
  print(paste0(i, "/21"))
}

deg_scen_4_plot <- deg_replace_case_4 %>% 
  group_by(year.replace, Final.Year) %>% 
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles, na.rm = TRUE), 
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles, na.rm = TRUE), 
            add.total.bus = sum(add.total.bus, na.rm = TRUE), 
            add.active.bus = sum(add.active.bus, na.rm = TRUE))

deg_scen_4_plot$year.behind <- deg_scen_4_plot$year.replace - deg_scen_4_plot$Final.Year

deg_flip_4 <- deg_replace_case_4

# How many years will the buses run on fossil fuel
deg_flip_4$ice.years <- deg_flip_4$year.replace - 2022
# Minimum is zero years
deg_flip_4[deg_flip_4$ice.years < 0, "ice.years"] <- 0

deg_flip_4[deg_flip_4$Final.Year < 2022, "Final.Year"] <- 2022

# If this is greater than the assumed bus life of 14 yrs
# replace with 14 years
deg_flip_4[deg_flip_4$ice.years > assum_bus_life, "ice.years"] <- assum_bus_life
# Give the buses an average EB mpkWh for their flip
deg_flip_4[deg_flip_4$Average.EB.mpkWh <= 0.001, "Average.EB.mpkWh"] <- eb.replace

# Merge the varying electricity grid data by final year (the year the buses are transitioned)
deg_flip_4 <- left_join(deg_flip_4, vary_deg, by = c("Final.Year" = "year.start", "Zip.Code" = "ZIP..numeric."))

#Account for current BEBs in service
deg_flip_4[deg_flip_4$Fuel.Type == "Electric Battery", "ice.years"] <- 0

deg_flip_4$elecgen_NOx..mt <- (deg_flip_4$eGRID.non.baseload.NOx..g.kWh..y / 
                                 deg_flip_4$Average.EB.mpkWh * 
                                 deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
deg_flip_4$elecgen_SOx..mt <- (deg_flip_4$eGRID.non.baseload.SO2..g.kWh..y / 
                                 deg_flip_4$Average.EB.mpkWh * 
                                 deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
deg_flip_4$elecgen_CO2e..mt <- (deg_flip_4$eGRID.non.baseload.CO2e..g.kWh..y / 
                                  deg_flip_4$Average.EB.mpkWh * 
                                  deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period /
                                  elec_eff_total) / 1000000

deg_flip_4$batt_VOC..mt <- 0
deg_flip_4$batt_CO..mt <- 0
deg_flip_4$batt_NOx..mt <- 0
deg_flip_4$batt_PM10..mt <- 0
deg_flip_4$batt_PM2.5..mt <- 0
deg_flip_4$batt_SOx..mt <- 0
deg_flip_4$batt_BC..mt <- 0
deg_flip_4$batt_OC..mt <- 0
deg_flip_4$batt_CO2e..mt <- 0

deg_flip_4$batt_VOC..mt <- ((deg_flip_4$batt..VOC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_4$replace.active.no.bus
deg_flip_4$batt_CO..mt <- ((deg_flip_4$batt..CO..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_4$replace.active.no.bus
deg_flip_4$batt_NOx..mt <- ((deg_flip_4$batt..NOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_4$replace.active.no.bus
deg_flip_4$batt_PM10..mt <- ((deg_flip_4$batt..PM10..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_4$replace.active.no.bus
deg_flip_4$batt_PM2.5..mt <- ((deg_flip_4$batt..PM2.5..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_4$replace.active.no.bus
deg_flip_4$batt_SOx..mt <- ((deg_flip_4$batt..SOx..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_4$replace.active.no.bus
deg_flip_4$batt_BC..mt <- ((deg_flip_4$batt..BC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_4$replace.active.no.bus
deg_flip_4$batt_OC..mt <- ((deg_flip_4$batt..OC..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_4$replace.active.no.bus
deg_flip_4$batt_CO2e..mt <- ((deg_flip_4$batt..GHGs..g.kWh * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_4$replace.active.no.bus

deg_flip_4$beb.mobile_CO2e..mt <- 0
deg_flip_4$beb.mobile_CO..mt <- 0
deg_flip_4$beb.mobile_NOx..mt <- 0
deg_flip_4$beb.mobile_PM10..mt <- (beb_replace$fuel..PM10..g.mi * deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period * (assum_bus_life - deg_flip_4$ice.years)) / 1000000
deg_flip_4$beb.mobile_PM2.5..mt <- (beb_replace$fuel..PM2.5..g.mi * deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period * (assum_bus_life - deg_flip_4$ice.years)) / 1000000
deg_flip_4$beb.mobile_VOC..mt <- 0
deg_flip_4$beb.mobile_SOx..mt <- 0

deg_flip_4$ice.mobile_CO2e..mt <- ((deg_flip_4$fuel..CO2..g.mi + ch4_gwp*deg_flip_4$fuel..CH4..g.mi + n2o_gwp*deg_flip_4$fuel..N2O..g.mi) * deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_4$ice.years)/1000000
deg_flip_4$ice.mobile_CO..mt <- (deg_flip_4$fuel..CO..g.mi * deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_4$ice.years) / 1000000
deg_flip_4$ice.mobile_NOx..mt <- (deg_flip_4$fuel..NOx..g.mi * deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_4$ice.years) / 1000000
deg_flip_4$ice.mobile_PM10..mt <- (deg_flip_4$fuel..PM10..g.mi * deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_4$ice.years) / 1000000
deg_flip_4$ice.mobile_PM2.5..mt <- (deg_flip_4$fuel..PM2.5..g.mi * deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_4$ice.years) / 1000000
deg_flip_4$ice.mobile_VOC..mt <- (deg_flip_4$fuel..VOC..g.mi * deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_4$ice.years) / 1000000
deg_flip_4$ice.mobile_SOx..mt <- (deg_flip_4$fuel..SOx..g.mi * deg_flip_4$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_4$ice.years) / 1000000

deg_flip_4$mobile_CO2e..mt <- deg_flip_4$beb.mobile_CO2e..mt + deg_flip_4$ice.mobile_CO2e..mt
deg_flip_4$mobile_CO..mt <- deg_flip_4$beb.mobile_CO..mt + deg_flip_4$ice.mobile_CO..mt
deg_flip_4$mobile_NOx..mt <- deg_flip_4$beb.mobile_NOx..mt + deg_flip_4$ice.mobile_NOx..mt
deg_flip_4$mobile_PM10..mt <- deg_flip_4$beb.mobile_PM10..mt + deg_flip_4$ice.mobile_PM10..mt
deg_flip_4$mobile_PM2.5..mt <- deg_flip_4$beb.mobile_PM2.5..mt + deg_flip_4$ice.mobile_PM2.5..mt
deg_flip_4$mobile_VOC..mt <- deg_flip_4$beb.mobile_VOC..mt + deg_flip_4$ice.mobile_VOC..mt
deg_flip_4$mobile_SOx..mt <- deg_flip_4$beb.mobile_SOx..mt + deg_flip_4$ice.mobile_SOx..mt

# Sum totals
deg_flip_4$total_CO2e..mt <- rowSums(deg_flip_4[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
deg_flip_4$total_NOx..mt <- rowSums(deg_flip_4[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
deg_flip_4$total_SOx..mt <- rowSums(deg_flip_4[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
deg_flip_4$total_PM10..mt <- rowSums(deg_flip_4[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
deg_flip_4$total_PM2.5..mt <- rowSums(deg_flip_4[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
deg_flip_4$total_VOC..mt <- rowSums(deg_flip_4[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
deg_flip_4$total_CO..mt <- rowSums(deg_flip_4[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)

deg_si_flip_4 <- deg_flip_4 %>% 
  group_by(Agency.Name.x, Zip.Code, State, Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE),
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_flip_4_sum <- deg_flip_4 %>% 
  group_by(Modes, Fuel.Type) %>% 
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE),
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE), 
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE), 
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE), 
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE), 
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE), 
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_flip_4_sum$Scenario <- 4
deg_si_flip_4$Scenario <- 4

deg_running_total <- rbind.data.frame(deg_running_total, deg_flip_4_sum)
deg_si_running_total <- rbind.data.frame(deg_si_running_total, deg_si_flip_4)

#### Decreasing eGRID: Fifth Scenario ####
deg_replace_case_5 <- right_join(top_agency_replace, deg_flip_3, by = "NTD.ID")

deg_replace_case_5[is.na(deg_replace_case_5$flip.yn), "flip.yn"] <- 0
deg_replace_case_5$flip.yn <- as.factor(deg_replace_case_5$flip.yn)
summary(deg_replace_case_5$flip.yn)
#only 1,523/4853 data rows will be transitioned in this scenario
#Calculate what percent of the fleet this is
sum(deg_replace_case_5[deg_replace_case_5$flip.yn == 1, "Total.Fleet.Vehicles"])
#45,702 vehicles to be transitioned
sum(deg_replace_case_5[deg_replace_case_5$flip.yn == 0, "Total.Fleet.Vehicles"])
#14,960  vehicles not to be transitioned in this scenario

deg_flip_5 <- deg_replace_case_5

#ice.years <12 will have (12 - ice.years) number of years as a BEB
deg_flip_5$ice.years <- assum_bus_life
for (i in 1:nrow(deg_flip_5)) {
  if (deg_flip_5$flip.yn[i] == 1) {

    deg_flip_5$ice.years[i] <- deg_flip_5$Final.Year[i] - 2022

  }
}

deg_flip_5[deg_flip_5$ice.years < 0, "ice.years"] <- 0
deg_flip_5[deg_flip_5$ice.years > assum_bus_life, "ice.years"] <- assum_bus_life
deg_flip_5[deg_flip_5$Average.EB.mpkWh <= 0.002, "Average.EB.mpkWh"] <- eb.replace

# Account for current BEBs in service
deg_flip_5[deg_flip_5$Fuel.Type == "Electric Battery", "ice.years"] <- 0

deg_flip_5$elecgen_NOx..mt <- (deg_flip_5$eGRID.non.baseload.NOx..g.kWh..y / 
                                 deg_flip_5$Average.EB.mpkWh * 
                                 deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
deg_flip_5$elecgen_SOx..mt <- (deg_flip_5$eGRID.non.baseload.SO2..g.kWh..y / 
                                 deg_flip_5$Average.EB.mpkWh * 
                                 deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
deg_flip_5$elecgen_CO2e..mt <- (deg_flip_5$eGRID.non.baseload.CO2e..g.kWh..y / 
                                  deg_flip_5$Average.EB.mpkWh * 
                                  deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period /
                                  elec_eff_total) / 1000000

deg_flip_5$batt_VOC..mt <- 0
deg_flip_5$batt_CO..mt <- 0
deg_flip_5$batt_NOx..mt <- 0
deg_flip_5$batt_PM10..mt <- 0
deg_flip_5$batt_PM2.5..mt <- 0
deg_flip_5$batt_SOx..mt <- 0
deg_flip_5$batt_BC..mt <- 0
deg_flip_5$batt_OC..mt <- 0
deg_flip_5$batt_CO2e..mt <- 0

deg_flip_5$beb.mobile_CO2e..mt <- 0
deg_flip_5$beb.mobile_CO..mt <- 0
deg_flip_5$beb.mobile_NOx..mt <- 0
deg_flip_5$beb.mobile_PM10..mt <- 0
deg_flip_5$beb.mobile_PM2.5..mt <- 0
deg_flip_5$beb.mobile_VOC..mt <- 0
deg_flip_5$beb.mobile_SOx..mt <- 0


for (i in 1:nrow(deg_flip_5)) {
  if (deg_flip_5$flip.yn[i] == 1) {
    deg_flip_5$batt_VOC..mt[i] <- ((deg_flip_5$batt..VOC..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_5$replace.active.no.bus[i]
    deg_flip_5$batt_CO..mt[i] <- ((deg_flip_5$batt..CO..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_5$replace.active.no.bus[i]
    deg_flip_5$batt_NOx..mt[i] <- ((deg_flip_5$batt..NOx..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_5$replace.active.no.bus[i]
    deg_flip_5$batt_PM10..mt[i] <- ((deg_flip_5$batt..PM10..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_5$replace.active.no.bus[i]
    deg_flip_5$batt_PM2.5..mt[i] <- ((deg_flip_5$batt..PM2.5..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_5$replace.active.no.bus[i]
    deg_flip_5$batt_SOx..mt[i] <- ((deg_flip_5$batt..SOx..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_5$replace.active.no.bus[i]
    deg_flip_5$batt_BC..mt[i] <- ((deg_flip_5$batt..BC..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_5$replace.active.no.bus[i]
    deg_flip_5$batt_OC..mt[i] <- ((deg_flip_5$batt..OC..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_5$replace.active.no.bus[i]
    deg_flip_5$batt_CO2e..mt[i] <- ((deg_flip_5$batt..GHGs..g.kWh[i] * batt_cap * (1 + batt_replace))/ 1000000) * deg_flip_5$replace.active.no.bus[i]
    
    deg_flip_5$beb.mobile_PM10..mt[i] <- (beb_replace$fuel..PM10..g.mi * deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period[i] * (assum_bus_life - deg_flip_5$ice.years[i])) / 1000000
    deg_flip_5$beb.mobile_PM2.5..mt[i] <- (beb_replace$fuel..PM2.5..g.mi * deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period[i] * (assum_bus_life - deg_flip_5$ice.years[i])) / 1000000
    
  }
}

deg_flip_5$ice.mobile_CO2e..mt <- ((deg_flip_5$fuel..CO2..g.mi + ch4_gwp*deg_flip_5$fuel..CH4..g.mi + n2o_gwp*deg_flip_5$fuel..N2O..g.mi) * deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_5$ice.years)/1000000
deg_flip_5$ice.mobile_CO..mt <- (deg_flip_5$fuel..CO..g.mi * deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_5$ice.years) / 1000000
deg_flip_5$ice.mobile_NOx..mt <- (deg_flip_5$fuel..NOx..g.mi * deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_5$ice.years) / 1000000
deg_flip_5$ice.mobile_PM10..mt <- (deg_flip_5$fuel..PM10..g.mi * deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_5$ice.years) / 1000000
deg_flip_5$ice.mobile_PM2.5..mt <- (deg_flip_5$fuel..PM2.5..g.mi * deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_5$ice.years) / 1000000
deg_flip_5$ice.mobile_VOC..mt <- (deg_flip_5$fuel..VOC..g.mi * deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_5$ice.years) / 1000000
deg_flip_5$ice.mobile_SOx..mt <- (deg_flip_5$fuel..SOx..g.mi * deg_flip_5$Total.Miles.on.Active.Vehicles.During.Period * deg_flip_5$ice.years) / 1000000

deg_flip_5$mobile_CO2e..mt <- deg_flip_5$beb.mobile_CO2e..mt + deg_flip_5$ice.mobile_CO2e..mt
deg_flip_5$mobile_CO..mt <- deg_flip_5$beb.mobile_CO..mt + deg_flip_5$ice.mobile_CO..mt
deg_flip_5$mobile_NOx..mt <- deg_flip_5$beb.mobile_NOx..mt + deg_flip_5$ice.mobile_NOx..mt
deg_flip_5$mobile_PM10..mt <- deg_flip_5$beb.mobile_PM10..mt + deg_flip_5$ice.mobile_PM10..mt
deg_flip_5$mobile_PM2.5..mt <- deg_flip_5$beb.mobile_PM2.5..mt + deg_flip_5$ice.mobile_PM2.5..mt
deg_flip_5$mobile_VOC..mt <- deg_flip_5$beb.mobile_VOC..mt + deg_flip_5$ice.mobile_VOC..mt
deg_flip_5$mobile_SOx..mt <- deg_flip_5$beb.mobile_SOx..mt + deg_flip_5$ice.mobile_SOx..mt

# Sum totals
deg_flip_5$total_CO2e..mt <- rowSums(deg_flip_5[, c("elecgen_CO2e..mt", "batt_CO2e..mt", "mobile_CO2e..mt")], na.rm = TRUE)
deg_flip_5$total_NOx..mt <- rowSums(deg_flip_5[, c("batt_NOx..mt", "mobile_NOx..mt", "elecgen_NOx..mt")], na.rm = TRUE)
deg_flip_5$total_SOx..mt <- rowSums(deg_flip_5[, c("batt_SOx..mt", "mobile_SOx..mt", "elecgen_SOx..mt")], na.rm = TRUE)
deg_flip_5$total_PM10..mt <- rowSums(deg_flip_5[, c("batt_PM10..mt", "mobile_PM10..mt")], na.rm = TRUE)
deg_flip_5$total_PM2.5..mt <- rowSums(deg_flip_5[, c("batt_PM2.5..mt", "mobile_PM2.5..mt")], na.rm = TRUE)
deg_flip_5$total_VOC..mt <- rowSums(deg_flip_5[, c("batt_VOC..mt", "mobile_VOC..mt")], na.rm = TRUE)
deg_flip_5$total_CO..mt <- rowSums(deg_flip_5[, c("batt_CO..mt", "mobile_CO..mt")], na.rm = TRUE)


deg_si_flip_5 <- deg_flip_5 %>% group_by(Agency.Name.x, Zip.Code, State, Modes, Fuel.Type) %>%
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE),
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE),
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE),
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE),
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE),
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE),
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_flip_5_sum <- deg_flip_5 %>% group_by(Modes, Fuel.Type) %>%
  summarise(total_CO2e..mt = sum(total_CO2e..mt, na.rm = TRUE),
            total_NOx..mt = sum(total_NOx..mt, na.rm = TRUE),
            total_SOx..mt = sum(total_SOx..mt, na.rm = TRUE),
            total_PM10..mt = sum(total_PM10..mt, na.rm = TRUE),
            total_PM2.5..mt = sum(total_PM2.5..mt, na.rm = TRUE),
            total_VOC..mt = sum(total_VOC..mt, na.rm = TRUE),
            total_CO..mt = sum(total_CO..mt, na.rm = TRUE))
deg_flip_5_sum$Scenario <- 5
deg_si_flip_5$Scenario <- 5

deg_running_total <- rbind.data.frame(deg_running_total, deg_flip_5_sum)
deg_si_running_total <- rbind.data.frame(deg_si_running_total, deg_si_flip_5)


deg_total_plot <- deg_running_total %>% 
  group_by(Scenario) %>% 
  summarise(CO2e = sum(total_CO2e..mt),
            CO = sum(total_CO..mt),
            NOx = sum(total_NOx..mt), 
            SOx = sum(total_SOx..mt), 
            PM10 = sum(total_PM10..mt), 
            PM2.5 = sum(total_PM2.5..mt), 
            VOC = sum(total_VOC..mt))

deg_total_plot_melt <- melt(deg_total_plot, id.vars = "Scenario")
deg_total_plot_melt$value <- deg_total_plot_melt$value/1000000
deg_total_plot_ghg <- filter(deg_total_plot_melt, variable == "CO2e")
deg_total_plot_ls <- filter(deg_total_plot_melt, variable == "CO" | variable == "NOx" | variable == "SOx")
deg_total_plot_ss <- filter(deg_total_plot_melt, variable != "CO2e" & variable != "CO" & variable != "NOx" & variable != "SOx")
deg_total_plot_ls$variable <- factor(deg_total_plot_ls$variable)
deg_total_plot_ss$variable <- factor(deg_total_plot_ss$variable)
scen_labels <- c("Base Case", "Replace All Bus Now", "Replace All Miles Now", "Natural Phase Out", "5% Yearly", "Top 100") %>% str_wrap(width = 6)
deg_total_plot_ghg <- left_join(deg_total_plot_ghg, total_plot_ghg, by = c("Scenario", "variable"))
deg_total_plot_ls <- left_join(deg_total_plot_ls, total_plot_ls, by = c("Scenario", "variable"))
deg_total_plot_ss <- left_join(deg_total_plot_ss, total_plot_ss, by = c("Scenario", "variable"))

#### FIGURE: Emissions ####
ghgs <- ggplot(data = deg_total_plot_ghg, aes(x = factor(Scenario), y = value.x)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "steelblue3") +
  geom_errorbar(aes(ymax = value.y, ymin=value.x, width = 0.2)) +
  theme_classic() +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        legend.key.size = unit(4, "mm")) + 
  labs(x = "Scenario", y = str_wrap("GHGs emitted over 14 year analysis period (MMT)", width = 30)) +
  scale_fill_discrete(name = "Emission Type:") +
  scale_x_discrete(labels = scen_labels) +
  ylim(0, 115)

ghgs

deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 0, "value.x"] - deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 1, "value.x"]
deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 0, "value.x"] - deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 2, "value.x"]
deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 0, "value.x"] - deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 3, "value.x"]
deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 0, "value.x"] - deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 4, "value.x"]
deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 0, "value.x"] - deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 5, "value.x"]

deg_flip_5[deg_flip_5$flip.yn == 1, "replace.total.no.bus"] %>% sum(na.rm = TRUE)

# ggsave("Graphs/deg_ghgs.png",
#        plot = ghgs,
#        dpi = 700,
#        width = 7,
#        height = 4,
#        units = "in")

a <- ggplot(data = deg_total_plot_ls, aes(x = factor(Scenario), y = value.x, fill = variable)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.8) +
  geom_errorbar(aes(ymax = value.y, ymin=value.x, width = 0.2),
                position = position_dodge(0.8)) +
  theme_classic() +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        legend.key.size = unit(4, "mm"),
        axis.title.y = element_text(hjust = 0)) + 
  labs(x = "", y = "") + 
  scale_fill_manual(name = str_wrap("Emission Type:", width = 8), 
                     values = c("CO" = "#E69F00", 
                                "NOx" = "#56B4E9", 
                                "SOx" = "#009E73")) +
  scale_x_discrete(labels = scen_labels) +
  ylim(0, 0.3)
   
a


b <- ggplot(data = deg_total_plot_ss, aes(x = factor(Scenario), y = value.x, fill = variable)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.8) +
  geom_errorbar(aes(ymax = value.y, ymin=value.x, width = 0.2),
                position = position_dodge(0.8)) +
  theme_classic() +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 14, family = "sans"), 
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        legend.key.size = unit(4, "mm"),
        axis.title.y = element_text(hjust = -0.2)) + 
  labs(x = "Scenario", y = str_wrap("Air pollutants emitted over 14 year analysis period (MMT)", width = 30, exdent = 2)) + 
  scale_fill_manual(name = str_wrap("Emission Type:", width = 8), 
                    values = c("PM10" = "#F0E442", 
                               "PM2.5" = "#0072B2", 
                               "VOC" = "#D55E00")) +
  scale_x_discrete(labels = scen_labels) + 
  ylim(0, 0.015)

a/b

# ggsave("Graphs/deg_all_pollutants.png",
#        plot = a/b,
#        dpi = 700,
#        width = 7,
#        height = 4.5,
#        units = "in")


#### Battery Replacement effect on emissions ####
# Run `CTR ALT B` before this section with batt_replace = 1,
# Then Run 'CTR ALT B' before this section with batt_replace = 0
deg_total_plot$batt_replacement <- batt_replace
#batt_effect <- deg_total_plot
#batt_effect <- rbind.data.frame(batt_effect, deg_total_plot)

for (i in 0:5) {
  
  temp <- batt_effect[batt_effect$Scenario == i,]
  temp_1 <- temp[temp$batt_replacement == 1, 1:8] - temp[temp$batt_replacement == 0, 1:8]
  temp_1$Scenario <- i
  temp_1[, 2:8] <- (temp_1[, 2:8]/temp[temp$batt_replacement == 1, 2:8]) * 100
  
  if (i == 0) {
    
    batt_effect_diff <- temp_1
    
  } else {
    
    batt_effect_diff <- rbind.data.frame(batt_effect_diff, temp_1)
    
  }
  
}

View(batt_effect_diff)
# Avg GHG saved by only one batt replacement
batt_effect_diff[2:6, 2] %>% mean()


#### Values for Levelized Cost of Driving ####
replacement_case_1[replacement_case_1$Average.Diesel.mpg < 0.001, "Average.Diesel.mpg"] <- NA
replacement_case_1[replacement_case_1$Average.CNG.mpg < 0.001, "Average.CNG.mpg"] <- NA
replacement_case_1[replacement_case_1$Average.EB.mpkWh < 0.001, "Average.EB.mpkWh"] <- NA

mean.diesel <- replacement_case_1$Average.Diesel.mpg %>% mean(na.rm = TRUE)
mean.cng <- replacement_case_1$Average.CNG.mpg %>% mean(na.rm = TRUE)
mean.eb <- replacement_case_1$Average.EB.mpkWh %>% mean(na.rm = TRUE)

# assume cng mpg is diesel equivalent
mean.cng.mpgde <- mean.cng
mean.eb.mpgde <- mean.eb * (129488 / 3412) 
#1 kWh = 3412 BTU & 1 gal diesel = 129488 Btu from GREET 2020

diesel_vmt_data <- filter(replacement_case_1, Fuel.Type == "Diesel" & Total.Miles.on.Active.Vehicles.During.Period > 0)
diesel_vmt_data$vmt.per.veh <- diesel_vmt_data$Total.Miles.on.Active.Vehicles.During.Period/diesel_vmt_data$Active.Fleet.Vehicles
d.vmt <- diesel_vmt_data$vmt.per.veh %>% median(na.rm = TRUE)

ng_vmt_data <- filter(replacement_case_1, Fuel.Type == "Compressed Natural Gas" & Total.Miles.on.Active.Vehicles.During.Period > 0)
ng_vmt_data$vmt.per.veh <- ng_vmt_data$Total.Miles.on.Active.Vehicles.During.Period/ng_vmt_data$Active.Fleet.Vehicles
ng.vmt <- ng_vmt_data$vmt.per.veh %>% median(na.rm = TRUE)

eb_vmt_data <- filter(replacement_case_1, Fuel.Type == "Electric Battery" & Total.Miles.on.Active.Vehicles.During.Period > 0)
eb_vmt_data$vmt.per.veh <- eb_vmt_data$Total.Miles.on.Active.Vehicles.During.Period/eb_vmt_data$Active.Fleet.Vehicles
eb.vmt <- eb_vmt_data$vmt.per.veh %>% median(na.rm = TRUE)

# Distribution of VMT of BEBs
hist(eb_vmt_data$Total.Miles.on.Active.Vehicles.During.Period)

all.vmt <- median(replacement_case_1$Total.Miles.on.Active.Vehicles.During.Period/replacement_case_1$Active.Fleet.Vehicles, na.rm = TRUE)

# LCOD RESULTS
lcod <- data.frame(Fuel.Type = c("Diesel", "Compressed Natural Gas", "Electric.rc", "Electric.sc"),
                   Discount.Rate = 0.05,
                   Discount.Rate.pes = 0.07,
                   Discount.Rate.opt = 0.03,
                   Lifetime = 12,
                   Capital.Costs.dollar = c(551000, 649000, beb_2021, beb_2021),
                   Capital.Costs.dollar.pes = c(673000, 897000, 952000, 952000),
                   Infrastructure.dollar = c(0, 62000, 56000, 68000),
                   Maintenance.dollar..mi = c(0.94, 0.94, 0.66, 0.66),
                   Maintenance.dollar..mi.pes = c(1.83, 1.83, 1.58, 1.58),
                   Maintenance.dollar..mi.opt = c(0.4, 0.4, 0.19, 0.19),
                   ind.VMT = c(d.vmt, ng.vmt, eb.vmt, eb.vmt),
                   all.VMT = c(d.vmt, ng.vmt, 30000, 30000),
                   Fuel.Price.dollar..dge = c(3.64, 2.65, 3.90, 3.90),
                   Fuel.Price.dollar..dge.pes = c(4.38, 3.18, 4.69, 4.69),
                   Fuel.Price.dollar..dge.opt = c(3.09, 2.36, 2.49, 2.49),
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
  labs(x="", y="Levelized Cost of Driving (2021$/mi)") +
  annotate(geom = "label", x = 1, y = lcod$final[1] + 1, label = lcod$final[1] %>% round(digits = 2), family = "sans", size = 5) +
  annotate(geom = "label", x = 2, y = lcod$final[2] + 1, label = lcod$final[2] %>% round(digits = 2), family = "sans", size = 5) +
  annotate(geom = "label", x = 3, y = lcod$final[3] + 1, label = lcod$final[3] %>% round(digits = 2), family = "sans", size = 5) +
  annotate(geom = "label", x = 4, y = lcod$final[4] + 1, label = lcod$final[4] %>% round(digits = 2), family = "sans", size = 5) +
  theme(text = element_text(size=14, family = "sans"),
        axis.text = element_text(size = 12, family = "sans"), 
        legend.text = element_text(size = 12, family = "sans"),
        legend.title = element_text(size = 12, family = "sans"),
        legend.key.size = unit(4, "mm")) +
  ylim(0, 15)

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
# cost competitive with diesel buses, assuming incresear in yearly mileage
((lcod[lcod$Fuel.Type == "Diesel", "final"] -
    lcod[lcod$Fuel.Type == "Electric.rc", "Fuel"] -
    lcod[lcod$Fuel.Type == "Electric.rc", "Maintenance"]) *
    lcod[lcod$Fuel.Type == "Electric.rc", "all.VMT"])/ 
  lcod[lcod$Fuel.Type == "Electric.rc", "CRF"] -
  lcod[lcod$Fuel.Type == "Electric.rc", "Infrastructure.dollar"]

#### Social Cost of Carbon Calculation ####
# GHGs mitigated in "top 100 agencies" scenario
deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 0, "value.x"] - deg_total_plot_ghg[deg_total_plot_ghg$Scenario == 5, "value.x"]

# Number of BEBs needed to do this replacement at 100 daily miles
deg_flip_5[deg_flip_5$flip.yn == 1, "replace.total.no.bus"] %>% sum(na.rm = TRUE)
# Number of BEBs needed at one to one replacement (VMT = 30,000)
deg_flip_5[deg_flip_5$flip.yn == 1, "Total.Fleet.Vehicles"] %>% sum(na.rm = TRUE)

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



#### Map of concentration of vehicles in US ####

cities <- us.cities
cities$name <- cities$name %>% str_sub(start = 1L, end = -4L)
beb_cities <- deg_replace_case_1 %>% 
  group_by(City, State) %>%
  summarise(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles),
            Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles))
beb_cities$City <- str_trim(beb_cities$City, side = "both")

(!(beb_cities$City %in% cities$name)) %>% sum()
# 187 cities left out, out of 615 (~30%)
beb_cities[!(beb_cities$City %in% cities$name), "Total.Fleet.Vehicles"] %>% sum()
beb_cities[, "Total.Fleet.Vehicles"] %>% sum()
# 12% of data not being mapped in smaller cities
cities[cities$name == "WASHINGTON", "name"] <- "Washington"
beb_conc_map <- left_join(cities, beb_cities, by = c("name" = "City", "country.etc" = "State"))
beb_conc_map <- filter(beb_conc_map, country.etc != "AK" & country.etc != "HI")
beb_conc_most <- filter(beb_conc_map, Active.Fleet.Vehicles > 500)
beb_conc_most <- filter(beb_conc_most, 
                        name != "Arlington Heights" & 
                          name != "Newark" &
                          name != "Oakland")

states <- map_data("state")

beb_conc <- full_join(beb_conc_map, states, by = c("lat", "long"))

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

conc_map <- ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "grey70", fill = "grey90") +
  coord_map(projection = "albers", lat0 = 30, lat1 = 35) +
  theme_bw() +
  ditch_the_axes +
  geom_count(data = beb_conc_map, 
             aes(x = long, y = lat, size = Active.Fleet.Vehicles, group = NA),
             color = "darkmagenta", alpha = 0.2)+
  theme(legend.position = c(0.88, 0.2),
        legend.title = element_text(size = 11)) +
  geom_text(data = beb_conc_most, 
            mapping = aes(x = long, y = lat, label = name, group = NA), 
            size = 2.3,
            position = position_jitter(height = 1.5, width = 1.5, seed = 25)) +
  guides(size = guide_legend("Active Fleet Vehicles")) +
  scale_size(breaks = c(50, 500, 1000),
             labels = c("50", "500", ">1000"))

conc_map
# Named cities have bus fleets over 500 vehicles. Hawaii and Alaska not shown

# ggsave("Graphs/concentration_map.png",
#        plot = conc_map,
#        dpi = 700,
#        width = 7.5,
#        height = 5,
#        units = "in")
