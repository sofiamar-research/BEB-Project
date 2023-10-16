base_emissions_model <- function(assumed_bus_life = 14, 
                                 elec_eff_total = (elec_transm_dist_eff * plug_eff),
                                 consumption_proportion = 0.5,
                                 batt_replace = 0,
                                 largest_pack = FALSE,
                                 DOD = 0.7,
                                 egrid_reduction = "Average"){
  
  # Don't want to alter the original data
  bus_data <- left_join(bus_base, egrid_zip[,c(2,4)], by = c("Zip.Code" = "ZIP..numeric."))
  bus_data$Year.Start <- bus_data$Final.Year + 1
  bus_data[bus_data$Year.Start<2021, "Year.Start"] <- 2021
  if(egrid_reduction == "Min"){
    
    egrid_case <- decrease_egrid[decrease_egrid$sensitivity == "min",]
    
    print("Using Minimum egrid emission decrease")
  } else if(egrid_reduction == "Max") {
    
    egrid_case <- decrease_egrid[decrease_egrid$sensitivity == "max",]
    
    print("Using Maximum egrid emission decrease")
  } else if(egrid_reduction == "Average") {
    
    egrid_case <- decrease_egrid[decrease_egrid$sensitivity == "avg",]
    
    print("Using Average egrid emission decrease")
  } else {
    
    print("Invalid grid case. Please select from 'Min', 'Max', or 'Average'.")
    
  }
  
  bus_data <- bus_data <- left_join(bus_data, egrid_case[,c(1,3:6)], by = c("eGRID.Subregion..1" = "eGRID.subregion.acronym",
                                                                  "Year.Start" = "year.start"))
  
  # Just to make this dataset compatible with other scenario datasets:
  bus_data$cFuel.Type <- bus_data$Fuel.Type
  
  # Emissions from Electricity Generation
  bus_data$elecgen_NOx..mt <- (bus_data$eGRID.subregion.annual.NOx.non.baseload.output.emission.rate..g.kWh/ 
                                 bus_data$bus.mpg * 
                                 bus_data$Total.Miles.on.Active.Vehicles.During.Period /
                                 elec_eff_total) / 1000000
  bus_data$elecgen_SOx..mt <- (bus_data$eGRID.subregion.annual.SO2.non.baseload.output.emission.rate..g.kWh / 
                                 bus_data$bus.mpg* 
                                 bus_data$Total.Miles.on.Active.Vehicles.During.Period  /
                                 elec_eff_total) / 1000000
  bus_data$elecgen_CO2e..mt <- (bus_data$eGRID.subregion.annual.CO2e.non.baseload.output.emission.rate..g.kWh / 
                                  bus_data$bus.mpg * 
                                  bus_data$Total.Miles.on.Active.Vehicles.During.Period /
                                  elec_eff_total) / 1000000
  
  bus_data[bus_data$Fuel.Type != "Electric Battery", c("elecgen_CO2e..mt",
                                                       "elecgen_SOx..mt",
                                                       "elecgen_NOx..mt")] <- NA
  
  # Emissions from Battery Lifecycle
  bus_data$batt_VOC..mt <- 0
  bus_data$batt_CO..mt <- 0
  bus_data$batt_NOx..mt <- 0
  bus_data$batt_PM10..mt <- 0
  bus_data$batt_PM2.5..mt <- 0
  bus_data$batt_SOx..mt <- 0
  bus_data$batt_BC..mt <- 0
  bus_data$batt_OC..mt <- 0
  bus_data$batt_CO2e..mt <- 0
  
  bus_data <- left_join(bus_data, 
                        batt_specs,
                        by = "make_model")
  
  # Calculate Battery size needed
  bus_data$consumption_used <- bus_data$consumption_low + (bus_data$consumption_high - bus_data$consumption_low) * consumption_proportion
  
  if (largest_pack == FALSE){
    bus_data[bus_data$Fuel.Type == "Electric Battery","batt_size"] <- bus_data[bus_data$Fuel.Type == "Electric Battery" ,"consumption_used"] * bus_data[bus_data$Fuel.Type == "Electric Battery" , "Average.Daily.Miles.on.Active.Vehicles.During.Period"] / DOD
  } else {
    bus_data[bus_data$Fuel.Type == "Electric Battery","batt_size"] <- bus_data[bus_data$Fuel.Type == "Electric Battery","batt_high"]
  }
  bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_size > bus_data$batt_high, "batt_size"] <- bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_size > bus_data$batt_high, "batt_high"]
  bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_size < bus_data$batt_low, "batt_size"] <- bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_size < bus_data$batt_low, "batt_low"]
  
  batt_chem_list <- unique(bus_data$batt_chem) %>% unlist() %>% as.character() %>% na.omit()
  
  for (chem in batt_chem_list) {
    
    # For each chemistry, compute each emission type
    bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem, "batt_VOC..mt"] <- ((batt_lc_emissions[batt_lc_emissions$Total.grams.per.lifetime.per.kWh == "VOC", chem] * 
                                                                                                           bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"batt_size"] * 
                                                                                                           (1 + batt_replace))/ 1000000) * bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"Total.Fleet.Vehicles"]
    bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem, "batt_CO..mt"] <- ((batt_lc_emissions[batt_lc_emissions$Total.grams.per.lifetime.per.kWh == "CO", chem] * 
                                                                                                          bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"batt_size"] * 
                                                                                                          (1 + batt_replace))/ 1000000) * bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"Total.Fleet.Vehicles"]
    bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem, "batt_NOx..mt"] <- ((batt_lc_emissions[batt_lc_emissions$Total.grams.per.lifetime.per.kWh == "NOx", chem] * 
                                                                                                           bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"batt_size"] * 
                                                                                                           (1 + batt_replace))/ 1000000) * bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"Total.Fleet.Vehicles"]
    bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem, "batt_PM10..mt"] <- ((batt_lc_emissions[batt_lc_emissions$Total.grams.per.lifetime.per.kWh == "PM10", chem] * 
                                                                                                            bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"batt_size"] * 
                                                                                                            (1 + batt_replace))/ 1000000) * bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"Total.Fleet.Vehicles"]
    bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem, "batt_PM2.5..mt"] <- ((batt_lc_emissions[batt_lc_emissions$Total.grams.per.lifetime.per.kWh == "PM2.5", chem] * 
                                                                                                             bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"batt_size"] * 
                                                                                                             (1 + batt_replace))/ 1000000) * bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"Total.Fleet.Vehicles"]
    bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem, "batt_SOx..mt"] <- ((batt_lc_emissions[batt_lc_emissions$Total.grams.per.lifetime.per.kWh == "SOx", chem] * 
                                                                                                           bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"batt_size"] * 
                                                                                                           (1 + batt_replace))/ 1000000) * bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"Total.Fleet.Vehicles"]
    bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem, "batt_BC..mt"] <- ((batt_lc_emissions[batt_lc_emissions$Total.grams.per.lifetime.per.kWh == "BC", chem] * 
                                                                                                          bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"batt_size"] * 
                                                                                                          (1 + batt_replace))/ 1000000) * bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"Total.Fleet.Vehicles"]
    bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem, "batt_OC..mt"] <- ((batt_lc_emissions[batt_lc_emissions$Total.grams.per.lifetime.per.kWh == "OC", chem] * 
                                                                                                          bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"batt_size"] * 
                                                                                                          (1 + batt_replace))/ 1000000) * bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"Total.Fleet.Vehicles"]
    bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem, "batt_CO2e..mt"] <- ((batt_lc_emissions[batt_lc_emissions$Total.grams.per.lifetime.per.kWh == "GHGs", chem] * 
                                                                                                            bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"batt_size"] * 
                                                                                                            (1 + batt_replace))/ 1000000) * bus_data[bus_data$Fuel.Type == "Electric Battery" & bus_data$batt_chem == chem,"Total.Fleet.Vehicles"]
  }  
  # Well to Wheel Conversions, specific to fleet's mpg
  # First well to tank:
  # eGRID is the electric battery equivalent to wtt
  
  bus_data$fuel.wtt..VOC..mt <- 0
  bus_data$fuel.wtt..SOx..mt <- 0
  bus_data$fuel.wtt..PM2.5..mt <- 0
  bus_data$fuel.wtt..PM10..mt <- 0
  bus_data$fuel.wtt..OC..mt <- 0
  bus_data$fuel.wtt..BC..mt <- 0
  bus_data$fuel.wtt..CO..mt <- 0
  bus_data$fuel.wtt..NOx..mt <- 0
  bus_data$fuel.wtt..CO2e..mt <- 0
  
  bus_data <- left_join(bus_data, welltotank, by = "Fuel.Type")
  
  bus_data[, "fuel.wtt..VOC..mt"] <- bus_data[,"VOC..g.gal"] /
    bus_data[,"bus.mpg"] * 
    bus_data[, "Total.Miles.on.Active.Vehicles.During.Period"] *
    assumed_bus_life / 1000000
  bus_data[, "fuel.wtt..CO..mt"] <- bus_data[,"CO..g.gal"] /
    bus_data[,"bus.mpg"] * 
    bus_data[, "Total.Miles.on.Active.Vehicles.During.Period"] *
    assumed_bus_life / 1000000
  bus_data[, "fuel.wtt..NOx..mt"] <- bus_data[,"NOx..g.gal"] /
    bus_data[,"bus.mpg"] * 
    bus_data[, "Total.Miles.on.Active.Vehicles.During.Period"] *
    assumed_bus_life / 1000000
  bus_data[, "fuel.wtt..PM10..mt"] <- bus_data[,"PM10..g.gal"] /
    bus_data[,"bus.mpg"] * 
    bus_data[, "Total.Miles.on.Active.Vehicles.During.Period"] *
    assumed_bus_life / 1000000
  bus_data[, "fuel.wtt..PM2.5..mt"] <- bus_data[,"PM2.5..g.gal"] /
    bus_data[,"bus.mpg"] * 
    bus_data[, "Total.Miles.on.Active.Vehicles.During.Period"] *
    assumed_bus_life / 1000000
  bus_data[, "fuel.wtt..SOx..mt"] <- bus_data[,"SOx..g.gal"] /
    bus_data[,"bus.mpg"] * 
    bus_data[, "Total.Miles.on.Active.Vehicles.During.Period"] *
    assumed_bus_life / 1000000
  bus_data[, "fuel.wtt..BC..mt"] <- bus_data[,"BC..g.gal"] /
    bus_data[,"bus.mpg"] * 
    bus_data[, "Total.Miles.on.Active.Vehicles.During.Period"] *
    assumed_bus_life / 1000000
  bus_data[, "fuel.wtt..OC..mt"] <- bus_data[,"OC..g.gal"] /
    bus_data[,"bus.mpg"] * 
    bus_data[, "Total.Miles.on.Active.Vehicles.During.Period"] *
    assumed_bus_life / 1000000
  bus_data[, "fuel.wtt..CO2e..mt"] <- bus_data[,"CO2e..g.gal"] /
    bus_data[,"bus.mpg"] * 
    bus_data[, "Total.Miles.on.Active.Vehicles.During.Period"] *
    assumed_bus_life / 1000000
  

# Then, tank to wheel:
  
  bus_data <- left_join(bus_data, emfac, by = c("Fuel.Type" = "Fuel", "Manufacture.Year" = "Model.Year"))
  
  bus_data$fuel.ttw..NOx_RUNEX..mt <- bus_data$NOx_RUNEX..g.gal / bus_data$bus.mpg * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  bus_data$fuel.ttw..CO2e_RUNEX..mt <- bus_data$CO2e_RUNEX..g.gal / bus_data$bus.mpg * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  bus_data$fuel.ttw..CO_RUNEX..mt <- bus_data$CO_RUNEX..g.gal / bus_data$bus.mpg * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  bus_data$fuel.ttw..PM10_RUNEX..mt <- bus_data$PM10_RUNEX..g.gal / bus_data$bus.mpg * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  bus_data$fuel.ttw..PM2.5_RUNEX..mt <- bus_data$PM2.5_RUNEX..g.gal / bus_data$bus.mpg * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  bus_data$fuel.ttw..VOC_RUNEX..mt <- bus_data$VOC_RUNEX..g.gal / bus_data$bus.mpg * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  bus_data$fuel.ttw..SOx_RUNEX..mt <- bus_data$SOx_RUNEX..g.gal / bus_data$bus.mpg * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  
  bus_data$fuel.ttw..PM10_PMBW..mt <- bus_data$PM10_PMBW..g.mi * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  bus_data$fuel.ttw..PM2.5_PMBW..mt <- bus_data$PM2.5_PMBW..g.mi * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  bus_data$fuel.ttw..PM10_PMTW..mt <- bus_data$PM10_PMTW..g.mi * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  bus_data$fuel.ttw..PM2.5_PMTW..mt <- bus_data$PM2.5_PMTW..g.mi * bus_data$Total.Miles.on.Active.Vehicles.During.Period * assumed_bus_life / 1000000
  
  # Final Well to Wheel
  bus_data$fuel.wtw..NOx..mt <- rowSums(bus_data[,c("elecgen_NOx..mt", "fuel.wtt..NOx..mt", "fuel.ttw..NOx_RUNEX..mt")], na.rm = TRUE)
  bus_data$fuel.wtw..CO2e..mt <- rowSums(bus_data[,c("elecgen_CO2e..mt", "fuel.wtt..CO2e..mt", "fuel.ttw..CO2e_RUNEX..mt")], na.rm = TRUE)
  bus_data$fuel.wtw..CO..mt <- rowSums(bus_data[,c("fuel.wtt..CO..mt", "fuel.ttw..CO_RUNEX..mt")], na.rm = TRUE)
  bus_data$fuel.wtw..PM10..mt <- rowSums(bus_data[,c("fuel.wtt..PM10..mt", "fuel.ttw..PM10_RUNEX..mt", "fuel.ttw..PM10_PMBW..mt", "fuel.ttw..PM10_PMTW..mt")], na.rm = TRUE)
  bus_data$fuel.wtw..PM2.5..mt <- rowSums(bus_data[,c("fuel.wtt..PM2.5..mt", "fuel.ttw..PM2.5_RUNEX..mt", "fuel.ttw..PM2.5_PMBW..mt", "fuel.ttw..PM2.5_PMTW..mt")], na.rm = TRUE)
  bus_data$fuel.wtw..VOC..mt <- rowSums(bus_data[,c("fuel.wtt..VOC..mt", "fuel.ttw..VOC_RUNEX..mt")], na.rm = TRUE)
  bus_data$fuel.wtw..SOx..mt <- rowSums(bus_data[,c("elecgen_SOx..mt", "fuel.wtt..SOx..mt", "fuel.ttw..SOx_RUNEX..mt")], na.rm = TRUE)
  
  
  # Group according to scenario
  final_data <- bus_data %>% 
    group_by(NTD.ID,
             Agency.Name.x,
             Modes,
             Fuel.Type,
             cFuel.Type,
             Address.Line.1,
             City,
             State,
             Zip.Code,
             Final.Year,
             Year.Start,
             batt_size) %>%
    summarize(Total.Fleet.Vehicles = sum(Total.Fleet.Vehicles, na.rm = TRUE),
              Active.Fleet.Vehicles = sum(Active.Fleet.Vehicles, na.rm = TRUE),
              Total.Miles.on.Active.Vehicles.During.Period = sum(Total.Miles.on.Active.Vehicles.During.Period, na.rm = TRUE),
              batt_VOC..mt = sum(batt_VOC..mt, na.rm = TRUE),
              batt_CO..mt = sum(batt_CO..mt, na.rm = TRUE),
              batt_NOx..mt  = sum(batt_NOx..mt, na.rm = TRUE),
              batt_PM10..mt  = sum(batt_PM10..mt, na.rm = TRUE),
              batt_PM2.5..mt  = sum(batt_PM2.5..mt, na.rm = TRUE),
              batt_SOx..mt  = sum(batt_SOx..mt, na.rm = TRUE),
              batt_BC..mt  = sum(batt_BC..mt, na.rm = TRUE),
              batt_OC..mt  = sum(batt_OC..mt, na.rm = TRUE),
              batt_CO2e..mt  = sum(batt_CO2e..mt, na.rm = TRUE),
              fuel.wtw..NOx..mt = sum(fuel.wtw..NOx..mt, na.rm = TRUE),
              fuel.wtw..CO2e..mt = sum(fuel.wtw..CO2e..mt, na.rm = TRUE),
              fuel.wtw..CO..mt = sum(fuel.wtw..CO..mt, na.rm = TRUE),
              fuel.wtw..PM10..mt = sum(fuel.wtw..PM10..mt, na.rm = TRUE),
              fuel.wtw..PM2.5..mt = sum(fuel.wtw..PM2.5..mt, na.rm = TRUE),
              fuel.wtw..VOC..mt = sum(fuel.wtw..VOC..mt, na.rm = TRUE),
              fuel.wtw..SOx..mt = sum(fuel.wtw..SOx..mt, na.rm = TRUE))
  
  return(final_data)

}