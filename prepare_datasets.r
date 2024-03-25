library(tidyverse)
# library(auk)
# library(tigris)

# # taxonomy <- get_ebird_taxonomy()
# ctSpecies <- tibble(Taxon_Common = readLines("ct_species_list.txt")) |>
#           left_join(taxonomy, by=c("Taxon_Common"="common_name")) |>
#           select(Taxon_Common, Taxon_Scientific=scientific_name, Family=family, Order=order)
# write_csv(ctSpecies, "taxonomy.csv")

# saveRDS(ctSpecies, file="taxonomy.RDS")

# auk_ebd("ebd_US-CT_smp_relJan-2024.txt",
#         file_sampling="ebd_US-ct_smp_relJan-2024_sampling.txt") |>
#         auk_species(ctSpecies$Taxon_Common) |>
#         auk_year(2015:2023) |>
#         auk_complete() |>
#         auk_filter(file="ebd_ct_complete.txt",
#                    file_sampling="ebd_ct_complete_sampling.txt",
#                    overwrite=TRUE)

# zeroed <- auk_zerofill("ebd_ct_complete.txt",
#                        sampling_events="ebd_ct_complete_sampling.txt",
#                        collapse=FALSE)
# write_csv(zeroed$observations, "zerofilled_observations.csv")
# write_csv(zeroed$sampling_events, "zerofilled_sampling.csv")


# observations <- read_csv("zerofilled_observations.csv", lazy=FALSE) |>
#              select(ID = checklist_id, Taxon_Scientific = scientific_name, Observed = species_observed)

# sampling <- read_csv("zerofilled_sampling.csv", lazy=TRUE) |>
#           select(ID = checklist_id, Date = observation_date, Latitude=latitude, Longitude=longitude) |>
#           mutate(Week=week(Date), Year=year(Date)) 

# summarizeSpeciesObs <- function(taxon) {
#     cat(paste0("Summarizing observations for ", taxon, ".\n"))

#     obs <- read_csv(paste0("Zerofilled_By_Species/", str_replace(taxon, " ", "_"), ".csv"), show_col_types=FALSE, col_types="cccccccl") |>
#         mutate(Observation_Count = as.numeric(ifelse(observation_count=="X", 1, observation_count))) |>
#         select(ID = checklist_id, Taxon_Scientific = scientific_name, Observed = species_observed, Observation_Count) |>
#         mutate(Observed = ifelse(Observed, "Yes", "No")) |>
#         left_join(sampling, by="ID")
        
#     proportions <- obs |>
#                 mutate(Week = week(Date), Year=year(Date)) |>
#                 group_by(Year, Week, Observed) |>
#                 tally(name="N") |>
#                 pivot_wider(id_cols=c(Year, Week), names_from=Observed, values_from=N, values_fill=0) |>
#                 mutate(Percentage_Checklists_Observed = Yes / (Yes+No) * 100,
#                        Week_of_2024 = ymd("2024-01-01") + weeks(Week-1),
#                        Taxon_Scientific = taxon) |>
#                 ungroup() |>
#                 select(Taxon_Scientific, Year, Week_of_2024, Percentage_Checklists_Observed)
    
#     locations <- obs |>
#               filter(Observed == "Yes") |>
#               group_by(Taxon_Scientific, Latitude, Longitude) |>
#               summarize(Max_Observed = max(Observation_Count), .groups="drop") |>
#               mutate(Log10_Max_Observed = log10(Max_Observed)) |>
#               select(Taxon_Scientific, Latitude, Longitude, Log10_Max_Observed)

#     return(list("Proportions"=proportions, "Locations"=locations))
# }

# cleaned <- map(ctSpecies$Taxon_Scientific, summarizeSpeciesObs)

# proportions <- map_df(1:length(cleaned), ~cleaned[[.]]$Proportions)
# saveRDS(proportions, file="species_observations_proportions.RDS")

# locations <- map_df(1:length(cleaned), ~cleaned[[.]]$Locations)
# saveRDS(locations, file="species_observations_locations.RDS")

# CT shapefiles
# states <- states(cb = TRUE, resolution = "500k")

# ct <- subset(states, NAME=="Connecticut")
# saveRDS(ct, file="ct_shapefile.RDS")

# maxWeek <- proportions |>
#         group_by(Taxon_Scientific, Week_of_2024) |>
#         summarize(Percentage_Checklists_Observed = median(Percentage_Checklists_Observed)) |>
#         slice_max(order_by=Percentage_Checklists_Observed, with_ties=FALSE)
# maxes <- maxWeek$Week_of_2024
# names(maxes) <- maxWeek$Taxon_Scientific         
# saveRDS(maxes, file="species_observations_maxWeeks.RDS")

# summarizeChanges <- function(taxon) {
#     df <- filter(proportions, Taxon_Scientific==taxon)
#     fit <- lm(Percentage_Checklists_Observed ~ Year + Week_of_2024, data=df) |>
#         summary()
#     estimate <- fit$coefficients["Year", "Estimate"]
#     estimateSummary <- "No Change"
#     if (estimate < 0) { 
#         estimateSummary <- "Decreasing" 
#     } else if (estimate > 0) {
#         estimateSummary <- "Increasing"
#     }
    
#     p <- fit$coefficients["Year", 4] 
#     pSummary <- "N.S."
#     if (p < 0.05) { 
#         pSummary <- "P < 0.05"
#     }
#     if (p < (0.05 / nrow(taxonomy))) {
#         pSummary <- "True significance"
#     }
#     ret <- tibble(Taxon_Scientific=taxon, Change=estimateSummary, Significance = pSummary)
#     return(ret)
# }
# changes <- map_df(taxonomy$Taxon_Scientific, summarizeChanges)
# saveRDS(changes, file="species_observations_changeModels.RDS")

# proportions <- readRDS("species_observations_proportions.RDS")
# taxonomy <- readRDS("taxonomy.RDS")

# summarizeSeasonalPattern <- function(taxon) {
#     df <- filter(proportions, Taxon_Scientific==taxon) |>
#     mutate(Week = week(Week_of_2024)) |>
#     mutate(Week2 = Week^2,
#             Week3 = Week^3,
#             Week4 = Week^4)
#     uniformFit <- lm(Percentage_Checklists_Observed ~ 1, data=df)
#     quadraticFit <- lm(Percentage_Checklists_Observed ~ Week + Week2, data=df)
#     quarticFit <- lm(Percentage_Checklists_Observed ~ Week + Week2 + Week3 + Week4, data=df)
#     aics <- map_dbl(list(uniformFit, quadraticFit, quarticFit), AIC)

#     # Default: uniform model preferred
#     seasonality <- "Uniform (common)"
#     if (uniformFit$coefficients[[1]] < 2.5) {
#         seasonality <- "Uniform (rare)"
#     }
#     if (aics[2] < (aics[1] - 5)) {
#         # Quadratic fit preferred
#         seasonality <- "Summer"
#         if (quadraticFit$coefficients["Week2"]>0) {
#             seasonality <- "Winter"
#         }
#     }
#     if (aics[3] < (aics[2] - 5) & aics[3] < (aics[1] - 5)) {
#         # Migratory fit preferred
#         seasonality <- "Migratory"
#     }
#     return(tibble(Taxon_Scientific=taxon, Seasonality=seasonality))
# }
# seasonalities <- map_df(taxonomy$Taxon_Scientific, summarizeSeasonalPattern)

# seasonalities |> left_join(taxonomy) |> View()
