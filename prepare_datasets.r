library(tidyverse)
library(auk)
library(tigris)

taxonomy <- get_ebird_taxonomy()
ctSpecies <- tibble(Taxon_Common = readLines("ct_species_list.txt")) |>
          left_join(taxonomy, by=c("Taxon_Common"="common_name")) |>
          select(Taxon_Common, Taxon_Scientific=scientific_name, Family=family, Order=order)
write_csv(ctSpecies, "taxonomy.csv")

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

sampling <- read_csv("zerofilled_sampling.csv", lazy=TRUE) |>
          select(ID = checklist_id, Date = observation_date, Latitude=latitude, Longitude=longitude) |>
          mutate(Week=week(Date), Year=year(Date)) 

summarizeSpeciesObs <- function(taxon) {
    cat(paste0("Summarizing observations for ", taxon, ".\n"))

    obs <- read_csv(paste0("Zerofilled_By_Species/", str_replace(taxon, " ", "_"), ".csv"), show_col_types=FALSE, col_types="cccccccl") |>
        select(ID = checklist_id, Taxon_Scientific = scientific_name, Observed = species_observed) |>
        mutate(Observed = ifelse(Observed, "Yes", "No")) |>
        left_join(sampling, by="ID")
        
    proportions <- obs |>
                mutate(Week = week(Date), Year=year(Date)) |>
                group_by(Year, Week, Observed) |>
                tally(name="N") |>
                pivot_wider(id_cols=c(Year, Week), names_from=Observed, values_from=N, values_fill=0) |>
                mutate(Percentage_Checklists_Observed = Yes / (Yes+No) * 100,
                       Week_of_2024 = ymd("2024-01-01") + weeks(Week-1),
                       Taxon_Scientific = taxon) |>
                ungroup() |>
                select(Taxon_Scientific, Year, Week_of_2024, Percentage_Checklists_Observed)
    
    locations <- obs |>
              filter(Observed == "Yes") |>
              select(Taxon_Scientific, Date, Latitude, Longitude)

    return(list("Proportions"=proportions, "Locations"=locations))
}

cleaned <- map(ctSpecies$Taxon_Scientific, summarizeSpeciesObs)

proportions <- map_df(1:length(cleaned), ~cleaned[[.]]$Proportions)
saveRDS(proportions, file="species_observations_proportions.RDS")

locations <- map_df(1:length(cleaned), ~cleaned[[.]]$Locations)
saveRDS(locations, file="species_observations_locations.RDS")

# CT shapefiles
states <- states(cb = TRUE, resolution = "500k")

ct <- subset(states, NAME=="Connecticut")

temp <- locations |>
     filter(Taxon_Scientific=="Branta canadensis")
ggplot(temp) +
    geom_sf(data=ct, colour="black", fill="white") +
    geom_point(aes(x=Longitude, y=Latitude)) +
    theme_void()

