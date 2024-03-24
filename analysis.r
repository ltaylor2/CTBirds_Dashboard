library(tidyverse)

taxonomy <- read_csv("CTBirds_Dashboard/taxonomy.csv")
obs <- read_csv("CTBirds_Dashboard/ebd_ct_cleaned_observation_proportions.csv")

spYear <- function(common) {
    if (!(common %in% taxonomy$Taxon_Common)) {
        return("ERROR")
    }
    spOps <- filter(obs, Taxon_Scientific == taxonomy[taxonomy$Taxon_Common==common, "Taxon_Scientific"][[1]])
    fit <- lm(Percentage_Checklists_Observed ~ Year + factor(Week_of_2024), data=spOps) |> summary()
    ret <- as.data.frame(fit$coefficient["Year",]) %>% 
        mutate(Parameter=row.names(.)) |>
        select(Parameter, Value=1) |>
        pivot_wider(names_from=Parameter, values_from=Value) |>
        select(Estimate=1, SE=2, T=3, P=4) |>
        mutate(Species=commom)
}
