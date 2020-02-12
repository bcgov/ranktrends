library(dplyr)
library(tidyverse)
library(furrr)

devtools::load_all()

library(readr)

#Example data set
tdata <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")

status_data_wts <- tdata %>%
  mutate(parsed_rank = ranks_to_numeric(SRank),
         parsed_rank_single = ranks_to_numeric(SRank, simplify = TRUE, round_fun = min),  # tried to extract 1st value ? not working
         wts = 5 - parsed_rank_single)

status_complete <- status_data_wts %>%
  group_by(Taxonomic_Group) %>%
  complete(nesting(Scientific_Name, Common_Name), Year) %>%
  semi_join(
    group_by(., Taxonomic_Group, Scientific_Name, Common_Name) %>%
      summarize())

# remove those species which are extinct
species_to_remove <- status_complete %>%
  filter(Year == min(Year) & SRank == "SX") %>%
  pull(Scientific_Name) %>%
  unique()

status_data_final <- status_complete  %>%
  filter(!Scientific_Name %in% species_to_remove) %>%
  ungroup() %>%
  mutate(Taxonomic_Group = ifelse(
    Taxonomic_Group %in% c("Amphibians", "Reptiles and Turtles"),
    "Reptiles & Amphibians",
    Taxonomic_Group))

# remove NAs
status_data_final <- status_data_final %>%
  filter(!is.na(wts))

#run function
csi <- sampled_index(status_data_final, "Taxonomic_Group","wts","Year")


# function to plot calculated data
rank_plot <- function(plot.data, t_group, yr_col) {

  csi.plot <- plot.data %>%
    group_by_(tax_group, yr_col) %>%
    summarize(mean = mean(mean_wt), lci = mean(lci), uci = mean(uci))

  ggplot(csi.plot, aes(x =  Year)) + # same issue here as line below
    #facet_wrap(~ !!!tax_group) +  # how to parse text string as column name
    facet_wrap(~ Taxonomic_Group) +
    geom_point(aes(y = mean)) +
    geom_line(aes(y = mean)) +
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)
}

#example run
outplot <- rank_plot(csi, "Taxonomic_Group", "Year")
outplot
