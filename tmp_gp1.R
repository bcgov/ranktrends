library(tidyverse)
library(furrr)

#install_github("bcgov/ranktrends")
library(ranktrends)

# read in data
tdata <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")

status_data <- tdata %>%
  mutate(parsed_rank = ranks_to_numeric(SRank, simplify = FALSE))

#status_data_wts <- status_data %>%
#  mutate(parsed_rank = ranks_to_numeric(SRank),
#         parsed_rank_single = map_dbl(parsed_rank, min),  # tried to extract 1st value ? not working
#         wts = unlist(map(parsed_rank_single, ~ 5 - .x))) # need to extract the first value

status_data_wts <- status_data %>%
  mutate(parsed_rank = ranks_to_numeric(SRank),
         parsed_rank_single = ranks_to_numeric(SRank, simplify = TRUE, round_fun = min),  # tried to extract 1st value ? not working
         wts = 5 - parsed_rank_single)

status_complete <- status_data_wts %>%
  group_by(Taxonomic_Group) %>%
  complete(nesting(Scientific_Name, Common_Name), Year) %>%
  semi_join(
    group_by(., Taxonomic_Group, Scientific_Name, Common_Name) %>%
      summarize()) # %>%
      #summarize(all_complete = all(valid_rank)) %>%
      #filter(all_complete),
    #by = c("Taxonomic_Group", "Scientific_Name", "Common_Name"))

# remove those species which are extinct
species_to_remove <- status_data %>%
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

plan(multiprocess)

status_data_final <- status_data_final %>%
  filter(!is.na(wts))

csi <- status_data_final %>%
      mutate(
       N = n(),
      samples = map(wts, ~ replicate(10,
                                    rli(map_dbl(.x, sample, 1)))),
      mean_wt = map_dbl(samples, mean),
      min_wt = map_dbl(samples, min),
      max_wt = map_dbl(samples, max),
      lci = map_dbl(samples, quantile, probs = 0.025),
      uci = map_dbl(samples, quantile, probs = 0.975))

csi <- group_by(status_data_final, Taxonomic_Group, Year) %>%
  nest() %>%
  mutate(
    N = map_dbl(data, nrow),
    samples = map(
      data,
      ~ replicate(10,
                  rli(map_dbl(.x$wts, sample, 1)))
    ),
    mean_wt = map_dbl(samples, mean),
    min_wt = map_dbl(samples, min),
    max_wt = map_dbl(samples, max),
    lci = map_dbl(samples, quantile, probs = 0.025),
    uci = map_dbl(samples, quantile, probs = 0.975))

csi.plot <- csi %>%
  group_by(Taxonomic_Group, Year) %>%
  summarize(mean = mean(mean_wt), lci = mean(lci), uci = mean(uci))


ggplot(csi.plot, aes(x = Year)) +
  facet_wrap(~Taxonomic_Group) +
  geom_point(aes(y = mean)) +
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin=lci,ymax=uci), alpha = 0.2)




# Pseudo-code for desired function
# rank_data %>%
#   group_by(tax_group, yr) %>%
#   sampled_index(wts_list) # sampled_index doesn't exist yet
#
# tax_group | yr | mean_wt | min_wt | max_wt | lci | uci
