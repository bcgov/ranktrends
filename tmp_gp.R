library(tidyverse)
library(furrr)

# https://gist.github.com/ateucher/fc12bebfc189ab39b310d5321307475e
# Base on Andy T's gist:

status_data <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")

head(status_data)

tdata <- status_data[1:5,]

tdata$SRank

ranks_to_numeric(tdata$SRank)

status_data_wts <- tdata %>%
  mutate(parsed_rank = ranks_to_numeric(SRank,simplify = TRUE),
         wts = map(parsed_rank, ~ 5 - .x))


status_complete <- status_data_wts %>%
  group_by(Taxonomic_Group) %>%
  complete(nesting(Scientific_Name, Common_Name), Year) %>%
  mutate(valid_rank = map_lgl(wts, ~ !all(is.na(.x)))) %>%
  semi_join(
    group_by(., Taxonomic_Group, Scientific_Name, Common_Name) %>%
      summarize(all_complete = all(valid_rank)) %>%
      filter(all_complete),
    by = c("Taxonomic_Group", "Scientific_Name", "Common_Name"))

species_to_remove <- status_complete %>%
  filter(Year == min(Year) & SRank == "SX") %>%
  pull(Scientific_Name) %>%
  unique()

status_data_final <- status_complete %>%
  filter(!Scientific_Name %in% species_to_remove) %>%
  ungroup() %>%
  mutate(Taxonomic_Group = ifelse(
    Taxonomic_Group %in% c("Amphibians", "Reptiles and Turtles"),
    "Reptiles & Amphibians",
    Taxonomic_Group))

plan(multiprocess)

csi <- group_by(status_data_final, Taxonomic_Group, Year) %>%
  nest() %>%
  mutate(
    N = map_dbl(data, nrow),
    samples = future_map(
      data,
      ~ replicate(10000,
                  rli(map_dbl(.x$wts, sample, 1)))
    ),
    mean_wt = map_dbl(samples, mean),
    min_wt = map_dbl(samples, min),
    max_wt = map_dbl(samples, max),
    lci = map_dbl(samples, quantile, probs = 0.025),
    uci = map_dbl(samples, quantile, probs = 0.975))

ggplot(csi, aes(x = Year)) +
  facet_wrap(~Taxonomic_Group) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "grey80") +
  geom_line(aes(y = mean_wt))
