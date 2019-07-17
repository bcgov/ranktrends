library(tidyverse)
library(furrr)


# https://gist.github.com/ateucher/fc12bebfc189ab39b310d5321307475e
# Base on Andy T's gist:

status_data <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")


# issues to fix:
# 1) non-breeding / breeding section needs fixing
    # remove non-ranking species : ?,  SNA?, and  Unranked (U)
    # add paramter to select B or N breeding ranks_to_numeric ie: breeding
    # how to deal with SZN ?

# 2)  unusual calls ( list.to.fix <- c("SNA", "SU" , "SNR"))
list.to.fix <- c("SNA", "SU" , "SNR")

tdata <- status_data %>%
  filter(Taxonomic_Group == "Mammals") %>%
  filter(!SRank %in% list.to.fix)

# 3) do we want the output of ranks_to_numeric to be a list? \
# working
status_data<- tdata %>%
  mutate(parsed_rank = ranks_to_numeric(SRank, simplify = FALSE))


# not working
status_data <- tdata %>%
  mutate(parsed_rank = ranks_to_numeric(SRank, simplify = TRUE))

# 4) getting error with double to interger # line 86?
#https://stackoverflow.com/questions/55397509/purrrmap-int-cant-coerce-element-1-from-a-double-to-a-integer

status_data_wts <- status_data %>%
  mutate(parsed_rank = ranks_to_numeric(SRank),
         parsed_rank_single = map_dbl(parsed_rank, min),  # tried to extract 1st value ? not working
         wts = unlist(map(parsed_rank_single, ~ 5 - .x))) # need to extract the first value

status_complete <- status_data_wts %>%
  group_by(Taxonomic_Group) %>%
  complete(nesting(Scientific_Name, Common_Name), Year) %>%
  mutate(valid_rank = map_lgl(wts, ~ !all(is.na(.x)))) %>%
  semi_join(
    group_by(., Taxonomic_Group, Scientific_Name, Common_Name) %>%
      summarize(all_complete = all(valid_rank)) %>%
      filter(all_complete),
    by = c("Taxonomic_Group", "Scientific_Name", "Common_Name"))

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

plan(multiprocess) # Ask Andy about this

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
