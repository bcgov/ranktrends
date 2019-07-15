library(tidyverse)
library(furrr)


# https://gist.github.com/ateucher/fc12bebfc189ab39b310d5321307475e
# Base on Andy T's gist:

status_data <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")


# issues to fix:
# non-breeding / breeding section needs fixing
    # remove non-ranking species : ?,  SNA?, and  Unranked (U)
    # add paramter to select B or N breeding ranks_to_numeric ie: breeding
    # how to deal with SZN ?

     "S3S4B,SZN" "S5B" "S4B"  "S4S5" "S3S4" "S4?"  "S4S5B"
[13] "S3B, S4N"  "S4?B"      "SU"     "S3B,S4N"
   "S3N,S4B"   "S2B"       "S4?B,SZN"  "S5B,S5N"   "S3B,SZN"
[25] "S2S3B,S4N" "S1B"       "S4S5B,S4N" "SNA"       "S2S3B"     "S2S3"
[31] "S1S2B"     "S4B,S4N"   "S3N"       "SHN"       "S1N"
[37] "S2M"       "SH"        "S2B,S3N"   "SNR"           "S3?"
[43] "S4N"       "S1S2N"     "S1B,SZN"   "S1B,S4N"   "S2B,SZN"   "S4B,SZN"
[49] "S4N,S5B"   "S3?B,SZN"  "S3S4N,SZN" "S2N,S3B"   "S1B,S3N"   "S5B,SZN"
[55] "S5B,S4N"   "S4S5B,SZN" "S2B,S4N"   "S3B,S5N"   "S4B,S3N"   "S3B,S2N"
[61] "S4S5B,S5N" "S4B,S5N"   "S1B,S2N"       "S3B,S3N"   "S5N"
[67] "SHB,S4N"   "SXB, SNAN" "S2?B"


# add unlist error check in rli function

list.to.fix <- c("SNA", "SU" , "SNR")

tdata <- status_data %>%
   filter(Taxonomic_Group == "Mammals") %>%
   filter(!SRank %in% list.to.fix)


# working
status_data<- tdata %>%
  mutate(parsed_rank = ranks_to_numeric(SRank, simplify = FALSE))

# not working
status_data<- tdata %>%
  mutate(parsed_rank = ranks_to_numeric(SRank, simplify = TRUE))
# getting error with double to interger # line 86?
#https://stackoverflow.com/questions/55397509/purrrmap-int-cant-coerce-element-1-from-a-double-to-a-integer

unique(tdata$SRank)



# select the subset of interest:

# 1) group by taxonomic group

status_data_wts <- status_data %>%
  mutate(parsed_rank = unlist(ranks_to_numeric(SRank)),
         wts = map(parsed_rank, ~ 5 - .x)) %>%
  mutate(valid_rank = map_lgl(wts, ~ !all(is.na(.x))))

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
