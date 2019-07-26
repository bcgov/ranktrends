

# Pseudo-code for desired function
# rank_data %>%
#   group_by(tax_group, yr) %>%
#   sampled_index(wts_list) # sampled_index doesn't exist yet
#
# tax_group | yr | mean_wt | min_wt | max_wt | lci | uci

# function to automate the rank trends by taxanomic group

# read in data
tdata <- read_csv("https://catalogue.data.gov.bc.ca/dataset/4484d4cd-3219-4e18-9a2d-4766fe25a36e/resource/842bcf0f-acd2-4587-a6df-843ba33ec271/download/historicalranksvertebrates1992-2012.csv")
tax_group <- Taxonomic_Group
species_name <- Scientific_Name


# example: tax_trends(tdata, Taxonomic_Group)



tax_trends <- function(rank_data, tax_group, NA = FALSE){




}




# create a function to calculate csi for sub groups

mammals <- status_data_final %>%
  filter(Taxonomic_Group == "Mammals")

csi <- function(status_data, reps, wts_list, NAs) {

  stats_data <- mammals
  reps = 10
  wts_list = 'wts'
  NAs = TRUE

  if (NAs == TRUE) {
    stats_data  <- stats_data %>%
      filter(!is.na(wts_list))
  }

  stats_data <- stats_data %>%
    mutate(N = n(),
           samples = map(wts, ~ replicate(reps,
                                          rli(map_dbl(.x, sample, 1)))),
           mean_wt = map_dbl(samples, mean),
           min_wt = map_dbl(samples, min),
           max_wt = map_dbl(samples, max),
           lci = map_dbl(samples, quantile, probs = 0.025),
           uci = map_dbl(samples, quantile, probs = 0.975))


}
