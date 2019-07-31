# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



# Pseudo-code for desired function
# rank_data %>%
#   group_by(tax_group, yr) %>%
#   sampled_index(wts_list) # sampled_index doesn't exist yet
#
# tax_group | yr | mean_wt | min_wt | max_wt | lci | uci

# function to automate the rank trends by taxanomic group

library(dplyr)


#  wt_data <- status_data_final
#  tax_group <-'Taxonomic_Group'
#  wts_col = "wts"
#  yr_col = "Year"

sampled_index <- function(wt_data, Tax_group, wts_col, yr_col){

  wt_data <- wt_data %>%
    mutate_(wts = wts_col)

  csi <- group_by_(wt_data, tax_group, yr_col) %>%
    nest() %>%
    mutate(
      N = map_dbl(data, nrow),
      samples = map(
        data,
        ~ replicate(10, rli(map_dbl(.x$wts, sample, 1)))
      ),
      mean_wt = map_dbl(samples, mean),
      min_wt = map_dbl(samples, min),
      max_wt = map_dbl(samples, max),
      lci = map_dbl(samples, quantile, probs = 0.025),
      uci = map_dbl(samples, quantile, probs = 0.975)
      )
 csi
}


csi <- sampled_index(wt_data, "Taxanomic_Group","wts","Year")



csi.plot <- csi %>%
  group_by(Taxonomic_Group, Year) %>%
  summarize(mean = mean(mean_wt), lci = mean(lci), uci = mean(uci))




rank_plot <- function(plot.data, t_group) { }

ggplot(csi.plot, aes(x = Year)) +
  facet_wrap(~Taxonomic_Group) +
  geom_point(aes(y = mean)) +
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin=lci,ymax=uci), alpha = 0.2)


}




}




# create a function to calculate csi for sub groups


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
