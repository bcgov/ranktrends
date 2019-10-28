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

sampled_index <- function(wt_data, tax_group, wts_col, yr_col){

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
