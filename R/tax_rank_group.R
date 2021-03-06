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


#' Function to automate the rank trends by taxanomic group
#'
#' This function generates confidence intervals for ranks by taxanomic groups by resampling
#' species within a given group at each time point.
#'
#' @param wt_data input dataframe of species weighted ranks
#' @param tax_group name of column that contains grouping variable e.g. "taxonomic group"
#' @param wts_col name of column with numeric weight values (e.g. "wts_col")
#' @param yr_col name of column that specifies assessment (e.g. "year")
#' @param nreps number of times to subsample species weights (default = 1000)
#'
#' @return a dataframe containing mean, upper and lower confidence interval for samples
#' conservation status index
#' @export
#'
#' @importFrom dplyr .data
#'
sampled_index <- function(wt_data, tax_group, wts_col, yr_col, nreps= 1000){

  wt_data <- dplyr::mutate_(wt_data, wts = wts_col)

  wt_data <- dplyr::group_by_(wt_data, tax_group, yr_col)
  wt_data <- tidyr::nest(wt_data)
  csi <- dplyr::mutate(wt_data,
      N = purrr::map_dbl(.data$data, nrow),
      # TODO : split rows into double or single ranks - with resample only for double ranks (ie 3,4 not sp with 1 wt)
      # add a probability (vector corresponding to probability or type ie S1, S2, S3, = c(0.1, 0.8, 0.1))
      samples = purrr::map(
        .data$data,
        ~ replicate(nreps, rli(purrr::map_dbl(.x$wts, sample, 1)))
      ),
      mean_wt = purrr::map_dbl(.data$samples, mean),
      min_wt = purrr::map_dbl(.data$samples, min),
      max_wt = purrr::map_dbl(.data$samples, max),
      lci = purrr::map_dbl(.data$samples, stats::quantile, probs = 0.025), # TODO AT: makes into params
      uci = purrr::map_dbl(.data$samples, stats::quantile, probs = 0.975)
    )
  csi

}
