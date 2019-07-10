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

#' Calculate status index from numeric status scores
#'
#' @param w species weights (numeric)
#' @param Wex maxiumum weighted score for extinct species. Default `5` for NatureServe
#' ranking system
#'
#' @param N number of species (default length of 'w')
#'
#' @return numeric vector on the scale between 0 and 1.
#' @export
#'
#' @examples
#' rli(c(0,2,5,2))
rli <- function(w, Wex = 5, N = length(w)) {

  if (is.character(w)) {
    stop("status scores should be a numeric vector", call. = FALSE)
  }

  any_na = purrr::map_lgl(w, ~ any(is.na(.x)))

  if (any(any_na)) {
    warning("Sorry it looks like you have one or more NA values in your input dataset",
            call. = FALSE)
  }

  M = Wex * N
  T = sum(w)
  (M - T) / M

}
