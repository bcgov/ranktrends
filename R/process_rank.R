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

#' Convert S ranks to numeric
#'
#' Converts an rank with S to a single number
#'
#' @param ranks character vector of input S ranks
#' @param simplify converts list to numeric vector. Default = FALSE
#' @param round for range ranks which are choose. Default = `'middle'`.
#' Ignored if simplify is FALSE.
#'
#' @return a list the same length as `ranks` of numeric vectors. For range ranks,
#'  the vector will be sequence from low to high (e.g.,`"S3S5"` becomes `c(3,4,5)`)
#'
#' @export
#'
#' @examples
#' ranks_to_numeric(c("S1","S4","S3S4"))
#'
ranks_to_numeric <- function(ranks, simplify = FALSE,
                            round = c ("middle", "up","down")) {
  ## Add argument check

  ranks_split <- strsplit(ranks, ",\\s?")
  single_ranks <- make_single_ranks(ranks_split)

  numeric_1 <- gsub("^[^0-9XH]|[^0-9XH]+$", "", single_ranks)
  numeric_2 <- gsub("X", "0", numeric_1)
  numeric_3 <- gsub("H", "0.5", numeric_2)
  numeric_3[!nzchar(numeric_3)] <- NA_character_
  char_list <- strsplit(numeric_3, "[a-zA-Z]")
  num_list <- lapply(char_list, as.numeric)
  lapply(num_list, function(x) {
    # If just one rank or two adjacent ranks, leave it
    if (length(x) == 1 || abs(diff(x)) == 1) {
      x
    } else {
      # Create a sequence (e.g. S3S5 -> c(3,4,5))
      seq(x[1], x[2])
    }
  })

  if (simplify) {
    longer_than_one <- purrr::map_lgl(num_list, ~ length(.x) > 1)

    if (any(longer_than_one)){}

    )
    num_list <- unlist(num_list)
  }
  num_list
}


#' Makes a single rank
#' @importFrom purrr map_chr
#' @noRd
make_single_ranks<- function(x) {
  map_chr(x, ~ {
    if (length(.x) == 1) {
      .x
    } else {
      b <- .x[grepl("B$", .x)]
      if (!length(b)) NA_character_ else b
    }
  })
}
