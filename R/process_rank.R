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

#' Convert NatureServe S ranks to numeric
#'
#' Converts NatureServ S ranks to a number from `0` (`"SX"`) to `5` (`"S5"`).
#' Historic (`"SH"`) is converted to `0.5` (i.e., halfway between `0` and `1`).
#' When there are range ranks they are converted to a numeric vector (e.g.,
#' `"S2S4"` becomes `c(2,3,4)`)
#'
#' @param ranks character vector of input S ranks
#' @param simplify converts list to numeric vector, and rounds range ranks
#' using the function supplied to `round_fun`. Default = FALSE
#' @param round_fun what function to use (default `median`) to round range ranks into a single
#' value when `simplify` is `TRUE`. Ignored if simplify is `FALSE`.
#'
#' @return a list the same length as `ranks` of numeric vectors. For range
#'   ranks, the vector will be sequence from low to high (e.g.,`"S3S5"` becomes
#'   `c(3,4,5)`)
#'
#' @export
#'
#' @examples
#' ranks_to_numeric(c("S1", "SX", "S2S4"))
ranks_to_numeric <- function(ranks, simplify = FALSE,
                             round_fun = stats::median) {

  if (!is.character(ranks)) {
    stop("'ranks' should be a character vector", call. = FALSE)
  }

  if (!is.logical(simplify) || length(simplify) != 1L) {
    stop("simplify should be TRUE or FALSE", call. = FALSE)
  }

  if (!is.function(round_fun)) {
    stop("round_fun should be a function", call. = FALSE)
  }

  single_ranks <- clean_ranks(ranks)

  # Remove S/N/G etc from the beginning and end
  numeric_1 <- gsub("^[^0-9XH]|[^0-9XH]+$", "", single_ranks)

  # Convert SX and SH to 0 and 0.5 respectively
  numeric_2 <- gsub("X", "0", numeric_1)
  numeric_3 <- gsub("H", "0.5", numeric_2)
  numeric_3[!nzchar(numeric_3)] <- NA_character_

  # Split compound/range ranks into character vectors (split on any letters)
  char_list <- strsplit(numeric_3, "[a-zA-Z]")

  # Create numeric vectors from character
  num_list <- lapply(char_list, function(x) {
    x <- as.numeric(x)
    # If just one rank or two adjacent ranks, leave it
    if (length(x) == 1 || abs(diff(x)) == 1) {
      x
    } else {
      # Create a sequence (e.g. S3S5 -> c(3,4,5))
      seq(x[1], x[2])
    }
  })

  any_na = purrr::map_lgl(num_list, ~ any(is.na(.x)))

  if (any(any_na)) {
    warning("There is one or more NA values in your input dataset",
            call. = FALSE)
  }

  if (simplify) {
    # find range ranks (vectors longer than 1)
    longer_than_one <- purrr::map_lgl(num_list, ~ length(.x) > 1)
    if (any(longer_than_one)) {
      # round using provided round_fun
      rounded <- purrr::map_dbl(num_list[longer_than_one], round_fun) # getting error with double to integer
      num_list[longer_than_one] <- rounded
    }
    num_list <- unlist(num_list)
  }
  num_list
}


#' Clean messy ranks into simple ranks, converting
#' ranks with more than one breeding status (e.g., "S5N,S2B") into
#' single ranks (specified by the `keep` paramater)
#' @importFrom purrr map_chr
#' @param ranks a character vector of ranks
#' @param keep which component to of a rank with multiple ranks for different
#' breeding statuses. `"B"` = breeding, `"M"` = migratory, `"N"` = non-breeding
#' @return character vector of cleaned ranks
#' @export
#'
clean_ranks <- function(ranks, keep = "B") {
  stopifnot(keep %in% c("B", "N", "M"))
  stopifnot(is.character(ranks))
  # split double-barreled ranks
  ranks_split <- strsplit(ranks, "(?<=[0-5XHQ?][NBM]),?(\\s+)?", perl = TRUE)

  # clean ranks, when double-barrelled, keeping the specified rank
  purrr::map_chr(ranks_split, clean_rank, keep)
}

clean_rank <- function(rank, keep) {
  # If no numeric, X, or H ranks return NA
  if (!any(grepl("[SNG][0-5HX]", rank))) return(NA_character_)

  non_keeps <- gsub(keep, "", "BNM")
  if (length(rank) == 1 && !grepl(sprintf("[0-5XHQ?][%s]$", non_keeps), rank)) {
    ret_rank <- rank
  } else {
    ret_rank <- rank[grepl(keep, rank)]
    if (!length(ret_rank)) {
      return(NA_character_)
    }
  }

  # Strip off C, Q, and breeding qualifier
  gsub(sprintf("C?Q?%s?$", keep), "", ret_rank)
}
