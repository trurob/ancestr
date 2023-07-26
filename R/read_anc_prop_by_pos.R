#' @export
#'
#' @title Read an `.anc_prop_by_pos` file into a tibble
#'
#' @description
#' The `.anc_prop_by_pos` file should follow the following format:
#' * First line in the file is the header with column names.
#' * Column 1 is named `CHROM` and denotes the chromosome number.
#' * Column 2 is named `POS` and denotes the position within the chromosome.
#' * The next k columns are named `ANCESTRY_i`, denoting the k different ancestries being studied. For example: AFRICA, AMERICA, EUROPE.
#' * The final column is named `MEAN_MAX_POST_PROB`, and denotes the local ancestry inference confidence heuristic.
#'
#' @details
#' An `.anc_prop_by_pos` file is a space, or tab, delimited table saved as a
#' text file that store ancestry count data for a population of interest.
#' Each row denotes a single position in the genome, with counts for each
#' ancestry present at that position, along with a posterior probability.
#' Ancestry counts across a row should equal the number of individuals in the
#' target population. All rows should have the same ancestry count.
#' The posterior probability should be any heuristic used to measure the
#' confidence of the local ancestry calls at that location.
#'
#' Currently this function is only meant to work with output files of `get_anc_prop_by_pos.py`.
#'
#' @param filepath a filepath to the `.anc_prop_by_pos` file
#' @return A tibble containing the `.anc_prop_by_pos` file data.
#' @author Robert Trujillo <github.com/trurob>

read_anc_prop_by_pos <- function(filepath){
  readr::read_table(filepath) %>%
    mutate(CHROM = as.numeric(gsub("chr", "", CHROM))) %>%
    return()
}
