#' @export
#'
#' @title Creates an `anc_data` object.
#'
#' @description
#' Parses relevant data and performs necessary calculations for ancestry-based selection analysis given a tibble of `.anc_prop_by_pos` data.
#'
#' @details
#' Currently only works with output from `read_anc_prop_by_pos()`. This function:
#' * Parses ancestries of interest
#' * Calculates total ancestry counts
#' * Calculates global and local ancestry proportions
#' * Calculates differences in proportion
#' * Calculates genome-wide mean + sd for diff in proportion
#' * Calculates mmpp percentile
#'
#' TODO: specify number of ancestries
create_anc_data_list.R <- function(anc_prop_by_pos, num_of_anc, percentile){
  ancestries = parse_anc(anc_prop_by_pos, num_of_anc)
  counts = count_anc(anc_prop_by_pos, ancestries)
  mmpp_percentile = calculate_mmpp_percentile(anc_prop_by_pos, percentile)
  global_anc_props = calculate_global_anc_prop(anc_prop_by_pos)

  # calculate local ancestry proportions
  for(ancestry in ancestries){
    anc_prop_by_pos[,paste(ancestry,"PROP",sep="_")] <- anc_prop_by_pos[,ancestry] / counts
  }

  # calculate difference in proportions
  for (ancestry in ancestries){
    anc_prop_by_pos[,paste(ancestry,"PROP", "DIFF", sep="_")] <-
      anc_prop_by_pos[,paste(ancestry,"PROP", sep="_")] - global_anc_props[paste(ancestry,"PROP", sep="_")]
  }

  # return all this data as a list
  list(anc_prop_by_pos = anc_prop_by_pos,
       num_of_anc = num_of_anc,
       ancestries = ancestries,
       counts = counts,
       mmpp_percentile = mmpp_percentile,
       global_anc_props = global_anc_props) %>%
    return()
}

