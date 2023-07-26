#' Helper functions for this package

#' Parse ancestries of interest
parse_anc <- function(data, num_of_anc){
  names(data)[3:(2+num_of_anc)] %>%
    return()
}

#' Calculate counts
count_anc <- function(data, ancestries){
  data %>%
    select(all_of(ancestries)) %>%
    slice(1) %>%
    rowSums() %>%
    return()
}

#' Calculate genome-wide percentile of mmpp
calculate_mmpp_percentile <- function(data, percentile){
  data %>%
    select(MEAN_MAX_POST_PROB) %>%
    as.matrix %>%
    c() %>%
    quantile(probs=percentile) %>%
    return()
}

#' Calculate global ancestry proportions
calculate_global_anc_prop <- function(data){
  data %>%
    select(7:9) %>%
    sapply(mean) %>%
    return()
}
