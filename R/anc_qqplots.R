#' @export

anc_qqplots.R <- function(anc_data_list){

  anc_prop_by_pos <- anc_data_list[["anc_prop_by_pos"]]

  qqplot_list <- vector(mode = "list", length = anc_data_list[["num_of_anc"]])

  i=0

  for (ancestry in anc_data_list[["ancestries"]]){

    i=i+1

    anc_counts <- as.vector(
      as.matrix(
        anc_prop_by_pos[anc_prop_by_pos$MEAN_MAX_POST_PROB > mmpp_percentile, ancestry]
        )
      )

    qqplot_list[[i]] <- qqPlot(
      x = anc_data_list[["anc_counts"]],
      y = NULL,
      distribution="norm",
      param.list = list(
        mean = mean(anc_data_list[["anc_counts"]]),
        sd = sd(anc_data_list[["anc_counts"]])
      ),
      estimate.params = FALSE,
      plot.type = "Q-Q",
      add.line = TRUE,
      qq.line.type = "0-1",
      line.col = "#4b2e83",
      main=paste("Gaussian qq-plot:", ancestry, "ancestry proportion", sep=" "),
      ylab=paste(ancestry, "ancestry proportion quantiles", sep=" ")
    )
  }
}
