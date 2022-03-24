# set-up
library(dplyr)

# load the iteration history
it <- haven::read_sav("ConvergenceSPSS/it.sav")
vrbs <- names(it)[-c(1:3)]

# tidy dataframe
it_clean <- it %>%
  dplyr::mutate(
    .imp = as.numeric(Imputations_),
    .it = as.numeric(Iteration_),
    .ms = factor(SummaryStatistic_, labels = c("mean", "sd"))
  )
it_long <- tidyr::pivot_longer(it_clean, vrbs, names_to = "vrb")

# plot trace 
ggplot2::ggplot(it_long, ggplot2::aes(x = .data$.it, y = .data$value, color = as.factor(.data$.imp))) +
  ggplot2::geom_line() +
  ggplot2::facet_grid(vrb ~ .ms, scales = "free") +
  ggplot2::labs(
    x = "Iteration",
    y = "",
    color = "Imputation number"
  ) +
  ggplot2::theme(legend.position = "bottom")
