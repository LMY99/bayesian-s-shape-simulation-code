mother_dir <- "/users/mli1/simulation/95Result_moreflex_s1s2_0.01"
setwd(mother_dir)
col_names <- c(
  "Truth", "Model", "Bounds",
  "Intercept", "X1", "X2",
  "Curve", "Inflection Point",
  "50% Threshold", "Random-effect variance",
  "Residual variance"
)
Model <- c("Truth", "S-shaped", "S-shaped", "Flexible", "Flexible", "Logistic")
directory <- c("logit", "asym")
Truth_list <- c("Logistic", "Asymmetric")
Range <- c(NA, "0-120", "30-90", "0-120", "30-90", NA)
for (dirr in directory) {
  setwd(sprintf("%s/%s", mother_dir, dirr))
  Truth <- c(Truth_list[directory == dirr], rep(NA, 5))
  filenames <- c(
    "para_CIs.rda", "sS_CIs.rda", "S_CIs.rda", "sflex_CIs.rda", "flex_CIs.rda"
  )
  df <- c()
  library(magrittr)
  library(matrixStats)
  for (filename in filenames) {
    load(filename)
    if (length(df) == 0) {
      df[1:3] <- colMeans(CI_covariate_repeat[, , 4])
      df[4] <- NA
      df[5] <- round(mean(true_turning), 1)
      df[6] <- round(mean(true_Q50), 1)
      df[7] <- mean(sigmaw_repeat[, 4])
      df[8] <- mean(sigmay_repeat[, 4])
    }
    row <- c()
    covar_cover <- apply(apply(CI_covariate_repeat, c(1, 2), function(x) (x[4] - x[2]) * (x[4] - x[3]) <= 0), 2, mean)
    bvm <- cbind(apply(CI_covariate_repeat, c(2, 3), mean)[, c(6, 7, 5)] * 1e4, covar_cover * 100)
    row <- apply(bvm, 1, function(x) sprintf("%.0f+%.0f=%.0f,%.0f%%", x[1], x[2], x[3], x[4]))
    avg_bvm <- apply(CI_repeat[, , c(5, 7, 8, 6)], c(2, 3), mean)
    avg_bvm <- sapply(2:4, function(j) integrate(approxfun(avg_bvm[, 1], avg_bvm[, j]), 0, 120)$value / 120)
    row <- c(row, sprintf(
      "%.2f, %.2f%%", signif(sqrt(avg_bvm[3]), 3),
      mean(apply(apply(CI_repeat, c(1, 2), function(x) (x[4] - x[2]) * (x[4] - x[3]) <= 0), 2, mean)) * 100
    ))
    if (!grepl("flex", filename)) {
      if (dirr == "logit") true_turning <- rep(70, 100)
      inflect_coverage <- mean((rowMaxs(turning[, 1:3]) - true_turning) * (rowMins(turning[, 1:3]) - true_turning) <= 0) * 100
      inflect_bvm <- colMeans(turning)[c(5, 6, 4)]
      row <- c(row, sprintf(
        "%.2f, %.0f%%",
        signif(sqrt(inflect_bvm[3]), 3),
        inflect_coverage
      ))
    } else {
      row <- c(row, NA)
    }
    if (dirr == "asym") {
      true_Q50 <- rep(69.31112, 100)
    }
    q_coverage <- mean((rowMaxs(Q50[, 1:3]) - true_Q50) * (rowMins(Q50[, 1:3]) - true_Q50) <= 0) * 100
    q_bvm <- colMeans(Q50)[c(5, 6, 4)]
    row <- c(row, sprintf(
      "%.2f, %.0f%%", signif(sqrt(q_bvm[3]), 3),
      q_coverage
    ))
    sw_cover <- mean((sigmaw_repeat[, 4] - sigmaw_repeat[, 2]) * (sigmaw_repeat[, 4] - sigmaw_repeat[, 3]) <= 0) * 100
    sw_bvm <- colMeans(sigmaw_repeat)[c(6, 7, 5)] * 1e4
    row <- c(row, sprintf(
      "%.0f+%.0f=%.0f,%.0f%%", sw_bvm[1], sw_bvm[2], sw_bvm[3],
      sw_cover
    ))
    sy_cover <- mean((sigmay_repeat[, 4] - sigmay_repeat[, 2]) * (sigmay_repeat[, 4] - sigmay_repeat[, 3]) <= 0) * 100
    sy_bvm <- colMeans(sigmay_repeat)[c(6, 7, 5)] * 1e4
    row <- c(row, sprintf(
      "%.0f+%.0f=%.0f,%.0f%%", sy_bvm[1], sy_bvm[2], sy_bvm[3],
      sy_cover
    ))
    df <- rbind(df, row)
  }
  df <- cbind(Truth, Model, Range, df)
  colnames(df) <- col_names
  df <- as.data.frame(df)
  rownames(df) <- NULL
  sink(sprintf("../table_%s.txt", dirr))
  print(xtable::xtable(df[, -c(4:6, 10, 11)], ), include.rownames = FALSE)
  sink()
}
setwd("..")
system("cat table_logit.txt table_asym.txt > table.txt")
