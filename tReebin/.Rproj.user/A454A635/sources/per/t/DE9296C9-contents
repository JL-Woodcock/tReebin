summary.VariableBinning <- function(variableBinning, data, target = NULL, variable = NULL) {

  # TODO: Check inputs.

}

predict.VariableBinning <- function(VariableBinning, data, variable = NULL) {

  df <- data.frame(Var = data[[VariableBinning$info$varName]],
                   Target = data[[VariableBinning$info$targetName]])

  if (!is.null(variable)) {
    varName <- variable
  } else {
    varName = VariableBinning$info$varName
  }

  df <- data.frame(Var = data[[varName]])

  df_Binned <- df %>%
    mutate(Bin = cut(Var, VariableBinning$cutPoints))

  if (VariableBinning$missingBin == "Own") {
    levels(df_Binned$Bin) <- c(levels(df_Binned$Bin), "Missing")
    df_Binned[is.na(df_Binned$Var),]$Bin <- "Missing"
  } else {
    df_Binned[is.na(df_Binned$Var), ]$Bin <- VariableBinning$missingBin
  }

  return(df_Binned$Bin)

}

plot.VariableBinning <- function(variableBinning, data) {

  # TODO: Check inputs.

  df <- data.frame(Var = data[[variableBinning$info$varName]],
                   Target = data[[variableBinning$info$targetName]]) %>%
    mutate(Bin = predict(variableBinning, ., variable = "Var"))

  df_WOE <- woeTable(data = df, target = "Target", variable = "Bin")

  df_Plot <- df_WOE %>%
    mutate(colour = ifelse(WOE < 0, "Negative", "Positive"))

  maxAbsValue <- max(abs(df_WOE$WOE))
  IV_Formatted <- round(sum(df_WOE$IV), 2)

  ggplot(data = df_Plot,
         aes(x = Bin, y = WOE, fill = colour)) +
    geom_bar(stat = "identity", colour = "black") +
    theme_bw() +
    labs(x = variableBinning$info$varName,
         y = "Weight of Evidence",
         title =  variableBinning$info$varName,
         subtitle = "Weight of Evidence by Bucket",
         caption = paste0("Information Value: ", IV_Formatted)) +
    lims(y = c(- 1.1 * maxAbsValue, 1.1 * maxAbsValue)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(legend.position = "none")


}

#' @export
woeTable <- function(data, target, variable) {

  df <- data.frame(Var = data[[variable]],
                   Target = data[[target]])

  outDf <- df %>%
    dplyr::group_by(Var) %>%
    dplyr::summarise(Good = n() - sum(Target),
                     Bad = sum(Target)) %>%
    dplyr::mutate(Bad_Rate = Bad / (Bad + Good)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(WOE = log(Good / Bad) - log(sum(Good)/sum(Bad))) %>%
    dplyr::mutate(IV = (Good/sum(Good) - Bad/sum(Bad)) * WOE)

  names(outDf)[1] <- variable

  return(outDf)

}


