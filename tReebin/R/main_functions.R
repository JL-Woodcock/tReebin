coarseClass <- function(data, target, variable, control = NULL) {

  df <- data.frame(Var = data[[variable]],
                   Target = data[[target]])

  df_NoMiss <- df %>%
    na.omit()

  control <- ctree_control(maxdepth = 3,
                           minbucket = nrow(df_NoMiss) * 0.02)

  cTree <- ctree(data = df_NoMiss, formula = Target ~ Var, control = control)

  df_Bin_Summary <- df_NoMiss %>%
    dplyr::mutate(Bin = predict(cTree, ., type = "node")) %>%
    dplyr::group_by(Bin) %>%
    dplyr::summarise(MaxBin = max(Var),
              MeanTarget = mean(Target),
              Volume = n())

  groupMaxs <- sort(df_Bin_Summary$MaxBin)

  if (length(groupMaxs) == 1) {
    binCuts <- c(-Inf, Inf)
  } else if (length(groupMaxs) == 2) {
    binCuts <- c(-Inf, groupMaxs[1], Inf)
  } else {
    binCuts <- c(-Inf, groupMaxs[1:(length(groupMaxs) - 1)], Inf)
  }

  return(binCuts)

}

#' @export
binContinuousVariable <- function(data, target, variable, missingStrategy = "auto") {

  MISSING_SIGNIFICANCE <- 0.05

  df <- data.frame(Var = data[[variable]],
                   Target = data[[target]])

  if (any(is.na(df[, 2]))) {
    stop("Input data cannot have missing values in the target column.")
  }

  df_Missing <- df %>%
    dplyr::filter(is.na(Var))

  df_NoMissing <- df %>%
    dplyr::filter(!is.na(Var))

  nonMissingCuts <- coarseClass(df_NoMissing, "Target", "Var")

  df_Binned <- df_NoMissing %>%
    dplyr::mutate(Bin = cut(Var, nonMissingCuts, right = T))

  df_Bin_Summary <- df_Binned %>%
    dplyr::group_by(Bin) %>%
    dplyr::summarise(Mean_Target = mean(Target),
              Volume = n())

  # Now we need to treat the missing category.

  missingTargetRate <- mean(df_NoMissing$Target)

  if (missingStrategy == "auto_agressive") {
    closestBin <- which.min(abs(missingTargetRate - df_Bin_Summary$Mean_Target))
  } else if(missingStrategy == "auto") {
    # TODO: Work this out...
    closestBin <- which.min(abs(missingTargetRate - df_Bin_Summary$Mean_Target))
  }

  # Test the missing target vs the bin target rate.
  closestBinLabel <- df_Binned %>%
    dplyr::filter(Bin == df_Bin_Summary$Bin[closestBin])

  missingTest <- t.test(df_Missing$Target, closestBinLabel$Target)
  missingDifferentFromGroup <- (missingTest$p.value <= MISSING_SIGNIFICANCE)

  missingBin <- ""

  if (missingStrategy %in% c("separate")) {
    missingBin = "Own"
  } else if (missingDifferentFromGroup) {
    missingBin = "Own"
  } else {
    message("Info: The target rate of the missing category is not significantly different to the target rate of the closest category. The missing cateogry has been merged.")
    missingBin <- df_Bin_Summary$Bin[closestBin]
  }

  outputObject <- list(
    cutPoints = nonMissingCuts,
    nBins = length(nonMissingCuts) - 1,
    missingBin = missingBin,
    info = list(
      varName = variable,
      targetName = target,
      variableType = "continuous"
    )
  )

  class(outputObject) <- "VariableBinning"

  outputObject

}
