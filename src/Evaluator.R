pmae <- function(predictions, actuals) {
  mae <- mean(abs(predictions - actuals))
  return(mae)
}