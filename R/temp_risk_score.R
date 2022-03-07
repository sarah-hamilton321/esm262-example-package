#' Extreme Temperature Risk Score
#'
#' Our function takes a vector of daily temperatures and returns the risk
#' associated with the temperature vector and the number of extremes.
#' @param temp vector of daily temperatures (C)
#' @param age average tree age (years) default=200
#' @param temp_ideal ideal temperature for the tree species being considered (C) default=20
#' @param highest_threshold highest risk threshold temperature (C) default=38
#' @param medium_threshold medium risk threshold temperature (C) default=32
#' @return risk, num_extremes

temp_risk_score = function(temp, age = 200, temp_ideal = 20, highest_threshold = 38, medium_threshold = 32) {
  num_high_extremes = 0
  num_med_extremes = 0
  num_not_extreme = 0
  i = 1

  #while loop to calculate number of high extreme heat days, medium extreme heat days, and normal days
  while (i < length(temp)) {
    if (temp[i] > highest_threshold)
      num_high_extremes = num_high_extremes + 1
    else if (temp[i] > medium_threshold)
      num_med_extremes = num_med_extremes + 1
    else
      num_not_extreme = num_not_extreme + 1
    i = i + 1
  }

  #calculate risk as a number using an equation
  risk_num = (age/100)*(mean(temp)/temp_ideal)*((num_high_extremes/length(temp)) + 0.5*(num_med_extremes/length(temp)))

  #determine the class of risk based on the risk number
  risk_class = case_when (risk_num < 0.1 ~ "low",
                    risk_num >= 0.1 &
                    risk_num < 0.2 ~ "med",
                    risk_num >= 0.2 ~ "high")

  #return the results
  results = c(risk_class, num_high_extremes, num_med_extremes)
  return(results)
}
