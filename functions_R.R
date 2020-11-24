location_p_values <- function(names, case, control) {
  require(MASS) #load MASS package
  fisher_p_values <- numeric(0) #create empty vector
  chi_p_values <- numeric(0) #create empty vector

  for (i in 1:length(case)) {
    cases <- c(case[i],sum(case)-case[i])
    controls <- c(control[i],sum(control)-control[i])
    location_data <- cbind(cases,controls)
    location_matrix <- as.matrix(location_data)
    fish <- fisher.test(location_matrix)
    chi <- chisq.test(location_matrix) 
    fisher_p_values[i] <- fish$p.value
    chi_p_values[i] <- chi$p.value
  }
  location_data_frame <- cbind.data.frame(names, case, control, fisher_p_values, chi_p_values)
  names(location_data_frame) <- c("location", "cases", "controls", "fisher p-values", "chi-squared p-values")
  
  print(location_data_frame)
}