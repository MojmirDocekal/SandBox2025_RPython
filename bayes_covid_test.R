# nice explanation:
# https://www.lri.fr/~mbl/COVID19/bayes.html
# Function to calculate P(sick | positive) given prevalence and accuracy
# prevalence: proportion of population who are sick (e.g., 0.10 for 10%)
# accuracy: probability that the test gives a correct result (e.g., 0.95 for 95%)

bayes_positive <- function(prevalence, accuracy) {
  # Calculate probabilities
  P_sick <- prevalence
  P_healthy <- 1 - P_sick
  P_accurate <- accuracy
  P_inaccurate <- 1 - P_accurate
  
  # P(positive | sick) = accuracy
  # P(positive | healthy) = 1 - accuracy (since false positive)
  # P(positive) = accuracy * prevalence + (1 - accuracy) * (1 - prevalence)
  P_positive <- P_accurate * P_sick + P_inaccurate * P_healthy
  
  # Bayes theorem: P(sick | positive) = P(positive | sick) * P(sick) / P(positive)
  P_sick_given_positive <- (P_accurate * P_sick) / P_positive
  
  return(P_sick_given_positive)
}

# Example: prevalence = 10%, accuracy = 95%
prevalence <- 0.10
accuracy <- 0.95
result <- bayes_positive(prevalence, accuracy)
cat(sprintf("P(sick | positive) = %.1f%%\n", 100 * result))
# Output should be approximately 67.9%