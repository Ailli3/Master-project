prob_from_temperature <- function(T, S_before, S_after, P){

  numerator_a_value <- lapply(S_before, function(S) {
    a_criterion_f(S, P)
  })
  denominator_a_value <- lapply(S_after, function(S) {
    a_criterion_f(S, P)
  })
  numerator <- sum(sapply(numerator_a_value, function(x) exp(-x / T)))
  denominator <- sum(sapply(denominator_a_value, function(x) exp(-x / T)))

  return(numerator/denominator)
}
