generate_data = function(n, p) {
  responses = rnorm(n, 0, 1)
  covariates = matrix(responses, n, p)
  return(list(covariates, responses))
}
