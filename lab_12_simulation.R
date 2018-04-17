generate_data = function(n, p) {
  responses = rnorm(n, 0, 1)
  covariates = matrix(responses, n, p)
  return(list(covariates, responses))
}

model_select = function(covariates, responses, cutoff) {
  lin.reg = lm(responses ~ covariates)
  ret.cov = subset(covariates, summary(lin.reg)$coefficients[, 4] <= cutoff)
  if (length(ret.cov) == 0) return(vector(length = 0))
  lin.reg2 = lm(responses ~ ret.cov)
  return(summary(lin.reg2)$coefficients[, 4])
}
