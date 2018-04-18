generate_data = function(n, p) {
  covariates = matrix(rnorm(n*p, 0, 1), n, p)
  responses = rnorm(n, 0, 1)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff) {
  lin.reg = lm(responses ~ covariates)
  all.p.vals = (summary(lin.reg)$coefficients)[-1, 4]
  ret.idx = which(all.p.vals <= cutoff)
  if (length(ret.idx) == 0) return(c())
  lin.reg2 = lm(responses ~ covariates[, ret.idx])
  new.p.vals = (summary(lin.reg2)$coefficients)[-1, 4]
  return(new.p.vals)
}

run_simulation = function(n_trials, n, p, cutoff) {
  p.values = vector(length = 0)
  for (i in 1:n_trials) {
    dat = generate_data(n, p)
    cov = dat$covariates
    res = dat$responses
    p.values = c(p.values, model_select(cov, res, cutoff))
  }
  save(p.values, file = "p_values.RData")
}

make_plot = function(datapath) {
  load(datapath)
  hist(p.values)
}

par(mfrow=c(3, 3), mar=c(2, 2, 2, 2))
for (i in c(100, 1000, 10000)) {
  for (j in c(10, 20, 50)) {
    run_simulation(10, i, j, cutoff = 0.05)
    make_plot("p_values.RData")
  }
}
