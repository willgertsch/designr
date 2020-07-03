# Person randomized trials


# computes the power for a single level trial
# N: total sample size
# delta: standardized effect size, (mu_A - mu_0)/sigma
# alpha: type 1 error rate
# rho: proportion of variation explained by a covariate
prt_1level_power  <- function(N = NULL, delta = 0.20, alpha = 0.05, rho = 0) {

  if (is.null(N)) {
    print("Error: Please specify a sample size for prt_1level_power.")
    return(NA)
  }

  # compute NCP
  lam = N*delta^2/(4*(1-rho^2))

  # degrees of freedom change if covariate is present
  if (rho == 0)
    df2 = N-2
  else
    df2 = N-3

  # power calculation
  power = 1-pf(qf(1 - alpha,  1, df2), 1, df2, lam)

  return(power)
}
