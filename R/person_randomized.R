# Person randomized trials


# computes the power for a single level trial
# N: total sample size
# delta: standardized effect size, (mu_A - mu_0)/sigma
# alpha: type 1 error rate
# rho: proportion of variation explained by a covariate
prt_1level_power <- function(N = NULL, delta = 0.20, alpha = 0.05, rho = 0) {

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

# computes power for a multisite trial
# n: number of subjects per site
# J: number of sites
# delta: standardized effect size: treatment effect / between persons variance
# tau: variability between sites on the treatment effect
# rho: proportion of variance explained by covariate
# B: percent of variance explained by blocking
# s2_delta
prt_multisite_power <- function(n, J, delta, s2_delta, alpha = 0.05,
                                B = 0, rho = 0) {

  # TODO:
  # - add support for fixed effect models
  # - add support for computing power for treatment effect testing

  # update for percent of variance explained by blocking
  delta = delta/sqrt(1 - B)
  s2_delta = s2_delta/(1 - B)

  # update for proportion of variance explained by covariate
  delta = delta/sqrt(1 - rho)
  s2_delta = s2_delta/(1 - rho)

  # NCP
  lam = (J * delta^2)/(s2_delta + 4/n)

  # power calculation
  df2 = J-1
  power = 1-pf(qf(1 - alpha,  1, df2), 1, df2, lam)
  return(power)
}
