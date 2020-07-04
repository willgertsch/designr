# Cluster randomized trials for continuous outcomes

# Compute power for 2 level CRTs
# n: number of subjects per cluster
# J: number of clusters
# delta: standardized effect size
# icc: intra-class correlation coefficient
# alpha: significance level
# R2: proportion of explained variation by covariate
crt_2level_power <- function(n, J, delta, icc, alpha = 0.05, R2 = 0) {


  # NCP
  lam = (J * delta^2)/(4 * ((1 - R2) * icc + (1 - icc)/n))

  # degrees of freedom change if covariate is present
  if (R2 == 0)
    df2 = J-2
  else
    df2 = J-3

  # power calculation
  power = 1-pf(qf(1 - alpha,  1, df2), 1, df2, lam)
  return(power)
}

