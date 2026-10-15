# Helper functions for the Cookie Cats A/B test analysis
# Author: Anurag Moyal

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales); library(broom); library(pwr); library(readr)
})

COL <- c(gate_30 = "#2F5D8A", gate_40 = "#E08A2E")        # control = blue, treatment = orange
theme_ab <- function() theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), legend.position = "top")

# Sample-ratio-mismatch check: were users split as designed (50/50)?
srm_check <- function(n_control, n_treat, expected = c(.5, .5)) {
  chisq.test(c(n_control, n_treat), p = expected)
}

# Two-proportion comparison with everything a reviewer asks for
# x1,n1 = control successes / size ; x2,n2 = treatment
prop_compare <- function(x1, n1, x2, n2, conf = .95) {
  p1 <- x1 / n1; p2 <- x2 / n2; d <- p2 - p1
  se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)                       # unpooled (Wald) for the CI
  z <- qnorm(1 - (1 - conf) / 2)
  pt <- prop.test(c(x2, x1), c(n2, n1), correct = FALSE)                     # pooled z-test (chi-square form)
  odds <- (p2 / (1 - p2)) / (p1 / (1 - p1))
  se_lor <- sqrt(1 / x1 + 1 / (n1 - x1) + 1 / x2 + 1 / (n2 - x2))
  tibble(control = p1, treatment = p2, diff_pp = d * 100, ci_low_pp = (d - z * se) * 100, ci_high_pp = (d + z * se) * 100,
         rel_lift_pct = d / p1 * 100,
         rel_ci_low_pct = (d - z * se) / p1 * 100, rel_ci_high_pct = (d + z * se) / p1 * 100,
         odds_ratio = odds, or_low = exp(log(odds) - z * se_lor), or_high = exp(log(odds) + z * se_lor),
         cohens_h = ES.h(p2, p1), z = sqrt(unname(pt$statistic)) * sign(d), p_value = pt$p.value)
}

# Beta-Binomial Bayesian comparison (flat Beta(1,1) priors)
bayes_prop <- function(x1, n1, x2, n2, draws = 200000, seed = 42) {
  set.seed(seed)
  a <- rbeta(draws, 1 + x1, 1 + n1 - x1); b <- rbeta(draws, 1 + x2, 1 + n2 - x2)
  list(prob_treat_better = mean(b > a), diff = b - a,
       expected_loss_ship = mean(pmax(a - b, 0)),      # what we lose, in pp of retention, if we ship and control was actually better
       expected_loss_keep = mean(pmax(b - a, 0)),      # what we give up if we keep control and treatment was actually better
       cred = quantile(b - a, c(.025, .975)))
}

# Non-parametric bootstrap CI for a difference in a statistic (e.g. mean / median) between treatment and control
boot_diff <- function(x_control, x_treat, stat = mean, B = 5000, seed = 42) {
  set.seed(seed)
  d <- replicate(B, stat(sample(x_treat, replace = TRUE)) - stat(sample(x_control, replace = TRUE)))
  c(estimate = stat(x_treat) - stat(x_control), quantile(d, c(.025, .975)), p_two_sided = 2 * min(mean(d <= 0), mean(d >= 0)))
}

fmt_p <- function(p) ifelse(p < 0.0001, "<0.0001", sprintf("%.4f", p))
