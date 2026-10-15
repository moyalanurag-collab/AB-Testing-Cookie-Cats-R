# A/B Testing & Experimentation Analysis: Cookie Cats (R)

**Author: Anurag Moyal** · Data Analytics Portfolio Project

A complete, statistically rigorous analysis of a real mobile-game experiment: **does moving the first gate from level 30 to level 40
keep more players coming back?** The project covers the whole experimentation workflow: data validation, hypothesis tests with confidence
intervals, a Bayesian view, multiple-testing control, robustness checks, power and sample-size planning, and a clear
**ship / don't-ship recommendation**.

| Piece | What it shows | Where |
|---|---|---|
| Analysis report | Data audit, sample-ratio-mismatch check, z-tests, CIs, logistic regression, robust tests for skewed data, Bayesian posterior, Holm correction, permutation and resampling checks, segment analysis | `reports/01_ab_test_analysis.html` (source: `notebooks/01_ab_test_analysis.Rmd`) |
| Power and decision report | MDE, sample-size tables, power curves, A/A simulation, cost of "peeking", business impact, decision matrix, recommendation | `reports/02_power_and_decision.html` (source: `notebooks/02_power_and_decision.Rmd`) |
| Reusable R functions | `prop_compare()`, `bayes_prop()`, `boot_diff()`, `srm_check()` | `R/helpers.R` |
| One-page summary | For a non-technical reader | `reports/Executive_Summary.md` |

Open the two `.html` reports in any browser; they are self-contained (charts embedded).

## Headline result
* **Day-7 retention fell from 19.02 % (gate_30) to 18.20 % (gate_40)**, a drop of 0.82 percentage points (95 % CI −1.33 to −0.31, p = 0.0016).
* Day-1 retention and rounds played did not change significantly. The loss is concentrated among players with 30–99 rounds, the ones who meet the gate.
* Probability that gate_40 is actually better: **0.1 %**. The result survives Holm's multiple-testing correction, a permutation test and resampling to equal arm sizes.
* **Recommendation: do not ship gate_40; keep the gate at level 30.**
* **Flagged risk:** a sample-ratio mismatch (44,700 vs 45,489 players, p = 0.009) means assignment was not exactly 50/50. It did not change the conclusion, but engineering should investigate.

## Project structure
```
data/raw/cookie_cats.csv          90,189 players (public copy of the Kaggle "Cookie Cats" dataset)
R/helpers.R                       statistical helper functions
R/render_reports.R                knits both reports to Markdown + HTML (no Pandoc needed)
R/install_packages.R              installs the required R packages
notebooks/*.Rmd                   the analyses (R Markdown)
reports/                          rendered reports, executive summary, figures
```

## Reproduce
1. Install R (4.3 or newer) from <https://cran.r-project.org>.
2. Install the packages and render the reports (about 4 minutes, mostly the simulations in report 2):
```bash
Rscript R/install_packages.R
Rscript R/render_reports.R
```
Or double-click `run_all.bat` on Windows. The `.Rmd` files can also be opened and knitted from RStudio.

## Methods in one paragraph
Players were randomised to *gate_30* (control) or *gate_40*. The primary metric (day-7 retention), secondary metric (day-1 retention) and guardrail
(week-1 rounds) were fixed before analysis. Proportions are compared with two-proportion z-tests, reported with Wald confidence intervals, odds ratios and Cohen's h.
Rounds are heavy-tailed with one extreme outlier, so they are tested with Mann-Whitney, a log-scale t-test and bootstrap CIs on the mean, winsorised mean and median.
A Beta-Binomial model gives the probability that the treatment is better. Holm's method adjusts for three metrics. Power analysis uses the `pwr` package, and an A/A
simulation checks that the testing procedure has a 5 % false-positive rate (and shows how peeking inflates it).

## Limitations
* Retention only: the data has no revenue, so a retention loss could in principle be offset by monetisation gains. The report says so and lists what would change the decision.
* The segment analysis is exploratory (engagement is measured after randomisation).
* The sample-ratio mismatch cannot be explained from the data alone.
* The dollar-value table in report 2 is illustrative (assumed value per retained player).

## CV bullets (edit to taste)
* *Analysed a 90k-player mobile-game A/B test in R: validated randomisation (found a sample-ratio mismatch), ran hypothesis tests with confidence intervals, Bayesian and permutation checks, and Holm correction; showed the later gate cut day-7 retention by 0.82 pp (p = 0.0016) and recommended not shipping.*
* *Built power and sample-size analyses (MDE, power curves) and an A/A simulation showing that peeking at results raises false positives from 5 % to 14–28 %.*

## Data source
Cookie Cats dataset, originally on Kaggle ("Mobile Games A/B Testing - Cookie Cats"); this copy comes from a public GitHub repository
(`yanny-alt/Cookie-Cats-A-B-Testing-Project`). Columns: `userid`, `version`, `sum_gamerounds`, `retention_1`, `retention_7`.
