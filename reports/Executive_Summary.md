# Executive summary: Moving the first gate in Cookie Cats

**Prepared by: Anurag Moyal**

## Decision
**Do not ship the level-40 gate. Keep the first gate at level 30.**

## Why
| | gate_30 (current) | gate_40 (test) | Difference |
|---|---|---|---|
| Players in experiment | 44,700 | 45,489 | |
| **Day-7 retention (primary)** | **19.02 %** | **18.20 %** | **−0.82 pp** (95 % CI −1.33 to −0.31; p = 0.0016) |
| Day-1 retention | 44.82 % | 44.23 % | −0.59 pp (p = 0.074, not significant) |
| Rounds played, week 1 | no significant difference | | |

* On a base of 100,000 new installs, the later gate would retain **about 820 fewer players** at day 7 (range 310–1,330).
* The probability that the later gate is actually better on day-7 retention is **0.1 %**.
* The result holds under a multiple-testing correction, a permutation test, and when the two groups are forced to equal size.
* The drop is concentrated among players who play 30–99 rounds in week 1, the ones who reach the gate but are not yet hooked.

## Caveats the reader should know
1. **Randomisation looks slightly off:** gate_40 received 0.9 % more players than gate_30 (p = 0.009 against a 50/50 split). It did not change the conclusion, but engineering should find out why.
2. **Retention only:** the dataset has no revenue. If the later gate earns much more per player, the business case could differ; finance should check before closing the decision.
3. **Short horizon:** only day-1 and day-7 retention are observed; a day-30 readout would confirm the effect persists.

## Recommended next steps
1. Investigate the sample-ratio mismatch in the assignment/logging pipeline.
2. Re-run the readout with revenue and 14/30-day retention added.
3. Test the opposite direction (an earlier gate, e.g. level 20), since "later" was worse.
4. Fix the sample size and stopping rule before launch: about 45,000 players per group detects a 0.75 pp change with 80 % power; peeking at results mid-test would raise false positives from 5 % to roughly 14–28 %.
