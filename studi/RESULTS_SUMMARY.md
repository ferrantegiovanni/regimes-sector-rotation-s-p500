# Market regimes, sector rotation and portfolio construction — summary of results

*Working summary of the three studies. All figures are from the tables in `tabelle_regime/`, `tabelle_ciclo/` and `tabelle_costruzione/`. In-text references were checked against bibliographic records in November 2026; Stangl, Jacobsen and Visaltanachoti (2009) is an unpublished Massey University working paper.*

## 1. Research questions

The project asks three questions on the historical constituents of the S&P 500:

1. Does a market-regime signal, identified out of sample and without look-ahead bias, improve the allocation across sectors relative to a rotation that ignores the regime?
2. Does the answer change when the regime is defined in economic terms (the business cycle, monetary policy) rather than statistically (a hidden Markov model on market returns)?
3. Holding the selection signal fixed, how much do portfolio construction methods affect return and risk?

## 2. Design

**Universe and data.**
- **Universe**: point-in-time S&P 500 membership; security-level identifiers; delisting returns; S&P 500 Total Return benchmark; Fama-French and Carhart factors. All built by the data pipeline in `R/00_universo.R`.
- **Sectors**: TRBC economic sectors (10 after merging one three-stock sector). GICS could not be used because LSEG provides no GICS classification for 290 of the 369 delisted instruments, which would have biased the sector baskets towards surviving firms.
- **Sector baskets**: value-weighted portfolios of index members at each formation date.

**Timing.**
- Information up to the end of month *t*; portfolio formed at the end of *t*; return earned in *t*+1.
- Every regime, and every expected return conditional on a regime, uses only information available at *t*.

**Rotation rule** (common to Studies 1 and 2).
- At each *t*, the expected next-month return of each sector in each regime is the average of its past returns, weighted by the regime known in real time at each past date and shrunk towards the sector's unconditional mean (prior weight of 12 months).
- The three sectors with the highest expected return are held in equal proportions, cap-weighted within each sector.
- Monthly rebalancing; 10 basis points per side.

**Benchmarks for the signal.**
- An *unconditional rotation*: the same rule without a regime. The difference between the two isolates the contribution of the regime.
- Equal-weighted sectors.
- The S&P 500 Total Return Index.

**Sample.** 257 months (February 2005 – June 2026), the same for every strategy. Inference uses Newey-West standard errors.

## 3. Study 1 — A statistical regime: hidden Markov model

**Regime identification.**
- A two-state Gaussian HMM (Hamilton, 1989) on the monthly market excess return (Kenneth French, from July 1926).
- Re-estimated every month on an expanding window, using filtered probabilities only.
- Two rules serve as comparisons: a 10-month moving average (Faber, 2007) and a negative 24-month market return (Daniel and Moskowitz, 2016).

**Table 1. Sector rotation with statistical regimes (net of costs)**

| Strategy | Annual return | Volatility | Sharpe | Difference vs unconditional (t-stat) |
|---|---|---|---|---|
| Equal-weight sectors | 10.48% | 14.70% | 0.64 | — |
| Unconditional rotation | 9.02% | 17.01% | 0.50 | — |
| HMM regime rotation | 8.85% | 16.84% | 0.49 | −0.19% (−0.30) |
| 10-month moving average | 7.07% | 16.45% | 0.40 | −1.92% (−1.72) |
| 24-month market return | 10.03% | 16.82% | 0.56 | +0.89% (0.72) |
| S&P 500 TR | 11.11% | 14.90% | 0.67 | — |

**Results.** The HMM regime adds nothing to the rotation.

**Why.** The model identifies *volatility* regimes, not economic ones. After a month classified as turbulent, the market was more volatile (24.5% against 14.3% annualised) but earned *higher* returns (20.2% against 11.1%), driven by the rebounds of 2009 and 2020. A volatility signal carries information about risk, not about the relative returns of sectors.

## 4. Study 2 — Economic regimes: the business cycle and monetary policy

**Business-cycle regime.**
- **Data**: real-time vintages of industrial production and non-farm payrolls from the Philadelphia Fed Real-Time Data Set for Macroeconomists (Croushore and Stark, 2001). At each *t* the indicator uses only the data vintage published in month *t*, whose last observation is month *t*−1.
- **Indicator**: the average of the two series' 12-month growth rates, measured relative to their 10-year moving average and smoothed over three months.
- **Phases**: a new phase is adopted only after five consecutive months of confirmation, in the spirit of the Bry and Boschan (1971) censoring rules.
- **How the specification was chosen**: the regime had to satisfy acceptance criteria set in advance and evaluated on the phases alone, never on returns:
  - each phase covers at least 10% of months;
  - each phase lasts at least five months on average;
  - NBER recessions fall in the weakest phase.
- **Final specification**: two phases (above and below trend). A four-phase version (level × direction) failed the coverage criterion, because recoveries covered only 8.2% of months, and is reported as a robustness check.
- **Result of the checks**: the final regime classifies all 20 NBER recession months as below trend and changes only four times after 2005.

**Monetary-policy regime.**
- Expansionary after a cut in the federal funds target, restrictive after a hike (Conover *et al.*, 2008).
- Source: the Federal Reserve Board's record of FOMC decisions since 1990.

**Table 2. Sector rotation with economic regimes (net of costs)**

| Strategy | Annual return | Volatility | Sharpe | Max drawdown | Difference vs unconditional (t-stat) |
|---|---|---|---|---|---|
| Unconditional rotation | 9.02% | 17.01% | 0.50 | −50.7% | — |
| Business-cycle rotation | 8.20% | 15.99% | 0.47 | −52.9% | −0.93% (−0.91) |
| Monetary-policy rotation | 8.99% | 16.31% | 0.51 | −53.1% | −0.15% (−0.10) |
| S&P 500 TR | 11.11% | 14.90% | 0.67 | −51.0% | — |

**Results.**
- **Neither economic regime improves on the unconditional rotation.**
- **Robustness**: none of nine variants changes the conclusion. The most favourable, the four-phase cycle, adds +0.89% a year with a t-statistic of 0.68. The variants change the phase definition, the indicator series, the number of sectors held, the shrinkage and transaction costs.

**Why.** Unlike the HMM, the business-cycle regime does separate market conditions:

| Month after the cycle signal | Market return (annualised) | Market volatility (annualised) |
|---|---|---|
| Below trend | 6.7% | 22.1% |
| Above trend | 13.4% | 12.9% |

The failure lies elsewhere:
- **Sectors lead the cycle.** In below-trend phases the best next-month returns came from cyclical sectors (Basic Materials, Consumer Cyclicals, Technology), not from defensive ones. This is consistent with evidence that industry returns lead the economy (Hong, Torous and Valkanov, 2007).
- **Too few episodes.** The sample contains only three below-trend episodes, so the regime-conditional estimates rest on very few observations. In the 2007 slowdown, for example, the rotation bought cyclical sectors ahead of the 2008 crash.
- **Literature.** The result is in line with Stangl, Jacobsen and Visaltanachoti (2009), who find modest gains from business-cycle sector rotation even with perfect knowledge of the cycle.

## 5. Study 3 — Portfolio construction with a fixed signal

**Design.**
- **Signal**: the stocks selected by the business-cycle rotation, held fixed (the members of the three chosen sectors with a full risk-estimation history, 135 stocks on average).
- **Methods compared** on the same stocks, dates and costs:
  - the reference construction (sectors equal-weighted, cap-weighted within);
  - equal-weighted and cap-weighted;
  - inverse volatility;
  - equal risk contribution (Maillard, Roncalli and Teiletche, 2010);
  - long-only global minimum variance.
- **Risk model**: a Ledoit-Wolf shrinkage covariance on up to 60 months of past returns.

**Table 3. Construction methods on the same signal (net of costs)**

| Method | Annual return | Volatility | Sharpe | Max drawdown | Monthly turnover | Effective no. of stocks | Market beta |
|---|---|---|---|---|---|---|---|
| Sectors EW, cap-weighted within | 7.90% | 15.86% | 0.45 | −52.8% | 12.2% | 42.8 | 0.91 |
| Equal-weighted | 7.99% | 19.01% | 0.41 | −57.5% | 14.9% | 134.9 | 1.12 |
| Cap-weighted | 10.65% | 17.69% | 0.57 | −51.6% | 10.9% | 25.5 | 1.01 |
| Inverse volatility | 8.03% | 17.50% | 0.44 | −54.2% | 15.7% | 124.3 | 1.03 |
| Equal risk contribution | 7.91% | 16.91% | 0.44 | −52.0% | 16.5% | 110.7 | 1.00 |
| Minimum variance | 7.72% | 13.90% | 0.49 | −47.1% | 25.3% | 16.3 | 0.70 |
| S&P 500 TR | 11.11% | 14.90% | 0.67 | −51.0% | — | — | — |

**Results: construction changes risk far more than return.**
- **Returns**: no method's return differs significantly from the equal-weighted portfolio. The largest gap, cap-weighted, is +2.16% a year with a t-statistic of 1.20.
- **Risk**: every method has significantly lower volatility than equal-weighting (Pitman-Morgan test, *p* < 0.01). Volatility falls from 19.0% to 13.9% for minimum variance, which also has the smallest drawdown and a beta of 0.70.

**The costs of minimum variance.**
- **Concentration**: on average 28 stocks held, with an effective number of 16.
- **Turnover**: twice that of the other methods.
- **Ex-ante risk underestimated**: predicted volatility was 8.9% against a realised 13.9%, the familiar in-sample optimism of optimised portfolios.

**Robustness.** A 5% cap on individual weights leaves the conclusions unchanged:
- minimum variance: 7.50% return, 14.1% volatility;
- cap-weighted: 9.95% return.

## 6. Conclusions

1. **The regime does not help sector rotation.** Across three independent definitions (a statistical volatility regime, the real-time business cycle and the monetary-policy stance), conditioning on the regime does not improve sector rotation among S&P 500 constituents, and no rotation beats the index net of costs.
2. **Economic regimes carry information about risk, not about relative sector returns.** The business-cycle regime identifies periods of lower and more volatile market returns, but sector prices move ahead of the real-time data.
3. **Construction matters for risk, not for return.** With the signal held fixed, construction methods produce large and significant differences in risk but no significant differences in return.

## 7. Limitations

- **Sector classification**: the most recent TRBC sector is applied to the whole sample.
- **Short sector history**: sector returns are available only from 2003, so regime-conditional estimates rest on few business cycles (three below-trend episodes).
- **Regime-specification process**: the specification of the business-cycle regime was revised three times on the basis of pre-set criteria applied to the phases. No rotation return had been computed at the time, but the revisions are documented in the decision log.
- **Market series for the HMM**: the French market series is used as a proxy for the investable market.
- **Not tested** (by design): Sharpe-ratio differences between construction methods, and break-even transaction costs.
