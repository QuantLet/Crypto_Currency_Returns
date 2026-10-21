# Portfolio verification

These scripts check the weekly portfolio notebooks in the parent directory without modifying their code, saved outputs, or figures.

## Run

Install numpy, pandas, scipy, and openpyxl, then run:

```bash
python verification/check_original_solver.py
python verification/check_portfolio.py
```

Place the five input files used by the notebooks in the parent directory: `CCI.csv`, `SP500.xlsx`, `BBG Commodity.csv`, `USD Index.xlsx`, and `US AGG.xlsx`. These source price files are not included in this verification upload. The input hashes and coverage are recorded in `results.json`.

- `check_original_solver.py` executes the data-processing, annualization, and tangency-optimization cells from the two existing weekly notebooks. It saves `original_solver_results.json`.
- `check_portfolio.py` independently checks the long-only optimum by enumerating active asset sets, verifies first-order optimality conditions, and computes 1,000 minimum-variance target-return points per universe and period. It saves `results.json` and `frontiers.csv`. The frontier CSV includes the lower minimum-variance branch; only the branch at or above the global minimum-variance portfolio is efficient.

## Verified results

| Sample | Including crypto: Sharpe | Excluding crypto: Sharpe | Weekly return observations |
|---|---:|---:|---:|
| 2013-04-28 to 2018-08-12 | 1.633434 | 1.237064 | 276 |
| 2013-04-28 to 2025-02-16 | 1.232535 | 0.919058 | 616 |

Dates above are weekly price labels. Both samples start their return series on 2013-05-05. The scripts preserve the original time interpolation, weekly resampling, annualized geometric asset-return estimates, annualized log-return covariance, fixed annual risk-free rate of 0.0157, and long-only/full-investment constraints. The final weekly label does not imply that every asset has a fresh price on that date. No out-of-sample validation is claimed.

## Original-paper values are a separate benchmark

The manuscript's earlier-period values (including/excluding crypto: returns 0.069/0.043, standard deviations 0.028/0.021, and Sharpe ratios 2.10/1.58) match Table 5, page 178, of Ankenbrand and Bieri (2018), DOI: [10.21511/imfi.15(3).2018.14](https://doi.org/10.21511/imfi.15(3).2018.14). They are reported original-study values and must not be replaced by this local rerun merely because the numbers differ. The local rerun is not a verified reproduction of the original study's exact data and settings.

The extended-sample manuscript values match the original notebook solver at the manuscript's displayed precision. Its data run through February 2025, not January 2025. The independent solver gives near-identical Sharpe ratios (differences below 5e-7); small weight differences reflect numerical optimization tolerance. Existing figure files were not regenerated or pixel-verified in this check.
