from pathlib import Path
from itertools import combinations
import json, hashlib
import numpy as np
import pandas as pd

root = Path(__file__).resolve().parent.parent
folder = root
out = Path(__file__).resolve().parent
files = {'Crypto': 'CCI.csv', 'Stocks': 'SP500.xlsx', 'Commodity': 'BBG Commodity.csv', 'FX': 'USD Index.xlsx', 'Bond': 'US AGG.xlsx'}
frames = []
sources = {}
for name, filename in files.items():
    p = folder / filename
    frame = pd.read_excel(p, index_col='Date', parse_dates=True) if p.suffix == '.xlsx' else pd.read_csv(p, index_col='Date', parse_dates=True)
    frame.columns = [name]
    frames.append(frame)
    sources[filename] = {'sha256': hashlib.sha256(p.read_bytes()).hexdigest(), 'first_date': str(frame.index.min()), 'last_date': str(frame.index.max())}
data = pd.concat(frames, axis=1).interpolate(method='time', axis=0).dropna().resample('W').last()
rf = 0.0157
results = []
frontiers = []
for period, prices in [('Old', data[data.index < '2018-08-15']), ('Extended', data)]:
    returns = np.log(prices / prices.shift(1)).dropna()
    mu_all = (np.exp(returns.mean()) ** 52 - 1)
    cov_all = returns.cov() * 52
    for inclusion in [True, False]:
        cols = list(mu_all.index) if inclusion else [c for c in mu_all.index if c != 'Crypto']
        mu = mu_all[cols].to_numpy()
        cov = cov_all.loc[cols, cols].to_numpy()
        excess = mu - rf
        candidates = []
        for size in range(1, len(cols) + 1):
            for subset in combinations(range(len(cols)), size):
                ids = list(subset)
                direction = np.linalg.solve(cov[np.ix_(ids, ids)], excess[ids])
                if direction.sum() <= 0:
                    continue
                active = direction / direction.sum()
                if np.min(active) < -1e-10:
                    continue
                w = np.zeros(len(cols)); w[ids] = active
                ret = w @ mu; vol = np.sqrt(w @ cov @ w)
                candidates.append(((ret-rf)/vol, ret, vol, w))
        sharpe, ret, vol, w = max(candidates, key=lambda item: item[0])
        # Independent first-order check of the long-only maximum-Sharpe solution.
        gradient = excess / vol - (w @ excess) * (cov @ w) / vol**3
        assert abs(w.sum()-1) < 1e-10 and w.min() >= -1e-10
        assert np.max(np.abs(gradient[w > 1e-8])) < 1e-8
        assert np.all(gradient[w <= 1e-8] <= 1e-8)
        result = {'period': period, 'including_cc': inclusion, 'first_price_week': str(prices.index.min().date()), 'last_price_week': str(prices.index.max().date()), 'first_return_week': str(returns.index.min().date()), 'return_observations': len(returns), 'rf': rf, 'return': ret, 'std': vol, 'sharpe': sharpe, 'weights': dict(zip(cols,w)), 'optimality_check': 'passed'}
        results.append(result)
        # Enumerate feasible active sets for each target-return minimum-variance portfolio.
        for target in np.linspace(mu.min(),mu.max(),1000):
            feasible = []
            for size in range(1,len(cols)+1):
                for subset in combinations(range(len(cols)),size):
                    ids=list(subset); sub_mu=mu[ids]; sub_cov=cov[np.ix_(ids,ids)]
                    if size == 1:
                        if abs(sub_mu[0]-target)>1e-10: continue
                        active=np.ones(1)
                    else:
                        constraints=np.vstack([np.ones(size),sub_mu]); rhs=np.array([1,target])
                        inv_constraints=np.linalg.solve(sub_cov,constraints.T)
                        active=inv_constraints @ np.linalg.solve(constraints @ inv_constraints,rhs)
                    if active.min() < -1e-8: continue
                    fw=np.zeros(len(cols)); fw[ids]=active
                    feasible.append((fw @ cov @ fw,fw))
            variance,fw=min(feasible,key=lambda item:item[0])
            frontiers.append({'period':period,'including_cc':inclusion,'return':float(fw@mu),'std':float(np.sqrt(variance)),'sharpe':float((fw@mu-rf)/np.sqrt(variance))})
payload={'method':'Notebook data transformations and annualization; exact active-set enumeration for long-only portfolio optimization; no source changes.', 'sources':sources,'results':results}
(out/'results.json').write_text(json.dumps(payload,indent=2)+'\n')
pd.DataFrame(frontiers).to_csv(out/'frontiers.csv',index=False)
print(json.dumps(payload,indent=2))
