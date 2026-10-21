from pathlib import Path
import json, os
import numpy as np
import pandas as pd
from scipy.optimize import minimize

root=Path(__file__).resolve().parent.parent
out=Path(__file__).resolve().parent
os.chdir(root)
results=[]
for name, function_cell in [('20250214 Asset Allocation Weekly Old Period.ipynb',12),('20250214 Asset Allocation Weekly.ipynb',13)]:
    nb=json.loads(Path(name).read_text())
    for i in [1,2,4,6,function_cell]:
        exec(''.join(nb['cells'][i]['source']))
    for inclusion in [True,False]:
        cols=list(returns.columns) if inclusion else [c for c in returns.columns if c!='Crypto']
        mu=avg_return[cols]; cov=returns[cols].cov()*freq
        answer=tangency_portfolio(mu,cov,rf)
        results.append({'notebook':name,'including_cc':inclusion,'return':answer[0],'std':answer[1],'sharpe':answer[2],'weights':dict(zip(cols,answer[3:]))})
(out/'original_solver_results.json').write_text(json.dumps(results,indent=2)+'\n')
print(json.dumps(results,indent=2))
