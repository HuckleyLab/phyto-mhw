# Contents

### `general_tpc_latitudinal.pkl`

A pickle file (via `dill`) containing a generalized phytoplankton thermal performance curve paramteterized by latitude via relationships modeled in [`TPC_params_latitudinal_generalization.ipynb`](./TPC_params_latitudinal_generalization.ipynb). To use: 

```
import dill 
import numpy 
import tpc 

with open("general_tpc_latitudinal.pkl", 'rb') as f:
    model_tpc = dill.loads(f.read())

```

### [`TPC_params_latitudinal_generalization.ipynb`](./TPC_params_latitudinal_generalization.ipynb)

Producing latitudinal models of $T_\mathrm{min}$, $T_\mathrm{max}$, and $T_\mathrm{opt}$.

### [`geographic_TPC_patterns.ipynb`](./geographic_TPC_patterns.ipynb)

An initial investigation into the latitudinal patterns present in phytoplankton TPC parameters. 

### [`aquamaps-globtherm.ipynb`](./aquamaps-globtherm.ipynb)

(*Outdated*) An initial investigation into thermal tolerance exceedances for fish species in the Pinsky database. 

### `[phytoplankton-sst.ipynb`](./phytoplankton-sst.ipynb)

(*Outdated*) Proof-of-concept figures for the phytoplankton thermal tolerance idea. 