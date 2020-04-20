# Contents

### `gtpc_modeled.pkl`

A pickle file (via `cloudpickle`) containing an instance of the `GeneralizedTPC` class defined in [`TPC_params_latitudinal_generalization.ipynb`](./TPC_params_latitudinal_generalization.ipynb), which represents a generalized thermal performance curve (as defined by [Deutsch *et al.* 2008](https://www.pnas.org/content/105/18/6668)), parameterized latitudinally via modeled relationships. The class in this file is instantiated using models derived in [`TPC_params_latitudinal_generalization.ipynb`](./TPC_params_latitudinal_generalization.ipynb). To use: 

```
import cloudpickle
import numpy as np

with open("gtpc_modeled.pkl", 'rb') as f:
    gtpc = cloudpickle.load(f)
    
latitude = 45
_tpc = gtpc.getLatitudinalTPC(latitude)
_topt, _tmin, _tmax = gtpc.getTPCParameters(latitude)
T = np.linspace(10, 40)
T_e = _tpc(T)

```

<!-- ### `general_tpc_latitudinal.pkl`

A pickle file (via `dill`) containing a generalized phytoplankton thermal performance curve paramteterized by latitude via relationships modeled in [`TPC_params_latitudinal_generalization.ipynb`](./TPC_params_latitudinal_generalization.ipynb). **Requires `getLatitudinalParameters` from `latitudinal_parameterizations.pkl`**. To use: 

```
import dill 
import numpy 
import tpc 

with open("latitudinal_parameterizations.pkl", 'rb') as f:
    getLatitudinalParameters = dill.loads(f.read())

with open("general_tpc_latitudinal.pkl", 'rb') as f:
    model_tpc = dill.loads(f.read())

```

### `latitudinal_parameterizations.pkl`

A pickle file (via `dill`) containing a method (`getLatitudinalParameters`) which produces Topt, Tmin, and Tmax for a given latitude via relationships modeled in [`TPC_params_latitudinal_generalization.ipynb`](./TPC_params_latitudinal_generalization.ipynb). To use: 

```
import dill 
import numpy 
import tpc 

with open("latitudinal_parameterizations.pkl", 'rb') as f:
    getLatitudinalParameters = dill.loads(f.read())

```
 -->
### [`TPC_params_latitudinal_generalization.ipynb`](./TPC_params_latitudinal_generalization.ipynb)

Producing latitudinal models of $T_\mathrm{min}$, $T_\mathrm{max}$, and $T_\mathrm{opt}$.

### [`geographic_TPC_patterns.ipynb`](./geographic_TPC_patterns.ipynb)

An initial investigation into the latitudinal patterns present in phytoplankton TPC parameters. 

### [`aquamaps-globtherm.ipynb`](./aquamaps-globtherm.ipynb)

(*Outdated*) An initial investigation into thermal tolerance exceedances for fish species in the Pinsky database. 

### `[phytoplankton-sst.ipynb`](./phytoplankton-sst.ipynb)

(*Outdated*) Proof-of-concept figures for the phytoplankton thermal tolerance idea. 