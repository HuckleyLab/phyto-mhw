# OISST MHW Detection

The code in this directory is designed to do the following tasks:

* Download the entire OISST record and convert it to a `zarr` format via `xarray` and `dask`: [`./oi-sst-to-zarr.ipynb`](./oi-sst-to-zarr.ipynb)
* Detect MHW events during this record for specified regions: [`./detect.py`](./detect.py)

These files are relatively documented, please find the majoryti of the details located there. Note the Requirements in the main repository README are particularly relevant here.
