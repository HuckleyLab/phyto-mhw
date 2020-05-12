"""
Defines a class OISST to load OISST SST dataset from cloud storage.

Parameterized with defaults using OISST on Google Cloud Storage.

"""

import xarray as xr
import gcsfs

DEFAULT_GCP_PROJECT_ID = '170771369993'
DEFAULT_OISST = 'oisst/oisst.zarr'


class OISST(object):

    """
    Provides interface to cloud OISST data as dask/xarray Dataset.
    """
    def __init__(self,
                 gcp_token_path,
                 gcp_project_id=None,
                 oisst_bucket=None,
                 reshape_coords=True):
        self.gcp_token_path = gcp_token_path
        self.gcp_project = (gcp_project_id
                            if gcp_project_id else DEFAULT_GCP_PROJECT_ID)
        self.oisst_bucket = (oisst_bucket
                             if oisst_bucket else DEFAULT_OISST)
        self.reshape_coords = reshape_coords

    def load(self):
        """Load OISST and return as Xarray Dataset"""
        fs = gcsfs.GCSFileSystem(
            project=self.gcp_project,
            token=self.gcp_token_path
        )
        oisst = xr.open_zarr(fs.get_mapper(self.oisst_bucket))
        if self.reshape_coords:
            oisst = oisst.assign_coords(
                lon=(((oisst.lon + 180) % 360) - 180)
            ).sortby('lon')

        return oisst
