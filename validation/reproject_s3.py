import os
import sys

import satpy 

from glob import glob
from cartopy import crs as ccrs

class S3OLCIReproject(object):
    """
    Crude class to resample a sentinel3 OLCI image
    to the mercator projection. Intended to be run in 
    parallel across a directory (e.g. via gnu parallel)
    """
     
    def __init__(self, sen3_dir, reader='olci_l2'):
        self.scene = satpy.Scene(
            glob(sen3_dir+"/*"), 
            reader=reader
        )
        self._dir = sen3_dir
     
    def reproject(self):
        dss_outfiles = []
        dss = ['chl_oc4me', 'chl_nn']
        try:
            self.scene.load(dss)
        except Exception as e:
            print(e)
            return
        
        rs_sc = self.scene.resample(
            self.scene['chl_nn'].area.compute_optimal_bb_area(
                ccrs.Mercator().proj4_params
            )
        )
        
        for i in dss:
            outpath = os.path.join(
                self._dir,
                f"{i}-{self.scene.attrs['start_time']:%Y%m%d}.tif"
            )

            rs_sc.save_dataset(
                i,
                outpath, 
                writer='geotiff',
                compute=True
            )
            dss_outfiles.append(outpath)

        return(dss_outfiles)
        
        


if __name__ == "__main__":
    _rp = S3OLCIReproject(sys.argv[1])
    print(_rp.reproject())
   