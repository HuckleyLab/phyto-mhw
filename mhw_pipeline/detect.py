"""
Provides MHWDetector class for the detection of Marine Heatwaves in OISST data.

Can currently produce detections at individual lat/lon points only.

Uses Eric Oliver's Python implementation (https://github.com/ecjoliver/marineHeatWaves)

Ref: Hobday, A.J. et al. (2016), A hierarchical approach to defining marine
heatwaves, Progress in Oceanography, 141, pp. 227-238,
doi: 10.1016/j.pocean.2015.12.014

"""

import sys

import numpy as np

# choose parameters from marineHeatWaves to save.
SAVED_DETECTION_PARAMS = [
    'intensity_max',
    'intensity_cumulative',
    'intensity_var',
    'intensity_mean',
    'rate_onset',
    'rate_decline',
    'index_start',
    'index_end',
    'index_peak',
    'duration'
]

# mapping from parameter index to dimension name for xr Dataset
SAVED_DETECTION_PARAMS_XR_DIMS = {
        **{
            i : SAVED_PARAMS[i]
            for i in range(len(SAVED_PARAMS))
        },
        **{
            len(SAVED_PARAMS): 'mhw',
            len(SAVED_PARAMS) + 1 : 'clim_thresh',
            len(SAVED_PARAMS) + 2 : 'clim_seas'
        }
}

class MHWDetector(object):
    """Detects marine heatwaves from OISST data available as
    xarray Dataset"""

    def _load_mhw_detection_code(self):
        """Attempts to import marineHeatWaves package from local path"""
        if self.mhw_detection_codepath:
            sys.path.append(self.mhw_detection_codepath)
            from marineHeatWaves import detect
            self.detect_mhws = detect

    def __init__(self, sst, mhw_detect_codepath, **kwargs):
        self.sst = oisst
        self.mhw_detection_codepath = mhw_detect_codepath
        self.detect_kwargs = kwargs


    def get_nearest_mhw_detections(self, lat, lon, tolerance=0.25):
        """
        Get MHW detections (all default arguments now) for grid cell
        closest to lat/lon (+/- tolerance).

        :param lat - latitude
        :param lon - longitude
        :param tolerance - tolerance to use with xr sel method 'nearest'


        Returns Dataset with Dask arrays.

        """
        these_sst = self.oisst.sel(lat=lat, lon=lon, method='nearest', tolerance=tolerance)
        dets = xr.apply_ufunc(
            self.mhw_1d,
            these_sst.sst.chunk({'time': -1}),
            these_sst.time,
            input_core_dims = [['time'], ['time']],
            output_core_dims=[["param","time"]],
            output_dtypes=['float64'],
            dask='parallelized',
            output_sizes={"param": len(SAVED_DETECTION_PARAMS) + 3}, # + 3 for binary MHW detection parameter and climatology
            vectorize=True
        )
        return(
            dets.to_dataset(
                dim='param'
            ).rename_vars(SAVED_DETECTION_PARAMS_XR_DIMS)
        )

    def _mhw_1d(self, temps, time):
        """
        1D (time) MHW detection routine. Returns np.array
        of size NxD where D == len(SAVED_DETECTION_PARAMS).

        :param temps
            list of temperature values (N)
        :param time
            list of time values (N)

        Returns numpy array.

        """
        SAVED_DETECTION_PARAMS_loc = SAVED_DETECTION_PARAMS.copy()
        if(np.isnan(temps).any()): return np.zeros((len(SAVED_DETECTION_PARAMS_loc) + 3, time.shape[0]))

        ordinals =  np.array([pd.Timestamp(t).toordinal() for t in time])
        dets = mh.detect(ordinals, temps.copy())
        events = dets[0]['n_events']
        del dets[0]['n_events']

        arrays = [
            np.zeros_like(time, dtype='float64')
            for _ in range(len(SAVED_DETECTION_PARAMS_loc))
        ]
        arrays.append(np.zeros_like(time, dtype='int'))

        for event_i in range(events):
            start_date = dets[0]['index_start'][event_i]
            end_date = dets[0]['index_end'][event_i]

            # set binary param
            arrays[-1][start_date:end_date] = event_i
            # set all params
            for _i, param in enumerate(SAVED_DETECTION_PARAMS_loc):
                param_data = dets[0][param][event_i]
                arrays[_i][start_date:end_date] = param_data


        clim_thresh = dets[1]['thresh']
        clim_seas = dets[1]['seas']

        return np.array(
            arrays + [clim_thresh, clim_seas]
        )
