# Seasonal and latitudinal effects of marine heatwaves on phytoplankton
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/HuckleyLab/phyto-mhw/master)
![GitHub](https://img.shields.io/github/license/HuckleyLab/phyto-mhw?style=flat)

**Tony Cannistra**, Lauren Buckley; University of Washington Department of Biology

>**New, May 2020**: Check out a presentation ([slides](./presentations/Cannistra_phytomhw_esci_seminar_20May.pdf), [video]()) Tony gave on this work as an eScience Institute Community Seminar at the University of Washington.  

## Project Abstract

Marine heatwave events (MHWs), defined as discrete, anomalously warm periods of ocean temperature, are projected to occur more frequently and with greater intensity in the coming decades as a result of a warming climate. Recent significant MHWs have been linked to severe ecological consequences, including population declines due to increased mortality and changes in community composition. However, the geographic and taxonomic extent of these prior studies has been limited. In this study we examine the effects of MHW events on phytoplankton fitness across the global ocean via a 38-year satellite record of sea surface temperature in an effort to elucidate global patterns of ecological impacts from MHWs. We use a robust geographically distributed data set of thermal reaction norms of phytoplankton growth rate to link MHW temperature anomalies to fitness across a broad latitudinal and taxonomic range, and assess the size and directionality of these effects via a statistical approach. We find that MHW events cause both positive and negative fitness consequences in phytoplankton with significant seasonal and latitudinal patterns. In addition, the unique biogeochemical and trophic context of phytoplankton may allow for the framing of phytoplankton consequences as context for whole-ecosystem responses to these climatological events. These findings serve as the first globally-distributed analysis of ecological impacts from MHW events, and represent both a framework for forecasting global patterns and as the foundation for further investigation into the ecological responses to future MHW events.



## Repository Structure

⚠️ Note that this is code as a part of an ongoing resarch project, and thus can change at any time. Please examine our [LICENSE](./LICENSE) for more information.

You can interactively explore this directory, and run some of the analyses, on [Binder](https://mybinder.org/v2/gh/HuckleyLab/phyto-mhw/master). Please note some technical details in the Requirements section.

| Folder | Purpose |
| ----   | ------ |
| [`./mhw_pipeline`](./mhw_pipeline) | Development of Python module for analysis of OISST and MHW data via xarray |
| [`./analysis`](./analysis) | Python exploratory and analytic notebooks for MHW/Phytoplankton thermal response analyses. Contains all analyses and figures.  Look here for most of the project code. 👀  |
| [`./validation`](./validation) | Notebooks + code for processing Sentinel-3 OLCI imagery to validate MHW phytoplankton thermal responses. |
| [`./data`](./data) | Phytoplankton physiology data. |
| [`./presentations`](./presentations) | Slides from presentations about this work. |
| [`./manuscript`](./manuscript) | A work in progress academic manuscript for this project. (*Current manuscript version not published on GitHub yet; in review.*) |

## Requirements

This code is written in Python 3 and R, via a combination of modules and Juypter notebooks. Due to the large nature of the OISST data, significant computational and storage resources are required to efficiently reproduce these results. We offer an always-updating [Binder](https://mybinder.org/v2/gh/HuckleyLab/phyto-mhw/master) version of this repository for analyses, but the computational resource provide by MyBinder.org are insufficient for some of the "heavier" analyses here.

Here's what you need to run the code in this analysis:
  * A powerful, high-memory computer. We used an Amazon Web Services Elastic Compute Cloud `m5a.8xlarge` instance (32 cores, 128 GB of RAM)
  * A correctly-configured environment. We provide [`environment.yml`](environment.yml) for you to use in creating a Conda environment for this analysis. **TODO:** Add R environment.
  * A working copy of Eric Oliver's marineHeatWaves package in your home directory: https://github.com/ecjoliver/marineHeatWaves
  * Comfort on a UNIX command line.


Once you've got those things running, we recommend using JupyterLab (e.g. `jupyter lab --ip 0.0.0.0`) to access the notebooks and code.



<!-- ## Contributors

* Tony Cannistra (@acannistra), project lead.
<!-- * Isaac Caruso (@icaruso21), undergraduate intern. -->
