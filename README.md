# DMBSim 2.0: calculate glacier mass balance from point measurements

The core idea is to run a gridded mass balance model simulating distributed accumulation and (enhanced temperature index) melt, based on a daily meteorological time series. The model parameters are optimized towards the best fit with a set of point measurements of mass balance. Several tools are provided to prepare the input data in a consistent format. Data ingestion, calculations and plotting are implemented in R.

<img src="cover_image.png" width="500">

Some parametrizations are derived from the mass balance extrapolation tool of Huss (2009) – doi:10.3189/172756409787769627
