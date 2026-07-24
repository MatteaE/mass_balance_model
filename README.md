<img src="icons/icon128.png" width="128">


# Contents

1. [DMBSim 3.0](#intro)

2. [Quick start](#quickstart)

    2.1. [Minimal installation instructions](#installation)

    2.2. [Minimal usage instructions](#usage)

3. [Additional features](#features)

4. [Published works with DMBSim applications](#publications)

5. [References](#references)


# <a name="intro"></a>DMBSim 3.0

DMBSim is a tool to calculate and homogenize glacier surface mass balance from point measurements. It works by running a gridded model of daily accumulation and melt, based on topographical and meteorological data. The main parameters are optimized to match the mass balance from the provided point measurements [[1]](#ref1). DMBSim is suitable for research, teaching, and operational mass balance monitoring. It is currently used to support the glaciological programs of countries in Central Asia: Kazakhstan, Kyrgyzstan, Tajikistan, and Uzbekistan.

**Main features:**

* Calculation of annual glacier-wide mass balance at regular intervals (the hydrological year)

* Leave-one-out validation of the point measurements

* Calculations over multiple years (conventional or reference-surface mass balance, cumulative change)

* Processing of multiple glaciers and full catchments

* Simulation of past conditions, future scenarios, years with no measurements

* High-quality output visualizations


DMBSim is implemented in R (with some C++ routines for performance), is fully cross-platform (Windows / Mac OS / Linux), and is designed to be run within RStudio. Several graphical tools are provided to easily prepare the input data in a consistent format.


<img src="cover_image.png" width="500">


# <a name="quickstart"></a>Quick start

[Tutorial 1](/doc/tutorial1_singleyear/DMBSim_tutorial_1.pdf) will guide you through system setup and simple mass balance calculations.

## <a name="installation"></a>Minimal installation instructions

1. Setup R via [the official installer](https://cran.r-project.org/) or from your package manager.

2. Setup RStudio via [the official installer](https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads).

3. If on Windows, setup RTools via [the official installer](https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html).

4. Download and extract [the DMBSim repository](https://github.com/MatteaE/mass_balance_model/archive/refs/heads/main.zip).

5. Open file `utils/install_packages.R` within RStudio and run it by clicking on the <em>Source</em> button.

That's it!

## <a name="usage"></a>Minimal usage instructions

1. Procure a Digital Elevation Model (DEM) of your area of interest, for example from [OpenTopography](https://portal.opentopography.org/dataCatalog). The DEM should have no gaps (missing data) over that area. The reference system (CRS) does not matter as long as it is defined (for example, in the GeoTiff format).

2. Procure an outline of your glacier of interest, for example from [the Randolph Glacier Inventory](https://www.glims.org/RGI/). You can also draw one yourself with a GIS program.

3. Open file `utils/make_input.R` within RStudio, run it by clicking on the <em>Run App</em> button, and follow the instructions there.

4. Place the folder created by `make_input.R` (with the glacier name) inside the `input` folder.

5. Prepare the text file with the meteorological time series and place it in folder `weather` inside the `input` folder. The tools under `utils/aggregate_meteo_data.R` and `utils/meteo_concatenate.R` can help preparing the data in the correct file format.

6. Prepare the text file with the mass balance measurements and place it in folder `massbalance` inside the `input` folder. See [the example from Tutorial 1](doc/tutorial1_singleyear/tutorial_1_input/yakarcha/massbalance) for the correct file format.

7. In RStudio, open file `set_params.R` and configure the model with the appropriate parameters - at least:
    * Glacier name
    
    * Input file names
    
    * Reference altitude of the meteorological series
    
    * Selection of years to be modeled

8. In RStudio, open file `main.R` and launch the calculations by clicking on the <em>Source</em> button.

For more advanced usage, check out [Tutorial 1](/doc/tutorial1_singleyear/DMBSim_tutorial_1.pdf) and [Tutorial 2](/doc/tutorial2_multiyear/DMBSim_tutorial_2.pdf).



# <a name="features"></a>Additional features

* **Simple usage, suitable as a teaching tool:**

    * In-depth [documentation](/doc)
    
    * Graphical tools to assist the preparation of input data

    * Comprehensive handling of exceptions, errors, and unexpected inputs
    
    * Detailed logging in RStudio console, log files, and modal dialogs

* **Accumulation routines:**

    * Spatial snow distribution based on topographic parameters, winter measurements, user-supplied files, previous model runs, or a combination of them
    
    * Snow redistribution using a process-based avalanche model [[2]](#ref2)
    
* **Ablation routines:**

    * Melt calculation based on an Enhanced Temperature Index model (air temperature and potential solar radiation) [[3]](#ref3)
    
    * Individual melt factors for snow, firn, clean ice, and debris-covered ice
    
    * Optional spatially variable albedo
    
* **Calibration to reference mass balance:**

    * Support for multiple measurements throughout the year
    
    * Optional clustering of measurements to improve spatial representativity

    * Support for accumulation measurements with no reference surface (unknown starting date)
    
* **Output:**

    * Publication-ready vector plots (maps, time series, profile distributions)
    
    * GeoTiff maps and CSV files with all calculated results
    
* **For developers:**

    * Modular, well-commented, cross-platform codebase
    
    * Custom multi-level logging engine
        
* **Minimal input data:**

    * One or more Digital Elevation Models (DEMs) covering the area of interest
    
    * One or more vector outlines of the glacier(s) of interest
    
    * A set of gridded files of potential solar radiation covering the area of interest. A tool is provided to calculate these from the DEMs
    
    * A daily time series of air temperature and total precipitation. A tool is provided to assemble the series from AWS measurements
    
    * A set of point measurements of mass balance. There is no constraint on the spatio-temporal distribution of those measurements. The tool will run (uncalibrated) even if the measurements are missing
    
* **Optional input data:**

    * One or more vector outlines of debris-covered regions
    
    * One or more maps of snow distribution to be used in the calculations
    
    * One or more files of year-specific parameters
    
    * A set of points where daily mass balance should be written to CSV files

    
    
# <a name="publications"></a>Published works with DMBSim applications

1. Azisov E., Barandun M., Saks T., Mattea E., Hoelzle M., Kim D., Bakirov K., Usubaliev R., and Kenzhebaev R. (2025). Reconstruction of the mass balance and dynamics of glaciers in the Orto-Koy-Suu basin (Northern Tien Shan). International Mountain Conference 2025, Innsbruck, Austria.

2. Belekov S., Hyvarinen A., Barandun M., Mattea E., Akmatov R., Warley J., Manninen H., Svensson J., and Kenzhebaev R. (2025). Dynamics of Mass Balance and Area Change of the Turgen-Aksuu Glacier (1970-2024). International Mountain Conference 2025, Innsbruck, Austria

3. Kabutov K., Amirzoda O. H., Sheralizoda N. S., Mattea E., Saks T., and Abdulloeva P. N. (2024). Modeling the mass balance of glacier №139 of the Eastern Pamir lake Karakul basin using the SMB model. Water Resources, Energy and Ecology, 4, 41–53.

4. Kenzhebaev R., Barandun M., Mattea E., Usubaliev R., Azisov E., Saks T., Mandychev A., and Hoelzle M. (2022). Mass balance and area change of glacier No. 354 and Batysh Sook (№419) of the Inner Tien Shan, Kyrgyzstan. Conference «Cryosphere and related hazards in High Mountain Asia in a changing climate», Almaty, Kazakhstan.

5. Kenzhebaev R., Barandun M., Mattea E., Azisov E., Esenaman Uulu M., Satarov S., Pohl E., Saks T., Hoelzle M., and Usubaliev R. (2025). Changes in glacier area and the glacial component of runoff in the upper reaches of the Naryn River. International Mountain Conference 2025, Innsbruck, Austria.

6. Navruzshoev H., Kayumov A., Kabutov K., Saks T., Barandun M., Mattea E., Smirnov A., and Hoelzle M. (2023). Basic research on glacier No. 457 of the Gunt river basin. Conference «Impact of climate change on the state of glaciers of the Republic of Tajikistan and protection of glaciers», Dushanbe, Tajikistan.

7. Navruzshoev H., Kayumov A., Saks T., Barandun M., Kabutov K., Mattea E., Smirnov A., and Hoelzle M. (2023). Glacier mass balance in the Gunt river basin, Pamir, Tajikistan. 21st Swiss Geoscience Meeting, Mendrisio, Switzerland.

8. Navruzshoev H., Saks T., Sheralizoda N., Kabutov K., Barandun M., Mattea E., and Hoelzle M. (2025). Glacier mass balance in the Gunt river basin, Pamir, Tajikistan. International Mountain Conference 2025, Innsbruck, Austria.

9. Severskiy I., Kapitsa V., Kassatkin N., Usmanova Z., Saks T., Mattea E., and Kissebayev D. (2025). Comparison of the modelled and measured mass balance of the Central Tuyuksu glacier, northern slope of Ili-Alatau, Journal of Geography and Environmental Management, 75, 4, 16–31, doi: 10.26577/JGEM.2024.v75.i4.2.

10. Umirzakov G., Kholtojiyeva O., Eshmuratov D., Gulmurzayeva B., Mattea E., and Barandun M. (2025). Mass balance modeling of the Barkrak glacier. (pp. 130–135). International Scientific and Practical Conference «Innovative methods for monitoring mountain glaciers under climate change and current challenges in glaciology». Tashkent, Uzbekistan.



# <a name="references"></a>References

1. <a id="ref1"></a> Huss M., Bauder A., and Funk M. (2009). Homogenization of long-term mass-balance time series. Annals of Glaciology 50(50):198-206. [doi:10.3189/172756409787769627](https://doi.org/10.3189/172756409787769627)

2. <a id="ref2"></a> Gruber S. (2007). A mass-conserving fast algorithm to parameterize gravitational transport and deposition using digital elevation models. Water Resources Research 43, W06412. [doi:10.1029/2006WR004868](https://doi.org/10.1029/2006WR004868)

3. <a id="ref3"></a> Hock, R. (2003). Temperature index melt modelling in mountain areas. Journal of Hydrology 282, 104–115. [doi:10.1016/S0022-1694(03)00257-9](https://doi.org/10.1016/S0022-1694\(03\)00257-9)
