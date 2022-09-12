
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sensorstrings

<img src="man/figures/hex_sensorstrings.png" width="25%" style="display: block; margin: auto;" />

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/dempsey-cmar/sensorstrings)
[![CodeFactor](https://www.codefactor.io/repository/github/dempsey-cmar/sensorstrings/badge)](https://www.codefactor.io/repository/github/dempsey-cmar/sensorstrings)
[![R build
status](https://github.com/dempsey-cmar/sensorstrings/workflows/R-CMD-check/badge.svg)](https://github.com/dempsey-cmar/sensorstrings/actions)

<!-- badges: end -->

**This package replaces the
[strings](https://github.com/Centre-for-Marine-Applied-Research/strings)
package.**

New features:

-   Cleaner code
-   More straightforward workflow
    -   don’t need to export intermediate “raw” and “trim” files
    -   few arguments to specify in templates
-   Additional functions (e.g., `ss_download_data()`)
-   More helpful `Error` and `Warning` messages

Compile, format, and visualize Water Quality (temperature, dissolved
oxygen, salinity) data measured by different sensors.

## Installation

You can install the development version of `sensorstrings` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dempsey-CMAR/sensorstrings")
```

## Background

The Centre for Marine Applied Research ([CMAR](https://cmar.ca/))
coordinates an extensive [Coastal Monitoring
Program](https://cmar.ca/coastal-monitoring-program/) to measure
[Essential Ocean
Variables](https://www.goosocean.org/index.php?option=com_content&view=article&id=14&Itemid=114)
from around the coast of Nova Scotia, Canada. There are three main
branches of the program: *Water Quality*, *Currents*, and *Waves*.
Processed data for each branch can be viewed and downloaded from several
sources, as outlined in the [CMAR Report & Data Access Cheat
Sheet](https://github.com/Centre-for-Marine-Applied-Research/strings/blob/master/man/figures/README-access-cheatsheet.pdf)
(download for clickable links).

The `strings` package is used to compile, format, and visualize data
from the *Water Quality* branch of the Coastal Monitoring Program.

*Water Quality* data (temperature, dissolved oxygen, and salinity) is
collected using “sensor strings”. Each sensor string is attached to the
seafloor by an anchor and suspended by a sub-surface buoy, with
autonomous sensors attached at various depths (Figure 1). A string
typically includes three sensor models: Hobo, aquaMeasure, and VR2AR
(Table 1). Strings are deployed at a station for several months and data
are measured every 1 minute to 1 hour, depending on the sensor.

<img src="man/figures/figure1.png" width="65%" />

Figure 1: Typical sensor string configuration (not to scale).

<br> <br>

After retrieval, data from each sensor is exported to a separate csv
file using manufacturer-specific software. Each type of sensor generates
a data file with unique columns and header fields, which poses a
significant challenge for compiling all data from a deployment into a
single format for analysis.

The `strings` package was originally built to address this challenge,
and now offers functions to compile, format, convert units, and
visualize sensor string data.

`strings` was developed specifically to streamline CMAR’s workflow, but
is flexible enough that other users can apply it to process data from
the accepted sensors (Table 1). Refer to the vignettes for more detail.

| Sensor                                                                                                                           | Variable(s) Measured          |
|:---------------------------------------------------------------------------------------------------------------------------------|:------------------------------|
| [HOBO Pro V2](https://www.onsetcomp.com/datasheet/U22-001)                                                                       | Temperature                   |
| [HOBO DO](https://www.onsetcomp.com/datasheet/U26-001)                                                                           | Temperature, Dissolved Oxygen |
| [aquaMeasure DOT](https://www.innovasea.com/wp-content/uploads/2021/07/Innovasea-Aquaculture-Intelligence-Spec-Sheet-062221.pdf) | Temperature, Dissolved Oxygen |
| [aquaMeasure SAL](https://www.innovasea.com/wp-content/uploads/2021/07/Innovasea-Aquaculture-Intelligence-Spec-Sheet-062221.pdf) | Temperature, Salinity         |
| [VR2AR](https://www.innovasea.com/wp-content/uploads/2021/06/Innovasea-Fish-Tracking-vr2ar-data-sheet-0621.pdf)                  | Temperature                   |

For more information on *Water Quality* data collection and processing,
visit the [CMAR Water Quality Data Collection & Processing Cheat
Sheet](man/figures/README-workflow-cheatsheet.pdf) (download for
clickable links).
