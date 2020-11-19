# Getting started

## Introduction

Welcome to the CNWS documentation.

The code is written in Free Pascal and can be compiled with [Lazarus](https://www.lazarus-ide.org/).
Since january 2017 the code history is stored in [git](https://git.fluves.net/fluves/cn_ws.git).

Alternatively, if you're working with Linux, it is possible to install the latest build of the master branch with

```
sudo apt-install ...
```

## Use the model

### Using the console

In a terminal the model executable can be run with

```
CN_WSmodel configfile.ini
```

The .ini-file contains all user choices, variables and other modelinput.
There are some mandatory sections in this file:
- Working directories
- User choices
- Variables
- Output maps
- Files

For a complete description of the .ini-file we redirect to the .ini-reference section.

### GUI version

A gui version exists but is not maintained since january 2017.

(inireference)=

## `.ini` reference

The `.ini` file contains several sections. Here we describe every section and every possible key-word in these sections. Every part starts with a valid example of the described section.

### Working directories

```
[working directories]
input directory = C:/CNWS/modelinput
output directory = C:/CNWS/modeloutput
```

The section working directories contains only two keys:
- `input directory`
- `output directory`

The value of these keys is the absolute path where all inputfiles (including the ini-file!) are stored and where the
results (the modeloutput) is written.
If the output directory does not exist, it is created by the model. The input directory is mandatory.

### User choices

```
[user choices]
simplified model version = 1
output per river segment = 1
manual outlet selection = 0
use r factor = 1
create ktc map = 0
calculate tillage erosion = 0
estimate clay content = 0
include ditches = 0
include dams = 0
include sewers = 1
include buffers = 1
force routing = 0
river routing = 1
l model = Desmet1996_Vanoost2003
s model = Nearing1997
adjusted slope = 1
buffer reduce area = 1
max kernel = 50
max kernel river = 100
```

CN-WS provides the following user choices:

__model choices__

- `Only Routing` (bool, default false), run the model but only the routing-part. The RUSLE-calculation and
sedimenttransport calculation is ignored.
- `Simplified model version` (bool, default false), run only the WS-part of the model, the CN-part will be
ignored during the model run
- `Calculate tillage erosion` (bool, default false), use the tillage erosion model
- `L model` (string, default ‘Desmet1996_Vanoost2003’), change the L-model. 'Desmet1996_McCool' or
‘Desmet1996_Vanoost2003’
- `S model` (string, default ‘Nearing1997’), change the S-model: 'Nearing1997' or 'Desmet1996'

__routing options__

- `Include tillage direction` (bool, default false), take tillage direction into account (not tested, not documented)
- `Include buffers` (bool, default false), take buffer basins into account.
- `Include ditches` (bool, default false), take ditches into account.
- `Include dams` (bool, default false), take dams into account.
- `Force Routing` (bool, default false), do not use the routing algorithm at some places, but use the routing defined by
the user
- `River Routing` (bool, default false), use the river routing defined by the user in stead of a routing based on the dtm
- `Max kernel` (int, default 50), if the routing algorithm is stuck in a pixel with no lower pixels around, look for a
lower pixel in a radius of maximum x pixels wide
- `Max kernel river` (int, default 100), if the routing algorithm is stuck in a river pixel with no lower (river) pixels
around, look for a lower river pixel in a radius of maximum x pixels wide
- `Manual outlet selection` (bool, default false), use the outlet defined by the user, if false, the lowest pixel of
the model domain will be considered as the outlet

__other__

- `Include sewers` (bool, default false), take sewers into account.
- `Output per river segment` (bool, default false), split sediment output per river segment
- `Create ktc map` (bool, default true), create a raster with transport capacities based on the C-factor map, if false
the user will have to provide a ktc map
- `Create ktil map` (bool, default false), create a raster with tillage factors based on the C-factor map, if false
the user will have to provide a ktil map
- `Adjusted Slope` (bool, default false)
- `Buffer reduce Area` (bool, default false), reduce the upstream area downstream of a buffer with the trapping
efficiency of the considered buffer
- `Convert output` (bool, default false)
- `Estimate clay content` (bool, default false)
- `Calibrate` (bool, default false): run the model in calibration mode.

More information and background on these options is provided in the [user choices page](./user_choices.md).

### Files

```{admonition} Remember
All inputfiles are idrisi rasters and must have the same spatial extent (number of rows and columns and resolution)
```

```
[files]
dtm filename = dtm.rst
parcel filename = landuse.rst
p factor map filename = p.rst
c factor map filename = c.rst
k factor filename = k.rst
ktc map filename = ktc.rst
sewer map filename = sewer.rst
buffer map filename = buffer.rst
river segment filename = riversegment.rst
river routing filename = riverrouting.rst
adjectant segments = adj_edges.txt
upstream segments = up_edges.txt
```

Following inputfiles are mandatory:
- `dtm filename` (float): digital elevation model, elevations in meter

- `parcel filename` (integer): landuse map. This raster contains following values:
	- -6: grasstrips
	- -5: water surfaces
	- -4: pasture
	- -3: forest
	- -2: infrastructure (roads, buildings, railways,...)
	- -1: river
	- 0: nodata/outside model domain
	- 1, 2,...n: arable land. Every parcel has its unique id (maximum value is 32577).

All other inputfiles are only required when using some model options.

- `Sewer map filename` (float): raster with all pixels with an entrance to the sewer system. The value (0 - 1) of every
pixel represents the relative amount of transported sediment that is trapped in this sewer pixel. This file is only
mandatory if `Include sewers = 1`
- `Tillage direction filename`: only mandatory if `Include tillage direction = 1`
- `Oriented roughness filename`: only mandatory if `Include tillage direction = 1`
- `Buffer map filename`: raster with all pixels containing retention ponds or buffer basins. Every buffer has a unique id.
If the buffer is larger than one pixel, the outletpixel of the buffer contains the value of the id. The other pixels of
same buffer get a 'buffer extension id' - idealy (id * 100). The buffer map is only mandatory if `include buffers = 1`
- `Ditch map filename`:
- `Dam map filename`:
- `P factor map filename`: raster with P-factor values (0 -1) for every pixel in the model domain.
- `River segment filename`: All pixels belonging to the same river segment get the same id in this raster.
Only mandatory if `Output per river segment = 1`.
- `Outlet map filename`: raster with outlet pixels defined by the user. Every pixel contains a unique id. Only mandatory
if `Manual outlet selection = 1`
- `CN map filename`: raster with CN values (0-100) for every pixel in the model domain. Only mandatory if
`Simplified model version = 0`
- `Rainfall filename`:
- `K factor filename` (integer): raster with K-factor values (kg.h)/(MJ.mm) for every pixel in the model domain
- `C factor map filename` (float): raster with C-factor values (0 - 1) for every pixel in the model domain.
- `ktc map filename` (float): raster with ktc values of every pixel in the model domain. Only mandatory if
`Create ktc map = 0`
- `ktil map filename` (float): raster with transportcoëficients for tillage erosion (kg/m/year) of every pixel in the
model domain. Only mandatory if `Create ktil map = 0`

When `River Routing = 1` the user defines the routing in the river pixels by setting following three files.
- `adjectant segments`:
- `upstream segments`:
- `river routing filename`:

When using the long term version of the model (i.e. to calculate multiple events over different years), some inputfiles
can be given for multiple years/seasons:
- `Parcel filename 1` and `Parcel filename 2` in stead of `parcel filename`
- `CN map spring 1`, `CN map spring 2`, `CN map summer 1`, `CN map summer 2`, `CN map fall 1`, `CN map fall 2`,
`CN map winter 1` and  `CN map winter 2` in stead of `CN map filename`.
- `C factor map spring 1`, `C factor map spring 2`, `C factor map summer 1`, `C factor map summer 2`,
`C factor map fall 1`, `C factor map fall 2`, `C factor map winter 1` and  `C factor map winter 2` in stead of
`C factor map filename`.

### Variables

Next to the inputfiles, some variables need to be defined in the ini-file.

```
[Variables]
parcel connectivity cropland = 90
parcel connectivity forest = 30
parcel trapping efficiency cropland = 0
parcel trapping efficiency forest = 75
parcel trapping efficiency pasture = 75
ls correction = 1.0
bulk density = 1350
r factor = 1250
endtime model = 0
sewer exit = 100
number of buffers = 31
```

WS-specific
- `Bulk density` (integer) kg/m³, used to convert volumes of eroded soil to masses.
- `R factor` (float): MJ.mm.ha-1.h-1.jaar-1, only mandatory if `Simplified model version = 1`
- `LS correction` (float, default 1) when using a resolution different from 20-25 m, one can correct the LS-factor for
resolution effects (a detailed explanation is given in ...). The LS-factor is divided by the correction factor.
- `Number of buffers` (integer), the number of buffers included in the model. Only mandatory if `Include buffers = 1`
- `Number of Forced Routing` (integer), the number of places where the user wants to force the routing in a certain
direction. Only mandatory if `Force Routing = 1`.
- `ktc low` (float): transport capacity for low erosive landuse, only mandatory if `Create ktc map = 1`
- `ktc high` (float): transport capacity for highly erosive landuse, only mandatory if `Create ktc map = 1`
- `ktc limit` (float): pixels with a C-factor higher than this value get ktc high, below this value ktc low.
Only mandatory if `Create ktc map = 1` or `Calibrate = 1`
- `ktil default` (float)
- `ktil threshold` (float)
- `Parcel connectivity cropland` (integer)
- `Parcel connectivity forest` (integer)
- `Parcel trapping efficiency cropland` (integer)
- `Parcel trapping efficiency pasture` (integer)
- `Parcel trapping efficiency forest` (integer)
- `Clay content parent material` (float)

CN-specific:
- `5-day antecendent rainfall` (float): in mm, proxy for initial soil moisture content
- `Stream velocity` (float), m/s
- `Alpha` (float): calibration factor for CN model, dimensionless
- `Beta` (float): calibration factor for CN model, dimensionless
- `Desired timestep for model` (integer)
- `Final timestep output` (integer)
- `Endtime model` (integer)

### Buffers

The .ini-file needs a separate section for every buffer in the buffer map. Of course, this is only mandatory if
`include buffers = 1`. The section is numbered by the id of the buffer in the buffermap.

```
[Buffer 1]
volume = 329.0
height dam = 0.377304780291678
height opening = 0
opening area = 0.03
discharge coefficient = 0.6
width dam = 7
trapping efficiency = 75
extension id = 100
```

In every buffer section following keys must be included:
- `Trapping efficiency` (float): the relative amount of transported sediment that is trapped in the buffer (0-100)
- `Extension ID` (int): the id of the pixels of the buffer in the buffer map (if the buffer is bigger than one pixel)

The other keys are also mandatory, but are only used when `Simplified model version = 0`
- `Volume` (float)
- `Height dam` (float)
- `Height opening` (float)
- `Opening area` (float)
- `Discharge coefficient` (float)
- `Width dam` (float)

### Force routing

For every location where the user wants to force the routing in a certain direction, a separate section needs to be
added to the .ini-file.

```
[Force routing 1]
from col = 25
from row = 55
target col = 30
target row = 55
```

All keys in these sections represent the location of the source pixel (col, row) and target pixel (col, row).

### Calibration

If one wants to run the model in calibration mode (i.e. running the model for several combinations of ktc low and ktc high)
an extra section needs to be added to de .ini-file. The calibration mode is enabled by `Calibrate = 1`

```
[Calibration]
KTcHigh_lower = 0
KTcHigh_upper = 20
KTcLow_lower = 0
KTcLow_upper = 20
steps = 20
```

Following keys are required in the Calibration section:
- `KTcHigh_lower` (float, default 5), the lower limit for ktc high
- `KTcHigh_upper` (float, default 40), the upper limit for ktc high
- `KTcLow_lower` (float, default 1), the lower limit for ktc low
- `KTcLow_upper` (float, default 20), the upper limit for ktc low
- `steps` (integer, default 12), the amount of steps between ktc lower and higher

### Model output options

```
[Output maps]
write aspect = 1
write ls factor = 1
write rainfall excess = 0
write rusle = 1
write sediment export = 1
write slope = 1
write total runoff = 0
write upstream area = 1
write water erosion = 1
write routing table = 1
```

it is possible to enable or disable some modeloutputs. By default all modeloutput is disabled.

- `write aspect` (bool, default false): write the calculated aspect to an idrisi raster
- `write LS factor` (bool, default false): write the calculated LS factor to an idrisi raster
- `write upsream area` (bool, default false): write the calculated upstream area to an idrisi raster
- `write slope` (bool, default false): write the calculated slope values to an idrisi raster
- `write routing table` (bool, default false): write the routing to an txt file
- `write routing column/row` (bool, default false):
- `write RUSLE` (bool, default false):
- `write sediment export` (bool, default false):
- `write water erosion` (bool, default false):
- `write rainfall exces` (bool, default false):
- `write total runoff` (bool, default false):

## Description of the modeloutput

When a model run finishes, the outputs are written to disk. See the [page on model outputs](./output.md) for detailed info on each of the outputs.
