### User Choices

#### L model

CN-WS allows the user to choose between two models to calculate the L-factor. The L-factor defines the impact of the slope length and is used
in the calculation of RUSLE and transport capacity (TC). The two L-models are:
- Desmet and Govers (1996)
- McCool et al. (1989, 1987) 

#### S model

CN-WS allows the user to choose between two models to calculate the S-factor. The S-factor defines the effect of slope steepness and is used
in the calculation of RUSLE and transport capacity (TC). The two S-models are:
- Desmet and Govers (1996)
- Nearing (1997)

#### Only Routing

By enabling the Only Routing option, only the routing will be determined by CN-WS. No sediment calculations or discharge calculations are done:
the WaTEM-SEDEM and CN modules are disabled.

#### Simple

TO DO

#### Calculate tillage erosion

TO DO

#### Create ktil map

CN-WS is able to create a raster with ktil-factors for ... 

#### Create ktc map

CN-WS is able to create a raster with ktc-factors for high erodible and non-erodible land-use. When `Create ktc map = 1` the model expects three variables:
`ktc low`, `ktc high`, `ktc limit`. The C-factor map will be reclassed by these values: C-factors higher than `ktc limit` will get the value of `ktc high`, 
otherwise `ktc low` is chosen. 

When `Create ktc map = 0` the user will have to make a ktc map himself. The model will expect the filename of this ktc map in `ktc map filename`.

#### Include sewers

When the include sewers-option is enabled, the user will have to provide two additional inputs:
- sewermap
- sewerexit

#### Include buffers

An infrastructural measure that traps an amount of transported sediment is called a buffer. These measures can be simulated in the model by enabling
the Include buffers option. This option 

#### Include ditches and dams

Ditches and dams alter the sediment flow. The sediment will follow the course of a ditch or dam in stead of along the steepest slope. 

Two options are available in the model:
- Include ditches
- Include dams

Both options are similar, but differences in C-factor and ktc

#### Force Routing

#### River Routing

By enabling the river routing option, the routing between river pixels is imposed by an input raster and two input tables. 
This option is usefull because the calculated routing in a river, based on the digital elevation model, is not always correct.  

Following input-files are required when `River Routing = 1`:
- river segement filename
- river routing filename
- adjectant segments
- upstream segments

When this option is disabled, the model will use the digital elevation model to determine the routing between all river pixels.

#### Include tillage direction

TO DO

#### Adjusted Slope

Normally, the slope of a pixel is determined by the algoritm of Zevenbergen and Thorne (1987) on the four neighbouring, cardinal cells. 
This procedure works good in areas where the routing is determined solely on the digital elevation model. In areas where the routing is imposed by 
other rules (e.g. at parcel boundaries, in buffers,...) the slope of the direction in the routing can be different than the calculated slope by 
Zevenbergen and Thorne (1987). In these cases the slope can be calculated by the absolute value of the height difference between the source 
and target pixel, divided by the distance between these two pixels. This calculation is enabled by setting `Adjusted Slope = 1`

#### Estimate Clay content

TO DO

#### Calibrate

The Calibrate-option allows the model user to run the model with a given set of options, variables and inputfiles for a number of combinations of ktc-factors. 
Both the ktc_high-factor as the ktc_low-factor are varied in an amount of steps between a lower and upper value. For every combination of ktc-factors where 
ktc_high > ktc_low, the model will make a calculation and 









