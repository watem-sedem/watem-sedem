# Configuration `ini` file

This overview is derived from {ref}`inireference`. For each input in the `ini` file, it defines:
- the name (key) of the variable
- the data type of the variable
- the default value (if available)
- the model part for which this is relevant
- Is it (for the given model part) M(andatory) of C(onditionally mandatory)?

The format need to be improved but it provides a single/complete general overview of the inputs.

## Overview

- [Working directories]

|NAME                                | DTYPE       | DEFAULT       | MODEL PART    | M or C?  |
|----------------------------------- |-------------|---------------|---------------|----------|
|Input directory                     | str         | ''            | R, WS, CN     | M        |
|Output directory                    | str         | ''            | R, WS, CN     |          |

- [User Choices]

|NAME                                | DTYPE       | DEFAULT       | MODEL PART    | M or C?  |
|----------------------------------- |-------------|---------------|---------------|----------|
|Only Routing                        | boolean     | false         | R             |          |
|Simplified model version            | boolean     | false         | WS            |          |
|Include sewers                      | boolean     | false         | R, WS, CN     |          |
|  | | | | |
|Use R factor                        | boolean     | false         | WS, CN        |          |
|Create ktc map                      | boolean     | true          | WS, CN        |          |
|Calculate Tillage Erosion           | boolean     | false         | WS, CN        |          |
|Create ktil map                     | boolean     | false         | WS, CN        |          |
|Estimate clay content               | boolean     | false         | WS, CN        | C        |
|  | | | | |
|Include tillage direction           | boolean     | false         | R, WS, CN     |          |
|Include buffers                     | boolean     | false         | R, WS, CN     |          |
|Include ditches                     | boolean     | false         | R, WS, CN     |          |
|Include dams                        | boolean     | false         | R, WS, CN     |          |
|  | | | | |          |
|Output per river segment            | boolean     | false         | R, WS, CN     |          |
|Max kernel                          | int         | 50            | R, WS, CN     |          |
|Max kernel river                    | int         | 100           | R, WS, CN     |          |
|  | | | | |
|Adjusted Slope                      | boolean     | false         | R, WS, CN     |          |
|Buffer reduce Area                  | boolean     | false         | R, WS, CN     |          |
|Force Routing                       | boolean     | false         | R, WS, CN     |          |
|River Routing                       | boolean     | false         | R, WS, CN     |          |
|  | | | | |
|L model                             | str         | Desmet1996_Vanoost2003  | R, WS, CN   | M |
|S model                             | str         | Nearing1997             | R, WS, CN   | M |
|  | | | | |
|Manual outlet selection             | boolean     | false         | R, WS, CN     |          |
|Convert output                      | boolean     | false         | R, WS, CN     |          |


- [Variables]

|NAME                                | DTYPE       | DEFAULT       | MODEL PART  | M or C?  |
|----------------------------------- |-------------|---------------|-------------|----------|
|Sewer exit                          | int         | ''            | R, WS, CN   | C        |
|Clay content parent material        | float       | ''            | R, WS, CN   | C        |
|  | | | | |
|5 day antecedent rainfall           | float       | ''            | CN          | C        |
|Stream velocity                     | float       | ''            | CN          | M        |
|Alpha                               | float       | ''            | CN          | M        |
|Beta                                | float       | ''            | CN          | M        |
|Bulk density                        | int         | ''            | WS, CN      | M        |
|R factor                            | float       | ''            | WS          | M        |
|  | | | | |
|LS correction                       | float       | '1'           | R, WS, CN   | M        |
|Number of buffers                   | int         | ''            | R, WS, CN   | C        |
|Number of Forced Routing            | int         | ''            | R, WS, CN   | C        |
|  | | | | |
|ktc low                             | float       | ''            | WS, CN      | C        |
|ktc high                            | float       | ''            | WS, CN      | C        |
|ktc limit                           | float       | ''            | WS, CN      | C        |
|ktil default                        | int         | ''            | WS, CN      | C        |
|ktil threshold                      | float       | ''            | WS, CN      | C        |
|  | | | | |
|Parcel connectivity cropland        | int         | ''            | WS, CN      | M        |
|Parcel connectivity forest          | int         | ''            | R, WS, CN   | M        |
|Parcel trapping efficiency cropland | int         | ''            | R, WS, CN   | M        |
|Parcel trapping efficiency forest   | int         | ''            | R, WS, CN   | M        |
|Parcel trapping efficiency pasture  | int         | ''            | R, WS, CN   | M        |
|  | | | | |
|Desired timestep for model          | int         | ''            | CN          | M        |
|Final timestep output               | int         | ''            | CN          | C        |
|Endtime model                       | int         | ''            | CN          | M        |


- [Calibration]

|NAME                                | DTYPE       | DEFAULT       | MODEL PART  | M or C?  |
|----------------------------------- |-------------|---------------|-------------|----------|
|Calibrate                           | boolean     | false         | WS, CN      |          |
|KTcHigh_lower                       | float       | 5             | R, WS, CN   | C        |
|KTcHigh_upper                       | float       | 40            | R, WS, CN   | C        |
|KTcLow_lower                        | float       | 1             | R, WS, CN   | C        |
|KTcLow_upper                        | float       | 20            | R, WS, CN   | C        |
|steps                               | int         | 12            | R, WS, CN   | C        |


- [Files]

|NAME                                | DTYPE       | DEFAULT       | MODEL PART  | M or C?  |
|----------------------------------- |-------------|---------------|-------------|----------|
|DTM filename                        | str         |               | R, WS, CN   | M        |
|Parcel filename                     | str         |               | R, WS, CN   | M        |
|Sewer map filename                  | str         |               | R, WS, CN   | C        |
|Tillage direction filename          | str         |               | R, WS, CN   | C        |
|Oriented roughness filename         | str         |               | R, WS, CN   | C        |
|Buffer map filename                 | str         |               | R, WS, CN   | C        |
|Ditch map filename                  | str         |               | R, WS, CN   | C        |
|Dam map filename                    | str         |               | R, WS, CN   | C        |
|P factor map filename               | str         |               | R, WS, CN   | M        |
|River segment filename              | str         |               | R, WS, CN   | C        |
|adjectant segments                  | str         |               | R, WS, CN   | C        |
|upstream segments                   | str         |               | R, WS, CN   | C        |
|river routing filename              | str         |               | R, WS, CN   | C        |
|  | | | | |
|CN map filename                     | str         |               | CN          | M        |
|  | | | | |
|Outlet map filename                 | str         |               | R, WS, CN   | C        |
|  | | | | |
|ktil map filename                   | str         |               | WS, CN      | C        |
|Rainfall filename                   | str         |               | WS, CN      | C        |
|K factor filename                   | str         |               | WS, CN      | M        |
|C factor map filename               | str         |               | WS, CN      | M        |
|ktc map filename                    | str         |               | WS, CN      | C        |


- [Output maps]

|NAME                                | DTYPE       | DEFAULT       | MODEL PART  | M or C?  |
|----------------------------------- |-------------|---------------|-------------|----------|
|Write aspect                        | boolean     | false         | R, WS, CN   |          |
|Write LS factor                     | boolean     | false         | R, WS, CN   |          |
|Write upstream area                 | boolean     | false         | R, WS, CN   |          |
|Write slope                         | boolean     | false         | R, WS, CN   |          |
|Write routing table                 | boolean     | false         | R, WS, CN   |          |
|Write routing column/row            | boolean     | false         | R, WS, CN   |          |
|  | | | | |
|Write RUSLE                         | boolean     | false         | WS, CN      |          |
|Write sediment export               | boolean     | false         | WS, CN      |          |
|Write water erosion                 | boolean     | false         | WS, CN      |          |
|Write rainfall excess               | boolean     | false         | CN          |          |
|Write total runoff                  | boolean     | false         | CN          |          |



- [Forced Routing X] IF Force Routing WITH X 1 -> Number of Forced Routing

|NAME                                | DTYPE       | DEFAULT       | MODEL PART  | M or C?  |
|----------------------------------- |-------------|---------------|-------------|----------|
|from col                            | int         | ''            | R, WS, CN   | M        |
|from row                            | int         | ''            | R, WS, CN   | M        |
|target col                          | int         | ''            | R, WS, CN   | M        |
|target row                          | int         | ''            | R, WS, CN   | M        |


- [Buffer X] IF Include buffers WITH X 1 -> Number of buffers

|NAME                                | DTYPE       | DEFAULT       | MODEL PART  | M or C?  |
|----------------------------------- |-------------|---------------|-------------|----------|
|Volume                              | float       | ''            | R, WS, CN   | M        |
|Height dam                          | float       | ''            | R, WS, CN   | M        |
|Height opening                      | float       | ''            | R, WS, CN   | M        |
|Opening area                        | float       | ''            | R, WS, CN   | M        |
|Discharge coefficient               | float       | ''            | R, WS, CN   | M        |
|Width dam                           | float       | ''            | R, WS, CN   | M        |
|Trapping efficiency                 | float       | ''            | R, WS, CN   | M        |
|Extension ID                        | int         | ''            | R, WS, CN   | M        |


## (C)onditionally mandatory Dependencies

The format is currently: if ... > then ... mandatory. These do not include the model part requirements, as these can be derived from tht table itself.

- Include_sewer > Sewer exit, Sewer map filename
- Calculate Tillage Erosion >  Create ktil map, ktil map filename
- NOT Create ktil map > ktil map filename
- NOT calibrate AND NOT Create ktc map > ktc map filename
- Include tillage direction > Tillage direction filename, Oriented roughness filename
- Include buffers > Buffer map filename, Number of buffers
- Include ditches > Ditch map filename
- Include dams > Dam map filename
- River Routing > River segment filename, adjectant segments, upstream segments, river routing filename
- Output per river segment > River segment filename
- NOT Use R factor > Rainfall filename
- Estimate clay content > Clay content parent material
- Manual outlet selection > Outlet map filename
- NOT Use R factor > 5|day antecedent rainfall
- Force Routing > Number of Forced Routing
- NOT calibrate AND Create ktc map > ktc low, ktc high
- Create ktc map > ktc limit
- calibrate > ktc limit, KTcHigh_lower, KTcHigh_upper, KTcLow_lower, KTcLow_upper, steps
- Create ktil map > ktil default, ktil threshold
- Convert output > Final timestep output

## Controlled vocabularies

L model -> Desmet1996_McCool, Desmet1996_Vanoost2003
S model -> Desmet1996, Nearing1997

## Multi|year (long-term)

- parcel filename -> parcel filename X
- CN map filename -> CN map SEASON X
- C factor map filename -> C factor map SEASON X

## Help variables used in pascal code

Simplified = "Only Routing" | "Simplified model version"  -> R | WS
top =  NOT "Include tillage direction"