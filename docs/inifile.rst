Summary ``ini``-file
==========================

This page gives an overview and summary of all possible entries in the ini-file
of the model.

The ini-file is a configuration file where all model options are controlled.

For each input in the ``ini``-file, this overview defines:

- NAME: the name (key) of the variable
- DTYPE: the data type of the variable
- DEFAULT: the default value (if available)
- MODEL PART: the model part for which this is relevant (R = Routing algorithm, WS = WaTEM/SEDEM, CN = Curved Number)
- REQUIREMENT: The requirement of the variable (M = mandatory, C = conditionally mandatory, O = optional)

Overview
--------
.. note::
   Note that in the ini-file the headers (given in [ ]) should also be included.
   
   
-  [:ref:`Working directories <folders>`]

+-------------------------------------+---------+-----------+--------------+-------------+
| NAME                                | DTYPE   | DEFAULT   | MODEL PART   | REQUIREMENT |
+=====================================+=========+===========+==============+=============+
| :ref:`Input directory <inputdir>`   | str     | ''        | R, WS, CN    | M           |
+-------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Output directory <outputdir>` | str     | ''        | R, WS, CN    | O           |
+-------------------------------------+---------+-----------+--------------+-------------+

-  [:ref:`Files <files>`]

+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| NAME                                                      | DTYPE   | DEFAULT   | MODEL PART   | REQUIREMENT |
+===========================================================+=========+===========+==============+=============+
| :ref:`DTM filename <dtmmap>`                              | str     |           | R, WS, CN    | M           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Parcel filename <prcmap>`                           | str     |           | R, WS, CN    | M           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Sewer map filename <sewermapfile>`                  | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Tillage direction filename <tildirmap>`             | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Oriented roughness filename <orientedroughnessmap>` | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Buffer map filename <buffermap>`                    | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Ditch map filename <ditchmap>`                      | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Dam map filename <dammap>`                          | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`P factor map filename <pmap>`                       | str     |           | R, WS, CN    | M           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`River segment filename <riversegmentfile>`          | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`adjectant segments <adjsegments>`                   | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`upstream segments <upstrsegments>`                  | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`river routing filename <riverroutingmap>`           | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`CN map filename <cnmap>`                            | str     |           | CN           | M           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Outlet map filename <outletmap>`                    | str     |           | R, WS, CN    | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`ktil map filename <ktilmap>`                        | str     |           | WS, CN       | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Rainfall filename <rainfallfile>`                   | str     |           | WS, CN       | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`K factor filename <kmap>`                           | str     |           | WS, CN       | M           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`C factor map filename <cmap>`                       | str     |           | WS, CN       | M           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`ktc map filename <ktcmap>`                          | str     |           | WS, CN       | C           |
+-----------------------------------------------------------+---------+-----------+--------------+-------------+

-  [:ref:`User Choices <choicespage>`]

+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| NAME                                                       | DTYPE     | DEFAULT                   | MODEL PART   | REQUIREMENT |
+============================================================+===========+===========================+==============+=============+
| :ref:`Only Routing <onlyrouting>`                          | boolean   | false                     | R            | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Curve Number <simple>`                               | boolean   | false                     | WS           | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Include sewers <inlcudesewers>`                      | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Create ktc map <createktc>`                          | boolean   | true                      | WS, CN       | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Calculate Tillage Erosion <calctileros>`             | boolean   | false                     | WS, CN       | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Create ktil map <createktil>`                        | boolean   | false                     | WS, CN       | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Estimate clay content <estimclay>`                   | boolean   | false                     | WS, CN       | C           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Include tillage direction <includetillagedirection>` | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Include buffers <includebuffers>`                    | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Include ditches <includeditches>`                    | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Include dams <includedams>`                          | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Output per river segment <outputsegment>`            | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Max kernel <maxkernel>`                              | int       | 50                        | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Max kernel river <maxkernelriver>`                   | int       | 100                       | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Adjusted Slope <adjustslope>`                        | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Buffer reduce Area <bufferreduce>`                   | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Force Routing <forcerouting>`                        | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`River Routing <riverrouting>`                        | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`L model <lmodel>`                                    | str       | Desmet1996\_Vanoost2003   | R, WS, CN    | M           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`S model <smodel>`                                    | str       | Nearing1997               | R, WS, CN    | M           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`TC model <tcmodel>`                                  | str       | VanOost2000               | R, WS, CN    | M           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| :ref:`Manual outlet selection <manualoutlet>`              | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+
| Convert output                                             | boolean   | false                     | R, WS, CN    | O           |
+------------------------------------------------------------+-----------+---------------------------+--------------+-------------+

.. _inioutput:

-  [:ref:`Output maps <outputchoices>`]

+----------------------------------------------------+-----------+-----------+--------------+------------+
| NAME                                               | DTYPE     | DEFAULT   | MODEL PART   |REQUIREMENT |
+====================================================+===========+===========+==============+============+
| :ref:`Write aspect <writeaspect>`                  | boolean   | false     | R, WS, CN    | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Write LS factor <writels>`                   | boolean   | false     | R, WS, CN    | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Write upstream area <writeuparea>`           | boolean   | false     | R, WS, CN    | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Write slope <writeslope>`                    | boolean   | false     | R, WS, CN    | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Write routing table <writerouting>`          | boolean   | false     | R, WS, CN    | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Write routing column/row <writeroutingrc>`   | boolean   | false     | R, WS, CN    | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Write RUSLE <writerusle>`                    | boolean   | false     | WS, CN       | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Write sediment export <writesedexport>`      | boolean   | false     | WS, CN       | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Write water erosion <writerwatereros>`       | boolean   | false     | WS, CN       | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Write rainfall excess <writerainfallexcess>` | boolean   | false     | CN           | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Write total runoff <writetotalrunoff>`       | boolean   | false     | CN           | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+
| :ref:`Saga_Grids <sagagrids>`                      | boolean   | false     | WS, CN       | O          |
+----------------------------------------------------+-----------+-----------+--------------+------------+

-  [:ref:`Variables <variables>`]

+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| NAME                                                              | DTYPE   | DEFAULT   | MODEL PART   | REQUIREMENT |
+===================================================================+=========+===========+==============+=============+
| :ref:`Sewer exit <sewerexit>`                                     | int     | ''        | R, WS, CN    | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Clay content parent material <claycontent>`                 | float   | ''        | R, WS, CN    | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`5 day antecedent rainfall <5dayrainfall>`                   | float   | ''        | CN           | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Stream velocity <streamvelocity>`                           | float   | ''        | CN           | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Alpha <alpha>`                                              | float   | ''        | CN           | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Beta <beta>`                                                | float   | ''        | CN           | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Bulk density <bulkdensity>`                                 | int     | ''        | WS, CN       | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`R factor <rfactor_var>`                                     | float   | ''        | WS           | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`LS correction <lscorrection>`                               | float   | '1'       | R, WS, CN    | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Number of buffers <nrbuffers>`                              | int     | ''        | R, WS, CN    | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Number of Forced Routing <nrforcedrouting>`                 | int     | ''        | R, WS, CN    | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`ktc low <ktclow>`                                           | float   | ''        | WS, CN       | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`ktc high <ktchigh>`                                         | float   | ''        | WS, CN       | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`ktc limit <ktclimit>`                                       | float   | ''        | WS, CN       | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`ktil default <ktildefault>`                                 | int     | ''        | WS, CN       | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`ktil threshold <ktilthres>`                                 | float   | ''        | WS, CN       | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Parcel connectivity cropland <parcelconncrop>`              | int     | ''        | WS, CN       | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Parcel connectivity forest <parcelconnforest>`              | int     | ''        | R, WS, CN    | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Parcel connectivity grasstrips <parcelconngras>`            | int     | '100'     | R, WS, CN    | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Parcel trapping efficiency cropland <parceltrapppingcrop>`  | int     | ''        | R, WS, CN    | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Parcel trapping efficiency forest <parceltrappingforest>`   | int     | ''        | R, WS, CN    | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Parcel trapping efficiency pasture <parceltrappingpasture>` | int     | ''        | R, WS, CN    | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Desired timestep for model <timestep>`                      | int     | ''        | CN           | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Final timestep output <finaltimestep>`                      | int     | ''        | CN           | C           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+
| :ref:`Endtime model <endtime>`                                    | int     | ''        | CN           | M           |
+-------------------------------------------------------------------+---------+-----------+--------------+-------------+

.. _inicalib:

-  [:ref:`Calibration <calibrationparamters>`]

+---------------------------------------+-----------+-----------+--------------+-------------+
| NAME                                  | DTYPE     | DEFAULT   | MODEL PART   | REQUIREMENT |
+=======================================+===========+===========+==============+=============+
| :ref:`Calibrate <calibrate>`          | boolean   | false     | WS, CN       | O           |
+---------------------------------------+-----------+-----------+--------------+-------------+
| :ref:`KTcHigh\_lower <ktchigh_lower>` | float     | 5         | R, WS, CN    | C           |
+---------------------------------------+-----------+-----------+--------------+-------------+
| :ref:`KTcHigh\_upper <ktchigh_upper>` | float     | 40        | R, WS, CN    | C           |
+---------------------------------------+-----------+-----------+--------------+-------------+
| :ref:`KTcLow\_lower <ktclow_lower>`   | float     | 1         | R, WS, CN    | C           |
+---------------------------------------+-----------+-----------+--------------+-------------+
|:ref:`KTcLow\_upper <ktclow_upper>`    | float     | 20        | R, WS, CN    | C           |
+---------------------------------------+-----------+-----------+--------------+-------------+
| :ref:`steps <steps>`                  | int       | 12        | R, WS, CN    | C           |
+---------------------------------------+-----------+-----------+--------------+-------------+

-  [:ref:`Forced Routing X <forcedroutingdata>`]

This section is only mandatory when :ref:`Force Routing <forcerouting>` is
enabled. The section is repeated for every force routing vector (i.e. X ranges
from 1 to :ref:`Number of forced routing <nrforcedrouting>`.

+--------------+---------+-----------+--------------+-------------+
| NAME         | DTYPE   | DEFAULT   | MODEL PART   | REQUIREMENT |
+==============+=========+===========+==============+=============+
| from col     | int     | ''        | R, WS, CN    | M           |
+--------------+---------+-----------+--------------+-------------+
| from row     | int     | ''        | R, WS, CN    | M           |
+--------------+---------+-----------+--------------+-------------+
| target col   | int     | ''        | R, WS, CN    | M           |
+--------------+---------+-----------+--------------+-------------+
| target row   | int     | ''        | R, WS, CN    | M           |
+--------------+---------+-----------+--------------+-------------+

-  :ref:`[Buffer X] <bufferdata>`

This section is only mandatory when :ref:`Include buffers <includebuffers>` is
enabled. The section is repeated for every buffer id (i.e. X ranges from 1 to
:ref:`Number of buffers <nrbuffers>`).

+-------------------------+---------+-----------+--------------+-------------+
| NAME                    | DTYPE   | DEFAULT   | MODEL PART   | REQUIREMENT |
+=========================+=========+===========+==============+=============+
| Volume                  | float   | ''        | R, WS, CN    | M           |
+-------------------------+---------+-----------+--------------+-------------+
| Height dam              | float   | ''        | R, WS, CN    | M           |
+-------------------------+---------+-----------+--------------+-------------+
| Height opening          | float   | ''        | R, WS, CN    | M           |
+-------------------------+---------+-----------+--------------+-------------+
| Opening area            | float   | ''        | R, WS, CN    | M           |
+-------------------------+---------+-----------+--------------+-------------+
| Discharge coefficient   | float   | ''        | R, WS, CN    | M           |
+-------------------------+---------+-----------+--------------+-------------+
| Width dam               | float   | ''        | R, WS, CN    | M           |
+-------------------------+---------+-----------+--------------+-------------+
| Trapping efficiency     | float   | ''        | R, WS, CN    | M           |
+-------------------------+---------+-----------+--------------+-------------+
| Extension ID            | int     | ''        | R, WS, CN    | M           |
+-------------------------+---------+-----------+--------------+-------------+

Conditionally mandatory Dependencies (C)
----------------------------------------

The format is currently: if ... > then ... mandatory. These do not
include the model part requirements, as these can be derived from the
table itself.

-  **Include\_sewer** > *Sewer exit* and *Sewer map filename*
-  **Calculate Tillage Erosion** > *Create ktil map* and *ktil map filename*
-  ***NOT*** **Create ktil map**> *ktil map filename*
-  ***NOT*** **calibrate** ***AND NOT*** **Create ktc map** > *ktc map filename*
-  **Include tillage direction** > *Tillage direction filename* and *Oriented
   roughness filename*
-  **Include buffers** > *Buffer map filename* and *Number of buffers*
-  **Include ditches** > *Ditch map filename*
-  **Include dams** > *Dam map filename*
-  **River Routing** > *River segment filename*, *adjectant segments*, *upstream
   segments* and *river routing filename*
-  **Output per river segment** > *River segment filename*
-  ***NOT*** **Use R factor** > *Rainfall filename* and *5\|day antecedent rainfall*
-  **Estimate clay content** > *Clay content parent material*
-  **Manual outlet selection** > *Outlet map filename* 
-  **Force Routing** > *Number of Forced Routing*
-  ***NOT*** **calibrate AND Create ktc map** > *ktc low* and *ktc high*
-  **Create ktc map** > *ktc limit*
-  **calibrate** > *ktc limit*, *KTcHigh\_lower*, *KTcHigh\_upper*, *KTcLow\_lower*,
   *KTcLow\_upper* and *steps*
-  **Create ktil map** > *ktil default* and *ktil threshold*
-  **Convert output** > *Final timestep output*

Controlled vocabularies
-----------------------
Some variables require specific input strings (keys) in order to select the right method for the calculations in the model. The variables and their respective posible keys are listed hereunder:

- **L model** -> *Desmet1996\_McCool* or *Desmet1996\_Vanoost2003*
- **S model** -> *Desmet1996* or *Nearing1997*
- **TC model** -> *VanOost2000* or *Verstraeten2007*

Multi\|year (long-term)
-----------------------

-  parcel filename -> parcel filename X
-  CN map filename -> CN map SEASON X
-  C factor map filename -> C factor map SEASON X
