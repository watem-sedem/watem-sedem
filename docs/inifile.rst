Summary ``ini``-file
==========================

This page gives an overview and summary of all possible entries in the ini-file
of the model.

The ini-file is a configuration file where all model options are controlled.

For each input in the ``ini``-file, this overview defines:

- NAME: the name (key) of the variable
- DTYPE: the data type of the variable
- DEFAULT: the default value (if available)
- MODEL PART: the model part for which this is relevant (R = Routing model, WS = WaTEM/SEDEM, CN = Curved Number)
- REQUIREMENT: The requirement of the variable (M = mandatory, C = conditionally mandatory, O = optional)

Overview
--------
.. note::
   Note that in the ini-file the headers (given in [ ]) should also be included.
   
   
-  [:ref:`Working directories <folders>`]

+--------------------+---------+-----------+--------------+-------------+
| NAME               | DTYPE   | DEFAULT   | MODEL PART   | REQUIREMENT |
+====================+=========+===========+==============+=============+
| Input directory    | str     | ''        | R, WS, CN    | M           |
+--------------------+---------+-----------+--------------+-------------+
| Output directory   | str     | ''        | R, WS, CN    | O           |
+--------------------+---------+-----------+--------------+-------------+

-  [:ref:`Files <files>`]

+-------------------------------+---------+-----------+--------------+-------------+
| NAME                          | DTYPE   | DEFAULT   | MODEL PART   | REQUIREMENT |
+===============================+=========+===========+==============+=============+
| DTM filename                  | str     |           | R, WS, CN    | M           |
+-------------------------------+---------+-----------+--------------+-------------+
| Parcel filename               | str     |           | R, WS, CN    | M           |
+-------------------------------+---------+-----------+--------------+-------------+
| Sewer map filename            | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| Tillage direction filename    | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| Oriented roughness filename   | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| Buffer map filename           | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| Ditch map filename            | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| Dam map filename              | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| P factor map filename         | str     |           | R, WS, CN    | M           |
+-------------------------------+---------+-----------+--------------+-------------+
| River segment filename        | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| adjectant segments            | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| upstream segments             | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| river routing filename        | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| CN map filename               | str     |           | CN           | M           |
+-------------------------------+---------+-----------+--------------+-------------+
| Outlet map filename           | str     |           | R, WS, CN    | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| ktil map filename             | str     |           | WS, CN       | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| Rainfall filename             | str     |           | WS, CN       | C           |
+-------------------------------+---------+-----------+--------------+-------------+
| K factor filename             | str     |           | WS, CN       | M           |
+-------------------------------+---------+-----------+--------------+-------------+
| C factor map filename         | str     |           | WS, CN       | M           |
+-------------------------------+---------+-----------+--------------+-------------+
| ktc map filename              | str     |           | WS, CN       | C           |
+-------------------------------+---------+-----------+--------------+-------------+

-  [:ref:`User Choices <choicespage>`]

+-----------------------------+-----------+---------------------------+--------------+-------------+
| NAME                        | DTYPE     | DEFAULT                   | MODEL PART   | REQUIREMENT |
+=============================+===========+===========================+==============+=============+
| Only Routing                | boolean   | false                     | R            | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Only WS                     | boolean   | false                     | WS           | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Include sewers              | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Create ktc map              | boolean   | true                      | WS, CN       | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Calculate Tillage Erosion   | boolean   | false                     | WS, CN       | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Create ktil map             | boolean   | false                     | WS, CN       | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Estimate clay content       | boolean   | false                     | WS, CN       | C           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Include tillage direction   | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Include buffers             | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Include ditches             | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Include dams                | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Output per river segment    | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Max kernel                  | int       | 50                        | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Max kernel river            | int       | 100                       | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Adjusted Slope              | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Buffer reduce Area          | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Force Routing               | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| River Routing               | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| L model                     | str       | Desmet1996\_Vanoost2003   | R, WS, CN    | M           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| S model                     | str       | Nearing1997               | R, WS, CN    | M           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Manual outlet selection     | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+
| Convert output              | boolean   | false                     | R, WS, CN    | O           |
+-----------------------------+-----------+---------------------------+--------------+-------------+

.. _inioutput:
-  [:ref:`Output maps <outputchoices>`]

+----------------------------+-----------+-----------+--------------+------------+
| NAME                       | DTYPE     | DEFAULT   | MODEL PART   |REQUIREMENT |
+============================+===========+===========+==============+============+
| Write aspect               | boolean   | false     | R, WS, CN    | O          |
+----------------------------+-----------+-----------+--------------+------------+
| Write LS factor            | boolean   | false     | R, WS, CN    | O          |
+----------------------------+-----------+-----------+--------------+------------+
| Write upstream area        | boolean   | false     | R, WS, CN    | O          |
+----------------------------+-----------+-----------+--------------+------------+
| Write slope                | boolean   | false     | R, WS, CN    | O          |
+----------------------------+-----------+-----------+--------------+------------+ 
| Write routing table        | boolean   | false     | R, WS, CN    | O          |
+----------------------------+-----------+-----------+--------------+------------+
| Write routing column/row   | boolean   | false     | R, WS, CN    | O          |
+----------------------------+-----------+-----------+--------------+------------+
| Write RUSLE                | boolean   | false     | WS, CN       | O          |
+----------------------------+-----------+-----------+--------------+------------+
| Write sediment export      | boolean   | false     | WS, CN       | O          |
+----------------------------+-----------+-----------+--------------+------------+
| Write water erosion        | boolean   | false     | WS, CN       | O          |
+----------------------------+-----------+-----------+--------------+------------+
| Write rainfall excess      | boolean   | false     | CN           | O          |
+----------------------------+-----------+-----------+--------------+------------+
| Write total runoff         | boolean   | false     | CN           | O          |
+----------------------------+-----------+-----------+--------------+------------+
| Saga_Grids                 | boolean   | false     | WS, CN       | O          |
+----------------------------+-----------+-----------+--------------+------------+

-  [:ref:`Variables <variables>`]

+---------------------------------------+---------+-----------+--------------+-------------+
| NAME                                  | DTYPE   | DEFAULT   | MODEL PART   | REQUIREMENT |
+=======================================+=========+===========+==============+=============+
| Sewer exit                            | int     | ''        | R, WS, CN    | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Clay content parent material          | float   | ''        | R, WS, CN    | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| 5 day antecedent rainfall             | float   | ''        | CN           | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Stream velocity                       | float   | ''        | CN           | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Alpha                                 | float   | ''        | CN           | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Beta                                  | float   | ''        | CN           | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Bulk density                          | int     | ''        | WS, CN       | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| R factor                              | float   | ''        | WS           | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| LS correction                         | float   | '1'       | R, WS, CN    | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Number of buffers                     | int     | ''        | R, WS, CN    | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Number of Forced Routing              | int     | ''        | R, WS, CN    | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| ktc low                               | float   | ''        | WS, CN       | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| ktc high                              | float   | ''        | WS, CN       | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| ktc limit                             | float   | ''        | WS, CN       | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| ktil default                          | int     | ''        | WS, CN       | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| ktil threshold                        | float   | ''        | WS, CN       | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Parcel connectivity cropland          | int     | ''        | WS, CN       | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Parcel connectivity forest            | int     | ''        | R, WS, CN    | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Parcel connectivity grasstrips        | int     | '100'     | R, WS, CN    | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Parcel trapping efficiency cropland   | int     | ''        | R, WS, CN    | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Parcel trapping efficiency forest     | int     | ''        | R, WS, CN    | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Parcel trapping efficiency pasture    | int     | ''        | R, WS, CN    | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Desired timestep for model            | int     | ''        | CN           | M           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Final timestep output                 | int     | ''        | CN           | C           |
+---------------------------------------+---------+-----------+--------------+-------------+
| Endtime model                         | int     | ''        | CN           | M           |
+---------------------------------------+---------+-----------+--------------+-------------+

.. _inicalib:
-  [:ref:`Calibration <calibrationparamters>`]

+------------------+-----------+-----------+--------------+-------------+
| NAME             | DTYPE     | DEFAULT   | MODEL PART   | REQUIREMENT |
+==================+===========+===========+==============+=============+
| Calibrate        | boolean   | false     | WS, CN       | O           |
+------------------+-----------+-----------+--------------+-------------+
| KTcHigh\_lower   | float     | 5         | R, WS, CN    | C           |
+------------------+-----------+-----------+--------------+-------------+
| KTcHigh\_upper   | float     | 40        | R, WS, CN    | C           |
+------------------+-----------+-----------+--------------+-------------+
| KTcLow\_lower    | float     | 1         | R, WS, CN    | C           |
+------------------+-----------+-----------+--------------+-------------+
| KTcLow\_upper    | float     | 20        | R, WS, CN    | C           |
+------------------+-----------+-----------+--------------+-------------+
| steps            | int       | 12        | R, WS, CN    | C           |
+------------------+-----------+-----------+--------------+-------------+

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
