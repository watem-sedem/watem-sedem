###########
Model input
###########

This page describes all possible modelinput. 

Files
=====

All input rasters must be `Idrisi-rasters <https://gdal.org/drivers/raster/Idrisi.html>`_ and must have the same amount of columns, rows and cellsize. 
If one of the  input rasters has a different spatial extent, the model will raise an exception and will stop the excecution. 

DTM filename
************

Filename of the DTM raster. The 

Parcel filename
***************

Filename of the Parcel or Landuse map.

Sewer map filename
******************

Filename of the sewer map. This raster is only mandatory when :ref:`Include sewers = 1 <inlcudesewers>`

Tillage direction filename
**************************

Oriented roughness filename
***************************

Buffer map filename
*******************

Filename of the buffer map. This raster is only mandatory when :ref:`Include buffers = 1 <includebuffers>`

Ditch map filename
******************

Filename of the ditch map. This raster is only mandatory when :ref:`Include ditches = 1 <includeditches>`

Dam map filename
****************

Filename of the dam map. This raster is only mandatory when :ref:`Include dams = 1 <includedams>`

P factor map filename
*********************

Filename of the P-factor map.

River segment filename
**********************

Filename of the river segment map. This raster is only mandatory when :ref:`Output per VHA river segment = 1 <outputVHA>`

adjectant segments
******************

Table with adjectant segments. This table is only mandatory when :ref:`River routing = 1 <riverrouting>`

upstream segments
*****************

Table with upstream segments. This table is only mandatory when :ref:`River routing = 1 <riverrouting>`

river routing filename
**********************

Filename of the river routing map. This raster is only mandatory when :ref:`River routing = 1 <riverrouting>`

CN map filename
***************

Filename of the CN map. This raster is only mandatory when :ref:`simple = 0 <simple>`

Outlet map filename
*******************

Filename of the outlet map. This raster is only mandatory when :ref:`Manual outlet selection = 1 <manualoutlet>`.

ktil map filename
*****************

Filename of the ktil map. This raster is only mandatory when :ref:`Create ktil map = 0 <createktil>`.

Rainfall filename
*****************

K factor filename
*****************

Filename of the K-factor map.

C factor map filename
*********************

Filename of the C-factor map.

ktc map filename
****************

Filename of the ktc map. This raster is only mandatory when :ref:`Create ktc map = 0 <createktc>`.


Parameters
==========

Sewer exit
**********

integer

Clay content parent material
****************************

float

5 day antecedent rainfall
*************************

float

stream velocity
***************

float, mandatory when :ref:`simple = 0 <simple>`

alpha
*****

float, mandatory when :ref:`simple = 0 <simple>`

beta
****

float, mandatory when :ref:`simple = 0 <simple>`

bulk density
************

int

R factor
********

float

LS correction
*************

float (default 1)

Number of buffers
*****************

int, mandatory when :ref:`Include buffers = 1 <includebuffers>`

Number of forced routing
************************

int

ktc low
*******

float, mandatory when :ref:`Create ktc map = 1 <createktc>`

ktc high
********

float, mandatory when :ref:`Create ktc map = 1 <createktc>`

ktc limit
*********

float, mandatory when :ref:`Create ktc map = 0 <createktc>` or :ref:`Calibrate = 1 <Calibrate>`

ktil default
************

int, mandatory when :ref:`Create ktil map = 1 <createktil>`

ktil threshold
***************

float, mandatory when :ref:`Create ktil map = 1 <createktil>`

Parcel connectivity cropland
****************************

int

Parcel connectivity forest
**************************

int

Parcel trapping efficiency cropland
***********************************

int

Parcel trapping efficiency pasture
**********************************

int

Desired timestep for model
**************************

int

Final timestep output
*********************

int

Endtime model
*************

int 


