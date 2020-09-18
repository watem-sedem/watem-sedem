############
Model output
############

In this section we will describe all possible modeloutput of the CN-WS model. Tables are written as txt-files, rasters are written as `Idrisi-rasters <https://gdal.org/drivers/raster/Idrisi.html>`_.

The created model output depends on the userchoices in in the ini-file. 

.. _onlyroutingoutput:

Routing only output
###################

When the user chooses to only use the routing algorithm of the cn-ws model, following output can be generated.

.. _routingtxt:

routing.txt
***********

Tab seperated table wich contains a row for every pixel in the spatial domain. For every pixel (row in the table) the following collumns are present:

* col, row: the position of the pixel in the raster
* target1col, target1row: the position of the first targetpixel in the raster. These values are -99 if target1 does not exist.
* part1: the relative amount of outgoing sediment/water to the first target pixel
* distance1: the distance (in m) between the sourcepixel and the first target pixel
* target2col, target2row: the position of the second targetpixel in the raster. These values are -99 if target2 does not exist.
* part2: the relative amount of outgoing sediment/water to the second target pixel. Togheter with part1 the sum must be 1.
* distance2: the distance (in m) between the sourcepixel and the second target pixel

The routing table is only generated when :ref:`write routing table = 1 <writerouting>`.

.. _missingroutingtxt:

routing_missing.txt
*******************

Tab seperated table with the same headers as :ref:`routing.txt <routingtxt>`. The entries in the table are a subset of those in routing.txt and are only included if...

The routing table is only generated when :ref:`write routing table = 1 <writerouting>`.

.. _routingcolrow:

routing_colrow.txt
******************

to do

.. _lsmap:

LS.rst
******

Raster with the calculated LS-factor (dimensionless). This raster is only written if :ref:`write ls factor = 1 <writels>`.

.. _aspectmap:

AspectMap.rst
*************

Raster with the direction of slope (the aspect) in radians. This raster is only written if :ref:`write aspect = 1 <writeaspect>`.

.. _slopemap:

SLOPE.rst
*********

Raster with the calculated slope in radians. This raster is only written if :ref:`write slope = 1 <writeslope>`.

.. _upareamap:

UPAREA.rst
**********

Raster with the total upstream area (m²) for every pixel. This raster is only written if :ref:`write upstream area = 1 <writeuparea>`.

.. _watemsedemoutput:

WaTEM-SEDEM output
##################

When the WaTEM-SEDEM model is used (:ref:`simple = 1 <simple>`), the following rasters and tables can be written as output.

.. _totalsedimenttxt:

Total Sediment.txt
******************

Txt-file where the first five rows give a summary of the results:
* Total erosion (kg): the total amount of sediment eroded in the landscape. 
* Total deposition (kg): the total amount of sediment deposited in the landscape (not entering sewers or rivers)
* Sediment leaving the catchment, via the river (kg): the amount of sediment that enters all riverpixels
* Sediment leaving the catchment, not via the river (kg): the amout of sediment that enters pixels outside the modeldomain
* Sediment trapped in buffers (kg): the amount of sediment that is trapped in all buffer basins. 

The file contains from 9th row on a tab-seprated table where for every outlet the amount of incoming sediment is reported.

.. _totalsedimentvhatxt:

Total Sediment VHA.txt
**********************

Tab seperated table. Every row contains the id of a river segment and the total amount of sediment (kg) entering the segment.
This table is only generated when :ref:`Output per VHA river segment = 1 <outputVHA>`.

.. _cumsedvhatxt:

Cumulative sediment VHA.txt
***************************

This table is only generated when :ref:`Output per VHA river segment = 1 <outputVHA>`.

.. _claycontentesedtxt:

Clay content sediment.txt
*************************

Tab seperated table with the mean clay content (%) at every outlet. This table is only generated when :ref:`estimate clay content = 1 <estimclay>`.

.. _cumulativerst:

cumulative.rst
**************

TO DO

sewer_in.rst
************

Raster with the amount of sediment (kg) that is trapped in every sewer pixel. This raster is only generated when :ref:`Include sewers = 1 <inlcudesewers>`.

.. _sediexportrst:

SediExport_kg.rst
*****************

Raster with for every river cell the calculated amounts of sedimentinput (kg). This raster is only written if :ref:`write sediment export = 1 <writesedexport>`.

.. _sedioutrst:

SediOut_kg.rst
**************

Raster with the amount of sediment (kg) that leaves every pixel and is distributed between the two target pixels.
This raster is only written if :ref:`write sediment export = 1 <writesedexport>`.

.. _sediinrst:

SediIn_kg.rst
*************

Raster with the amount of sediment (kg) that enters a pixel from the upstream pixels.
This raster is only written if :ref:`write sediment export = 1 <writesedexport>`.

.. _watereroskgrst:

WATEREROS (kg per gridcel).rst
******************************

Raster with the total amount of erosion or sedimentation in every pixel in kg. Negative values
indicate erosion (i.e. Incoming Sediment + RUSLE < Capacity), positive values indicate sedimentation
(i.e. Incoming Sediment + RUSLE > Capacity). This raster is only written if :ref:`write water erosion = 1 <writerwatereros>`.

.. _watererosmmrst:

WATEREROS (mm per gridcel).rst
******************************

Raster with the total amount of erosion or sedimentation in every pixel in mm. Negative values
indicate erosion (i.e. Incoming Sediment + RUSLE < Capacity), positive values indicate sedimentation
(i.e. Incoming Sediment + RUSLE > Capacity). This raster is only written if :ref:`write water erosion = 1 <writerwatereros>`.

.. _capacityrst:

Capacity.rst
************

Raster with the calculated transport capacity (kg/m²) for every pixel.

.. _ruslerst:

RUSLE.rst
*********

Raster with the calculated RUSLE-values, the potential soil loss, for every pixel in kg/m².
This raster is only written if :ref:`write rusle = 1 <writerusle>`

TILEROS.rst
***********

Raster with the calculated tillage erosion (mm/year). Negative values indicate erosion, positive values give sedimentation.
This raster is only written if :ref:`calculate tillage erosion = 1 <calctileros>`.

.. _calibrationtxt:

Calibration.txt
***************

Only writen when :ref:`Calibratie = 1 <calibrate>`.

TO DO 

CN-output
#########

When the CN-module is enabled (:ref:`simple=0 <simple>`) it is possible to generate some additional output.

Discharge.txt
*************

Table with discharge (m³/s) as a function of time for every outlet.

Discharge_VHA.txt
*****************

Table with discharge (m³/s) as a function of time for every river segment. This table is only
generated when :ref:`Output per VHA river segment = 1 <outputVHA>`.

Sediment concentration.txt
**************************

Table with the concentration of sediment (g/l) as a function of time for every outlet.

Sediment concentration_VHA.txt
******************************

Table with the concentration of sediment (g/l) as a function of time for every river segment.
This table is only generated when :ref:`Output per VHA river segment = 1 <outputVHA>`.

Sediment.txt
************

Table with the sediment load (kg) as a function of time for every outlet.

Sediment_VHA.txt
****************

Table with the sediment load (kg) as a function of time for river segment.
This table is only generated when :ref:`Output per VHA river segment = 1 <outputVHA>`.

Spillover per buffer.txt
************************

Table with the amount of water (m³) that leaves every buffer basin via the overflow.

Total discharge.txt
*******************

Table with the total amount of water (m³) that arives in every outlet after a rainfall event.

.. _remaprst:

Remap.rst
*********

Raster with the total amount of discharge (=rainfall - infiltration) per pixel for a rainfall event.

.. _totalrunofrst:

Total runoff.rst
****************

Raster with total runoff (m³) generated in every pixel during a rainfall event. The value in every pixel
is the sum of the amount of rainfall and the amount of water flowing from upstream pixels, minus the infiltration
in the pixel.
