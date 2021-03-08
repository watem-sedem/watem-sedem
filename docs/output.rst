.. _modeloutput:

############
Model output
############

In this section we will describe all possible outputs of the CN-WS model. Tables
are written as tab-delimited txt-files, rasters are written as
`Idrisi-rasters <https://gdal.org/drivers/raster/Idrisi.html>`_.

The created model output depends on the userchoices in the ini-file. 

.. _onlyroutingoutput:

Routing only output
###################

Folllowing output can be generated when only the ‘routing only’ option in the
user choices is set to 1:

.. _routingtxt:

routing.txt
***********

Tab-delimited table wich contains a row for every pixel in the spatial domain.
Following columns are present for every pixel (every pixel is a row in the
table):

* col, row: the position of the pixel in the raster
* target1col, target1row: the position of the first target pixel in the raster.
  These values are -99 if target1 does not exist.
* part1: the relative amount of outgoing sediment/water to the first target
  pixel
* distance1: the distance (in m) between the source pixel and the first target
  pixel
* target2col, target2row: the position of the second target pixel in the raster.
  These values are -99 if target2 does not exist.
* part2: the relative amount of outgoing sediment/water to the second target
  pixel. Together with part1 the sum must be 1.
* distance2: the distance (in m) between the source pixel and the second target
  pixel

The routing table is only generated when
:ref:`write routing table = 1 <writerouting>`.

TO DO: col/row orientation.

.. _missingroutingtxt:

routing_missing.txt
*******************

Tab-delimited table with the same headers as :ref:`routing.txt <routingtxt>`.
The entries in the table are a subset of those in routing.txt and are only
included if...

TO DO: extend documentation of routing_missing.txt

The routing table is only generated when
:ref:`write routing table = 1 <writerouting>`.

.. _routingcolrow:

routing_colrow.txt
******************

to do

.. _lsmap:

LS.rst
******

Raster with the calculated :ref:`LS-factor <lsfactor>` (-). This
raster is only written if :ref:`write ls factor = 1 <writels>`.

.. _aspectmap:

AspectMap.rst
*************

Raster with the direction of slope (the aspect) in radians. This raster is only
written if :ref:`write aspect = 1 <writeaspect>`.

.. _slopemap:

SLOPE.rst
*********

Raster with the calculated slope in radians. This raster is only written if
:ref:`write slope = 1 <writeslope>`.

.. _upareamap:

UPAREA.rst
**********

Raster with the total upstream area :math:`(m^2)` for every pixel. This raster is only
written if :ref:`write upstream area = 1 <writeuparea>`.

.. _watemsedemoutput:

WaTEM-SEDEM output
##################

When WaTEM-SEDEM or the full CN-WS model is used, the following rasters and
tables can be written as output.

.. _totalsedimenttxt:

Total Sediment.txt
******************

Txt-file where the first rows give a summary of the results:

* Total erosion :math:`(kg)`: the total amount of sediment eroded in the landscape.
* Total deposition :math:`(kg)`: the total amount of sediment deposited in the landscape
  (not entering sewers or rivers)
* Sediment leaving the catchment, via the river :math:`(kg)`: the amount of sediment
  that enters all riverpixels
* Sediment leaving the catchment, not via the river :math:`(kg)`: the amount of sediment
  that enters pixels outside the modeldomain
  
if :ref:`Include buffers = 1 <includebuffers>` following row is added to the file: 

* Sediment trapped in buffers :math:`(kg)`: the amount of sediment that is trapped in
  all buffer basins.
  
if :ref:`Include sewers = 1 <inlcudesewers>` following row is added to the file: 

* Sediment entering sewer system :math:`(kg)`: the amount of sediment that enters the sewer pixels

After the above mentioned rows, a tab-seprated table where for every outlet
the amount of incoming sediment is reported.

An example output is given here:

.. literalinclude:: ../testfiles/molenbeek/modeloutput_ref/Total sediment.txt
    :language: vim

.. _totalsedimentsegmenttxt:

Total Sediment segments.txt
***************************

Tab-delimited table. Every row contains the id of a river segment and the total
amount of sediment :math:`(kg)` entering the segment.
This table is only generated when
:ref:`Output per river segment = 1 <outputsegment>`.

.. _cumsedsegmenttxt:

Cumulative sediment segments.txt
********************************

This table is only generated when
:ref:`Output per river segment = 1 <outputsegment>`.

.. _claycontentesedtxt:

Clay content sediment.txt
*************************

Tab-delimited table with the mean clay content (%) at every outlet. This table
is only generated when :ref:`estimate clay content = 1 <estimclay>`.

.. _claycontentesedsegmenttxt:

Clay content sediment segments.txt
**********************************

Tab-delimited table with the mean clay content (%) in every river segment. This
table is only generated when :ref:`estimate clay content = 1 <estimclay>` and
:ref:`Output per river segment = 1 <outputsegment>`.


.. _cumulativerst:

cumulative.rst
**************

TO DO

sewer_in.rst
************

Raster with the amount of sediment :math:`(kg)` that is trapped in every sewer pixel.
This raster is only generated when :ref:`Include sewers = 1 <inlcudesewers>`.

.. _sediexportrst:

SediExport_kg.rst
*****************

Raster with for every river cell the calculated amounts of sedimentinput :math:`(kg)`.
This raster is only written if :ref:`write sediment export = 1 <writesedexport>`.

.. _sedioutrst:

SediOut_kg.rst
**************

Raster with the amount of sediment :math:`(kg)` that leaves every pixel and is
distributed between the two target pixels.
This raster is only written if :ref:`write sediment export = 1 <writesedexport>`.

.. _sediinrst:

SediIn_kg.rst
*************

Raster with the amount of sediment :math:`(kg)` that enters a pixel from the upstream
pixels. This raster is only written if
:ref:`write sediment export = 1 <writesedexport>`.

.. _watereroskgrst:

WATEREROS (kg per gridcel).rst
******************************

Raster with the total amount of erosion or sedimentation in every pixel in :math:`(kg)`.
Negative values indicate erosion (i.e. Incoming Sediment + RUSLE < Capacity),
positive values indicate sedimentation (i.e. Incoming Sediment + RUSLE >
Capacity). This raster is only written if
:ref:`write water erosion = 1 <writerwatereros>`.

.. _watererosmmrst:

WATEREROS (mm per gridcel).rst
******************************

Raster with the total amount of erosion or sedimentation in every pixel in :math:`(mm)`.
Negative values indicate erosion (i.e. Incoming Sediment + RUSLE < Capacity),
positive values indicate sedimentation (i.e. Incoming Sediment + RUSLE >
Capacity). This raster is only written if
:ref:`write water erosion = 1 <writerwatereros>`.

.. _capacityrst:

Capacity.rst
************

Raster with the calculated transport capacity :math:`(kg.m^{-2})` for every
pixel. The values in this raster are calculated according the chosen formula for
the :ref:`transport capacity <tcmodel>`

.. _ruslerst:

RUSLE.rst
*********

Raster with the calculated RUSLE-values, the potential soil loss, for every
pixel in :math:`(kg.m^2)`. This raster is only written if
:ref:`write rusle = 1 <writerusle>`

TILEROS (mm per gridcel).rst
****************************

Raster with the calculated tillage erosion :math:`(mm.year^{-1})`. Negative values indicate
erosion, positive values give sedimentation.
This raster is only written if :ref:`calculate tillage erosion = 1 <calctileros>`.

TILEROS (kg per gridcel).rst
****************************

Raster with the calculated tillage erosion :math:`(kg.year^{-1})`. Negative values indicate
erosion, positive values give sedimentation.
This raster is only written if :ref:`calculate tillage erosion = 1 <calctileros>`.

SEDTIL_IN.rst
*************

Raster with the amount of sediment :math:`(kg)` due to tillage erosion that enters a
pixel from the upstream pixels.
This raster is only written if :ref:`calculate tillage erosion = 1 <calctileros>`.

SEDTIL_OUT.rst
**************

Raster with the amount of sediment :math:`(kg)` due to tillage erosion that leaves every
pixel and is distributed between the two target pixels.
This raster is only written if :ref:`calculate tillage erosion = 1 <calctileros>`.

.. _calibrationtxt:

Calibration.txt
***************

This file contains the same output as
:ref:`Total Sediment.txt <totalsedimenttxt>`, but for all ktc combinations defined
in the :ref:`Calibration-option <calibration>`. It is only writen when
:ref:`Calibratie = 1 <calibrate>`.

The txt-file contains a table, all columns seperated by ';'. The columns in the
table are: ktc_low, ktc_high, tot_erosion, tot_sedimentation, sed_river,
sed_noriver, sed_buffer, sed_openwater, outlet_1, outlet_2.

.. _CNoutput:

CN-output
#########

When the CN-module is enabled (:ref:`simple=0 <simple>`) it is possible to
generate some additional output.

Discharge.txt
*************

Table with discharge :math:`(m^3 s^{-1})` as a function of time for every outlet.

.. _dischargesegment:

Discharge_segments.txt
**********************

Table with discharge :math:`(m^3 s^{-1})` as a function of time for every river segment. This
table is only generated when :ref:`Output per river segment = 1 <outputsegment>`.

Sediment concentration.txt
**************************

Table with the concentration of sediment :math:`(g.l^{-1})` as a function of time for every
outlet.

.. _sedconcensegment:

Sediment concentration segments.txt
***********************************

Table with the concentration of sediment :math:`(g.l^{-1})` as a function of time for every
river segment.
This table is only generated when :ref:`Output per river segment = 1 <outputsegment>`.

Sediment.txt
************

Table with the sediment load :math:`(kg)` as a function of time for every outlet.

.. _sedsegmenttxt:

Sediment_segments.txt
*********************

Table with the sediment load :math:`(kg)` as a function of time for river segment.
This table is only generated when
:ref:`Output per river segment = 1 <outputsegment>`.

Spillover per buffer.txt
************************

Table with the amount of water :math:`(m^3)` that leaves every buffer basin via the
overflow.

Total discharge.txt
*******************

Table with the total amount of water :math:`(m^3)` that arives in every outlet after a
rainfall event.

.. _remaprst:

Remap.rst
*********

Raster with the total amount of discharge (=rainfall - infiltration) per pixel
for a rainfall event.

.. _totalrunofrst:

Total runoff.rst
****************

Raster with total runoff :math:`(m^3)` generated in every pixel during a rainfall event.
The value in every pixel is the sum of the amount of rainfall and the amount of
water flowing from upstream pixels, minus the infiltration in the pixel.
