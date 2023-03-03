.. _modeloutput:

############
Model output
############

In this section we will describe all possible outputs of the CN-WS model. Tables
are written as tab-delimited txt-files, rasters are written as
`Idrisi-rasters <https://gdal.org/drivers/raster/Idrisi.html>`_ (or as
`Saga-rasters <https://gdal.org/drivers/raster/sdat.html>`_ if the
':ref:`saga grids <sagagrids>`' option is enabled).

The output returned by the model depends on some of the
:ref:`user choices <choicespage>` that can be defined in the ini-file.

.. _onlyroutingoutput:

Only Routing output
###################

Following output is generated if the model choice
‘:ref:`only routing <onlyrouting>`’ is enabled:

.. _routingtxt:

routing.txt
***********

Tab-delimited table, containing a row for every pixel in the spatial domain.
Following columns are present for every pixel (every pixel is a row in the
table):

* **col**: the vertical position of the pixel in the raster
* **row**: the horizontal position of the pixel in the raster
* **target1col**: the vertical position of the first target pixel in the raster.
  These values are -99 if target1 does not exist.
* **target1row**: the horizontal position of the first target pixel in the raster.
  These values are -99 if target1 does not exist.
* part1: the relative amount of outgoing sediment/water to the first target
  pixel
* distance1: the distance (in m) between the source pixel and the first target
  pixel
* **target2col**: the vertical position of the second target pixel in the raster.
  These values are -99 if target2 does not exist.
* **target2row**: the horizontal position of the second target pixel in the raster.
  These values are -99 if target2 does not exist.
* part2: the relative amount of outgoing sediment/water to the second target
  pixel. *The sum of part1 and part2 must equal 1.*
* distance2: the distance (in m) between the source pixel and the second target
  pixel

The routing table will only be generated if the model choice
':ref:`write routing table <writerouting>`' is enabled.

An example of (a part) of a routing.txt is given below

.. literalinclude:: ../testfiles/molenbeek/modeloutput_ref/routing.txt
    :language: text
    :lines: 1-15

See :ref:`the section on grid coordinates <gridcoordinates>` for more
information on the orientation of the rows and columns in this file.

.. _missingroutingtxt:

routing_missing.txt
*******************

Tab-delimited table with the same headers as :ref:`routing.txt <routingtxt>`.
The records in this table are a subset of those in routing.txt and are only
included if they were not treated in te model after the routing is calculated.
This output is intended as debugging information for the model. Ideally, this
file is empty. 

This table will only be generated if the model choice
':ref:`write routing table <writerouting>`' is enabled.

See :ref:`the section on grid coordinates <gridcoordinates>` for more
information on the orientation of the rows and columns in this file.

.. _routingcolrow:

routing_rowcol.txt
******************

Tab-delimited table. Every row represents a pixel coordinate (column + row).
The pixels are sorted according to the sequence they are treated in the modelrun:
i.e. the first record int the table is the pixel treated first in the modelrun,
the last record of the table is the last treated pixel of the modelrun.
Together with the :ref:`routing.txt <routingtxt>`, the routing applied by the model
can be fully reconstructed.

This table will only be generated if the model choice
':ref:`write routing table <writerouting>`' is enabled.

An example of (a part) of a routing_rowcol.txt is given below

.. literalinclude:: ../testfiles/molenbeek/modeloutput_ref/routing_rowcol.txt
    :language: text
    :lines: 1-15

See :ref:`the section on grid coordinates <gridcoordinates>` for more
information on the orientation of the rows and columns in this file.

.. _lsmap:

LS.rst
******

Raster with the calculated :ref:`LS-factor <lsfactor>` (-). This
raster will only be written if the model choice ':ref:`write ls factor <writels>`' is enabled.

.. _aspectmap:

AspectMap.rst
*************

Raster with the direction of slope (the aspect) in radians. This raster will only be
written if the model choice ':ref:`write aspect <writeaspect>`' is enabled.

.. _slopemap:

SLOPE.rst
*********

Raster with the calculated slope in radians. This raster will only be written if the model choice
':ref:`write slope<writeslope>`' is enabled.

.. _upareamap:

UPAREA.rst
**********

Raster with the total upstream area :math:`(m^2)` for every pixel. This raster will only be
written if the model choice ':ref:`write upstream area <writeuparea>`' is enabled.

.. _watemsedemoutput:

WaTEM/SEDEM output
##################

When only the WaTEM/SEDEM module (see :ref:`Only WS <simple>`) or if the full CN-WS model is used, the following rasters and
tables are written as output.

.. _totalsedimenttxt:

Total Sediment.txt
******************

Txt-file where the first rows give a summary of the results:

* Total erosion :math:`(kg)`: the total amount of sediment eroded in the landscape.
* Total deposition :math:`(kg)`: the total amount of sediment deposited in the landscape
  (not entering sewers or rivers)
* Sediment leaving the catchment, via the river :math:`(kg)`: the amount of sediment
  that enters any of the river pixels
* Sediment leaving the catchment, not via the river :math:`(kg)`: the amount of sediment
  that exits the model domain (not via river pixels)
  
if the model choice ':ref:`Include buffers <includebuffers>`' is enabled, following row is added to the file: 

* Sediment trapped in buffers :math:`(kg)`: the amount of sediment that is trapped in
  all buffer basins.
  
if the model choice ':ref:`Include sewers <inlcudesewers>`' is enabled, following row is added to the file: 

* Sediment entering sewer system :math:`(kg)`: the amount of sediment that enters the sewer pixels

After the above mentioned rows, a tab-separated table where for every outlet
the amount of incoming sediment is reported.

An example output is given here:

.. literalinclude:: ../testfiles/molenbeek/modeloutput_ref/Total sediment.txt
    :language: text

.. _totalsedimentsegmenttxt:

Total Sediment segments.txt
***************************

Tab-delimited table. Every row contains the id of a river segment and the total
amount of sediment :math:`(kg)` entering the segment.
This table is only generated if the model choice
':ref:`Output per river segment <outputsegment>`' is enabled.

An example output is given here:

.. literalinclude:: ../testfiles/molenbeek/modeloutput_ref/Total sediment segments.txt
    :language: text

.. _cumsedsegmenttxt:

Cumulative sediment segments.txt
********************************


Tab-delimited table. Every row contains the id of a river segment and the cumulative
amount of sediment transported from all upstream segments towards this segment.
This table is only generated if the model choice
':ref:`Output per river segment <outputsegment>`' is enabled.

.. _claycontentesedtxt:

Clay content sediment.txt
*************************

Tab-delimited table with the estimated mean clay content (in %) at every outlet. This table
is only generated when if the model choice ':ref:`estimate clay content <estimclay>`' is enabled.

.. _claycontentesedsegmenttxt:

Clay content sediment segments.txt
**********************************

Tab-delimited table with the estimated mean clay content (in %) entering every river segment. This
table is only generated if the model choices ':ref:`estimate clay content <estimclay>`' and
':ref:`Output per river segment <outputsegment>`' are enabled.

.. _cumulativerst:

cumulative.rst
**************

This raster is only written if the model choice ':ref:`River routing <riverrouting>`' is enabled.

.. _sewerinrst:

sewer_in.rst
************

Raster with the amount of sediment :math:`(in kg)` trapped in every sewer pixel.
This raster is only generated if the model choice ':ref:`Include sewers <inlcudesewers>`' is enabled.

.. _sediexportrst:

SediExport_kg.rst
*****************

Raster with the calculated amounts of sediment input :math:`(kg)` for every river cell.
This raster is only written if the model choice ':ref:`write sediment export <writesedexport>`' is enabled.

.. _sedioutrst:

SediOut_kg.rst
**************

Raster with the amount of sediment :math:`(kg)` that leaves every pixel and is
distributed between its target pixels.
This raster is only written if the model choise ':ref:`write sediment export <writesedexport>`' is enabled.

.. _sediinrst:

SediIn_kg.rst
*************

Raster with the amount of sediment :math:`(kg)` that enters a pixel from the upstream
pixels. This raster is only written if the model choice
':ref:`write sediment export <writesedexport>`' is enabled.

.. _watereroskgrst:

WATEREROS (kg per gridcel).rst
******************************

Raster with the net erosion or sedimentation in every pixel in
(in :math:`kg`). Negative values indicate erosion, while positive values indicate
sedimentation. This raster is only written if the model choice 
':ref:`write water erosion <writerwatereros>`' is enabled. See the
:ref:`concept of WaTEM-SEDEM <Concept>` for more information on the calculation of
this value.

.. _watererosmmrst:

WATEREROS (mm per gridcel).rst
******************************

Raster with net erosion or sedimentation in every pixel in
(in :math:`mm`). Negative values indicate erosion, while positive values indicate
sedimentation. This raster is only written if the model choice
':ref:`write water erosion <writerwatereros>`' is en enabled. See the
:ref:`concept of WaTEM-SEDEM <Concept>` for more information on the calculation of
this value.

.. _capacityrst:

Capacity.rst
************

Raster with the calculated transport capacity (TC) (in :math:` kg pixel^{-1} year^{-1}`) for every
pixel. The values in this raster are calculated according the chosen formula in the model choice ':ref:`transport capacity <tcmodel>`'.

.. _ruslerst:

RUSLE.rst
*********

Raster with the calculated RUSLE-values, the potential soil loss, for every
pixel in (in:math:` kg m^{-2} year^{-1}`). This raster is only written if the model choice
':ref:`write rusle <writerusle>`' is enabled.

TILEROS (mm per gridcel).rst
****************************

Raster with the calculated tillage erosion (in :math:`mm year^{-1}`). Negative values indicate
erosion, while positive values indicate sedimentation.
This raster is only written if the model choice ':ref:`calculate tillage erosion <calctileros>`' is enabled.

TILEROS (kg per gridcel).rst
****************************

Raster with the calculated tillage erosion (in :math:`kg year^{-1}`). Negative values indicate
erosion, positive values indicate sedimentation.
This raster is only written if the model choice ':ref:`calculate tillage erosion<calctileros>`' is enabled.

SEDTIL_IN.rst
*************

Raster with the amount of sediment (in :math:`kg`) that enters a
pixel from the upstream pixels, due to tillage erosion.
This raster is only written if the model choice ':ref:`calculate tillage erosion <calctileros>`' is enabled.

SEDTIL_OUT.rst
**************

Raster with the amount of sediment (in :math:`kg`) that leaves every
pixel and is distributed between its target pixels, due to tillage erosion.
This raster is only written if the model choice ':ref:`calculate tillage erosion <calctileros>`' is enabled.

.. _calibrationtxt:

Calibration.txt
***************

This file contains the same output as
:ref:`Total Sediment.txt <totalsedimenttxt>`, but for all possible ktc combinations defined
in the :ref:`Calibration-option <calibration>`. This output is only written if the model choice
':ref:`Calibrate <calibrate>`' is enabled.

This txt-file contains a ;-seperated table. The columns in the
table are: ktc_low, ktc_high, tot_erosion, tot_sedimentation, sed_river,
sed_noriver, sed_buffer, sed_openwater, outlet_1, outlet_2.

An example output where ktc_low and ktc_high were varied from 1 to 5 is given here:

.. literalinclude:: ../testfiles/molenbeek/modeloutput_ref/calibration.txt
    :language: text
    :lines: 1,28-32,53-56,77-79,100-101,122

.. _CNoutput:

CN-output
#########

When the CN-module is enabled (i.e. :ref:`Only WS <simple>` is disabled) some additional output is generated.

Discharge.txt
*************

Table with discharge (in :math:`m^3 s^{-1}`) as a function of time for every outlet.

.. _dischargesegment:

Discharge_segments.txt
**********************

Table with discharge (in :math:`m^3 s^{-1}`) as a function of time for every river segment. This
table is only generated if the model choice ':ref:`Output per river segment <outputsegment>`' is enabled.

Sediment concentration.txt
**************************

Table with the concentration of sediment (in :math:`g.l^{-1}`) as a function of time for every
outlet.

.. _sedconcensegment:

Sediment concentration segments.txt
***********************************

Table with the concentration of sediment (in :math:`g.l^{-1}`) as a function of time for every
river segment.
This table is only generated if the model choice ':ref:`Output per river segment <outputsegment>`' is enabled.

Sediment.txt
************

Table with the sediment load (in :math:`kg`) as a function of time for every outlet.

.. _sedsegmenttxt:

Sediment_segments.txt
*********************

Table with the sediment load (in :math:`kg`) as a function of time for the river
segments. This table is only generated if the model choice
':ref:`Output per river segment = 1 <outputsegment>`' is enabled.

Spillover per buffer.txt
************************

Table with the total amount of water (in :math:`m^3`) that leaves every buffer basin via the
overflow.

Total discharge.txt
*******************

Table with the total amount of water (in :math:`m^3`) that arrives in every outlet
after a rainfall event.

.. _remaprst:

Remap.rst
*********

Rainfall Excess Raster, a raster with the total amount of discharge (in :math:`m^3 s^{-1}`) per pixel
for a rainfall event. Here the discharce is defined by the amount of rainfall minus the ammount of infiltration.

.. _totalrunofrst:

Total runoff.rst
****************

Raster with total runoff (in :math:`m^3`) generated in every pixel during a rainfall event.
The value in every pixel is the sum of the amount of rainfall and the amount of
water flowing from upstream pixels, minus the infiltration in the pixel.
