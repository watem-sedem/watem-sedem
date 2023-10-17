.. _modelinput:

###########
Model input
###########

This page describes all the possible model inputs.

CN-WS accepts input files and **a lot of** parameters. Below, all possible input
layers and parameters are described.

.. _folders:

Folders
=======

.. _inputdir:

input directory
***************

Path to the directory where all the input files, used by the model, can be found. If the path does not
exist, an exception is raised and the model run will stop.

.. _outputdir:

output directory
****************

Path to the directory where all the model output files will be written. If the directory
does not exist, it is created by the model.

.. _files:

Files
=====

All input rasters must have
the same amount of columns, rows and cell size.
If one of the  input rasters has a different spatial extent, the model will
raise an exception and stop the execution. See
:ref:`the section on the format <rasterformat>` for more information.

.. _dtmmap:

DTM filename
************

The filename of the raster with a digital terrain model (DTM). This raster must
contain an elevation value (in meters)for every pixel inside the model domain or 
catchment.
The Idirisi raster must be formatted as float32.

.. note::
	CN-WS does not take nodata values (e.g. -9999) into account. When a nodata
	value in the DTM raster is encountered, it is considered as an elevation value.
	Consequently, slopes will be calculated wrongly. Thus, the user must ensure that all
	pixels in the model domain have an elevation value, and that at least two
	pixels outside the model domain have a valid elevation value.

.. _prcmap:

Parcel filename
***************

The filename of the Parcel or Land cover map. CN-WS requires information about
land cover
and parcel boundaries in the routing algorithm, but also when distributing the
sediment through the model domain. Every pixel in the model domain must therefore contain
a land cover value. Every value > 0 indicates a unique agricultural field. Meaning that
all pixels withing one agricultural field should have the same, unique value, while pixels belonging to 
another parcel should have a different value. These unique parcel values are important to 
define the routing within a parcel.

Pixels with a value < 0 indicate land cover that is not an agricultural field. In the table below the
different land cover classes are shown.


.. csv-table::
    :file: _static/csv/landcover_pixelid.csv
    :header-rows: 1
    :align: center

The definition of these unique parcel values are important to define the routing
within a parcel. Note that the data type of this raster is integer 16.

.. note::

    1. The Parcel raster can contain only values between -32757 and 32757.
    Therefore, only 32757 unique agricultural field id's can be used in the
    parcel map. When more parcel id's are necessary (e.g. in very large
    catchments), two or more agricultural fields can be assigned the same id.
    Theoretically, the model will consider these two parcels as a single
    parcel. In practice, these two parcels will never be treated as one
    because chance that these parcels are adjacent parcels is negligibly small.

    2. The concept of land use (agriculture, grass strips) and land cover
    (river, infrastructure, forest, pasture) are used interchangeably in this
    definition of the parcel raster. In this manual, we aim to define this as
    land cover rather than land use.

.. _sewermapfile:

Sewer map filename
******************

The filename of the sewer map. This raster is only mandatory if the :ref:`Model Choice <choicespage>`:
:ref:`Include sewers <inlcudesewers>` is set to 1.

All pixels in the sewer map should contain values between 0 and 1. These values represent
the fraction of the outgoing sediment in this pixel that is entering the sewer
system. A pixel with value 0 can be interpreted as a pixel where no sewer is
present. 

The datatype of the sewer map is float32.

.. _tildirmap:

Tillage direction filename
**************************

The filename of a raster with the tillage direction in degrees to the North.
This raster is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`Include tillage direction <includetillagedirection>` is set to 1.

The datatype of the tillage direction raster is float32.

.. _orientedroughnessmap:

Oriented roughness filename
***************************

The filename of a raster with the oriented roughness. The oriented roughness is the
height of the microrelief (in cm) due to ploughing. This raster is only mandatory 
if the :ref:`Model Choice <choicespage>`: :ref:`Include tillage direction <includetillagedirection>` is set to 1.

The datatype of the oriented roughness raster is float32.

.. _buffermap:

Buffer map filename
*******************

The filename of the buffer map. This raster is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`Include buffers <includebuffers>` is set to 1.

The figure below shows an example of a buffermap with three buffer basins. The outlet
of every buffer is marked with a buffer id (1, 2 and 3 in this example). The
other pixels belonging to the buffer get the
:ref:`extension id <bufferdata>`. All other pixels in the raster are set to
zero.

.. figure:: _static/png/buffermap.png
    :align: center

    Example of a buffermap with three buffer basins.

The datatype of the buffermap is integer16.

.. _ditchmap:

Ditch map filename
******************

The filename of the conductive ditch map. This raster is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`Include ditches <includeditches>` is set to 1.
See :ref:`further <routingmap>` for
more information on how to create these routing maps.

.. _dammap:

Dam map filename
****************

The filename of the conductive dam map. This raster is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`Include dams <includedams>` is set to 1. See :ref:`further <routingmap>` for more
information on how to create these routing map.

.. _pmap:

P factor map filename
*********************

The filename of the :ref:`P-factor <pfactor>` map. 
All pixels in the P factor map should contain values between 0 and 1. 

As CN-WS takes erosion control measures into account in several ways
(e.g. ditches, dams, buffers, grasstrips), it is not needed to get information
about the P-factors. Therefore, a good practice is to set the P-factor  
for all pixels within the catchment to 1 and all pixels
outside the catchment get value 0. 

The datatype of the raster is float32.

.. _riversegmentfile:

River segment filename
**********************

The filename of the river segment map. This raster is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`Output per river segment <outputsegment>` is set to 1.

A river segment is a part of the river (usually a part between two confluences of the river
with its tributaries). If detailed information about the sediment
entering every river segment is requested, the user can make use of the river segment map option. 

The river segment map is a raster where every river pixel (i.e. every pixel with value
-1 in the :ref:`parcel map <prcmap>`) gets the id of the segment where it
belongs to. Every segment has a unique (integer) id.

In the figure below, an example of a river segment map with seven segments is
given. All pixels which are no river pixels get the value 0.

.. figure:: _static/png/riversegment.png
    :align: center

    Example of a river segment map with seven segments.

The datatype of the river segment map is integer16.

.. _adjsegments:

adjectant segments
******************

The filename of the Tab separated table with adjectant  river segments. T
his table is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`River routing <riverrouting>` is set to 1. The table consists of two columns:
'from' and 'to'. Every row indicates a connection between two segments:
segment *from* flows into segment *to*. The values in the table represent the
segment-ids of the :ref:`river segment map <riversegmentfile>`.

Based on the example :ref:`river segment map <riversegmentfile>`, an example
table with adjectant river segments is displayed below:

.. table:: example adjectant segment file
    :align: center

    +-----+---+
    |from |to |
    +=====+===+
    |1    |3  |
    +-----+---+
    |2    |3  |
    +-----+---+
    |3    |5  |
    +-----+---+
    |4    |5  |
    +-----+---+
    |6    |2  |
    +-----+---+
    |7    |5  |
    +-----+---+

.. _upstrsegments:

upstream segments
*****************

The filename of the tab separated table with upstream segments. 
This table is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`River routing <riverrouting>` is set to 1. In the table three columns are present, namely:

- edge (integer): segment id of the receiving segment
- upstream edge (integer): segment id of one of the upstream segments of *edge*
- proportion (float, between 0 and 1): the fraction of the upstream segment that
  flows into the considered downstream segment. If the fraction is < 1, the
  upstream segment should flow into two downstream segments adding up to 1.

Based on the example :ref:`river segment map <riversegmentfile>`, an example
table with adjectant upstream segments is displayed below:

.. table:: example upstream segment file
    :align: center

    +-----+--------------+-----------+
    |edge |upstream edge |proportion |
    +=====+==============+===========+
    |2    |6             |1.0        |
    +-----+--------------+-----------+
    |3    |1             |1.0        |
    +-----+--------------+-----------+
    |3    |2             |1.0        |
    +-----+--------------+-----------+
    |3    |6             |1.0        |
    +-----+--------------+-----------+
    |5    |1             |1.0        |
    +-----+--------------+-----------+
    |5    |2             |1.0        |
    +-----+--------------+-----------+
    |5    |3             |1.0        |
    +-----+--------------+-----------+
    |5    |4             |1.0        |
    +-----+--------------+-----------+
    |5    |6             |1.0        |
    +-----+--------------+-----------+
    |5    |7             |1.0        |
    +-----+--------------+-----------+

.. _riverroutingmap:

river routing filename
**********************

The filename of the river routing map. This raster is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`River routing = 1 <riverrouting>` is set to 1.
See :ref:`further <routingmap>` for more information on how to create these routing maps.

.. _routingmap:

Routing maps
************

The routing algorithm of CN-WS can take into account rasters that impose a
single-flow routing along a line element in the
landscape as defined by the user. The
:ref:`river routing map <riverroutingmap>`, :ref:`ditchmap <ditchmap>` and
:ref:`dam map <dammap>` are made according to the principles described below.

A routing map contains integer values between 0 and 8. Every value indicates the
direction which the routing should follow. A pixel set to zero has no imposed routing.

Consider pixel X in the figure below. If the routing must flow from X to the
upper cardinal cell, pixel X will get value 1 in the routing map. If the routing
must flow from X to the lower left pixel, X will get value 6. All other
directions are set in the same way, according to the numbers in the figure.

.. figure:: _static/png/direction_routingmap.png
    :align: center

    Definition of flow routing.

An example of a routing map with two imposed routings is given here:

.. figure:: _static/png/routingmap.png
    :align: center

    Example of a routing map

The datatype of a routing raster is integer16.

.. _cnmap:

CN map filename
***************

The filename of the CN map. This raster is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`Only WS <simple>` is set to 0.

This raster contains a CN-value (between 0 and 100) for every pixel in the model
domain.

The datatype of the CN raster is float32.

.. _outletmap:

Outlet map filename
*******************

The filename of the outlet map. This raster is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`Manual outlet selection <manualoutlet>` is set to 1.

Every user defined river outlet needs a unique id (integers). The outlet pixels are given the value 
of their respective id's in the outlet map. All other pixels have a value equal to zero.

The datatype of the outlet map is integer16.

.. _ktilmap:

ktil map filename
*****************

The filename of the ktil map. The ktil map contains values for ktil, the transport
capacity coefficient for tillage erosion (see :ref:`here: <tillageerosionmodel>` for
more information about ktil an the tillage erosion model).
This raster is only mandatory when :ref:`Create ktil map = 0 <createktil>`.

The datatype of the ktil map is integer16.

.. _rainfallfile:

Rainfall filename
*****************

Filename of a textfile with rainfall values. The text file contains a table
(tab-delimited) with two columns without header. The first column contains the
time in minutes (starting from 0), the second column contains the rainfall in mm.
The rainfall of the first timestamp must be zero. 

.. _kmap:

K factor filename
*****************

The filename of the :ref:`K-factor <kfactor>` map. The soil erodibility factor or
K-factor of the RUSLE-equation for every pixel in the model domain is stored in
the K-factor map (in :math:`kg.h.MJ^{-1}.mm^{-1}`).

The datatype of the K-factor raster map is integer16.

.. _cmap:

C factor map filename
*********************

The filename  of the :ref:`C-factor <cfactor>` map. This raster contains values
between 0 and 1 and represent the dimensionless C-factor in the RUSLE equation.
Pixels outside the model domain are set to zero.

The datatype of the outlet map is float32.

.. _ktcmap:

ktc map filename
****************

The filename of the ktc map, a raster with transport capacity coefficients. This
raster is only mandatory if the :ref:`Model Choice <choicespage>`: :ref:`Create ktc map <createktc>` is set to 1.

The dataype of the ktc map is float32.

.. _variables:

Variables
=========

.. _sewerexit:

Sewer exit
**********

An integer value between 0 and 100 that represents the fration of the discharge
that enters the sewer system. It is only applied on pixels where the 
:ref:`sewer map <sewermapfile>` is not zero. 

This variable is only mandatory if the :ref:`Model Choice <choicespage>`: :ref:`Only WS = 0 <simple>` is set to 0.

.. note::
   1. The values stored in the :ref:`sewer map <sewermapfile>` are not used in the
   discharge calculations of the CN module. The sewer map is only used to check
   if a pixel is a sewer or not. 
   
   2. In the sediment calculations, a different trapping efficiency for every sewer
   pixel in the model can be defined, but this is not the case in the discharge
   calculations.

.. _claycontent:

Clay content parent material
****************************

The average fraction of clay in the soil of the modelled catchment (in
decimals; float32, between 0 and 1). This variable is only mandatory if the :ref:`Model Choice <choicespage>`: 
:ref:`estimate clay content <estimclay>` is set to 1.


.. _5dayrainfall:

5-day antecedent rainfall
*************************

The total rainfall (in mm) during 5 days before the start of the rainfall event.
This variable is only mandatory if the :ref:`Model Choice <choicespage>`: :ref:`Only WS = 0 <simple>` is set to 0.

.. _streamvelocity:

stream velocity
***************

As float, only mandatory if the :ref:`Model Choice <choicespage>`: :ref:`Only WS = 0 <simple>` is set to 0.

.. _alpha:

alpha
*****

Alpha (as float) is a calibration parameter of the CN-model. It determines the relation
between the runoff and the rainfall intensity. This parameter is only mandatory if the 
:ref:`Model Choice <choicespage>`: :ref:`Only WS <simple>` is set to 0.

.. _beta:

beta
****

Beta (as float) is a calibration parameter of the CN-model. It determines the
relation between the runoff and the antecedent rainfall. This parameter is 
only mandatory if the :ref:`Model Choice <choicespage>`: :ref:`Only WS <simple>` is set to 0.


.. _bulkdensity:

bulk density
************

The average bulk density (in :math:`kg.m{-3}`) of the soil in the catchment
(as integer). This value is used to convert the mass of the transported sediment to
volumes. A good default value for Flanders is 1350 kg/m³.

.. _rfactor_var:

R factor
********

The :ref:`R-factor <rfactor>` or rainfall erosivity factor in the RUSLE
equation (float, in :math:`MJ.mm.ha{-1}.h{-1}.year{-1}`).
This input is mandatory, except *except* if the :ref:`Model Choice <choicespage>`:
:ref:`Only routing <onlyrouting>` is set to 1.


.. note::
    1. the user must make sure that the R and C-factor are calculated for the same
    time span (year, month, week,...).

    2. R-factor values can be computed with the
    `R-factor Python package <https://cn-ws.github.io/rfactor/>`_.

.. _lscorrection:

LS correction
*************

Notebaert et al. (2005) describes that changes in spatial resolution have major
scaling effects on topographic variables like the :ref:`L and S-factor <lsfactor>`.

The LS-factor will
decrease on a higher resolution (smaller pixels, more height information) and
extreme LS values will occur more. To be able to compare the calculated RUSLE
values on different spatial resolutions, a correction factor can be calculated.
This correction factor :math:`LS_{cor}` is calculated as:

.. math::
    LS_{cor} = \frac{LS_{avg,x}}{LS_{avg,y}}

with

- :math:`LS_{avg,x}`: the average LS factor in a catchment on resolution x
- :math:`LS_{avg,y}`: the average LS factor in a catchment on resolution y

The input variable is a float (default value 1, i.e. no correction).
The LS-factor in the model is divided by this variable.

.. _nrbuffers:

Number of buffers
*****************

The amount of buffers present in the :ref:`buffer map <buffermap>` is given in
this parameter (as integer). The parameter is only mandatory if the
:ref:`Model Choice <choicespage>`: :ref:`Include buffers <includebuffers>` is set to 1.

.. _nrforcedrouting:

Number of forced routing
************************

The amount of locations where the user wants to force the routing is given by this
parameter (as integer). This variable defines the number of 
:ref:`forced routing sections <forcedroutingdata>` in the
ini-file. 
This variable is only mandatory if the :ref:`Model Choice <choicespage>`:
:ref:`Force Routing <forcerouting>` is set to 1.

.. _ktclow:

ktc low
*******

ktc low is the transport capacity coefficient (as float) for pixels with a low
erosion potential (see :ref:`ktc limit<ktclimit>`). 
The parameter is only mandatory if the :ref:`Model Choice <choicespage>`:
:ref:`Create ktc map <createktc>` is set to 1.

.. _ktchigh:

ktc high
********

ktc high is the transport capacity coefficient (float) for pixels with a high
erosion potential (see :ref:`ktc limit <ktclimit>`). The parameter is only mandatory if 
the :ref:`Model Choice <choicespage>`: :ref:`Create ktc map <createktc>` is set to 1.

.. _ktclimit:

ktc limit
*********

ktc limit is a threshold value (float). Pixels with a C-factor higher than
ktc limit will get the value of :ref:`ktc high <ktchigh>` in the ktc map,
pixels with a C-factor below ktc limit, will get :ref:`ktc low <ktclow>` in the
ktc map. This parameter is only mandatory if the :ref:`Model Choice <choicespage>`
:ref:`Create ktc map <createktc>` is set to 1 or :ref:`Calibrate = 1 <Calibrate>`


.. _ktildefault:

ktil default
************

The transport capacity coefficient for tillage erosion on agricultural fields. 
This value (as integer) should be expressed in :math:`kg.m{-1}.year{-1}`.
A recommended default value is :math:`600 kg.m{-1}.year{-1}`.

This parameter is only mandatory if the :ref:`Model Choice <choicespage>`:
:ref:`Create ktil map <createktil>` is set to 1.

.. _ktilthres:

ktil threshold
***************

ktil threshold is a float between 0 and 1. Pixels with a C-factor higher than
ktil threshold will get the value of :ref:`ktil default <ktildefault>` in the ktil map,
pixels with a C-factor below ktil threshold, are set to 0. A typical value for
ktil threshold is 0.01.

This parameter is only mandatory if the :ref:`Model Choice <choicespage>`:
:ref:`Create ktil map <createktil>` is set to 1.

.. _parcelconncrop:

Parcel connectivity cropland
****************************

The 'parcel connectivity cropland' expresses the reduction of the upstream area (:math:`A_{pixel}`)
at a parcel boundary. It is an integer value between 0 and 100. The reduction
on the upstream area is applied when the target pixel is of the land cover
'cropland' (:ref:`Parcel map value <prcmap>`: >0).

.. math::

      A_{pixel} =  A_{pixel}\frac{connectivity_{cropland}}{100}


.. _parcelconngras:

Parcel connectivity grasstrips
******************************

The 'parcel connectivity grasstrips' expresses the reduction of the upstream area (:math:`A_{pixel}`)
at the boundary between a parcel and a grasstrip. It is an integer value between 0
and 100. The reduction on the upstream area is applied when the target pixel is 
of the land cover 'grasstrip' (:ref:`Parcel map value <prcmap>`: -6). The default value for this parameter is 100.

.. math::

      A_{pixel} =  A_{pixel}\frac{connectivity_{grasstrip}}{100}


.. _parcelconnforest:

Parcel connectivity forest
**************************

The 'parcel connectivity forest' expresses the reduction of the upstream area (:math:`A_{pixel}`)
at a boundary of a forest. It is an integer value between 0 and 100. The
reduction on the upstream area is applied when the target pixel is of the
land cover 'forest' (:ref:`Parcel map value<prcmap>`: -3).

.. math::

      A_{pixel} =  A_{pixel}\frac{connectivity_{forest}}{100}


.. _parceltrapppingcrop:

Parcel trapping efficiency cropland
***********************************

The parcel trapping efficiency (PTEF) is used to compute the upstream area for
every raster pixel (:math:`A_{pixel}`) (see also :ref:`L-model <lmodel>`). The PTEF also
takes the land-use, defined by :ref:`the CN-WS parcels raster <prcmap>`, into account. 
This then, contributes to the upstream area by a
given percentage (100-PTEF). 

The parcel trapping efficiency for cropland is 
defined by the 'Parcel trapping efficiency cropland' (in % as integer; e.g. PTEF = 87).

.. math::

      A_{pixel} =  res^2(1-\frac{PTEF_{cropland}}{100})

.. _parceltrappingpasture:

Parcel trapping efficiency pasture
**********************************

The parcel trapping efficiency for pasture is defined by the 'Parcel trapping 
efficiency pasture' (in % as integer e.g. PTEF = 25). For a definition of the Parcel trapping
efficiency, see
:ref:`Parcel trapping efficiency cropland <parceltrapppingcrop>`

.. _parceltrappingforest:

Parcel trapping efficiency forest
**********************************

The parcel trapping efficiency for forest is defined by the 'Parcel trapping 
efficiency forest' (in % as integer e.g. PTEF = 25). For a definition of the Parcel trapping
efficiency, see
:ref:`Parcel trapping efficiency cropland <parceltrapppingcrop>`

.. _timestep:

Desired timestep for model
**************************

Runoff calculations are done using this timestep. The given timestep must comply
with the Courant Criterium. This criterium limits the timestep as a function of
the spatial resolution (m) and the stream velocity of water over land (m/s).

.. math::
    dt \leq \frac{spatial   resolution}{stream   velocity}

The parameter is an integer value expressed in minutes.

.. _finaltimestep:

Final timestep output
*********************

The user has the option to resample the time-dependent output (runoff, sediment
concentration, sediment load) to a different timestep than the
:ref:`Desired timestep <timestep>` of the model. The parameter is an integer value
expressed in minutes.

.. _endtime:

Endtime model
*************

Total timespan (in minutes) the model has to simulate. This parameter is an
integer value and must be a multiple of the :ref:`timestep <timestep>` of the
model.

.. note::
	In a first model run for a catchment with a given rainfall event, a large enough endtime should be given.
	This, in order to ensure that the
	whole runoff peak is modelled. After the first simulation, the model user
	can shorten the endtime to optimise the calculation time of the model.

.. _maxkernel:

max kernel
**********

If the routing algorithm of CN-WS encounters a local minimum in the
:ref:`digital elevation model, <dtmmap>` it will not find a lower, neighbouring
pixel. Therefore, the algorithm is set to search for a lower pixel within a search
radius around the local minimum (see :ref:`routing algorithm <onetarget>`. 
The variable 'max kernel' defines this search
radius expressed in pixels.

.. _maxkernelriver:

max kernel river
****************

If the routing algorithm of CN-WS encounters a local minimum in the
:ref:`digital elevation model <dtmmap>` it will not find a lower, neighbouring
pixel. If this pixel is a river pixel, the routing will remain in the river and
the routing will look within a search radius around the local minimum with the
same landuse (river). The variable 'max kernel river' defines the search radius
expressed in pixels.

.. _bufferdata:

Bufferdata
==========	

The inclusion of erosion control buffers is based on input rasters and
buffer parameters. How these input rasters should be created, is described
:ref:`here <buffermap>`. If the :ref:`Model Choice <choicespage>`:
:ref:`include buffers <includebuffers>` is set to 1,
the buffer parameters must be defined in the ini-file in the following manner:

.. code-block:: ini

    [Buffer 1]
    volume = 329.0
    height dam = 0.37
    height opening = 0
    opening area = 0.03
    discharge coefficient = 0.6
    width dam = 7
    trapping efficiency = 75
    extension id = 16385

    [Buffer 2]
    volume = 1123.0
    height dam = 1.5
    height opening = 0
    opening area = 0.03
    discharge coefficient = 0.6
    width dam = 7
    trapping efficiency = 75
    extension id = 16386

with:

 - volume: the maximum volume of water that can be trapped in the
   bufferbasin, :math:`V_{basin}` (:math:`m^{3}`).

 - height dam: the height of the dam of the buffer basin, :math:`H_{dam}`
   (:math:`m`).

 - height opening: the height of the opening of the discharge pipe of the
   basin, :math:`H_{opening}` (m).


 - opening area: the area of the discharge opening :math:`A_0` (:math:`m^{2}`).

 - discharge coefficient: the discharge coefficient :math:`C_d` (-) of the
   buffer basin.

 - width dam: the width of the overflow on the bufferbasin dam
   :math:`W_{dam}` (m).

 - trapping efficiency: the trapping efficiency is the fraction of the incoming
   sediment that is trapped.

 - extension id of a buffer is calculated as the buffer id + 16384. It is an
   integer value. All pixels of the buffer in the :ref:`buffer map <buffermap>`
   are given the value of the extension id, except the outlet pixel.

The extension id and trapping efficiency are mandatory for every buffer.
The other buffer parameters are only mandatory when the the CN-module seperately
(i.e. the :ref:`Model Choice <choicespage>`: :ref:`Only WS <simple>` is set to 0).

A full description of the CN calculation in buffers can be found
:ref:`here <bufferbasins>`.

.. note::
    The definition of the buffer extension id equal to buffer id + 16384,
    implies only 16384 can be modelled.

.. _forcedroutingdata:

Forced routing data
===================

In the case that the analysis of the routing and field validation shows that the
routing is defined incorrectly by the model, a forced routing from a specified source to target pixel can be defined by
the user. Forced routing is defined by stating the column and
row of both the source and target pixel in the ini-file, as shown here:

.. code-block:: ini

        [Forced Routing 1]
        from col = 10
        from row = 10
        target col = 11
        target row = 11

        [Forced Routing 2]
        from col = 15
        from row = 16
        target col = 20
        target row = 19

Note that the amount of sections with
forced routing vectors has to be defined with the variable
:ref:`Number of forced routing <nrforcedrouting>`

.. _calibrationparamters:

Calibration data
================

The following parameters are only mandatory if the :ref:`Model Choice <choicespage>`:
:ref:`Calibrate <Calibrate>` is set to 1 .
These parameters must be grouped in a seperate section in the ini-file with the
header 'Calibration', as shown here:

.. code-block:: ini

    [Calibration]
    KTcHigh_lower=1
    KTcHigh_upper=20
    KTcLow_lower=1
    KTcLow_upper=20
    steps=20

.. _ktchigh_lower:

KTcHigh_lower
*************

The lower range of ktc-high values in the calibration mode. The value is a float
and by default 5.

.. _ktchigh_upper:

KTcHigh_upper
*************

The upper range of ktc-high values in the calibration mode. The value is a float
and by default 40.

.. _ktclow_lower:

KTcLow_lower
*************

The lower range of ktc-low values in the calibration mode. The value is a float
and by default 1.

.. _ktclow_upper:

KTcLow_upper
*************

The upper range of ktc-low values in the calibration mode. The value is a float
and by default 20.

.. _steps:

steps
*****

The amount of steps between the lower and upper values for ktc low and ktc high
during a calibration run. This value is an integer and by default 12.

References
==========

Notebaert, B,. Govers, G.n Verstraeten, G., Van Oost, K., Ruysschaert, G.,
Poesen, J., Van Rompay, A. (2005): Verfijnde ersoiekaart Vlaanderen: eindrapport,
Departement Omgeving, Brussel, 53 pp.
