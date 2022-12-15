.. _modelinput:

###########
Model input
###########

This page describes all possible model input.

CN-WS accepts input files and **a lot** of parameters. All these inputs are
described below.

.. _folders:

Folders
=======

input directory
***************

Path to the directory where all input files can be found. If the path does not
exist, an exception is raised and the model run will stop.

output directory
****************

Path to the directory where all model output will be written. If the directory
does not exist, it is created by the model.

.. _files:

Files
=====

All input rasters must have
the same amount of columns, rows and cell size.
If one of the  input rasters has a different spatial extent, the model will
raise an exception and will stop the execution. See
:ref:`the section on the format <rasterformat>` for more information.

.. _dtmmap:

DTM filename
************

Filename of the raster with a digital terrain model (DTM). This raster contains
at least for every pixel inside the model domain or catchment an elevation value
in meters.
The idirisi raster must be formatted as float32.

.. note::
	CN-WS does not take nodata values (e.g. -9999) into account. When a nodata
	value in the DTM raster is encountered, it is considered as an elevation.
	Consequently, slopes are calculated wrong. Thus, the user must ensure all
	pixels in the model domain have an elevation value, and that at least two
	pixels outside the model domain have a valid elevation value.

.. _prcmap:

Parcel filename
***************

Filename of the Parcel or Land cover map. CN-WS requires information about
land cover
and parcel boundaries in the routing algorithm, but also when distributing the
sediment through the model domain. Every pixel in the model domain must contain
a land cover value. Every value > 0 indicates a unique agricultural field. So,
all
pixels of an agricultural field have the same value and pixels belonging to a
different parcel have different value.
The definition of these unique parcel values are important to define the routing
within a parcel. Note that the data type of this raster is integer 16.

.. csv-table::
    :file: _static/csv/landcover_pixelid.csv
    :header-rows: 1
    :align: center

.. note::

    1. The Parcel raster can contain only values between -32757 and 32757.
    Therefore, only 32757 unique agricultural field id's can be used in the
    parcel map. When more parcel id's are necessary (e.g. in very large
    catchments), you can give two or more agricultural fields the same id.
    Theoretically, the model will consider these two parcels as a single
    parcel. In practice, these two parcels will never be treated as one
    because chance that these parcels are adjacent parcels is negligibly small.

    2. The concept of land use (agriculture, grass strips) and land cover
    (river, infrastructure, forest, pasture) are used interchangeably in the
    definition of the parcel raster. In this manual, we aim to define this as
    land cover rather than land use.

.. _sewermapfile:

Sewer map filename
******************

Filename of the sewer map. This raster is only mandatory when
:ref:`Include sewers = 1 <inlcudesewers>`.

All pixels in the sewer map contain values between 0 and 1. The value represents
the fraction of the outgoing sediment in this pixel that is entering the sewer
system. A pixel with value 0 can be interpreted as a pixel where no sewer is
present. 

The datatype of the sewer map is float32.

.. _tildirmap:

Tillage direction filename
**************************

Filename of a raster with the tillage direction in degrees to the North.

The datatype of the tillage direction raster is float32.

.. _orientedroughnessmap:

Oriented roughness filename
***************************

Filename of a raster with the oriented roughness. The oriented roughness is the
height of the microrelief (in cm) due to ploughing.

The datatype of the oriented roughness raster is float32.

.. _buffermap:

Buffer map filename
*******************

Filename of the buffer map. This raster is only mandatory when
:ref:`Include buffers = 1 <includebuffers>`.

The figure shows an example of a buffermap with three buffer basins. The outlet
of every buffer is marked with the buffer id (1, 2 and 3 in this example). The
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

Filename of the conductive ditch map. This raster is only mandatory when
:ref:`Include ditches = 1 <includeditches>`. See :ref:`here <routingmap>` for
more information on how to create this map.

.. _dammap:

Dam map filename
****************

Filename of the conductive dam map. This raster is only mandatory when
:ref:`Include dams = 1 <includedams>` See :ref:`here <routingmap>` for more
information on how to create this map.

.. _pmap:

P factor map filename
*********************

Filename of the :ref:`P-factor <pfactor>` map. 

The datatype of the raster is float32.

.. _riversegmentfile:

River segment filename
**********************

Filename of the river segment map. This raster is only mandatory when
:ref:`Output per river segment = 1 <outputsegment>`.

A river segment is a part of the river (usually a part between two confluences
with other rivers). To give detailed information about the sediment
entering every river segment, a river segment map must be created. 

The river segment map is a raster where every river pixel (every pixel with value
-1 in the :ref:`parcel map <prcmap>`) gets the id of the segment where it
belongs to. Every segment has a unique (integer) id.

In the figure below, an example of a river segment map with seven segments is
given. All pixels which are no river pixels get the value 0.

.. figure:: _static/png/riversegment.png
    :align: center

    Example of a river segment map with seven segments.

The datatype of the river segment map is integer16. The segments id's in the
context of Flanders are defined by the VHA (see also note
:ref:`here <outputsegment>`).

.. _adjsegments:

adjectant segments
******************

Table with adjectant  river segments. This table is only mandatory when
:ref:`River routing = 1 <riverrouting>`. The table consists out of two columns:
from and to. Every row indicates a connection between two segments:
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

Table with upstream segments. This table is only mandatory when
:ref:`River routing = 1 <riverrouting>`. In the table three columns are present:

- edge (integer): segment id
- upstream edge (integer): segment id of one of the upstream segments of *edge*
- proportion (float, between 0 and 1): the fraction of the upstream segment that
  flows into the considered downstream segment. If the fraction is < 1, the
  upstream segment flows into two downstream segments.

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

Filename of the river routing map. This raster is only mandatory when
:ref:`River routing = 1 <riverrouting>`. See :ref:`here <routingmap>` for more
information on how to create this map.

.. _routingmap:

Routing maps
************

The routing algorithm of CN-WS can take into account rasters that impose a
single-flow routing along a line element in the
landscape as defined by the user. The
:ref:`river routing map <riverroutingmap>`, :ref:`ditchmap <ditchmap>` and
:ref:`dam map <dammap>` are made according to the principles described here.

A routing map contains integer values between 0 and 8. Every value indicates the
direction the routing will follow. A pixel set to zero has no imposed routing.

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

Filename of the CN map. This raster is only mandatory when
:ref:`simple = 0 <simple>`.

This raster contains a CN-value (between 0 and 100) for every pixel in the model
domain.

The datatype of the CN raster is float32.

.. _outletmap:

Outlet map filename
*******************

Filename of the outlet map. This raster is only mandatory when
:ref:`Manual outlet selection = 1 <manualoutlet>`.

Every outlet pixel needs a unique id. These integer id's are stored in the outlet
map. All other pixels are zero.

The datatype of the outlet map is integer16.

.. _ktilmap:

ktil map filename
*****************

Filename of the ktil map. The ktil map contains values for ktil, the transport
capacity coefficient for tillage erosion.
This raster is only mandatory when :ref:`Create ktil map = 0 <createktil>`.

The datatype of the ktil map is integer16.

.. _rainfallfile:

Rainfall filename
*****************

Filename of a textfile with rainfall values. The text file contains a table
(tab-delimited) with two columns without header. The first column contains the
time in minutes (starting from 0), the second column contains the rainfall in mm.

.. _kmap:

K factor filename
*****************

Filename of the :ref:`K-factor <kfactor>` map. The soil erodibility factor or
K-factor of the RUSLE-equation for every pixel in the modeldomain is stored in
the K-factor map (kg.h/MJ.mm).

The datatype of the K-factor raster map is int16.

.. _cmap:

C factor map filename
*********************

Filename of the :ref:`C-factor <cfactor>` map. This raster contains values
between 0 and 1 and represent the dimensionless C-factor in the RUSLE equation.
Pixels outside the modeldomain are set to zero.

The datatype of the outlet map is float32.

.. _ktcmap:

ktc map filename
****************

Filename of the ktc map, a raster with transport capacity coefficients. This
raster is only mandatory when :ref:`Create ktc map = 0 <createktc>`.

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

This variable is only mandatory when :ref:`simple = 0 <simple>`.

.. note::
   1. The values stored in the :ref:`sewer map <sewermapfile>` are not used in the
   discharge calculations of the CN module. The sewer map is only used to check
   if a pixel is a sewer or not. 
   
   2. In the sediment calculations a different trapping efficiency for every sewer
   pixel in the model can be defined, but this is not the case in the discharge
   calculations.

.. _claycontent:

Clay content parent material
****************************

The average fraction of clay in the soil in the modelled catchment in
percentages (float, between 0 and 1). This variable is only mandatory when
:ref:`estimate clay content <estimclay>` is enabled.

.. _5dayrainfall:

5 day antecedent rainfall
*************************

The total rainfall (in mm) during 5 days before the start of the rainfall event.
This variable is only mandatory when :ref:`simple = 0 <simple>`.

.. _streamvelocity:

stream velocity
***************

Float, mandatory when :ref:`simple = 0 <simple>`

.. _alpha:

alpha
*****

Alpha (float) is a calibration parameter of the CN-model. It determines the relation
between runoff and rainfall intensity. The parameter is only mandatory when
:ref:`simple = 0 <simple>`

.. _beta:

beta
****

Beta (float) is a calibration parameter of the CN-model. It determines the
relation between runoff and antecedent rainfall. The parameter is only mandatory
when :ref:`simple = 0 <simple>`

.. _bulkdensity:

bulk density
************

The average bulk density (in kg/m³) of the soil in the catchment (integer). This
value is used to convert the mass of transported sediment to volumes. A good
default value for Belgium is 1350 kg/m³.

.. _rfactor_var:

R factor
********

The :ref:`R-factor <rfactor>` or rainfall erosivity factor in the RUSLE
equation (float, in MJ.mm/ha.h.year). This input is mandatory, except when
:ref:`Only routing <onlyrouting>` is used.


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
This correction factor :math:`LS_{cor}` is calculated as

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
this parameter (integer). The parameter is only mandatory when
:ref:`Include buffers = 1 <includebuffers>`.

.. _nrforcedrouting:

Number of forced routing
************************

The amount of locations where the user wants to force the routing is given by this
parameter (integer).
This is only mandatory when :ref:`Force Routing = 1 <forcerouting>`

.. _ktclow:

ktc low
*******

ktc low is the transport capacity coefficient (float) for pixels with a low
erosion potential. The parameter is only mandatory when
:ref:`Create ktc map = 1 <createktc>`.

.. _ktchigh:

ktc high
********

ktc high is the transport capacity coefficient (float) for pixels with a high
erosion potential. The parameter is only mandatory when
:ref:`Create ktc map = 1 <createktc>`.

.. _ktclimit:

ktc limit
*********

ktc limit is a threshold value (float). Pixels with a C-factor higher than
ktc limit will get :ref:`ktc high <ktchigh>` in the ktc map,
pixels with a C-factor below ktc limit, will get :ref:`ktc low <ktclow>` in the
ktc map. This parameter is only mandatory when
:ref:`Create ktc map = 0 <createktc>` or :ref:`Calibrate = 1 <Calibrate>`

.. _ktildefault:

ktil default
************

The transport capacity coefficient for tillage erosion on agricultural fields. 
The integer value is expressed in kg/m/year. A recommended default value is
600 kg/m/year.

This parameter is only mandatory when :ref:`Create ktil map = 1 <createktil>`

.. _ktilthres:

ktil threshold
***************

ktil threshold is a float between 0 and 1. Pixels with a C-factor higher as
ktil threshold will get :ref:`ktil default <ktildefault>` in the ktil map,
pixels with a C-factor below ktil threshold, are set to 0. A typical value for
ktil threshold is 0.01.

ktil threshold is only mandatory when :ref:`Create ktil map = 1 <createktil>`.

.. _parcelconncrop:

Parcel connectivity cropland
****************************

The parcel connectivity cropland expresses the reduction of the upstream area
at a parcel boundary. It is an integer value between 0 and 100. The reduction
on the upstream area is applied when the target pixel is of the land cover
'cropland'.

.. math::

      A_{pixel} =  A_{pixel}\frac{connectivity_{cropland}}{100}


.. _parcelconngras:

Parcel connectivity grasstrips
******************************

The parcel connectivity grasstrips expresses the reduction of the upstream area
at boundary between a parcel and a grasstrip. It is an integer value between 0 
and 100. The reduction on the upstream area is applied when the target pixel is 
of the land cover 'grasstrip' (-6). The default value for this parameter is 100.

.. math::

      A_{pixel} =  A_{pixel}\frac{connectivity_{grasstrip}}{100}


.. _parcelconnforest:

Parcel connectivity forest
**************************

The parcel connectivity cropland expresses the reduction of the upstream area
at a boundary of a forest. It is an integer value between 0 and 100. The
reduction on the upstream area is applied when the target pixel is of the
land cover 'forest':

.. math::

      A_{pixel} =  A_{pixel}\frac{connectivity_{forest}}{100}


.. _parceltrapppingcrop:

Parcel trapping efficiency cropland
***********************************

The parcel trapping efficiency (PTEF) is used to compute the upstream area for
every raster pixel :math:`A` (see also :ref:`L-model <lmodel>`). The PTEF 
takes into account the land-use defined in :ref:`the CN-WS parcels raster <prcmap>`:
as a function of the land-use. This will contribute to the upstream area with a
given percentage (100-PTEF). The parcel trapping efficiency for cropland is 
defined by the Parcel trapping efficiency cropland (% as int e.g. 87).

.. math::

      A_{pixel} =  res^2(1-\frac{PTEF_{cropland}}{100})

.. _parceltrappingpasture:

Parcel trapping efficiency pasture
**********************************

The parcel trapping efficiency for pasture is defined by the Parcel trapping 
efficiency pasture (% as int e.g. 25). For a definition of the Parcel trapping
efficiency, see
:ref:`Parcel trapping efficiency cropland <parceltrapppingcrop>`

.. _parceltrappingforest:

Parcel trapping efficiency forest
**********************************

The parcel trapping efficiency for forest is defined by the Parcel trapping 
efficiency forest (% as int e.g. 25). For a definition of the Parcel trapping
efficiency, see
:ref:`Parcel trapping efficiency cropland <parceltrapppingcrop>`

.. _timestep:

Desired timestep for model
**************************

Runoff calculations are done with this timestep. The chosen timestep must comply
with the Courant Criterium. This criterium limits the timestep as a function of
the spatial resolution (m) and the stream velocity of water over land (m/s).

.. math::
    dt \leq \frac{spatial resolution}{stream velocity}

The parameter is an integer value expressed in minutes.

Final timestep output
*********************

The user has the option to resample the time-dependent output (runoff, sediment
concentration, sediment load) to a different timestep than the
:ref:`timestep <timestep>` of the model. The parameter is an integer value
expressed in minutes.

.. _endtime:

Endtime model
*************

Total timespan (in minutes) the model has to simulate. This parameter is an
integer value and must be a multiple of the :ref:`timestep <timestep>` of the
model.

.. note::
	In a first model run for a catchment with a given rainfall event, the user
	must choose the endtime large enough. By doing this, he makes sure the the
	whole runoff peak is modelled. After this first simulation, the model user
	can deminish the endtime to optimise the calculation time of the model.

.. _maxkernel:

max kernel
**********

If the routing algorithm of CN-WS encounters a local minimum in the
:ref:`digital elevation model, <dtmmap>` it will not find a lower, neighbouring
pixel. Therefore, the algorithm will search for a lower pixel within a search
radius around the local minimum. The variable 'max kernel' defines the search
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
buffer parameters. The generation of the input rasters is described
:ref:`here <buffermap>`. The buffer parameters must be defined in the
ini-file when :ref:`include buffers = 1 <includebuffers>`.

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
   bufferbasin, :math:`V_{basin}` (:math:`m^{3}`). This parameter is only
   mandatory when using the CN-module (i.e. :ref:`simple = 0 <simple>`).

 - height dam: the height of the dam of the buffer basin, :math:`H_{dam}`
   (m). This parameter is only mandatory when using the CN-module (i.e.
   :ref:`simple = 0 <simple>`).

 - height opening: the height of the opening of the discharge pipe of the
   basin, :math:`H_{opening}` (m). This parameter is only mandatory when using
   the CN-module (i.e. :ref:`simple = 0 <simple>`).

 - opening area: the area of the discharge opening :math:`A_0` (:math:`m^{2}`).
   This parameter is only mandatory when using the CN-module (i.e.
   :ref:`simple = 0 <simple>`).

 - discharge coefficient: the discharge coefficient :math:`C_d` (-) of the
   buffer basin. This parameter is only mandatory when using the CN-module
   (i.e.:ref:`simple = 0 <simple>`).

 - width dam: the width of the overflow on the bufferbasin dam
   :math:`W_{dam}` (m). This parameter is only mandatory when using the
   CN-module (i.e. :ref:`simple = 0 <simple>`).

 - trapping efficiency: the trapping efficiency is the fraction of the incoming
   sediment that is trapped.

 - extension id of a buffer is calculated as the buffer id + 16384. It is an
   integer value. All pixels of the buffer in the :ref:`buffer map <buffermap>`
   are given the value of the extension id, except the outlet pixel.

A full description about the CN calculation in buffers can be found
:ref:`here <bufferbasins>`.

.. note::
    The definition of the buffer extension id equal to buffer id + 16384,
    implies only 16384 can be modelled.

.. _forcedroutingdata:

Forced routing data
===================

A forced routing from a specified source to target pixel can be defined by
the user, if the analysis of the routing and field validation shows that the
routing is defined incorrectly. Forced routing is defined by the column and
row of both the source and target pixel as follows:

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

These lines are added to the ini-file. Note that the amount of sections with
forced routing vectors is defined by the variable
:ref:`Number of forced routing <nrforcedrouting>`

.. _calibrationparamters:

Calibration data
================

The following parameters are only mandatory when :ref:`Calibrate=1 <calibrate>`.
These parameters must be grouped in a seperate section in the ini-file with the
header 'Calibration':

.. code-block:: ini

    [Calibration]
    KTcHigh_lower=1
    KTcHigh_upper=20
    KTcLow_lower=1
    KTcLow_upper=20
    steps=20

KTcHigh_lower
*************

The lower range of ktc-high values in the calibration mode. The value is a float
and by default 5.

KTcHigh_upper
*************

The upper range of ktc-high values in the calibration mode. The value is a float
and by default 40.

KTcLow_lower
*************

The lower range of ktc-low values in the calibration mode. The value is a float
and by default 1.

KTcLow_upper
*************

The upper range of ktc-low values in the calibration mode. The value is a float
and by default 1.

steps
*****

The amount of steps between the lower and upper values for ktc low and ktc high
during a calibration run. This value is an integer and by default 12.

References
==========

Notebaert, B,. Govers, G.n Verstraeten, G., Van Oost, K., Ruysschaert, G.,
Poesen, J., Van Rompay, A. (2005): Verfijnde ersoiekaart Vlaanderen: eindrapport,
Departement Omgeving, Brussel, 53 pp.
