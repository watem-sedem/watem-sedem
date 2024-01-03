
.. _choicespage:

#############
Model choices
#############

Most model choices are boolean options and can be enabled in the .ini-file with
'1' and disabled with '0'. However, some options expect a string value. The possible
strings are described together with the model option.

User Choices
************

.. _lmodel:

L model
#######

WaTEM/SEDEM allows the user to choose between two models to calculate the
:ref:`L-factor <lsfactor>`. The L-factor defines the impact of the slope length
and is used in the calculation of :ref:`RUSLE <rusle>` and
:ref:`transport capacity (TC) <TC>`.

The L-model is calculated according to the work of Desmet and Govers (1996):

.. math::
    L = \frac{(A+D^2)^{m+1}-A^{m+1}}{D^{m+2}.x^m.22,13^m}

with
 - :math:`A`: upstream area for every raster pixel (:math:`\text{m}^2`).
 - :math:`D`: grid resolution :math:`(m)`.
 - :math:`m`: length exponent :math:`(-)`.
 - :math:`x`: factor incorporating the flow direction :math:`(-)`.

:math:`x` is calculated as a function of the aspect :math:`\alpha` of the pixel:

.. math::
    x = |sin(\alpha)| + |cos(\alpha)|

The upstream area :math:`A` in a pixel is determined by the stream flow
algorithm, and considers the parcel trapping efficiencies and the parcel
connectivities.

For the computation of :math:`m`, however, two options exist:

**1. Van Oost et al. 2003:**

Van Oost et al. (2003) uses an :math:`m` depending on the surface of
the upstream area :math:`A`. If the upstream area is smaller than
:math:`A_{ref}` = 10.000 ha, :math:`m` is calculated as:

.. math::
    m = 0.3 + (\frac{A}{A_{ref}})^c

otherwise :math:`m` is set to 0.72. in the model :math:`c` is 'hard coded' as
0.8, meaning that this value is fixed for this model and cannot be changed by
the user.

**2. McCool et al. (1989):**

McCool et al. (1989) calculates :math:`m` as:

.. math::
    m = \frac{\beta}{\beta + 1}

with :math:`\beta`:

.. math::
    \beta = \frac{\frac{sin(\theta)}{0.0896}}{3.sin^{0.8}(\theta) + 0.56}

where :math:`\theta` stands for the slope of the pixel in percentages.

The preferred method (i.e. Van Oost et al. (2003) or McCool et al. (1989, 1987))
can be selected by setting the model choice *L model* to 'Desmet1996_Vanoost2003'
or 'Desmet1996_McCool', respectively, in the ini-file.
This should be done in the following manner (mind the quotes):

.. code-block:: ini

    L model='Desmet1996_Vanoost2003'

This is the default value for this model option. or:

.. code-block:: ini

    L model='Desmet1996_McCool'

.. _smodel:

S model
#######

WaTEM/SEDEM allows the user to choose between two models to calculate the
:ref:`S-factor <lsfactor>`. The S-factor defines the effect of slope steepness
and is used in the calculation of :ref:`RUSLE <rusle>` and
:ref:`transport capacity (TC) <TC>`.

Both models are a function of :math:`\theta`: the inclination angle or slope
(%). The computation of the inclination angle is based on the four cardinal
neighbouring pixels (Zevenbergen and Thorne, 1987).

The two S-models are:

**1. Nearing (1997)**:

.. math::
    S = -1,5+\frac{17}{1+e^{2,3-6.1.\sin{\theta}}}


**2. McCool et al. (1987)**

McCool et al. (1987) distinguishes between two cases, namely:

.. math::
    100.tan(\theta) < 9.0; and: 100.tan(\theta) \geq 9.0
    
In the first case, S is calculated as: 

.. math::
    S = (10.8.sin(\theta)) + 0.03

In the other case, S is calculated as:

.. math::
    S = (16.8.sin(\theta)) - 0.5

The preferred method (i.e. Nearing (1997) or McCool et al. (1987)) can be selected by setting
the model choice *S model* to 'Nearing1997' or 'McCool1987', respectively, in the ini-file. 
This should be done in the following manner (mind the quotes):

.. code-block:: ini

    S model='Nearing1997'

This is the default method to calculate the S-factor. or:

.. code-block:: ini

    S model='McCool1987'

.. _tcmodel:

TC Model
########

The Transport Capacity (TC) can be calculated in two ways in WaTEM/SEDEM. The default
method is the method proposed by Van Oost et al. (2000):

.. math::
    TC = kTC.R.K.(LS - 4.12.S_g^{0.8})

with

- :math:`kTC`: transport capacity coeffient :math:`(m)`
- :math:`R`: :ref:`rain fall erosivity <rfactor>`
- :math:`K`: :ref:`soil erobility factor <kfactor>`
- :math:`LS`: :ref:`slope length and slope steepness factor <lsfactor>`
- :math:`S_g`: local slope (:math:`\frac{\text{m}}{\text{m}}`)

Most studies using WaTEM/SEDEM use this method by Van Oost et al. (2000). 
It can be activated in WaTEM/SEDEM by setting
*TC model* to 'VanOost2000' in the ini-file (mind the quotes):

.. code-block:: ini

    TC model='VanOost2000'


However, a second method, proposed by Verstraeten et al. (2007), can be used as
well, namely:

.. math::
    TC = kTC.R.K.A^{1.4}.S_g^{1.4}

with

- :math:`A`: the upstream area :math:`(m^2)` of the pixel

A detailed description and comparison of both TC models can be found in
Verstraeten et al. (2007).

The method of Verstraeten et al. (2007) can be activated in WaTEM/SEDEM by setting
*TC model* to 'Verstraeten2007' in the ini-file (mind the quotes):

.. code-block:: ini

    TC model='Verstraeten2007'


.. _onlyrouting:

Only Routing
############

By enabling the Only Routing option, only the routing algorithm will
be run. This means that the WaTEM/SEDEM and CN modules of the model are disabled, and
no sediment calculations or discharge calculations are done. When using this option only
:ref:`a limited model output <onlyroutingoutput>` will be returned by the model.

This option is usefull in large catchments to evaluate the routing without
calculating the sediment transport or discharges. It is enabled in the ini-file as follows:

.. code-block:: ini

    Only Routing = 1

The default is: ``Only Routing = 0``

.. _simple:

Curve Number
###########

When this option is enabled, the CN-module will be used.

To enable this option, following code should be
written in the ini-file:

.. code-block:: ini

    Curve Number = 1

The default is: ``Curve Number = 0``

The following input is mandatory when using this option:

- :ref:`alpha <alpha>`
- :ref:`beta <beta>`
- :ref:`stream velocity <streamvelocity>`
- :ref:`5-day antecedent rainfall <5dayrainfall>`
- :ref:`desired timestep for model <timestep>`
- :ref:`endtime model <endtime>`
- :ref:`cn map <cnmap>`
- :ref:`rainfall file <rainfallfile>`

.. _calctileros:

Calculate tillage erosion
#########################

This option enables the tillage erosion model of Van Oost et al. (2000). We
refer to :ref:`the dedicated section <tillageerosionmodel>` for more information
about this model. This option can be enabled by writing the following in the ini-file:

.. code-block:: ini

    Calculate Tillage Erosion = 1

The default is: ``Calculate Tillage Erosion = 0``

.. _createktil:

Create ktil map
###############

WaTEM/SEDEM is able to create a raster with ktil-factors. The ktil value is the
transport capacity coefficient for tillage erosion. We
refer to :ref:`the dedicated section <tillageerosionmodel>` for more information
about this coefficient. When ``Create ktil map = 1``,
the model expects two input variables, namely: :ref:`ktil default <ktildefault>` and
:ref:`ktil threshold <ktilthres>`. With this option enabled, the C-factor map
will be reclassified based on the
values given as input for :ref:`ktil default <ktildefault>` and
:ref:`ktil threshold <ktilthres>`. The C-factor values higher than *ktil threshold* 
will be set to the value of *ktil default*, while
the other pixels will be set to zero.  

If the 'Create ktil map' is disabled, the user must
create a ktil map himself, and this map should be given as input for the model
by entering its filename next to :ref:`ktil map filename <ktilmap>` in the
ini-file. This option is only mandatory if
:ref:`Calculate tillage erosion = 1 <calctileros>`. 

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Create ktil map = 1

The default is: ``Create ktil map = 0``

The Create ktil map - option is only mandatory when
:ref:`Calculate tillage erosion <calctileros>` is enabled. When no tillage
erosion is calculated (:ref:`Calculate tillage erosion = 0 <calctileros>`), the
Create ktil map option will be ignored by the model. 

.. _createktc:

Create ktc map
##############

WaTEM/SEDEM is able to create a raster with ktc-factor values for high erodible and
non-erodible land-uses. When the 'Create ktc map' option is enabled, the model
expects three variables: :ref:`ktc low <ktclow>`, :ref:`ktc high <ktchigh>`,
:ref:`ktc limit <ktclimit>`. The C-factor map will be reclassified based these values.
The C-factor values higher than *ktc limit* will be set to the value of *ktc high*, while 
the other pixels will be set to *ktc low*.

When the 'Create ktc map' option is disabled, the user must create a ktc map himself, 
and this map should be given as input for the
model by entering its filename next to
:ref:`ktc map filename <ktcmap>` in the ini-file.

To disable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Create ktc map = 0

The default is: ``Create ktc map = 1``

.. _inlcudesewers:

Include sewers
##############

When the 'Include sewers' option is enabled, the user
must provide two additional inputs, namely:
:ref:`sewer map filename <sewermapfile>` and :ref:`sewer exit <sewerexit>`.

The value of the pixel in the sewer map is used when the amount of outgoing
sediment in a pixel is calculated. This value should give the fraction of water and
sediment that is trapped in the sewer system via this pixel.
The practical implication of this value is that the outgoing
sediment of the pixel and the uparea of the target pixels are reduced by this fraction. 

The amount of trapped sediment per pixel is written to the output raster
:ref:`sewer_in.rst <sewerinrst>`.

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Create ktc map = 1

The default is: ``Create ktc map = 0``


.. note::
    This option is fully tested for the model option: ':ref:`Only WS=1 <simple>`',
    but it is not yet tested when the CN model extension is .

.. _includebuffers:

Include buffers
###############

An infrastructural measure that traps an amount of transported sediment is
called a buffer. These measures can be simulated in the model by enabling
the 'Include buffers' option. When this option is enabled, the
:ref:`buffer map filename <buffermap>` becomes a mandatory line in the ini-file.
In addition, the ini-file must contain the variable
:ref:`number of buffers <nrbuffers>` and a separate section for every buffer
in the buffer map. In every buffer section in the ini-file some variables must
be given (see :ref:`here <bufferdata>`).

The 'Include buffers' option adjusts the routing calculated by the model. Routing
within a buffer is defined from the pixels with a buffer extension id towards
one outlet pixel with a buffer id, coupled to the buffer extension id. The
amount of sediment that flows out of the outlet pixel to downstream pixels is
reduced with the trapping efficiency of the buffer. The definitions of buffer
extension id, buffer id and trapping efficiency are explained in the
:ref:`buffer data section <bufferdata>`.

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Include buffers = 1

The default is: ``Include buffers = 0``

.. _bufferreduce:

Buffer reduce area
##################

This option allows the model to reduce the
:ref:`upstream area <upstreamarea>` (:math:`A`) downstream of a buffer
with the efficiency of the buffer (see :ref:`buffer data section <bufferdata>`).

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Buffer reduce Area = 1

The default is: ``Buffer reduce Area = 0``


.. _includeditches:

Include ditches
###############

The use of ditches will alter the routing simulated by the model.
When included, sediment and water will follow the course of the
ditches instead of the steepest slope in the ditch locations.
When this option is enabled, a :ref:`Ditch map <ditchmap>`
(a raster with information about the direction) should be given as model input.

The model sets the :ref:`C-factor <cfactor>` at every ditch pixel tot 0.01,
assuming that the ditch is covered with grass.
It therefor overwrites the value of the pixel in the :ref:`C-factor raster <cmap>`.
The ktc value of the pixel is set to 9999.

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Include ditches = 1

The default is: ``Include ditches = 0``


.. _includedams:

Include dams
############

The use of Dams alter the routing in a similar way as ditches. The sediment and water will
follow the course of a dam instead of the steepest slope on dam locations. When this
option is enabled, :ref:`dam map <dammap>` (a raster with information about
the direction) should be given as model input.

The model sets the C-factor at every dam pixel to 0, assuming that no erosion
takes place inside the dams. It therefor overwrites
the value of the pixel in the :ref:`C-factor raster <cmap>`.
The ktc value of the pixel is set to :ref:`ktc low <ktclow>`.

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Include dams = 1

The default is: ``Include dams = 0``

.. _forcerouting:

Force Routing
#############

When the routing based on the built-in rules of the model is not correct (e.g.
in the neighbourhood of infrastructure), the user has the possibility to impose
the routing. This is done by enabling the Force Routing option. With force
routing the routing algorithm will use the routing imposed by the user instead
of the digital elevation model.

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Force Routing = 1

The default is: ``Force Routing = 0``

When this option is enabled, the user will have to provide additional input: the
variable :ref:`Number of forced routing <nrforcedrouting>` and a separate
section for every routing vector the user wants to add.

An example of a valid forced routing section looks like

.. code-block:: ini

    [Force routing 1]
    from col = 25
    from row = 55
    target col = 30
    target row = 55


The keys in every force routing section are `from col`, `from row`, `target col`
and `target row`. These are integer values representing the location of source
and target pixel in the raster. See :ref:`here <forcedroutingdata>` for more information on the
input variables for forced routing. More information about the raster coordinates and the orientation 
of rows and columns can be found in :ref:`the section on grid coordinates <gridcoordinates>`. 

.. _riverrouting:

River Routing
#############

By enabling the river routing option, the routing between
river pixels is imposed by an input raster and two input tables.
This option can be usefull since the calculated routing in a river, based on the
digital elevation model, is not always correct.

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    River Routing = 1

The default is: ``River Routing = 0``

Following input-files are required when River Routing is enabled:

* :ref:`river segment file <riversegmentfile>`
* :ref:`river routing file <riverroutingmap>`
* :ref:`adjectant segments file <adjsegments>`
* :ref:`upstream segments file <upstrsegments>`

When this option is disabled, the model will use the digital elevation model to
determine the routing between all river pixels.


Cardinal Routing River
######################

It is possible to change the routing towards river pixels. When 'Cardinal Routing River'
is enabled (it is enabled by default), only the cardinal neighbouring pixels of a
river pixels are routed to the river pixel. If the user wants pixels that touch 
a river pixel diagonally to route to the river, this option must be disabled.

To disable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Cardinal Routing River = 0 

The default is: ``Cardinal Routing River= 1``

.. _includetillagedirection:

Include tillage direction
#########################

This option alters the routing on agricultural fields. When this option is
enabled, the routing will follow the given tillage direction on these fields.

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Include tillage direction = 1

The default is: ``Include tillage direction = 0``

Following input-files are required if this option is enabled:

* :ref:`tillage direction map <tildirmap>`
* :ref:`oriented roughness map <orientedroughnessmap>`

.. note::
    This option has not been yet tested.

.. _adjustslope:

Adjusted Slope
##############

The slope of a pixel in a standard model run is determined by the algorithm of
Zevenbergen and Thorne (1987), using the four neighbouring, cardinal cells of
the pixel.
This procedure works well in areas where the routing is solely based on the
digital elevation model. However, when the routing is imposed by other rules 
(e.g. at parcel boundaries, in buffers,...), as well, the slope direction in the
routing can be different from the calculated slope by Zevenbergen and
Thorne (1987). The Adjusted Slope option gives the user the ability to correct
the slope if the imposed routing targets a single cell instead of two.
In this case the slope can be calculated by dividing the
absolute value of the height difference between the source and target pixel,
with the distance between these two pixels. 

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Adjusted Slope = 1

The default is: ``Adjusted Slope = 0``

.. _estimclay:

Estimate Clay content
#####################

When using the full CN-WS model (i.e. :ref:`Only WS = 0 <simple>`), it is possible
to estimate the clay content at every outlet (and in every river
segment if :ref:`output per river segment <outputsegment>` is enabled).
In order to estimate the clay content, the
user needs to enable this option and, additionally, needs to define the
:ref:`clay content of the parent material <claycontent>`
(:math:`CC_{text{parent}}`).

The estimation of the clay content is handled in two steps:

First, the enrichment factor :math:`EF` for clay is calculated:

.. math::
    EF = 1 + 0.7732.\exp^{-0.0508.SC}

where :math:`SC` is the sediment concentration :math:`(g/l)`.

Then, the estimated clay content :math:`CC` :math:`(in \%)` for an outlet or
segment is calculated as a function of :math:`EF` and :math:`CC_{parent}`:

.. math::
    CC = CC_{parent}.EF

After these calculations, following files are written:

* :ref:`Clay content sediment.txt <claycontentesedtxt>`
* :ref:`Clay content sediment segments.txt <claycontentesedsegmenttxt>`

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Estimate clay content = 1

The default is: ``Estimate clay content = 0``

.. note::
    This option is not yet tested.

.. _calibrate:

Calibrate
#########

The Calibrate option allows the user to calibrate the ktc-factors for the model. 
With this option enabled, the model will use a given set of
options, variables and inputfiles, and return output values for a number of
combinations of ktc-factors.
Both the ktc_high-factor as the ktc_low-factor are varied in an amount of steps
between a lower and upper value. For every combination of ktc-factors where
ktc_high > ktc_low, the model will make a calculation and write the results to a
:ref:`Calibration file <calibrationtxt>`.
A more detailed explanation about how and why to calibrate can be found
:ref:`here <calibration>`

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Calibrate = 1

The default is: ``Calibrate = 0``

When this option is enabled, the user will have to provide additional input,
namely: the separate section ``[Calibration]`` (see :ref:`here <calibrationparamters>`)
needs to be added to the ini-file in the :ref:`according manner <inicalib>`.

.. _outputsegment:

Output per river segment
########################

A river segment is defined as a series of consequent river pixels. Mostly, a
segment starts at a confluence of tributaries and it stops at the next
confluence. WaTEM/SEDEM has the option to make a summary of the results based on the available river
segment. For every segment the total sedimentinput, total discharge or the
sediment concentration is calculated.

River segments are defined in a :ref:`separate raster <riversegmentfile>`. This
raster is mandatory when this option is enabled.

When this option is enabled (``Output per river segment=1``),
following output is written when only WaTem-SEDEM (:ref:`Only WS=1 <simple>`) is
used:

- :ref:`Total Sediment segments.txt <totalsedimentsegmenttxt>`
- :ref:`Cumulative sediment segments.txt <cumsedsegmenttxt>`

When the CN module is activated (:ref:`Only WS=0 <simple>`) additional output per
segment is generated:

- :ref:`Discharge_segments.txt <dischargesegment>`
- :ref:`Sediment concentration segments.txt <sedconcensegment>`
- :ref:`Sediment_segments.txt <sedsegmenttxt>`

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Output per river segment = 1

The default is: ``Output per river segment = 0``


.. _manualoutlet:

Manual outlet selection
#######################

By default, the model will determine the outlet pixel at the lowest (river)
pixel within the model domain. However, by enabling this option, the user can define the outlets manually.
This is done by creating an :ref:`outlet raster <outletmap>` (integer raster where
the outlet pixels are numbered from 1 to n). The user has to provide the filename of this input
raster in the ini-file.

To enable this option, the following line must be written in the ini-file:

.. code-block:: ini

    Manual outlet selection = 1

The default is: ``Manual outlet selection = 0``

.. _outputchoices:

Output
******

The user has the option to generate extra (or change characteristics of the)
output by defining following keys in
the [:ref:`Output maps <inioutput>`]-section of the .ini-file.

.. _sagagrids:

Saga_Grids
##########

(bool, default false): write output rasters as Saga Grids. If false, Idrisi
rasters are written.

.. _writeaspect:

write aspect
############

(bool, default false): write :ref:`AspectMap.rst <aspectmap>`

.. _writels:

write LS factor
###############

(bool, default false): write :ref:`LS.rst <lsmap>`

.. _writeuparea:

write upstream area
###################

(bool, default false): write :ref:`UPAREA.rst <upareamap>`

.. _writeslope:

write slope
###########

(bool, default false): write :ref:`SLOPE.rst <slopemap>`

.. _writerouting:

write routing table
###################

(bool, default false): writes :ref:`routing.txt <routingtxt>` and
:ref:`routing_missing.txt <missingroutingtxt>`

.. _writeroutingrc:

write routing column/row
########################

(bool, default false): writes :ref:`routing_colrow.txt <routingcolrow>`

.. _writerusle:

write RUSLE
###########

(bool, default false): writes :ref:`RUSLE.rst <ruslerst>`

.. _writesedexport:

write sediment export
#####################

(bool, default false): writes :ref:`SediExport_kg.rst <sediexportrst>`,
:ref:`SediOut_kg.rst <sedioutrst>`, and :ref:`SediIn_kg.rst <sediinrst>`

.. _writerwatereros:

write water erosion
###################

(bool, default false): writes
:ref:`WATEREROS (kg per gridcel).rst <watereroskgrst>` and
:ref:`WATEREROS (mm per gridcel).rst <watererosmmrst>`

.. _writerainfallexcess:

write rainfall excess
#####################

(bool, default false): writes :ref:`Remap.rst <remaprst>`

.. _writetotalrunoff:

write total runoff
##################

(bool, default false): writes :ref:`Total runoff.rst <totalrunofrst>`

.. note::
    In the section `[User Choices]` two keys impose some output too:

    - `Include sewer` (bool, default false): writes sewer_in.rst
    - `Output per river segment` (bool, default false): writes
      Total Sediment segments.txt, Total discharge.txt, Sediment_segments.txt,
      Sediment concentration segments.txt, Cumulative sediment segments.txt


