
.. _extensionspage:

################
Model extensions
################

WaTEM/SEDEM is used in different contexts and researches. In the past, several model extensions
were incorporated in the model to accomodate certain needs of the researchers. On this page
we these model extensions and their use are explained.

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

.. _simple:

Curve Number
############

When this option is enabled, the CN-module will be used. We refer to the
:ref:`dedicated section <CN>` for more information about the CN-module.

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

    Include sewers = 1

The default is: ``Include sewers = 0``


.. note::
    This option is only fully tested for WaTEM/SEDEM,
    but it is not yet tested when the CN model extension is enabled (':ref:`curve number=1 <simple>`').

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

When using the CN module (i.e. :ref:`curve number = 1 <simple>`), it is possible
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
following output is written when only WaTem-SEDEM is
used:

- :ref:`Total Sediment segments.txt <totalsedimentsegmenttxt>`
- :ref:`Cumulative sediment segments.txt <cumsedsegmenttxt>`

When the CN module is activated (:ref:`curve number=1 <simple>`) additional output per
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

.. _outputextensions:

Output
******

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