
.. _choicespage:

#############
Model choices
#############

Most model choices are boolean options and are enabled in the .ini-file with
'1' and disabled with '0'. Some options expect a string value. The possible
strings are described together with the model option.

Input
*****

.. _lmodel:

L model
#######

CN-WS allows the user to choose between two models to calculate the
:ref:`L-factor <lsfactor>`. The L-factor defines the impact of the slope length
and is used in the calculation of :ref:`RUSLE <rusle>` and
:ref:`transport capacity (TC) <TC>`.

The L-model is calculated according to the work of Desmet and Govers (1996):

.. math::
    L = \frac{(A+D^2)^{m+1}-A^{m+1}}{D^{m+2}.x^m.22,13^m}

with
 - :math:`A`: upstream area for every raster pixel (:math:`\text{m}^2`).
 - :math:`D`: grid resolution :math:`(m)`.
 - :math:`m`: length exponent (-).
 - :math:`x`: factor incorporating the flow direction (-).

:math:`x` is calculated as a function of the aspect :math:`\alpha` of the pixel:

.. math::
    x = |sin(\alpha)| + |cos(\alpha)|

The upstream area :math:`A` in a pixel is determined by the stream flow
algorithm, by considering a parcel trapping efficiency and the parcel
connectivity.

For the computation of :math:`m` two options exist:

**Van Oost et al. 2003:**

Van Oost et al. (2003) uses an :math:`m` depending on the surface of
the upstream area :math:`A`. If the upstream area is smaller than
:math:`A_{ref}` = 10.000 ha, :math:`m` is calculated as

.. math::
    m = 0.3 + (\frac{A}{A_{ref}})^c

otherwise :math:`m` is set to 0.72. The value for c is hard coded as 0.8.

The calculation method of Van Oost et al. (2003) can be chosen by setting
the model choice *L model* to 'Desmet1996_Vanoost2003' in the .ini-file (mind the quotes):

.. code-block:: ini

    L model='Desmet1996_Vanoost2003'

This is the default value for this model option.

**McCool et al. (1989):**

McCool et al. (1989) calculates :math:`m` as:

.. math::
    m = \frac{\beta}{\beta + 1}

with :math:`\beta` calculated as

.. math::
    \beta = \frac{\frac{sin(\theta)}{0.0896}}{3.sin^{0.8}(\theta) + 0.56}

with :math:`\theta` the slope of the pixel in percentages.

The calculation method of McCool et al. (1989, 1987) can be chosen by setting
the model choice *L model* to 'Desmet1996_McCool' in the ini-file (mind the quotes):

.. code-block:: ini

    L model='Desmet1996_McCool'

.. _smodel:

S model
#######

CN-WS allows the user to choose between two models to calculate the
:ref:`S-factor <lsfactor>`. The S-factor defines the effect of slope steepness
and is used in the calculation of :ref:`RUSLE <rusle>` and
:ref:`transport capacity (TC) <TC>`.

Both models are a function of :math:`\theta`: the inclination angle or slope
(%). The computation of the inclination angle is based on the four cardinal
neighbouring pixels (Zevenbergen and Thorne, 1987).

The two S-models are:

**Nearing (1997)**:

.. math::
    S = -1,5+\frac{17}{1+e^{2,3-6.1.\sin{\theta}}}

The method of Nearing (1997) can be activated in CN-WS by setting
*S model* to 'Nearing1997' in the ini-file (mind the quotes):

.. code-block:: ini

    S model='Nearing1997'

This is the default method to calculate the S-factor.

**McCool et al. (1987)**

McCool et al. (1987) distinguish two cases:

.. math::
    S = (10.8.sin(\theta)) + 0.03

is valid when:

.. math::
    100.tan(\theta) < 9.0

otherwise, S is calculated as:

.. math::
    S = (16.8.sin(\theta)) - 0.5

The method of McCool et al. (1987) can be activated in CN-WS by setting
*S model* to 'McCool1987' in the ini-file (mind the quotes):

.. code-block:: ini

    S model='McCool1987'

.. _tcmodel:

TC Model
########

The Transport Capacity (TC) can be calculated in two ways in CN-WS. The default
method is the method proposed by Van Oost et al. (2000):

.. math::
    TC = kTC.R.K.(LS - 4.12.S_g^{0.8})

with

- :math:`kTC`: transport capacity coeffient :math:`(m)`
- :math:`R`: :ref:`rain fall erosivity <rfactor>`
- :math:`K`: :ref:`soil erobility factor <kfactor>`
- :math:`LS`: :ref:`slope length and slope steepness factor <lsfactor>`
- :math:`S_g`: local slope (:math:`\frac{\text{m}}{\text{m}}`)

Most studies using WaTEM/SEDEM use the method of Van Oost et al. (2000). The
method of Van Oost et al. (2000) can be activated in CN-WS by setting
*TC model* to 'VanOost2000' in the ini-file (mind the quotes):

.. code-block:: ini

    TC model='VanOost2000'

This is the default value.

However, a second method, proposed by Verstraeten et al. (2007), also exists:

.. math::
    TC = kTC.R.K.A^{1.4}.S_g^{1.4}

with

- :math:`A`: the upstream area :math:`(m^2)` of the pixel

A detailed description and comparison of both TC models can be found in
Verstraeten et al. (2007).

The method of Verstraeten et al. (2007) can be activated in CN-WS by setting
*TC model* to 'Verstraeten2007' in the ini-file (mind the quotes):

.. code-block:: ini

    TC model='Verstraeten2007'

.. _onlyrouting:

Only Routing
############

By enabling the Only Routing (`Only Routing = 1`) option, only the routing will
be determined by CN-WS. No sediment calculations or discharge calculations are
done: the WaTEM/SEDEM and CN modules are disabled. When using this option only
:ref:`a limited model output <onlyroutingoutput>` is possible.

This option is usefull in large catchments to evaluate the routing without
calculating the sediment transport or discharges.

.. _simple:

Only WS
#######

When the option 'Only WS' is enabled, only WaTEM/SEDEM is used and the CN-model
is disabled. By disabling 'Only WS', you will use the complete CN-WS model.

The user has to provide following minimal input if this option is enabled:

- :ref:`digital elevation model <dtmmap>`
- :ref:`P factor map <pmap>`
- :ref:`K factor map <kmap>`
- :ref:`C factor map <cmap>`
- :ref:`R factor <rfactor_var>`
- :ref:`ktc low <ktclow>`
- :ref:`ktc high <ktchigh>`
- :ref:`ktc limit <ktclimit>`
- :ref:`parcel connectivity cropland <parcelconncrop>`
- :ref:`parcel connectivity forest <parcelconnforest>`
- :ref:`parcel trapping efficiency cropland <parceltrapppingcrop>`
- :ref:`parcel trapping efficiency forest <parceltrappingforest>`
- :ref:`parcel trapping efficiency pasture <parceltrappingpasture>`

Additional or alternative inputs are possible based on the chosen
model options.

When you disable this option, you use the CN-module and the following extra
input is mandatory:

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
about this model.

.. _createktil:

Create ktil map
###############

CN-WS is able to create a raster with ktil-factors. The ktil value is the
transport capacity coefficient for tillage erosion. When `Create ktil map = 1`,
the model expects two input variables: :ref:`ktil default <ktildefault>` and
:ref:`ktil threshold <ktilthres>`. The C-factor map will be reclassed by these
values: C-factors higher than ktil threshold will get the value of ktil default,
other pixels are set to zero. When `Create ktil map = 0` the user will have to
make a ktil map himself. The model will expect the filename of this ktil map
in :ref:`ktil map filename <ktilmap>`. This option is only mandatory if
:ref:`Calculate tillage erosion = 1 <calctileros>`

.. _createktc:

Create ktc map
##############

CN-WS is able to create a raster with ktc-factors for high erodible and
non-erodible land-use. When `Create ktc map = 1` the model expects three
variables: :ref:`ktc low <ktclow>`, :ref:`ktc high <ktchigh>`,
:ref:`ktc limit <ktclimit>`. The C-factor map will be reclassed by these values:
C-factors higher than ktc limit will get the value of ktc high, otherwise ktc
low is chosen.

When `Create ktc map = 0` the user will have to make a ktc map himself. The
model will expect the filename of this ktc map in
:ref:`ktc map filename <ktcmap>`.

.. _inlcudesewers:

Include sewers
##############

When the include sewers-option is enabled (`Include sewers = 1`), the user
will have to provide two additional inputs:
:ref:`sewer map filename <sewermapfile>` and :ref:`sewer exit <sewerexit>`.

The value of the pixel in the sewer map is checked when the amount of outgoing
sediment in a pixel is calculated. This value is the fraction of water and
sediment that is trapped in the sewer system via this pixel. The outgoing
sediment of the pixel and the uparea of the target pixels is reduced with this
fraction. The amount of trapped
sediment is written to the output raster :ref:`sewer_in.rst <sewerinrst>`.

.. note::
    This option is fully tested for :ref:`simple=1 <simple>`, but it is not yet
    tested for the full CN-WS model.

.. _includebuffers:

Include buffers
###############

An infrastructural measure that traps an amount of transported sediment is
called a buffer. These measures can be simulated in the model by enabling
the Include buffers option. By enabling this option (`Include buffers = 1`)
the :ref:`buffer map filename <buffermap>` becomes mandatory in the ini-file.
In addition, the ini-file must contain the variable
:ref:`number of buffers <nrbuffers>` and a separate section for every buffer
in the buffer map. In every buffer section in the ini-file some variables must
be given.

The `Include buffers` option adjusts the routing in the pixels. Routing
within a buffer is defined from the pixels with a buffer extension id to
one outlet pixel with a buffer id coupled to the buffer extension id. The
amount of sediment that flows out of the outlet pixel to downstream pixels is
reduced with the trapping efficiency of the buffer. The definitions of buffer
extension id, buffer id and trapping efficiency are explained in the
:ref:`buffer data section <bufferdata>`.

.. _bufferreduce:

Buffer reduce area
##################

This option (boolean) allows to reduce the upstream area downstream of a buffer
with the efficiency of the buffer (see :ref:`buffer data section <bufferdata>`)

.. _includeditches:

Include ditches
###############

Ditches alter the routing. The sediment and water will follow the course of a
ditch instead of the steepest slope. When this option is enabled
(`Include ditches = 1`), :ref:`a raster with information about the direction
<ditchmap>` is mandatory.

The model sets the :ref:`C-factor <cfactor>` at every ditch pixel tot 0.01,
assuming that the ditch is covered with grass.
Thus, it overwrites the value of the pixel in the :ref:`C-factor raster <cmap>`.
The ktc value of the pixel is set to 9999.

.. _includedams:

Include dams
############

Dams alter the routing in the same way as ditches. The sediment and water will
follow the course of a dam instead of the steepest slope. When this
option is enabled (`Include dams = 1`), :ref:`a raster with information about
the direction <dammap>` is mandatory.

The model sets the C-factor at every dam pixel to 0. Thus, it overwrites
the value of the pixel in the :ref:`C-factor raster <cmap>`.
The ktc value of the pixel is set to :ref:`ktc low <ktclow>`.

.. _forcerouting:

Force Routing
#############

When the routing based on the built-in rules of the model is not correct (e.g.
in the neighbourhood of infrastructure), the user has the possibility to impose
the routing. This is done by enabling the Force Routing option. With force
routing the routing algorithm will use the routing imposed by the user instead
of the digital elevation model.

When `Force Routing = 1` the user will have to provide additional input: the
variable `number of force routing` and a separate section for every routing
vector the user wants to add. `Number of force routing` contains an integer
value with the amount of routing vectors that are imposed by the user.

An example of a valid forced routing section looks like

.. code-block:: ini

    [Force routing 1]
    from col = 25
    from row = 55
    target col = 30
    target row = 55


The keys in every force routing section are `from col`, `from row`, `target col`
and `target row`. These are integer values representing the location of source
and target pixel in the raster.

See :ref:`the section on grid coordinates <gridcoordinates>` for more
information on the orientation of the rows and columns.

.. _riverrouting:

River Routing
#############

By enabling the river routing option (`River Routing = 1`), the routing
between
river pixels is imposed by an input raster and two input tables.
This option is usefull because the calculated routing in a river, based on the
digital elevation model, is not always correct.

Following input-files are required when `River Routing = 1`:

* :ref:`river segment file <riversegmentfile>`
* :ref:`river routing file <riverroutingmap>`
* :ref:`adjectant segments file <adjsegments>`
* :ref:`upstream segments file <upstrsegments>`

When this option is disabled, the model will use the digital elevation model to
determine the routing between all river pixels.


Cardinal Routing River
######################
This option enables only cardinal routing from source pixels to the river
pixels. Default equal to True.

.. _includetillagedirection:

Include tillage direction
#########################

This option alters the routing on agricultural fields. When this option is
enabled, the routing will follow the tillage direction on these fields.

Following input-files are required when `Include tillage direction = 1`:

* :ref:`tillage direction map <tildirmap>`
* :ref:`oriented roughness map <orientedroughnessmap>`

.. note::
    This option is not yet tested.

.. _adjustslope:

Adjusted Slope
##############

Normally, the slope of a pixel is determined by the algorithm of Zevenbergen and
Thorne (1987) on the four neighbouring, cardinal cells.
This procedure works good in areas where the routing is solely based on the
digital elevation model. In areas where the routing is imposed by other rules
(e.g. at parcel boundaries, in buffers,...) the slope of the direction in the
routing can be different than the calculated slope by Zevenbergen and
Thorne (1987). The *Adjusted Slope*-option gives the user the ability to correct
the slope if the imposed routing targets a single cell instead of two cells.
In this case the slope can be calculated by dividing the
absolute value of the height difference between the source and target pixel,
with the distance between these two pixels. This calculation is enabled by
setting `Adjusted Slope = 1` in the ini-file.

.. _estimclay:

Estimate Clay content
#####################

When using the full CN-WS model (i.e. :ref:`simple=0 <simple>`), it is possible
to estimate the clay content at every outlet and in every river
segment (the latter only when :ref:`output per river segment <outputsegment>`
is enabled). To do this (`Estimate clay content = 1` in the ini-file), the
user additionally needs to define the
:ref:`clay content of the parent material <claycontent>`
(:math:`CC_{text{parent}}`).

First, the enrichment factor :math:`EF` for clay is calculated:

.. math::
    EF = 1 + 0.7732.\exp^{-0.0508.SC}

where :math:`SC` is the sediment concentration :math:`(g/l)`.

The estimated clay content :math:`CC` :math:`(%)` for an outlet or segment is calculated
as a function of :math:`EF` and :math:`CC_{text{parent}}`:

.. math::
    CC = CC_{parent}.EF

After the calculation, following files are written:

* :ref:`Clay content sediment.txt <claycontentesedtxt>`
* :ref:`Clay content sediment segments.txt <claycontentesedsegmenttxt>`

.. note::
    This option is not yet tested.

.. _calibrate:

Calibrate
#########

The Calibrate-option allows the model user to run the model with a given set of
options, variables and inputfiles for a number of combinations of ktc-factors.
Both the ktc_high-factor as the ktc_low-factor are varied in an amount of steps
between a lower and upper value. For every combination of ktc-factors where
ktc_high > ktc_low, the model will make a calculation and write the results to a
:ref:`Calibration file <calibrationtxt>`.
A more detailed explanation about how and why to calibrate can be found
:ref:`here <calibration>`

.. _outputsegment:

Output per river segment
########################

A river segment is defined as a series of consequent river pixels. Mostly, a
segment starts at a confluence of different rivers and it stops at the next
confluence. CN-WS has the option to make a summary of the results per river
segment. For every segment the total sedimentinput, total discharge or the
sediment concentration is calculated.

River segments are defined in a :ref:`separate raster <riversegmentfile>`. This
raster is mandatory when this option is enabled.

When this option is enabled, following output is written:

- :ref:`Total Sediment segments.txt <totalsedimentsegmenttxt>`
- :ref:`Cumulative sediment segments.txt <cumsedsegmenttxt>`
- :ref:`Discharge_segments.txt <dischargesegment>`
- :ref:`Sediment concentration segments.txt <sedconcensegment>`
- :ref:`Sediment_segments.txt <sedsegmenttxt>`

.. _manualoutlet:

Manual outlet selection
#######################

By default, the model will determine the outlet pixel as the lowest (river)
pixel within the model domain. However, by setting `Manual outlet selection = 1`,
the model expects an :ref:`outlet raster <outletmap>`: an integer raster where
the outletpixels are numbered from 1 to n. The user has to provide this input
file.

.. _outputchoices:

Output
******

The user has the option to generate extra output by defining following keys in
the [Output maps]-section of the .ini-file.

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

write routing column/row
########################

(bool, default false):

.. _writerusle:

write RUSLE
###########

(bool, default false): writes :ref:`RUSLE.rst <ruslerst>`

.. _writesedexport:

write sediment export
#####################

(bool, default false): writes :ref:`SediExport_kg.rst <sediexportrst>`,
:ref:`SediIn_kg.rst <sediinrst>`, :ref:`SediOut_kg.rst <sedioutrst>`

.. _writerwatereros:

write water erosion
###################

(bool, default false): writes
:ref:`WATEREROS (kg per gridcel).rst <watereroskgrst>` and
:ref:`WATEREROS (mm per gridcel).rst <watererosmmrst>`

.. _writerainfallexcess:

write rainfall exces
####################

(bool, default false): writes :ref:`Remap.rst <remaprst>`

.. _writetotalrunoff:

write total runoff
##################

(bool, default false): writes :ref:`Total runoff.rst <totalrunofrst>`

In the section `[User Choices]` two keys impose some output too:

- `Include sewer` (bool, default false): writes sewer_in.rst
- `Output per river segment` (bool, default false): writes
  Total Sediment segments.txt, Total discharge.txt, Sediment_segments.txt,
  Sediment concentration segments.txt, Cumulative sediment segments.txt


