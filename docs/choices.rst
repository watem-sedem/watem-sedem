
.. _choicespage:

#############
Model Choices
#############

Most model options are boolean options and can be enabled in the .ini-file with
'1' and disabled with '0'. However, some options expect a string value. The possible
strings are described together with the model option. Two large groups of model choices
can be distinguished: 'Model options' and 'Output options'. The Model options describe
how the model will run: which options and/or extensions of WaTEM/SEDEM are used? The
output choices describe which output that must be generated. On this page we describe
the model choices for a basic run of WaTEM/SEDEM. Model choices that enable optional
model extensions (e.g. incorporation of erosion management practices) are described in a
seperate section.

Model options
*************

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
be run. This means that the WaTEM/SEDEM calculations are disabled, and
no sediment calculations are done. When using this option only
:ref:`a limited model output <onlyroutingoutput>` will be returned by the model.

This option is usefull in large catchments to evaluate the routing without
calculating the sediment transport or discharges. It is enabled in the ini-file as follows:

.. code-block:: ini

    Only Routing = 1

The default is: ``Only Routing = 0``

.. _calctileros:

Calculate tillage erosion
#########################

This option enables the tillage erosion model of Van Oost et al. (2000). We
refer to :ref:`the dedicated section <tillageerosionmodel>` for more information
about this model. This option can be enabled by writing the following in the ini-file:

.. code-block:: ini

    Calculate Tillage Erosion = 1

The default is: ``Calculate Tillage Erosion = 0``

.. _outputchoices:

Output
******

The user has the option to generate extra (or change characteristics of the)
output by defining following keys in
the [:ref:`Output <inioutput>`]-section of the .ini-file.

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


