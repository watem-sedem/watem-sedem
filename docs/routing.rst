#######
Routing
#######

The complex routing algorithm of CN-WS
TO DO: write intro

Rivers
======
TO DO: explain routing river and pixels adjecent to river.

Land
====

In a first step, the routing algorithm checks whether the flow direction is
steered by the tillage direction (for the format of the input of the tillage
direction, see :ref:`Tillage direction <filename_tildirmap>`). This check
compares the angle of the steepest descend with the tillage direction to
define the flow (see Takken et al. (2001)). At the end of this step, the
direction is mapped to the (inter-)cardinal directions. In next step the
flow directions (cardinal space) are adjusted according to elevation,
land-use and presence of grass buffers, as shown in the scheme below:

.. figure:: _static/png/sketch_flow_algorithm.png
	:scale: 80%

The `target1` or `target2` tag indicate that flow will follow strictly the path
of the first or second cardinal flow direction. This is this case the flow
is uni-directional, instead of two-directional. The `topology` tag indicates
that the direction follows the direction as determined above (direction of
steepest descent and tillage direction). A `jump` indicates the flow is
routed further than one pixel. In this case, it skips its neighbouring
pixels. This step is explained later.

TO DO: shortlhy explain scheme
TO DO: jumps



Buffers & Ditches
=================
TO DO:

References
==========
Takken, I., Govers, G., Jetten, V., Nachtergaele, J., Steegen, A., Poesen, J
., 2001. Effects of tillage on runoff and erosion patterns. Soil and Tillage
Research 61, 55–60. https://doi.org/10.1016/S0167-1987(01)00178-7
