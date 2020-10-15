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
is uni-directional, instead of two-directional. The `find_lower` tag indicates
that the direction is determined by a subfunction find_lower, which will be
explained later in this section. A `jump` indicates the target is not a
neighbouring pixel of the source. This typically occurs when a source
is located in a minimum in the height profile.

In the sketch, three features of the two targets ands sources are accounted
for to define a rule-bank for the flow direction: the height, the land-use
code (landuse) and presence of grass buffers. First, it is checked whether
the targets are higher or lower than the source pixel. In case one of the
target pixels is higher, than the flow will be defined by the other target
based on the landuse code and presence of grass buffers.

If both targets cells are lower, than the landuse code of both targets is
checked. If both are equal to the landuse code of the source, than the
find_lower function is called. If one or both have a different land-use
code, than it is checked whether the pixels is (are) (a) grass buffer(s): in
this case flow will always be defined by the grass buffers.

Buffers & Ditches
=================
TO DO:

References
==========
Takken, I., Govers, G., Jetten, V., Nachtergaele, J., Steegen, A., Poesen, J
., 2001. Effects of tillage on runoff and erosion patterns. Soil and Tillage
Research 61, 55–60. https://doi.org/10.1016/S0167-1987(01)00178-7
