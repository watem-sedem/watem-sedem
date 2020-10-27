#######
Routing
#######
The flow and sediment routing is based on a multiple flow direction
algorithm (ref?) implemented on a fixed grid. Specifically, the algorithm
makes use of the height profile and definition of land-use to define flow
from a source pixel to one or two target pixels. It is important to note
that the multi-flow routing algorithm developed for CN-WS is an algorithm
adjusted to the context of erosion and agriculture. This implies that the
routing should represent the average pattern of routing. This way, off-site
impacts (e.g. effect of erosion on an agricultural field on nearby urban
area) can be estimated. This also implies that the model outputs at
interest (routing over land, sediment load per pixel) are sensitive
to the defined height profile.

The flow routing varies a function of the difference in height between
source and potential target pixels, and the land cover in the source and
targets. The land cover is defined in following table (see also :ref:`here
<prcmap>`). The codes listed in this table are used to define the routing in
CN-WS. In a first step, we make a distinction between the land cover of
(a) target(s) being equal to the class `river` (-1) (yes/no). Do note that in
this manual the non-river pixels are sometimes referred to as `land pixels`.

.. note::

Routing flows from land to land pixels, land to river pixels, from river
to river pixels, but not from river to land pixels!


+----------------------+-----------+
|Land cover/use class  | pixel id  |
+======================+===========+
| agricultural fields  | > 0       |
+----------------------+-----------+
| outside model domain |  0        |
+----------------------+-----------+
| river                | -1        |
+----------------------+-----------+
| infrastructure       | -2        |
+----------------------+-----------+
| forest               | -3        |
+----------------------+-----------+
| pasture              | -4        |
+----------------------+-----------+
| open water           | -5        |
+----------------------+-----------+
| grass strips         | -6        |
+----------------------+-----------+

Situation 1: Target(s) is/are equal to `river`
==============================================
In this situation, two cases can be defined:

 - The source pixel is a land pixel: routing follows the direction of the
river pixel (one target).

 - The source pixel is a river pixel: the direction of routing is defined by
the river routing raster (see :ref:`here <routingmap>`), if the river
routing option is set to one (see :ref:`here <riverrouting>`). The routing
is defined as a unidirectional routing. If the river routing option is set
to zero, than river pixels are considered as sinks.

Situation 2: Target(s) is/are not equal to `river`
==================================================

In a first step, the routing algorithm checks whether the flow direction is
steered by the steepest descent direction or the  tillage direction (for the
format of the input of the tillage direction, see :ref:`Tillage direction
<filename_tildirmap>`). In this check, the angle of the steepest descend is
compared with the tillage direction to define the flow (see Takken et al.
(2001)). At the end of this step, the direction is mapped to the (inter-)
cardinal directions. These cardinal directions define the `target1` and
`target2` pixels, and the weight (:math:`\in[0,1], \sum \text{weight} = 1`)
they receive from the source pixel. This amount is used to weight the sediment
load per pixel, the direct run-off depth and upstream area for each
target pixel. In next step the flow directions and weights (cardinal space)
are adjusted according to elevation and land cover, as shown in the scheme
below:

.. figure:: _static/png/sketch_flow_algorithm.png
	:scale: 80%

The `target1` or `target2` tag indicate that flow will follow strictly the path
of the first or second cardinal flow direction. In this case the flow
is uni-directional, instead of two-directional. The `find_lower` tag
indicates that the algorithm will search for the lowest neighbouring
 pixel (single target). A `jump` indicates the target is not a neighbouring
pixel of the source, and the routing jumps to a single target further than
its vicinity defined as a window :math:`W`. This occurs when a source is
located in a local elevation minimum. An important note is that the
routing will always jump to the river :math:`W` if a river pixel is present in
the window :math:`W`.

In the sketch, three features of the two targets and sources are accounted
for to define a rule-bank for the routing direction: the height, the land cover
code and presence of grass strips. First, it is checked whether
the targets are higher or lower than the source pixel. In case one of the
target pixels is higher, than the flow will be defined by the other target
based on the land cover code and presence of grass strips.

If both targets pixels are lower, than the land-use code of both targets is
checked. If both are equal to the land-use code of the source, than the
find_lower function is called. If one or both have a different land-use
code, than it is checked whether the pixels is (are) (a) grass strip(s): in
this case flow will always be defined by the grass strips.

Buffers, ditches and routing dams
=================================

For buffers and ditches, exceptions for the routing are defined. In case of one
of the targets is a buffer, routing will flow to that one target. Within the
buffer, all routing is defined to a single target pixel: the buffer_id (see
also :ref:`here<buffermap>`). This is the pixel which is considered as the
outlet. From this pixel, routing occurs are described above.

For ditches and routing dams, the routing is defined by the user by using
routing map (see :ref:`here<routingmap>`). The routing is uni-directional.

.. note::
 - Routing to ditches can also be defined as an end-point. In this case, the
ditch is considered to be a sink (see :ref:`here <sewermapfile>`).

References
==========
Takken, I., Govers, G., Jetten, V., Nachtergaele, J., Steegen, A., Poesen, J
., 2001. Effects of tillage on runoff and erosion patterns. Soil and Tillage
Research 61, 55–60. https://doi.org/10.1016/S0167-1987(01)00178-7
