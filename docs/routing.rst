.. _routing:

#######
Routing
#######

The flow and sediment routing is based on a multiple flow direction
algorithm implemented on a fixed grid. Specifically, the algorithm
makes use of the height profile and definition of land cover to define flow
from a source pixel to one or two target pixels. It is important to note
that the multi-flow routing algorithm developed for CN-WS is an algorithm
adjusted to the context of erosion and agriculture.

The flow routing varies a function of the difference in height between
source and **potential** target pixels, and the **land cover** in the source
and targets. The land cover is defined in following table (see also :ref:`here
<prcmap>`). The codes listed in this table are used to define the routing in
CN-WS. A distinction is made between the land cover of (a) target(s) being
equal to the class `river` (-1) and not being equal to the class `river`
(non-river pixels). Do note that in this manual the non-river pixels are
sometimes referred to as `land pixels`.

.. csv-table::
    :file: _static/csv/landcover_pixelid.csv
    :header-rows: 1
    :align: center

Definitions
===========

- Routing flows from land to land pixels, land to river pixels, river
  to river pixels, but not from river to land pixels!
- **River routing** can be defined in rivers, conductive buffer dams and
  conductive ditches, see in the section :ref:`below <riverrouting_exp>`.
- **Adjacent pixels** are defined by the eight neighbouring pixels of the
  target pixel, see also
  :ref:`the definition of the flow routing <routingmap>`.
- **One-target** routing is defined in the cardinal and
  ordinal directions, and **two-target** routing is defined by the cardinal
  and ordinal direction.
- A **buffer** is not the same as a **conductive buffer dam** as
  **buffers** are defined as a measure with an trapping efficiency, whereas
  **conductive buffer dams** are defined by routing paths. As such, it is
  up to the user to define a buffer measure as a **buffer** (with a trapping
  efficiency) (1) or a **conductive buffer dam** (with a routing).

Flow scheme of the routing algorithm
====================================

In this section we will describe the routing algorithm in depth, with several
flow charts as an illustration.

The first step of the routing algorithm is to sort all pixels in the raster from
high to low, based on the :ref:`digital elevation model <dtmmap>`. In the
next step, the algorithm loops over all pixels, starting from the highest
pixel:

 - If the pixel is a **land pixel**, the default routing algorithm is used. If
   the user enables the :ref:`Forced Routing <forcerouting>`-option in the
   ini-file, the calculation of the routing by the default routing algorithm
   is altered by the user-defined routing. Thus, this user-defined routing
   will overrule the rulebank of the routing algorithm.
 - If the pixel is a **river pixel** and the river routing option is enabled
   (see :ref:`here <riverrouting>`), the algorithm will use the user-defined
   routing in the rivers. If the pixel is a river pixel and the river routing
   is not enabled, than no river routing is calculated.
 - If the landcover of a pixel has a value of 0, it is skipped and no routing
   is calculated for this pixel.


Default routing algorithm
*************************

In the first part of the default routing algorithm, it is checked if the
routing of a considered pixel is determined by a **buffer**,
**conductive ditch** or **conductive buffer dam** or if the pixel is adjacent
to a river, conductive ditch or conductive buffer dam. This part of the
algorithm is illustrated in the figure below. There is only one target pixel
in case that a pixel is a conductive ditch, conductive buffer dam or adjacent
to a conductive ditch, conductive buffer dam or river.

.. figure:: _static/png/flow_algorithm_part1.png
    :align: center

    Flow-chart of the routing algorithm in CN-WS describing the first steps in
    the algorithm.

If the source pixel is a buffer pixel than two cases are defined:

    1. The considered pixel has a buffer_id. This is the outlet pixel of the
       buffer. The default routing algorithm is used in this pixel.

    2. The considered pixel has a buffer extension-id. In this case there is
       only one target pixel: the pixel within the buffer with the buffer_id
       (i.e. the outlet of the buffer).

We refer to the :ref:`section on buffers <includebuffers>` for a complete
description of how buffers are defined. For description on the definition of
conductive buffer dams and conductive ditches, we refer to the section on
:ref:`river routing <riverrouting_exp>`.

One- and two-target routing
^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the routing is not determined by a buffer, conductive ditch, conductive
buffer dam or a river, the routing algorithm checks whether the flow direction
is steered by the steepest descent direction or the **tillage direction** (for
the format of the input of the tillage direction, see :ref:`here <tildirmap>`).
In this check, the angle of the **steepest descend** is compared with the
tillage direction to define the routing (see Takken et al. (2001)). At the end
of this step, the direction is mapped to the cardinal directions.
These cardinal directions define the `target1` and `target2` pixels, and the
weight (:math:`\in[0,1], \sum \text{weight} = 1`) they receive from the
source pixel. This amount can be used to weigh the sediment load per
pixel (WS), the direct run-off depth (CN) and upstream area (CN/WS) for each
target pixel.

Routing over land pixels can be defined two (cardinal directions)
or one targets (ordinal and cardinal directions).

 - Two targets: routing is defined by one or two targets as a function of the
   direction only in the **cardinal direction**, thus
   **four adjacent pixels**). Flows and sediment loads are distributed
   according to the angle between the direction (float number between 0 and
   360 degrees) and the cardinal axis (see section two-target routing). This
   direction is determined by the digital elevation model (and if relevant the
   tillage direction).
 - One target: routing is defined by one routing vector, and can be in the
   **cardinal** and **ordinal direction**, thus **eight adjacent pixels**.
   One-target routing is based on a
   decision tree (see figure :ref:`section one-target routing <onetarget>`)
   using land cover and the digital elevation
   model. The starting point for one-target routing is the two-target routing.

Two-target routing is computed first based on the digital elevation model and
the tillage direction, whereas adaptation to this two-target routing is
computed in the one-target routing based on the land cover of the targets.
Note that the digital elevation information is still used in step the
one-target routing scheme (in case of jumps).

.. _twotarget:

Two-target routing
^^^^^^^^^^^^^^^^^^

In the figure below it is shown how the two targets are determined by the
routing direction. The routing direction is first split in two cardinal
directions. Depending on the quadrant the direction points to, index shifts
with one pixel in the x-direction (columns in rasters) and y-direction
(rows in rasters) are defined (see table below). The index shifts are used
to define the two target pixels. The amount of flow and sediment load that
is routed to each of the two targets is calculated by computing the angle
between the cardinal direction of the targets.

.. figure:: _static/png/cardinalflow.png
    :align: center

    Illustration of how two-target flow routing is determined. D = direction,
    T1 = Target1 (first clockwise target), T2 = Target2 (second clockwise
    target). I, II, III, IV = quadrant.



.. csv-table:: Index shifts (one unit) for the targets depending on the flow direction.
    :file: _static/csv/flowdirection.csv
    :header-rows: 1
    :align: center


.. _onetarget:
One-target routing
^^^^^^^^^^^^^^^^^^

One-target routing is determined by the digital elevation model and the land
cover of the two targets determined in the section above. The flow directions
and weights (cardinal space) are adjusted according to elevation and land
cover, as shown in the scheme below. Do note that in this procedure two-target
routing is adjusted to one-target routing:

.. figure:: _static/png/sketch_flow_algorithm.png
    :align: center

    Flow-chart of the routing algorithm in CN-WS - adjusting routing according
    to elevation and land cover. Note that eight adjacent are taken into
    account.

In this figure, the `Flow(target1)` or `Flow(target2)` tag indicate that
routing will follow strictly the path of the first or second flow
direction. In these cases, the flow is uni-directional, instead of
two-directional. The `find_lower` tag indicates that the algorithm will
search for the lowest neighbouring pixel: this is functionality is used to
indicate a single target (cardinal and ordinal directions) is used instead of
two targets (cardinal direction). A`jump` indicates
the target is not a adjacent pixel of the source: the routing jumps
to a single target further than its eights adjacent pixels.  Jumps are
defined within a window :math:`W`. This occurs when a source is located in a
local elevation minimum. With increasing :math:`W` more potential targets are
considered, e.g. 16 for :math:`W` = 2, 25 for :math:`W` = 3, ...

An important note is that the routing will always
jump to the closest river in :math:`W` if a river pixel is present in the
window :math:`W`. This window :math:`W` can be defined in the ini-file with the
:ref:`kernel - variable <maxkernel>`.

In the sketch, three features of the source pixel and two target pixels
are accounted for to define a rule-bank for the routing direction: the height,
the land cover code and presence of grass strips. First, it is checked whether
the targets are higher or lower than the source pixel. In case one of the
target pixels is higher, than the flow will be defined by the other target
based on the land cover code and presence of grass strips.

If both target pixels are lower, the land cover code of both targets is
checked. If both are different to the land cover code of the source, the
`find_lower`-function is called. If one or both have a different land cover
code, it is checked whether the pixels is (are) (a) grass strip(s): in
this case the flow direction will always be defined by the grass strips.

The implementation of this rule-bank aims to satisfy following conditions:

 - The routing should generally follow the height profile.

 - Routing within one agricultural parcel will remain in the parcel until
   the lowest point of the parcel is reached. Thus, the routing will follow the
   height profile in the direction of parcel boundaries rather than the
   steepest descent (in cardinal and ordinal direction).

 - Routing should target rivers as a priority target (in cardinal and ordinal
   direction).

 - Routing should target grass strips as a second priority target (in
   cardinal and ordinal). An exception is defined if the two target pixels and
   the source pixel all have different land cover codes (with one target pixel
   being a grass strip), and the target grass strip being higher than the
   other target: here the routing follows the direction of to the lowest pixel.

Forced routing
**************
**Forced routing** is typically used to force a routing vector from a specific
source to a target pixel, in case of a local suboptimal routing pattern.
Forced routing is user-defined. The instructions for defining forced routing
are found `here <forcerouting>`.

.. _riverrouting_exp:

River routing
**************
**River routing** is defined by making use of
:ref:`routing maps <routingmap>`. In addition, these map can be used to define
routing in **conductive buffer dams** and **conductive ditches**. We refer to
separate sections for the definition of routing in
:ref:`rivers <riverrouting>`, :ref:`ditches <ditchmap>` and
:ref:`dams <dammap>`. The workflow on how to create these rasters is described
in the section on :ref:`routing maps <routingmap>`.

References
==========
Takken, I., Govers, G., Jetten, V., Nachtergaele, J., Steegen, A., Poesen, J
., 2001, Effects of tillage on runoff and erosion patterns. Soil and Tillage
Research 61, 55–60. https://doi.org/10.1016/S0167-1987(01)00178-7
