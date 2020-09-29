###########
WaTEM-SEDEM
###########

.. _Concept:

Concept
=======

WaTEM-SEDEM is a spatially distributed model that was created at the
Laboratry for Experimental Geomorphology (KU Leuven, Belgium). WaTEM stands
for  Water and Tillage erosion model (Van Oost et al., 2000) and SEDEM is
the abbreviation of Sediment Delivery Model (Van Rompaey et al., 2001).

In WaTEM-SEDEM the mean annual soil erosion rate :math:`E` (see :ref:`here <RULSE>`)
and transport capacity :math:`TC` (see :ref:`here <TC>`) are calculated for every
pixel in the model  domain. Next, the model iterates over all pixels
according to the order determined by the routing algorithm. During the
iteration, the outgoing  sediment for every pixel is calculated by comparing
the the total available  sediment in the cell (incoming sediment + *E*) with
the transport capacity.

Two cases exist:
 - The total available sediment in a cell is lower than *TC*: the resulting
   mass balance is negative (the pixel can transport more than the amount of
   sediment available to transport, so 'erosion' will occur). The outgoing
   sediment is set equal to the available sediment
 - The available sediment exceeds *TC*: the resulting mass balance is
   positive (enough sediment to transport some sediment will be 'deposited' in
   the cell). The outgoing sediment is equal to *TC* and (available sediment -
   *TC*) will be deposited in the cell.

The outgoing sediment of a cell is distributed to one or two target pixels.
The target cells are determined by the routing algorithm. The outgoing
sediment of pixel X to pixel Y is added to the incoming sediment of pixel Y.
Pixel Y can receive sediment of multple pixels.

This process is illustrated in figure (TO DO: create figure with pixel).

.. _RUSLE:

Mean annual soil erosion rate
=============================

For every pixel in the model domain or catchment, the mean annual soil
erosion  rate is calculated with an adapted version of the RUSLE (Revised
Universal Soil Loss Equation, Renard et al., 1997). The mean annual soil
erosion rate :math:`E` (:math:`\frac{\text{kg}}{\text{m}^{2}.\text{year}}`) is calculated by

.. math::
    E = R.K.LS.C.P

Where:

- :math:`R`: rainfall erosivity factor (:math:`\frac{\text{MJ.mm}}{\text{m}^2.\text{h.year}}`)
- :math:`K`: soil erodibility factor (:math:`\frac{\text{kg.h}}{\text{MJ.mm}}`)
- :math:`LS`: topgographical slope and length factor (-)
- :math:`C`: crop erosivity factor (-, :math:`\in [0,1]`)
- :math:`P`: erosion control factor (-, :math:`\in [0,1]`)

A detailed description of these factors is given :ref:`here <ruslefactors>`.

.. _TC:

Transport capacity calculation
==============================

For every grid cell the transport capacity :math:`TC` (:math:`\frac{\text{kg}}{\text{m.year}}`)
is calculated by:

.. math::
    TC = kTC.R.K.(LS - 4.12.S_g^{0.8})

Where:

- :math:`kTC`: transport capacity coeffient (m)
- :math:`S_g`: local slope (:math:`\frac{\text{m}}{\text{m}}`)

A detailed description of these factors is given :ref:`here <ruslefactors>`.
It is important to note that the :math:`kTC` factor is identified as a calibration
factor. In addition, in order to compare :math:`kTC` with the available sediment in
a pixel (see :ref:`here <Concept>`), units are converted to :math:`\frac{\text{kg}}{\text{pixel}}`
or :math:`\frac{\text{m}^3}{\text{pixel}}` by making use of the model resolution (m) and bulk
density (:math:`\frac{\text{kg}}{\text{m}^3}`)


Tillage erosion
===============

TO DO, see Van Oost et al. 2000.

.. _ruslefactors:

RUSLE factors
=============

In this paragraph the different parameters of the RUSLE equation (Renard et al.
, 1997) are described.

.. _rfactor:

R-factor
########
The erosive power of rainfall is quantified in the rainfall erosivity factor
:math:`kTC`. This is a measure for the total erosivity of a number of rainfall
events within a defined timeframe (year, month, number of days). The factor
is computed by calculating the depth of rainfall (mm) and the kinetic energy
of one event. For applications of the rainfall erosivity factor in the
context of Flanders a value of 870 :math:`\frac{\text{MJ.mm}}{\text{ha.h.year}}` is
used since 2006 (Verstraeten et al., 2006). Recently, this value has been
updated to 1250 :math:`\frac{\text{MJ.mm}}{\text{ha.h.year}}` (Deproost et al., 2018)

.. _kfactor:

K-factor
########

TO DO

.. _lsfactor:

LS-factor
#########

Erosion increases as the slope length (:math:`L`) and slope gradient (:math:`S`) increases. The effects of these factors are typically evaluated together. In the CN-WS model, contrary to the original RUSLE model, the LS-factor is computed by considering the 2-D stream flow algorithm in the CN-WS (Desmet and Govers, 1996). This allows for computing concentrated erosion flow, such as rill and gully erosion.

The topographic length factor (L-factor) can be computed by using the formulation of Desmet and Govers (1996), considering the upstream area (:math:`A\text{ m}^2`) for every raster pixel:

.. math::
    L = \frac{(A+D^2)^{m+1}-A^{m+1}}{D^{m+2}.x^m.22,13^m}

with:

:math:`D` = grid resolution (m)
:math:`m` = length exponent.
:math:`x` = factor incorporating the flow direction.

For the computation of :math:`m` and :math:`x`,, we refer to Deproost et al. (2018). The upstream area in a pixel is determined by the stream flow algorithm, by considering a parcel trapping efficiency and the parcel connectivity. The parcel trapping efficiency (PTEF) is used to potentially reduce the upstream area. It typically varies as a function of a number of land-use categories, e.g. forest, agriculture and infrastructure. For pixels with a land-use 'agriculture', the PTEF is typically set to 0. The parcel connectivity quantifies the flow amount, expressed in upstream area, that flows from an upstream to a downstream parcel (Notebaert et al., 2006). The upstream area is multiplied with a factor equal to the parcel connectivity. The parcel connectivity typically varies as a function of the land-use of the target pixel (Deproost et al., 2018).

The S-factor is computed based on Nearing (1997):

with :math:`\theta` = the inclination angle (%)

The computation of the inclincation angle is based on the four cardinal neighbouring pixels (Zevenbergen and Thorne, 1987).

.. _cfactor:

C-factor
########

The crop erosivity factor (C-factor) is based on the concept of deviation
from a standard, in this case defined by a parcel under clean-tilled
continuous-fallow conditions (Renard et al., 1997). It can be quantified
as the ratio of the soil loss of a specific parcel with crop cover -
cultivated under specific conditions - and soil loss that would occur on the
same parcel without crop growth (with plowing perpendicular to the
height lines) (Verbist et al., 2004). For an in-depth overview of the
C-factor we refer to Renard et al. (1997).

.. _pfactor:

P-factor
########

TO DO


References
==========

Deproost, P., Renders, D., Van de Wauw, J., Van Ransbeeck, N.,
Verstraeten, G., 2018, Herkalibratie van WaTEM/SEDEM met het DHMV-II als
hoogtemodel: eindrapport. Brussel.  https://archief.onderzoek.omgeving.vlaanderen.be/Onderzoek-1812384

Desmet, P.J.J., Govers, G., 1996. A gis procedure for automatically calculating the USLE LS factor on topographically complex landscapes. Journal of Soil and Water Conservation 51, 427–433.

Nearing, M.A., 1997. A single continuous function for slope steepness influence on soil loss. Soil Science Society of America Journal 61, 917–919.


Notebaert, B., Govers, G., Verstraeten, G., Van Oost, K., Poesen, J., Van Rompaey, A., 2006. Verfijnde erosiekaart Vlaanderen: eindrapport. K.U. Leuven, Leuven.

Renard, K.G., Foster, G.R., Weesies, G.A., McCool, D.K., Yoder, D.C.,
1997, Predicting soil erosion by water: a guide to conservation planning with
the revised universal soil loss equation (RUSLE), Agriculture Handbook. U.S.
Department of Agriculture, Washington.
https://www.ars.usda.gov/ARSUserFiles/64080530/RUSLE/AH_703.pdf

Van Oost, K., Govers, G., Desmet, P., 2000, Evaluating the effects of
changes in landscape structure on soil erosion by water and tillage.
Landscape Ecology 15, 577–589. https://doi.org/10.1023/A:1008198215674

Van Rompaey, A.J.J., Verstraeten, G., Van Oost, K., Govers, G., Poesen, J
., 2001, Modelling mean annual sediment yield using a distributed approach.
Earth Surf. Process. Landforms 26, 1221–1236. https://doi.org/10.1002/esp.275

Verbist, K., Schiettecatte, W., Gabriels, D., 2004, End report.
Computermodel RUSLE c-factor. Universiteit Gent, Gent.

Verstraeten, G., Poesen, J., Demarée, G., Salles, C., 2006, Long-term
(105 years) variability in rain erosivity as derived from 10-min rainfall
depth  data for Ukkel (Brussels, Belgium): Implications for assessing soil
erosion rates. J. Geophys. Res. 111, D22109. https://doi.org/10.1029/2006JD007169

Verstraeten, G., Van Rompaey, A., Poesen, J., Van Oost, K., Govers, G.,
2003, Evaluating the impact of watershed management scenarios on changes in
sediment delivery to rivers? Hydrobiologia 494, 153–158.

Zevenbergen, L.W., Thorne, C.R., 1987. Quantitative analysis of land surface topography. Earth Surf. Process. Landforms 12, 47–56. https://doi.org/10.1002/esp.3290120107

