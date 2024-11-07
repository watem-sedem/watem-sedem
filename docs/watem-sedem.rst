.. _WS:

#############################################################################
WaTEM/SEDEM: a spatially distributed soil erosion and sediment delivery model
#############################################################################

.. _Concept:

Concept
=======

WaTEM/SEDEM is a spatially distributed soil erosion and sediment delivery model that is
originally developed at the beginning of the 21st century at the Laboratory for
Experimental Geomorphology (KU Leuven, Belgium – nowadays the Geography and Tourism
Research Division at the KU Leuven Department of Earth and Environmental Sciences) as a
user-friendly and optimised version of the WaTEM and SEDEM models.  WaTEM stands for
Water and Tillage Erosion Model (Van Oost et al., 2000) and SEDEM is the abbreviation of
Sediment Delivery Model (Van Rompaey et al., 2001). The combined use as WaTEM/SEDEM was
specifically developed to simulate the impact of soil conservation and sediment control
measures as well as land use changes in the framework of an integrated catchment
management, on the local soil loss and sediment delivery to rivers
(Verstraeten et al., 2002).

In its original format, WaTEM/SEDEM focuses on the spatial, and less the temporal,
variability of relevant parameters. As such, WaTEM/SEDEM allows the incorporation of
landscape structure or the spatial organisation of different land units and the
connectivity between them. The water erosion component of WaTEM/SEDEM uses an adapted
version of the Revised Universal Soil loss equation (RUSLE) to calculate mean annual
soil loss values. Runoff patterns are calculated with a flow algorithm that takes into
account field borders, tillage direction and road infrastructure. Sediment is routed
along these flow paths to the nearest river using a transport capacity term that is
proportional to the potential rill erosion rate. WaTEM/SEDEM, however, does not predict
sediment transport within a river, bank erosion or floodplain sediment deposition.

In WaTEM/SEDEM, :ref:`the mean annual soil erosion rate <rusle>` :math:`E`
and :ref:`transport capacity <TC>` :math:`TC`
are calculated for every pixel in the model domain. Next, the model iterates
over all pixels according to the order determined by the :ref:`routing algorithm <routing>`.
During the iteration, the outgoing sediment for every pixel is calculated by
comparing the total available sediment in the cell :math:`S_A` (incoming
sediment, :math:`S_i` + :math:`E`) with the transport capacity.

Two cases exist:
 - :math:`S_A \leq TC`: the pixel can transport the total
   available sediment :math:`S_A`, so erosion will occur at the mean annual soil
   erosion rate. The outgoing sediment :math:`S_o` equals the total available
   sediment :math:`S_A`.
 - :math:`S_A` > :math:`TC`: the total available sediment :math:`S_A` is higher
   than the amount of sediment that can be transported. The outgoing sediment
   (:math:`S_o`) equals the transport capacity (:math:`TC`).
   The net erosion rate is lower than the mean annual erosion rate :math:`E`
   and equals :math:`TC - S_i`. If the incoming sediment :math:`S_i` is higher
   than the transport capacity :math:`TC`, net sediment deposition will occur
   and equals :math:`S_i - TC`.

Or:

.. math::
        S_o = S_A \quad\text{ if } S_A \leq TC

.. math::
        S_o = TC \quad\text{ if } S_A>TC

This binary evaluation allows the model to simulate both detachment limited and
transport limited cases of sediment transport. The outgoing sediment of each cell is
distributed to one or two target pixels, which are determined by the flux decomposition
routing algorithm within the model. The outgoing
sediment of pixel X to pixel Y is added to the incoming sediment of pixel Y.
Pixel Y can receive sediment of multiple pixels.

.. note::

  Erosion is expressed as a negative value in WaTEM/SEDEM code, whereas deposition
  (on land, in buffers, in sewers, in rivers) is expressed as a positive value.

Gross versus net erosion
========================

When using WaTEM/SEDEM, a distinction between gross and net erosion is made
(Verstraeten et al., 2007). Gross erosion varies in the landscape according to the
RUSLE equation, adapted to a 2-dimensional grid (Desmet and Govers., 1996).
A transport capacity is defined at each cell which quantifies the maximum amount of
sediment which can be exported from the cell. By evaluating gross erosion against this
transport capacity, the model can evaluate cases of net erosion or net deposition at the
cellular level.

Net erosion is defined at pixels where the rate of gross erosion is larger than the
rate of sedimentation (amount above :math:`TC`). Net
deposition is defined in pixels where the rate of sedimentation (amount above
:math:`TC`) is larger than the rate of gross erosion. In the latter case, no net erosion
is defined.

.. _rusle:

Mean annual soil erosion rate
=============================

For every pixel in the model domain or catchment, the mean annual soil
erosion rate is calculated with an adapted version of the RUSLE (Revised
Universal Soil Loss Equation, Renard et al., 1997). The mean annual soil
erosion rate :math:`E` (:math:`\frac{\text{kg}}{\text{ha.year}}`) is
calculated by

.. math::
    E = R \cdot K \cdot LS \cdot C \cdot P

with

- :math:`R`: rainfall erosivity factor (:math:`\frac{\text{MJ.mm}}{\text{ha.h.year}}`)
- :math:`K`: soil erodibility factor (:math:`\frac{\text{kg.h}}{\text{MJ.mm}}`)
- :math:`LS`: topographical slope and length factor (-)
- :math:`C`: crop management factor (-, :math:`\in [0,1]`)
- :math:`P`: erosion control factor (-, :math:`\in [0,1]`)

A detailed description of these factors is given :ref:`here <ruslefactors>`.

.. _TC:

Transport capacity calculation
==============================

For every grid cell the transport capacity :math:`TC`
is calculated according to

.. math::
    TC = ktc \cdot R \cdot K \cdot T

with

- :math:`ktc`: transport capacity coeffient :math:`(m)`
- :math:`R`: :ref:`rainfall erosivity factor <rfactor>`
- :math:`K`: :ref:`soil erobility factor <kfactor>`
- :math:`T`: topographical factor (-)

It is important to note that the :math:`ktc` factor is identified as a
calibration factor.

WaTEM/SEDEM includes two ways to calculate :math:`T`. See
:ref:`the section about the different TC models <TCmodel>` for more information.

.. _tillageerosionmodel:

Tillage erosion
===============

Tillage erosion, or soil translocation by tillage, is calculated according to
the method of Van Oost et al. (2000). When soil is moved in the upslope direction by a
tillage implement, the consequential downslope translocation will exceed that of the
prior upslope component. The result is net displacement of soil in the downslope
direction. For every pixel the outgoing flux
:math:`Q_{s,t}` :math:`(kg.m^{-1})`  due to tillage translocation is calculated as

.. math::
    Q_{s,t} = k_{til} \cdot S

with

- :math:`ktil`: tillage transport coefficient :math:`(kg.m^{-1})`
- :math:`S`: local slope gradient (-)

:math:`S` is calculated as

.. math::
    S = dh/dx

with

- :math:`dh`: change in height :math:`(m)`
- :math:`dx`: change in distance in horizontal direction :math:`(m)`

Note that WaTEM/SEDEM uses the same slope calculation for the calculation
of the LS-factor and the tillage erosion. The calculated slope can be consulted
in the :ref:`slope raster <slopemap>`.

The local erosion or deposition rate by tillage (:math:`E_t`) can then be calculated as:

.. math::
    E_t = - \frac{Q_{s,t}}{dx}

The outgoing sediment volume of a cell is distributed to one or two target pixels.
Every target cell receives a fraction of the available sediment of the source cell.
The target cells are determined by the routing algorithm. The fraction of the
outgoing sediment of pixel X to pixel Y is added to the incoming sediment of
pixel Y.
Pixel Y can receive sediment of multiple pixels. The volume is converted to mass
via the :ref:`bulk density <bulkdensity>`.

Soil redistribution by tillage only takes place within agricultural fields. In these
areas, tillage may be modelled as a diffusion process. The functional equation implies
that tillage erosion is controlled by the change in slope gradient, not by the slope
gradient itself, so that erosion takes place on convexities while soil accumulation
occurs in concavities. The intensity of the process is controlled by the value of a
single constant, ktil (the diffusion constant) which need to be given by the user.

.. _ruslefactors:

RUSLE factors
=============

In this section, the different parameters of the RUSLE equation (Renard et al.
, 1997) are described briefly.

.. _rfactor:

R-factor
########

The erosive power of rainfall is quantified via the rainfall erosivity factor
(:math:`R`-factor). The R-factor quantifies the mean annual average rainfall erosivity,
calculated by combining rainfall events over multiple years (22 years according to the
USLE definition) and is provided in :math:`\frac{\text{MJ.mm}}{\text{ha.h.year}}`.

Note that the R-factor is given as a single integer value representative for a small
(e.g. a small catchment) spatial area. For larger areas, spatial variability in rainfall
erosivity may be more important, however. If this is the case, this can be compensated
by multiplying a rainfall erosivity map with the soil erodibility map (K-factor) and use
this map as input for the K-factor. The value of R then needs to be 1.

.. _kfactor:

K-factor
########

The soil erodibility factor, :math:`K` quantifies the
change in the soil loss per unit of rainfall energy.
The unit of :math:`K` is expressed in soil loss per rainfall erosion index unit,
in this case :math:`\frac{\text{kg.h}}{\text{MJ.mm}}` (Renard et al., 1997).

Soil erodibility can be calculated from the USLE nomograph or using empirical equations
predicting the K-factor from the geometric mean particle diameter and organic matter
content.

The K-factor has large temporal variations, so the values always represent a long term
average.

.. _lsfactor:

LS-factor
#########

The effect of topography on erosion is quantified in the LS-factor. Erosion
increases as the slope length increases - quantified in the slope length
factor (L), and as the slope steepness factor (S) increases.
The effects of the L- and S-factor factors are typically evaluated together.
In WaTEM/SEDEM model, contrary to the original RUSLE model, the LS-factor is
computed by considering the two-dimensional stream flow algorithm of WaTEM/SEDEM
(Desmet and Govers, 1996). This allows for computing concentrated erosion
flow, such as rill and gully erosion.
The algorithm (Desmet en Govers 1996) uses a DEM (Digital Elevation Model) and a parcel
map and was adjusted by Takken et al. (2001) so that tillage direction is taken into
account. The location of roads is also included: water on a road will always follow that
road to the lowest point. Both topographical parameters are derived from a DEM
(Verstraeten et al. 2002).

Van Oost et al. (2000) showed that field parcel boundaries may have an important impact
on erosion and sediment delivery at the catchment scale depending on how effective
individual boundaries are in trapping water and/or sediment. In WaTEM/SEDEM, the effect
of field parcel boundaries, or more generally when water flow from one land use type to
another, can be simulated through the parcel connectivity value and PTEF-value, which
changes the LS-factor.
The PTEF-value refers to the way that for each land use class the pixel's own
contribution to the upstream contributing area is reduced. For instance, if for forest
pixels, a value of 75 per cent is given, every pixel of 20 by 20 meter will only
contribute 100 m² to the downstream pixel, instead of 400 m². In fact, this means that
for a whole forest or pasture, less runoff will be simulated, thereby decreasing
downstream LS values, and thus, erosion rates. Total upstream contributing area from
upstream the forest will however be delivered entirely through the forest to another
land use class. Values for PTEF can be chosen for cropland, pasture and forest
separately. Alternatively, a whole PTEF map can be given as well.

The parcel connectivity  refers to the way the total upstream contributing area is
reduced at a parcel border. Two categories of field boundaries are acknowledged: the
transition from any type of land use to cropland and the transition from any type of
land use to forest or pasture. If, for instance, the value 'to forest/pasture' equals
75 per cent, and the total contributing area of the pixel upstream the forest border is
300 ha, the contributing area of the first pixel in the forest will only by 75 ha. In
this way, forests or pastures also trap runoff from upstream, thereby again reducing
downstream LS and erosion rates.
Note that the effect of PTEF and parcel connectivity has not been evaluated in-depth.

It is important to note that the computation of the LS-factor is defined
through the flow routing algorithm, and not by the neighbouring
pixels. This is important in cases where the flow routing is not defined by
digital elevation model, but by other factors (see :ref:`routing <routing>`).  
Finally, note that there exist  different ways to compute the 
:ref:`L- <lmodel>` and :ref:`S-factor <smodel>` (Alewell et al., 2019), of which several
are incorporated into WaTEM/SEDEM

.. _cfactor:

C-factor
########

The crop management factor (C-factor) is a dimensionless factor (0 – 1) that represents
the erosional susceptibility of a given land use type compared to a non-vegetated or
bare land cover.

Note that within the RUSLE method, the C-factor over a period of time (e.g. one-year) is
calculated for a field according to the weighted sum of the soil loss ratio (SLR) and
15-day average rainfall erosivity:

.. math::
    C = \frac{\sum_i^t{R_i} \cdot SLR_i}{\sum_i^t{R_i}}


with
 - :math:`R_i`: rainfall erosivity factor (:math:`\frac{\text{MJ.mm}}{\text{ha.h.TR}}`) with
   :math:`\text{TR}`: temporal resolution (typically 15-days).
 - :math:`t`: the maximum number of the increments.
 - :math:`SLR`: the soil loss ratio (-).

We refer to Renard et al. (1997) for an in-depth overview of the C- and SLR-factor.
However, briefly, the SLR for each time-period is composed of 5 sub-factors:

.. math::
        SLR = PLU * SR * CC * SC * SM

with
 - :math:`PLU`: prior landuse sub-factor
 - :math:`SR`: soil roughness sub-factor
 - :math:`CC`: crop canopy sub-factor
 - :math:`SC`: soil coverage sub-factor (i.e. crop residual value)
 - :math:`SM`: soil moisture sub-factor

Within WaTEM/SEDEM, the user can provide a C-factor map representing the spatial
variability in land use, e.g. on a field parcel basis. Alternatively, mean C-factor
values for the most important land use categories can be provided and these values can
be assigned to the land use categories in the land use map.


.. _pfactor:

P-factor
########

The support practice factor is a dimensionless factor that represents the  the ratio of
soil loss for a field with structural soil and water conservation (SWC) measures
compared to a situation without. Whilst this factor is typically applied in the RUSLE,
its use is less relevant within WaTEM/SEDEM and also has not been tested so far.
The P-factor traditionally incorporates the effects of SWC measures on downstream flow
patterns and volumes which are not simulated otherwise in a static RUSLE approach.
However, WaTEM/SEDEM routes sediment in the landscape and by altering upstream
vegetation cover (e.g. buffer strips), field boundaries, or ponds, their impact on
downstream erosion and sediment delivery can be simulated implicitly.

References
==========

Alewell, C., Borrelli, P., Meusburger, K., & Panagos, P. (2019). Using the USLE:
Chances, challenges and limitations of soil erosion modelling. International Soil and
Water Conservation Research, 7. https://doi.org/10.1016/j.iswcr.2019.05.004

Declercq, F., Poesen, J., 1992, Evaluation of two models to calculate the
soil erodibility factor K. Pedologie XLII, 149–169.

Deproost, P., Renders, D., Van de Wauw, J., Van Ransbeeck, N.,
Verstraeten, G., 2018, Herkalibratie van WaTEM/SEDEM met het DHMV-II als
hoogtemodel: eindrapport. Brussel.
https://archief.onderzoek.omgeving.vlaanderen.be/Onderzoek-1812384

Desmet, P.J.J., Govers, G., 1996, A gis procedure for automatically
calculating the USLE LS factor on topographically complex landscapes.
Journal of Soil and Water Conservation 51, 427–433.
https://www.jswconline.org/content/51/5/427

Nearing, M.A., 1997, A single continuous function for slope steepness
influence on soil loss. Soil Science Society of America Journal 61, 917–919.
https://doi.org/10.2136/sssaj1997.03615995006100030029x

Notebaert, B., Govers, G., Verstraeten, G., Van Oost, K., Poesen, J., Van
Rompaey, A., 2006, Verfijnde erosiekaart Vlaanderen: eindrapport. K.U.
Leuven, Leuven.
https://omgeving.vlaanderen.be/sites/default/files/atoms/files/Verfijnde_erosiekaart.pdf

Panagos, P., Ballabio, C., Borrelli, P., Meusburger, K., Klik, A., Rousseva,
S., Tadić, M.P., Michaelides, S., Hrabalíková, M., Olsen, P., Aalto, J.,
Lakatos, M., Rymszewicz, A., Dumitrescu, A., Beguería, S., Alewell, C., 2015
. Rainfall erosivity in Europe. Science of The Total Environment 511, 801–814.
https://doi.org/10.1016/j.scitotenv.2015.01.008

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
Earth Surface Processes and Landforms 26, 1221–1236. 
https://doi.org/10.1002/esp.275

Verbist, K., Schiettecatte, W., Gabriels, D., 2004, End report.
Computermodel RUSLE c-factor. Universiteit Gent, Gent.

Verstraeten, G., Poesen, J., Demarée, G., Salles, C., 2006, Long-term
(105 years) variability in rain erosivity as derived from 10-min rainfall
depth data for Ukkel (Brussels, Belgium): Implications for assessing soil
erosion rates. J. Geophys. Res. 111, D22109. https://doi.org/10.1029/2006JD007169

Verstraeten, G., Prosser, I.P., Fogarty, P., 2007. Predicting the spatial
patterns of hillslope sediment delivery to river channels in the
Murrumbidgee catchment, Australia. Journal of Hydrology 334, 440–454.
https://doi.org/10.1016/j.jhydrol.2006.10.025

Verstraeten, G., Van Oost, K., Van Rompaey, A., Poesen, J., & Govers, G. (2002).
Evaluating an integrated approach to catchment management to reduce soil loss
and sediment pollution through modelling. Soil use and management, 18(4), 386-394
https://doi.org/10.1111/j.1475-2743.2002.tb00257.x

Wischmeier, W. H., & Smith, D. D. (1978). Predicting Rainfall Erosion Losses: A Guide
to Conservation Planning. In United States Department of Agriculture. Hyattsville, Md.
(USA) US Dept. of Agriculture, Science and Education Administration.

Zevenbergen, L.W., Thorne, C.R., 1987, Quantitative analysis of land surface
topography. Earth Surface Processes and Landforms 12, 47–56.
https://doi.org/10.1002/esp.3290120107

