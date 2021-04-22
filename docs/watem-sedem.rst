.. _WS:

###################################
Erosion/sediment model: WaTEM/SEDEM
###################################

.. _Concept:

Concept
=======

WaTEM/SEDEM is a spatially distributed model that was created at the
Laboratry for Experimental Geomorphology (KU Leuven, Belgium). WaTEM stands
for Water and Tillage erosion model (Van Oost et al., 2000) and SEDEM is
the abbreviation of Sediment Delivery Model (Van Rompaey et al., 2001).

In WaTEM/SEDEM, the mean annual soil erosion rate :math:`E` (see
:ref:`here <rusle>`) and transport capacity :math:`TC` (see :ref:`here <TC>`)
are calculated for every pixel in the model domain. Next, the model iterates
over all pixels according to the order determined by the routing algorithm.
During the iteration, the outgoing sediment for every pixel is calculated by
comparing the the total available sediment in the cell :math:`S_A` (incoming
sediment, :math:`S_i` + :math:`E`) with the transport capacity.

Two cases exist:
 - :math:`S_A` < :math:`TC`: the resulting mass balance is negative (the
   pixel can transport more than the amount of sediment available to
   transport, so 'erosion' will occur). The outgoing sediment is set equal
   to the available sediment
 - :math:`S_A` > or equal to :math:`TC`: the resulting mass balance is
   positive (enough sediment to transport, some sediment will be 'deposited'
   in the pixel). The outgoing sediment (:math:`S_o` is equal to :math:`TC`
   and (S_A - :math:`TC`) will be deposited in the cell.

Or:

.. math::
        S_o = S_A \quad\text{ if } S_A \leq TC

.. math::
        S_o = S_A-TC \quad\text{ if } S_A>TC

The outgoing sediment of a cell is distributed to one or two target pixels.
The target cells are determined by the routing algorithm. The outgoing
sediment of pixel X to pixel Y is added to the incoming sediment of pixel Y.
Pixel Y can receive sediment of multiple pixels.

This process is illustrated in figure (TO DO: create figure with pixel).

.. _rusle:

Mean annual soil erosion rate
=============================

For every pixel in the model domain or catchment, the mean annual soil
erosion rate is calculated with an adapted version of the RUSLE (Revised
Universal Soil Loss Equation, Renard et al., 1997). The mean annual soil
erosion rate :math:`E` (:math:`\frac{\text{kg}}{\text{m}^{2}.\text{year}}`) is
calculated by

.. math::
    E = R.K.LS.C.P

with

- :math:`R`: rainfall erosivity factor (:math:`\frac{\text{MJ.mm}}{\text{m}^2.\text{h.year}}`)
- :math:`K`: soil erodibility factor (:math:`\frac{\text{kg.h}}{\text{MJ.mm}}`)
- :math:`LS`: topographical slope and length factor (-)
- :math:`C`: crop management factor (-, :math:`\in [0,1]`)
- :math:`P`: erosion control factor (-, :math:`\in [0,1]`)

A detailed description of these factors is given :ref:`here <ruslefactors>`.

.. _TC:

Transport capacity calculation
==============================

For every grid cell the transport capacity :math:`TC`
(:math:`\frac{\text{kg}}{\text{m.year}}`) is calculated according to

.. math::
    TC = kTC.R.K.T

with

- :math:`kTC`: transport capacity coeffient :math:`(m)`
- :math:`R`: :ref:`rainfall erosivity factor <rfactor>`
- :math:`K`: :ref:`soil erobility factor <kfactor>`
- :math:`T`: topographical factor (-)

It is important to note that the :math:`kTC` factor is identified as a
calibration factor. In addition, in order to use :math:`TC` to compare with the
available sediment in a pixel (see :ref:`here <Concept>`), units are converted
to :math:`kg.pixel^{-1}` or
:math:`m^3.pixel^{-1}` by making use of the model resolution
(m) and bulk density (:math:`kg.m^{-3}`)

CN-WS includes two ways to calculate :math:`T`. See
:ref:`the section about the different TC models <TCmodel>` for more information.

.. _tillageerosionmodel:

Tillage erosion
===============

Tillage erosion, or soil translocation by tillage, is calculated according to
the method of Van Oost et al. (2000). For every pixel the outgoing flux
:math:`Q_{s,t}` :math:`(kg.m^{-1})`  due to tillage translocation is calculated as

.. math::
    Q_{s,t} = k_{til}.S

with

- :math:`ktil`: tillage transport coefficient :math:`(kg.m^{-1})`
- :math:`S`: local slope gradient (-)

:math:`S` is calculated as

.. math::
    S = dh/dx

with

- :math:`dh`: change in height :math:`(m)`
- :math:`dx`: change in distance in horizontal direction :math:`(m)`

Note that the CN-WS model uses the same slope calculation for the calculation
of the LS-factor and the tillage erosion. The calculated slope can be consulted
in the :ref:`slope raster <slopemap>`.

The local erosion or deposition rate by tillage (:math:`E_t`) can then be calculated as:

.. math::
    E_t = - \frac{Q_{s,t}}{dx}

The outgoing sediment volume of a cell is distributed to one or two target pixels.
The target cells are determined by the routing algorithm. The outgoing
sediment of pixel X to pixel Y is added to the incoming sediment of pixel Y.
Pixel Y can receive sediment of multple pixels. The volume is converted to mass
via the :ref:`bulkdensity <bulkdensity>`.

Soil redistribution by tillage only takes place within agricultural fields.

.. _ruslefactors:

RUSLE factors
=============

In this section, the different parameters of the RUSLE equation (Renard et al.
, 1997) are described.

.. _rfactor:

R-factor
########
The erosive power of rainfall is quantified in the rainfall erosivity factor
:math:`R`. This is a measure for the total erosivity of a number of rainfall
events within a defined timeframe (year, month, number of days). The factor
is computed by calculating the yearly sum of -for every rainfall event- the
sum of the depth of rainfall (mm) and the kinetic energy, and taking the
mean over all years:

.. math::

    R = \frac{1}{n}\sum_{j=1}^{n}[\sum_{k=1}^{m_j}E_k.(I_{30})_k]_j

with
 - :math:`R`: rainfall erosivity factor(:math:`\frac{\text{J
   .mm}}{\text{m}^2.\text{h.year}}`)
 - :math:`n`, increment :math:`j`: number of years
 - :math:`m_j`, increment :math:`k`: number of rain events in year :math:`j`
 - :math:`E`: the total kinetic energy of one single rain event
   (:math:`\frac{J}{m^2}`).
 - :math:`I_{30}` (:math:`mm.h^{-1}`): the maximum rain intensity
   recorded within 30 consecutive minutes.

The total kinetic energy for one single rain event can be defined as:


.. math::

    E = \sum_{r=1}^0 e_r \Delta V_r

with
 - :math:`e_r`: the rain energy per unit depth
   (:math:`\frac{\text{J}}{\text{m}^{2}.\text{mm}}`). There are a number of
   ways to compute, see Verstraeten et al. (2006) and Panagos et al. (2015).
 - :math:`\Delta V_r`: the rain depth :math:`(mm)`.

For applications of the rainfall erosivity factor in the context of Flanders
a value of 870 :math:`\frac{\text{MJ.mm}}{\text{ha.h.year}}` is used since
2006 (Verstraeten et al., 2006). Recently, this value has been updated to
1250 :math:`\frac{\text{MJ.mm}}{\text{ha.h.year}}` (Deproost et al., 2018).

.. note::
    The R-factor can also be defined with other temporal resolutions.
    For computing WaTEM/SEDEM on a monthly resolution, the value :math:`R` can
    be defined by the mean of each value for each month over a number of years
    (mean fo all january values over 10 years). In this case the unit is
    :math:`\frac{\text{J.mm}}{\text{m}^2.\text{h.month}}`

.. _kfactor:

K-factor
########

The soil erodibility factor, :math:`K`,  is an index that quantifies the
change in the soil per unit of applied external force or energy, in this
case rainfall. It is thus related to the integrated effect of rainfall,
runoff and infiltration on soil loss. The unit of :math:`K` is expressed in
soil loss per rainfall erosion index unit, in this case
:math:`\frac{\text{kg.h}}{\text{MJ.mm}}` (Renard et al., 1997). In
practical terms, the :math:`K`-factor is a lumped parameter often varying as
a function of the soil texture. In the context of Flanders,  Declrercq and
Poesen (1991) applied this on the soil texture classes in the digital soil
map of Flanders:

.. math::

    K  = 0.0035 + 0.03888 \exp^{0.5(\frac{\log_{10}{D_g}+1.519}{0.7584})^2}

with
 - :math:`D_g` =  geometric mean particle diameter :math:`(mm)`:

.. math::

    D_g = \exp^{\sum{f_i \ln(d_i+d_{i-1})0.5}}

with
 - :math:`i` = the weight percentage of the texture class :math:`i` (fraction).
 - :math:`d_i` and :math:`d_{i-1}` = the maximum and minimum diameter of the
   texture class :math:`i` :math:`(mm)`.

By using the latter two equations with the soil texture map of Flanders, a
K-factor was defined for every soil texture class.

.. _lsfactor:

LS-factor
#########

The effect of topography on erosion is quantified in the LS-factor. Erosion
increases as the slope length increases - quantified in the slope length
factor (L), and as the slope steepness factor (S) increases. In general the
L-factor is defined as the horizontal distance from the origin of overland flow to the
point where either (1) the slope gradient decreases to the degree that
deposition occurs or (2) runoff becomes concentrated in a defined channel.
The effects of the L- and S-factor factors are typically evaluated together
. In the CN-WS model, contrary to the original RUSLE model, the LS-factor is
computed by considering the two-dimensional stream flow algorithm of CN-WS
(Desmet and Govers, 1996). This allows for computing concentrated erosion
flow, such as rill and gully erosion. 

It is important to note that the computation of the LS-factor is defined
by the definition of the flow routing algorithm, and not by the neighbouring
pixels. This is important in cases where the flow routing is not defined by
digital elevation model, but by other factors (see :ref:`routing <routing>`).  
Finally, note that there exist  different ways to compute the 
:ref:`L- <lmodel>` and :ref:`S-factor <smodel>`.

.. _cfactor:

C-factor
########

The crop management factor (C-factor) is based on the concept of deviation
from a standard, in this case defined by a parcel under clean-tilled
continuous-fallow conditions (Renard et al., 1997). It can be quantified
as the ratio of the soil loss of a specific parcel with crop cover -
cultivated under specific conditions - and soil loss that would occur on the
same parcel without crop growth (with plowing perpendicular to the
contour lines) (Verbist et al., 2004). For a run of the simplified version of
CN-WS, the C-factor is defined in the context of one year. The use of the
long-term version of the model will require the definition of the C-factor
for every season.

There are a number of ways to set the C-factor:

1. Use default values varying as a function of the land-use. In the context
of Flanders, the general values 0.37, 0.01 and 0.001 are used to define the C-factor
for pixels with respectively a land use equal to agriculture, grassland
and forest.

2. Use the default values as defined in 1., but vary the C-factor for pixels
with land-use `agriculture` as a function of the crop.

3. Use the default values as defined in 1., but vary the C-factor in
function of a crop growth model and crop rotation scheme, eventualy combined
with crop management (this for pixels with land-use `agriculture`:

.. math::
    C = \frac{\sum_i^t{R_i}.SLR_i}{\sum_i^t{R_i}}


with
 - :math:`R_i`: rainfall erosivity factor (:math:`\frac{\text{J.mm}}{\text{m}^2.\text{h.TR}}`) with :math:`\text{TR}`: temporal resolution.
 - :math:`t`: the maximum number of the increments.
 - :math:`SLR`: the soil loss ratio (-). The SLR varies as a function of the
   used C-factor model. We refer to Renard et al. (1997) for an in-depth
   overview of the C- and SLR-factor.

.. _pfactor:

P-factor
########

The support practice factor is the ratio of soil loss with a specific
support practice to the corresponding loss with upslope and downslope
tillage (Renard et al., 1997). Support practice should affect erosion by
modifying the flow pattern, grade or direction of surface run-of and by
reducing the amount an rate of run-off.

References
==========

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

Verstraeten, G., Van Rompaey, A., Poesen, J., Van Oost, K., Govers, G.,
2003, Evaluating the impact of watershed management scenarios on changes in
sediment delivery to rivers? Hydrobiologia 494, 153–158.
https://link.springer.com/chapter/10.1007/978-94-017-3366-3_21

Zevenbergen, L.W., Thorne, C.R., 1987, Quantitative analysis of land surface
topography. Earth Surface Processes and Landforms 12, 47–56.
https://doi.org/10.1002/esp.3290120107

