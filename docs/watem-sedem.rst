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

In WaTEM-SEDEM the mean annual soil erosion rate *E* and transport capacity
*TC* are calculated for every pixel in the model domain. Next, the model
iterates over all pixels according to the order determined by the routing
algorithm. During the iteration, the outgoing sediment for every pixel is
calculated by comparing the the total available sediment in the cell
(incoming sediment + *E*) with the transport capacity.

Two cases exist:

- The total available sediment in a cell is lower than *TC*:
	- the resulting mass balance is negative (the pixel can transport more
than  the amount of sediment available to transport, so 'erosion' will occur)
	- the outgoing sediment is set equal to the available sediment
- The available sediment exceeds *TC*:
	- the resulting mass balance is positive (enough sediment to transport,
some sediment will be 'deposited' in the cell)
	- the outgoing sediment is equal to *TC* and (available sediment - TC)
will be deposited in the cell.

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
erosion rate *E* (kg. m$^{-2}$. year$^{-1}$) is calculated by

	*E = R.K.LS.C.P*

Where:

- *R*: rainfall erosivity factor (MJ. mm. m$^{-2}$. h$^{-1}$. year$^{-1}$)
- *K*: soil erodibility factor (kg. h. MJ$^{-1}$. mm$^{-1}$)
- *LS*: topgographical slope and length factor (-)
- *C*: crop erosivity factor (-, $\in[0,1]$)
- *P*: erosion control factor (-, $\in[0,1]$)

A detailed description of these factors is given :ref:`here <ruslefactors>`.


Transport capacity calculation
==============================

For every grid cell the transport capacity *TC* (kg m$^{-1}$ year$^{-1}$)
is calculated by:

	*TC = kTC.R.K.(LS - 4.12.S$_g$^{0.8}$)*

Where:

- *kTC*: transport capacity coeffient (m)
- *R*: rainfall erosivity factor (MJ. mm. m$^{-2}$. h$^{-1}$. year$^{-1}$)
- *K*: soil erodibility factor (kg. h. MJ$^{-1}$. mm$^{-1}$)
- *LS*: topographical slope and length factor (-)
- *S$_g$*: local slope (m. m$^{-1}$)

A detailed description of these factors is given :ref:`here <ruslefactors>`.
It is important to note that the *kTC* factor is identified as a calibration
factor. In addition, in order to compare *TC* with the available sediment in
a pixel (see :ref:`here <Concept>`), units are converted to kg pixel$^{-1}$
or m$^3$ pixel$^{-1}$ by making use of the model resolution (m) and bulk
density (kg m$^{-1}$)


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
*R*. This is a measure for the total erosivity of a number of rainfall
events within a defined timeframe (year, month, number of days). The factor
is computed by calculating the depth of rainfall (mm) and the kinetic energy
of one event. For applications of the rainfall erosivity factor in the
context of Flanders a value of 870 MJ mm  ha$^{-1}$  h$^{-1}$ jaar$^{-1}$ is
used since 2006 (Verstraeten et al., 2006). Recently, this value has been
updated to 1250 MJ. mm. m$^{-2}$. h$^{-1}$ year$^{-1}$ (Deproost et al., 2018)

.. _kfactor:

K-factor
########

TO DO

.. _lsfactor:

LS-factor
#########

TO DO

.. _cfactor:

C-factor
########

The crop erosivity factor (C-factor) is based on the concept of deviatgion
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

