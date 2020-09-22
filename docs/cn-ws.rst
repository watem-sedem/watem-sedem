#####
CN-WS
#####

CN-WS was developed in 2016 by KU Leuven (in a consortium with Antea group Belgium).
This model was made to to simulate the sediment transport to watercourses and to
evaluate the effects of erosion control measures, both in space and time, based
on time series of rainfall events. The model combines two existing models:
WaTEM-SEDEM and the Curve Number model.

Description of the CN-WS model
==============================

As stated above, two models are combined to simulate sediment transport to a
river during a rainfall event: the Curve Number model and WaTEM-SEDEM.

The CN model is used to simulate the run-off as a function of time and space. 
For every outlet or (optionally) river segment a hydrogram (stream velocity as a
fuction of time) is generated.

WaTEM-SEDEM is used to model the total sediment load leaving the model domain
via rivers or an outlet. This model is not time dependent, but the sediment is
distributed spatially. CN-WS calculates the R-factor for WaTEM-SEDEM from the
time series of rainfall data used in the CN model. The calculation of the
R-factor is done with the methodology of Verstraeten et al. (2006).

The output of both models is combined: the total sediment load is distributed
over the hydrogram to obtain a sedigram (sediment concentration and sedimentload
as a fuction of time). The sediment concentration *SC* is calculated for every
timestep as:

.. math::
    SC = \frac{(SV*1000)}{R*1000}

With:

- *SC*, the sediment concentration (g/l)
- *SV*, the sedimentload (kg)
- *R*, the run-off (m^3)

Additional model features
=========================

Some additional functionalities are built-in CN-WS, next to the basic model
features of WaTEM-SEDEM and the CN model. These additional functionalities have
a large impact on water and sediment transport through the model domain.

Buffer basins
*************

TO DO

Dams and ditches
****************

Dams and ditches change the direction of water and sediment transport and, thus,
alter the routing. The routing along a dam or ditch is incorporated in the
routing algorithm. A detailed explaination about these functionalities is given
in the user choices section.

Sewers/endpoints
****************

TO DO

References
==========

- Van Oost, K., Govers, G. & Desmet, P.J.J (2000) Evaluating the effects of
  changes in the landscape structure on soil erosion by water and tillage.
  Landscape Ecology 15, 577-589.
  `link2 <https://doi.org/10.1023/A:1008198215674>`_
- Van Rompaey, A., Verstraeten, G., Van Oost, K. Govers, G. & Poesen, J. (2001)
  Modelling mean annual sediment yield using a distributed approach. Earth
  Surface Processes and Landforms 26(11), 1221-1236.
  `link3 <https://doi.org/10.1002/esp.275>`_
- Verstraeten, G., Van Oost, K., Van Rompaey, A., Poesen, J. & Govers, G. (2003)
  Evaluating an integrated approach to catchment management to reduce soil loss
  and sediment pollution through modelling. Soin Use and Management, 18, 386-394.
  `link4 <https://doi.org/10.1111/j.1475-2743.2002.tb00257.x>`_
- Verstraeten, G., Poesen, J., Demarée, G., Salles, C. (2006) Long-term (105
  years) variability in rain erosivity as derived from 10-min rainfall depth
  data for Ukkel (Brussels, Belgium): Implications for assessing soil erosion
  rates. Journal of geophysical research, 111.
  `link5 <https://doi.org/10.1029/2006JD007169>`_