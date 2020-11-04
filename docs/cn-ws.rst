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
R-factor is done with the methodology of Verstraeten et al.
(2006).

The output of both models is combined: the total sediment load is distributed
over the hydrogram to obtain a sedigram (sediment concentration and sedimentload
as a fuction of time). The sediment concentration *SC* is calculated for every
timestep as:

.. math::
    SC = \frac{(SV*1000)}{R*1000}

With:

- *SC*, the sediment concentration (:math:`g.l^{-1}`)
- *SV*, the sedimentload (:math:`kg`)
- *R*, the run-off (:math:`m^{3}`)

Additional model features
=========================

Some additional functionalities are built-in CN-WS, next to the basic model
features of WaTEM-SEDEM and the CN model. These additional functionalities have
a large impact on water and sediment transport through the model domain. By
default, these additional features are disabled, but can be enabled in in the
ini-file with the correct user choice. When the user enables an extra feature,
the model will expect more user input (rasters and variables).

.. _bufferbasins:

Buffer basins
*************

Buffer basins have a large impact on the dynamics of water and sediment run-off
in the landscape. These constructions are temporary storages of water and
sediment is deposited in them. The delayed water run-off and sediment deposition
is included in the model.

In the WaTEM-SEDEM part of CN-WS, all sediment entering the pixels of a buffer
is multiplied with the sediment trapping efficiency of the buffer. This trapping
efficiency is the fraction of the incoming sediment that is trapped in the
buffer basin.

The delayed water run-off is based on the report of Meert and Willems (2013) and
uses following principles in the CN-model in CN-WS:

Water will only flow out of a buffer basin when the water height in the basin
exceeds the height of discharge pipe of the buffer. Therefore, the dead
volume, :math:`V_{dead}`, is calculated by

.. math::
    V_{dead} = (\frac{H_{opening}}{H_{dam}}){V_{basin}}


Where:

- :math:`H_{opening}` is the height of the opening of the discharge pipe of the
  buffer basin (m)
- :math:`H_{dam}` is the height of the dam of the buffer basin (m)
- :math:`V_{basin}` is the maximum volume of water that can be trapped in the
  bufferbasin (:math:`m^{3}`).

Two cases exist. A first case can be defined as when the watervolume in the
buffer basin is larger than :math:`V_{dead}`, but smaller than :math:`V_{basin}`,
the water will flow through the discharge pipe acording to

.. math::
    R(t) = (Q_{max}.\sqrt{\frac{V(t)}{V_{basin} - V_{dead}}}).dt

where:

- :math:`R(t)` is the amount of run-off during timestep t (:math:`m^{3}`)
- :math:`Q_{max}` is the maximum discharge (:math:`m^{3} s^{-1}`)
- :math:`V(t)`, the volume of water present in the buffer basin at timestep t
  (:math:`m^{3}`)
- :math:`dt`, the timestep (s)

:math:`Q_{max}` is calculated for every buffer basin according to

.. math::
    Q_{max} = C_d.A_0.\sqrt{2.g.(H_{dam} - H_{opening})}

Where :

- :math:`C_d` is the discharge coefficient (-),
- :math:`A_0` is the area of the discharge opening (:math:`m^{2}`)
- :math:`g` is the gravitational acceleration (9.81 :math:`m.s^{-2}`)

A second case arises when the watervolume in the buffer basin is larger than
:math:`V_{basin}`. In this case the water will flow through the discharge pipe
:math:`R_{opening}` as via the overflow of the dam :math:`R_{overflow}`.

.. math::
    R = R_{opening} + R_{overflow}

    R_{opening} = Q_{max}.dt

    R_{overflow} = C_d.W_{dam}.\sqrt{g}.h(t)^{3/2}.dt

Where:

- :math:`W_{dam}` is the width of the overflow on the bufferbasin dam (m)
- :math:`h` is the height of the water above the overflow (m) and is calculated
  for every timestep by:

.. math::
    h(t) = \frac{V(t) - V_{basin}}{A_{basin}}

Where :math:`A_{basin}` represents the area of the buffer basin in :math:`m^{2}`

The practical use of buffer basins in the model is described
:ref:`here <includebuffers>`.

Dams and ditches
****************

Dams and ditches influence the direction of water and sediment transport and,
thus, alter the routing. The routing along a dam or ditch is incorporated in the
routing algorithm. A detailed explaination about these functionalities is given
in the user choices sections about :ref:`dams <includedams>` and
:ref:`ditches <includeditches>`.

Sewers/endpoints
****************

TO DO
see :ref:`here <inlcudesewers>` for more info

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
