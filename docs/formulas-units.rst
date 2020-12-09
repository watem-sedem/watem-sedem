.. _formulasunits:


###############################
Overview of formula's and units
###############################

The aim of this page is to give an overview of the formula's and the units
used in CN-WS. The motivation to this is to make a distinction between the
units used in the :ref:`model description<modeldescription>`,
:ref:`model description<modelinput>`, model states and
:ref:`model outputs<modeloutput>`. This overview can help interpret the
outputs of CN-WS.

.. note::
    This page is under construction. For now, the formula and units analysis
    has only been done for WaTEM/SEDEM.

RUSLE and Capacity
==================

As explained in :ref:`here <WS>`, the amount of erosion and deposition in each
pixel is determined by comparing the mean annual soil erosion with the
transport capacity. In the CN-WS code this is done by comparing **volumes**
(:math:`m^3`) or (see :ref:`concept <Concept>`, with :math:`S_A =S_i+E`):

.. math::
    S_o & = & S_i+E & \text{ if } & S_i+E≤TC \\
        & = & S_i+E-TC & \text{ else } & S_i+E>TC

Yet, the formulas are for :math:`E` and :math:`TC` are expressed in
(:math:`\frac{\text{kg}}{\text{m}^{2}.\text{year}}`) and
(:math:`\frac{\text{kg}}{\text{m.year}}`) (see :ref:`here <rusle>` and
:ref:`here <TC>`). These units are converted to :math:`\frac{m^3}{year}` (
:math:`E_v` and :math:`TC_v`) by making use of the pixel area (:math:`A_c
(m^2)`), the soil bulk density (:math:`\rho (\frac{kg}{m^3})`) and the correction
factor for the grid cell dimension (:math:`x = |sin(\alpha)| +|cos(\alpha)|`).

.. math::
    E_v = \frac{E * A_c}{\rho}
    TC_v = \frac{TC * x}{\rho}

