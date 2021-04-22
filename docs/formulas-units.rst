.. _formulasunits:


##############################
Overview of formulas and units
##############################

The aim of this page is to present an overview of the formulas and the units
used in CN-WS. The motivation for this page is to clarify the differences
between the units used in the :ref:`model description<modeldescription>`,
:ref:`model inputs<modelinput>`, model states and :ref:`model
outputs<modeloutput>`. This overview can help interpret the outputs of CN-WS.

.. note::
    This page is under construction. For now, the formula and units analysis
    is only presented for WaTEM/SEDEM.

WaTEM/SEDEM
===========

As explained in :ref:`the concept <WS>`, the amount of erosion and deposition in
each pixel is determined by comparing the mean annual soil erosion and incoming
sediment with the transport capacity. In the CN-WS code this is done by
comparing **volumes** (:math:`m^3`) or (see :ref:`concept <Concept>`, with
:math:`S_A =S_i+E`):

.. math::
    S_o = S_i + E \quad\text{ if } S_i+E \leq TC

.. math::
    S_o = S_i + E -TC \quad\text{ if } S_i>TC

with the units for :math:`E` and :math:`TC`:

 - :math:`E = \frac{\text{kg}}{\text{m}^{2}.\text{year}}` (see :ref:`here<rusle>`)
 - :math:`TC = \frac{\text{kg}}{\text{m.year}}` (see :ref:`here <TC>`).

These units are converted to :math:`\frac{m^3}{year}` (:math:`E_v` and
:math:`TC_v`) by making use of the pixel area (:math:`A_c, unit: m^2`), the soil
bulk density (:math:`\rho, unit: \frac{kg}{m^3}`) and the correction factor for
the grid cell dimension (:math:`x`, resolution multiplied by :math:`|sin
(\alpha)|+|cos (\alpha)|`).

.. math::
    E_v = \frac{E * A_c}{\rho} (\frac{m^3}{year})

.. math::
    TC_v = \frac{TC * x}{\rho} (\frac{m^3}{year})

The values for the outgoing sediment per pixel are in the code thus defined
as state variables in :math:`\frac{m^3}{year}`. Consequently, the
values for the incoming sediment (:math:`S_i`) for every (river) pixel is
also expressed in the model as volumes. These computations are all defined in
the procedure `Water` in the file `lateralredistribution.pas`.

WS application
================
The model inputs for the CN-WS model in the .inifile used to computes
:math:`E` and :math:`TC` are the :math:`R`-value, the :math:`C`-raster, the
:math:`K`-raster, the :math:`P`-raster and the digital height model raster
(:math:`LS`, slope). :math:`LS` and :math:`\tan(\text{slope})^{0.8}` are
dimensionless, and are considered as model outputs as they are computed in
CN-WS. The :math:`C`- and :math:`P`-rasters have no units.

The units of the :math:`R`-value and :math:`K`-raster are equal to:

 - :math:`R = \frac{\text{MJ.mm}}{\text{ha}.\text{h.year}}`. The unit of the
   R-value is converted (in CN-WS) to
   :math:`\frac{\text{MJ.mm}}{\text{m}^2.\text{h.year}}`.
 - :math:`K = \frac{\text{kg.h}}{\text{MJ.mm}}`.

The model outputs `incoming sediment` :math:`S_i` (:ref:`SediIn
<sediinrst>`), `outgoing sediment` :math:`S_o` (:ref:`SediOut
<sedioutrst>`), `sediment export` :math:`S_e` (:ref:`SediExport
<sediexportrst>`) and `transport capacity` :math:`TC`
(:ref:`Capacity <capacityrst>`) all have unit **kg** (converted by using
:math:`\rho`). The model output RUSLE :math:`E` value (:ref:`RUSLE
<ruslerst>`) is expressed in :math:`\frac{\text{kg}}{\text{m}^2}`!

In the table below the units of import input and output values/rasters are
listed.

.. csv-table::
    :file: _static/csv/units.csv
    :header-rows: 1
    :align: center
