.. _formulasunits:


###############################
Overview of formula's and units
###############################

The aim of this page is to give an overview of the formula's and the units
used in CN-WS. The motivation to this is to make a distinction between the
units used in the :ref:`model description<modeldescription>`,
:ref:`model inputs<modelinput>`, model states and
:ref:`model outputs<modeloutput>`. This overview can help interpret the
outputs of CN-WS.

.. note::
    This page is under construction. For now, the formula and units analysis
    has only been done for WaTEM/SEDEM.

WaTEM/SEDEM
===========

As explained in :ref:`here <WS>`, the amount of erosion and deposition in each
pixel is determined by comparing the mean annual soil erosion with the
transport capacity. In the CN-WS code this is done by comparing **volumes**
(:math:`m^3`) or (see :ref:`concept <Concept>`, with :math:`S_A =S_i+E`):

.. math::
    S_o & = & S_i+E & \text{ if } & S_i+E≤TC \\
        & = & S_i+E-TC & \text{ else } & S_i+E>TC

with the units for :math:`E` and :math:`TC`:

 - :math:`E = \frac{\text{kg}}{\text{m}^{2}.\text{year}}` (see :ref:`here<rusle>`)
 - :math:`TC = \frac{\text{kg}}{\text{m.year}}` (see :ref:`here <TC>`).

These units are converted to :math:`\frac{m^3}{year}` ( :math:`E_v` and
:math:`TC_v`) by making use of the pixel area (:math:`A_c, m^2`), the soil
bulk density (:math:`\rho, \frac{kg}{m^3}`) and the correction factor for
the grid cell dimension (:math:`x`, resolution multiplied by :math:`|sin
(\alpha)|+|cos (\alpha)|`).

.. math::
    E_v & = & \frac{E * A_c}{\rho} (\frac{m^3}{year})\\
    TC_v  & = & \frac{TC * x}{\rho} (\frac{m^3}{year})

The values for the outgoing sediment per pixel are in the model thus defined
as state variables in :math:`\frac{m^3}{year}`. Consequently, the incoming
sediment (:math:`S_i`) for every (river) pixel is also expressed in the
model in volume. These computations are all defined in the procedure `Water`
in the file `lateralredistribution.pas`.

WS application
================
The model inputs for the CN-WS model in the .inifile used to computes
:math:`E` and :math:`TC` are the :math:`R`-value, the :math:`C` raster, the
:math:`K` raster, the :math:`P` raster and the digital height model raster
(:math:`LS`, slope). :math:`LS` and :math:`\tan(\text{slope})^{0.8}` are
dimensionless, and are considered as model outputs as they are computed in
CN-WS. The :math:`C`- and :math:`P`-rasters have no units.

The units of the :math:`R`-value and :math:`K`-raster are equal to:

 - :math:`R = \frac{\text{MJ.mm}}{\text{ha}.\text{h.year}}`. The unit of the R-value is converted (in CN-WS) to :math:`\frac{\text{MJ.mm}}{\text{m}^2.\text{h.year}}`.
 - :math:`K = \frac{\text{kg.h}}{\text{MJ.mm}}`.

The model outputs sediment input :math:`S_i` (:ref:`SediIn <sediinrst>`),
sediment output :math:`S_o` (:ref:`SediOut <sedioutrst>`), sediment
export :math:`S_e` (:ref:`SediExport <sediexportrst>`) and :math:`TC`
(:ref:`Capacity <capacityrst>`) all have unit **kg** (converted by using
:math:`\rho`). The RUSLE :math:`E` value (:ref:`RUSLE <ruslerst>`) is
expressed in :math:`\frac{\text{kg}}{\text{m}^2}`!

In the table below the units of import input and output valeus/rasters are
listed.

.. csv-table::
    :file: _static/csv/units.csv
    :header-rows: 1
