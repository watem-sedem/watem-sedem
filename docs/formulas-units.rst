.. _formulasunits:


#############################################
Overview of formulas and units in WaTEM/SEDEM
#############################################

The aim of this page is to present an overview of the formulas and the units
used in WaTEM/SEDEM. The motivation for this page is to clarify the differences
between the units used in the :ref:`model description<modeldescription>`,
:ref:`model inputs<modelinput>`, model states and :ref:`model
outputs<modeloutput>`. This overview can help interpret the outputs of WaTEM/SEDEM.

As explained in :ref:`the model description <WS>` of WaTEM/SEDEM, the amount of erosion and deposition in
each pixel, as well as the amount of sediment leaving a pixel downstream (:math:`S_o`) is determined by comparing
the mean annual soil erosion :math:`E`
and incoming sediment :math:`S_i` with the transport capacity :math:`TC` at the pixel level.
In the model code this is done by
comparing **volumes** (:math:`m^3`):

.. math::
    S_o = TC \quad\text{ if } S_i+E > TC

.. math::
    S_o = S_A = S_i + E \quad\text{ if } S_i \leq TC

with the units for :math:`E` and :math:`TC`:

 - :math:`E = \frac{\text{kg}}{\text{m}^{2}.\text{year}}` (see :ref:`here<rusle>`)
 - :math:`TC = \frac{\text{kg}}{\text{m.year}}` (see :ref:`here <TC>`).

These units are converted to :math:`\frac{m^3}{year}` (:math:`E_v` and
:math:`TC_v`) by making use of the pixel area (:math:`A_c, unit: m^2`), the soil
bulk density (:math:`\rho, unit: \frac{kg}{m^3}`) and the slope correction factor for
the grid cell dimension (:math:`x`, resolution multiplied by :math:`|sin
(\alpha)|+|cos (\alpha)|`).

.. math::
    E_v = \frac{E \cdot A_c}{\rho} (\frac{m^3}{year})

.. math::
    TC_v = \frac{TC \cdot x}{\rho} (\frac{m^3}{year})

The values for the outgoing sediment per pixel are in the code thus defined
as state variables in :math:`\frac{m^3}{year}`. Consequently, the
values for the incoming sediment (:math:`S_i`) for every (river) pixel is
also expressed in the model as volumes. These computations are all defined in
the procedure `Water` in the file `lateralredistribution.pas`.

The model inputs for WaTEM/SEDEM in the .inifile that are used to compute
:math:`E` and :math:`TC` are the :math:`R`-value, the :math:`C`-raster, the
:math:`K`-raster, the :math:`P`-raster and the digital height model raster
(:math:`LS`, slope). :math:`LS` and :math:`\tan(\text{slope})^{0.8}` are
dimensionless, and are considered as model outputs as they are computed in
WaTEM/SEDEM. The :math:`C`- and :math:`P`-rasters have no units.

The units of the :math:`R`-value and :math:`K`-raster are equal to:

 - :math:`R = \frac{\text{MJ.mm}}{\text{ha}.\text{h.year}}`. The unit of the
   R-value is converted (in WaTEM/SEDEM) to
   :math:`\frac{\text{MJ.mm}}{\text{m}^2.\text{h.year}}`.
 - :math:`K = \frac{\text{kg.h}}{\text{MJ.mm}}`.

The model outputs `incoming sediment` :math:`S_i` (:ref:`SediIn
<sediinrst>`), `outgoing sediment` :math:`S_o` (:ref:`SediOut
<sedioutrst>`), `sediment export` :math:`S_e` (:ref:`SediExport
<sediexportrst>`) and `transport capacity` :math:`TC`
(:ref:`Capacity <capacityrst>`) all have unit **kg** (converted by using
:math:`\rho`). The model output RUSLE :math:`E` value (:ref:`RUSLE
<ruslerst>`) is expressed in :math:`\frac{\text{kg}}{\text{m}^2}`!

Note that :math:`S_e` is equal to :math:`S_o`, yet masked respectively for
river and land pixels.

WaTEM/SEDEM outputs include an erosion map that expresses :math:`E` in kg/m²/year,
equivalent to area-specific erosion rates reported in most other studies.
Values can be multiplied with 10 to obtain typical erosion rates in ton/ha/year.
A transport capacity map is also provided as an output, whereby the amount of sediment that
can be transported through a pixel in downstream direction is shown in kg/pixel/year.

In the table below the units of import input and output values/rasters are
listed.

.. csv-table::
    :file: _static/csv/units.csv
    :header-rows: 1
    :align: center
