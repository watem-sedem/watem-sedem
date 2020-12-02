.. _CN:

##########################
Runoff model: Curve Number
##########################

The curve number (CN) method is an empirical runoff modeling approach which knows widespread use in the 
hydrologic community. The low model complexity and limited input requirements explain the popularity of
this model, especially in data-scarce areas. The original CN method has been developed in the mid-50’s 
by the former American Soil Conservation Service (SCS) and has since then been revised more
than once (NRCS, 2010). Based on field experiments with standardised
rectangular runoff plots, local coefficients, so-called 'curve numbers' were originally determined and tabulated. 
These take into account local conditions (soil type and land cover, ...) in order to accurately estimate 
runoff from recorded rainfall input. Important to note is that the output of the original model formulation 
is event-based, and is thus not temporally nor spatially distributed. However, in larger modeling frameworks 
in which runoff is only one of the simulated phenomena, a spatio-temporal representation of the model output
is common. Therefore, in order to incorporate the curve number approach, a translation of this event-based
output is necessary. 

In the documentation below the following 2 parts are discussed:
- A description of the original CN method
- The translation of the original model to the CN-based module which is in use in the CN-WS modelling framework

The original CN method
======================

The main governing assumption of the CN method is the following:

.. math::
    \frac{Q}{P-I_a} = \frac{F}{S} 

with:

- :math:`Q`: runoff depth (:math:`m`)
- :math:`P`: rainfall depth (:math:`m`)
- :math:`I_a`: initial abstractions (e.g. infiltration depth before runoff starts,
  evaporation, interception)(:math:`m`)
- :math:`S`: potential loss (maximum water storage after runoff starts) (:math:`m`)
- :math:`F`: actual loss (effective water storage) (:math:`m`)

Important to note is that the variables described above, do not represent time series
variables. The CN method is an event-based model. All the variables are
cumulative depths over an event. The term 'depth' indicates water volume per square meter, 
hence the unit :math:`m`. 

The mass balance implies following equation:

.. math::
    P = Q+F+I_a

Furthermore :math:`I_a` is expressed as a fraction of the potential loss :math:`S`:

.. math::
    I_a=cS

A higher value for the dimensionless constant :math:`c` essentially implies 
that more water is initially 'lost' by mainly infiltration and 
evapotranspiration before the runoff phenomena starts to take place. It was originally set to 0.2, 
however, the quality of choice has been often questioned. Nowadays it is often put at lower values
(Ponce  V.M.  &  Hawkins, 1996), certainly in an urbanized context.
Combining equations 1, 2 and 3 results in the following expression for :math:`Q`:

.. math::

    Q(P,S) =
        \Bigg\{
            \begin{array}{ll}
                \frac{(P-cS)^2}{P+(1-c)S} & \text{if} & P>I_a \\
                0   & \text{else} & P \leq I_a
            \end{array}

:math:`S`, finally, can be expressed as a function of the dimensionless
curve number :math:`CN`, an empirical parameter ranging between 0
(everything infiltrates, e.g. a dry very porous soil) and 100 (nothing
infiltrates, e.g. a parking lot):

.. math::
    S = \frac{25400}{CN}-254
    
The :math:`CN` values can be extracted from tables (NRCS, 2010), based on soil
type, land use, hydrologic condition, and initial moisture conditions. 
Combining last two equations, :math:`Q` can thus be expressed solely as a
function of :math:`P` and :math:`CN` like depicted in the graph below
(source: USDA technical release 55, urban hydrology for small watersheds)

.. image:: _static/png/cn_graph.png
    :width: 600px


The simplistic nature of the original CN method explains its widespread use.
It is important to note that original CN method is best suited for
applications in an agricultural context. Furthermore, one has to keep in mind that
this method spatio-temporarily lumpes output at the event-scale. Finally, it is
important to note that abstraction is made of certain processes like e.g. rainfall
intensity, surface crust formation, crop cover, antecedent conditions, etc.

The runoff module used in CNWS does not represent the original CN method. To
overcome these shortcomings mentioned above, some adjustments to the
original CN method have been made. On the one hand some processes are incorporated
into the equations, on the other hand a spatio-temporal translation of the
event-based output is implemented. In the following section the current
implementation of the CN-based runoff module is presented.

CN-based module in CN-WS 
========================

For every grid cell the total event-based runoff is calculated based on the
local :math:`CN` value and the total rainfall depth :math:`P` following the original CN-formulation.
A first adaptation suggested by Van Oost 2003 is pre-processing the tabulated
:math:`CN` values so that effects of crop cover and soil crusting are also
accounted for:

.. math::
    CN = CN_{max}  – \frac{CC}{100} c_1 + \frac{Cr}{5} c_2

with:

- :math:`CN_{max}`: the maximum CN derived from the USDA SCS handbook (:math:`-`)
- :math:`CC`:  the percentage of crop cover (:math:`\%`)
- :math:`c_1`: coefficient related to the crop cover(:math`-`)
- :math:`Cr`: the crusting stage (:math:`-`)
- :math:`c_2`: coefficient related to the crusting stage (:math:`-`)

The :math:`c_2` coefficient is a constant (3), the :math:`c_2` coefficient is set in such a way that the calculated CN value
is equal to the minimum value derived from the USDA SCS handbook in case of a crop cover of 100%. Soils prone to crust formation
tend to be less permeable which results in more runoff generation. The crusting stage :math:`Cr` brings this effect into account.
More details about this coefficient can be found in Govers et al. (1986).

Furthermore, Van Oost 2003 also suggested a correction factor for the total
simulated runoff value using rainfall intensity and antecedent rainfall depth:

.. math::
    Q = Q_{CNII} \left(\frac{IN_{max10}}{10}\right)^{\alpha}  + \frac{AR5}{10} \beta

with:

- :math:`Q_{CNII}`: the estimated direct runoff using antecedent moisture
  condition II (:math:`m`)
- :math:`IN_{max10}`: the maximum 10-minute rainfall intensity (:math:`m`)
- :math:`AR5`: the 5 days antecedent rainfall depth (:math:`m`)
- :math:`α, β`: tuning parameters (:math:`-`)

:math:`CNII` indicates the standard tabulated curve number value. This value represents the curve number under
average soil moisture conditions. A conversion table exists to translate :math:`CNII` into :math:`CNI` (dry soil) and :math:`CNIII` (wet soil).
However Van Oost, 2003 opted for the 5 days antecedent rainfall depth :math:`AR5` to take into account antecedent 
wetness conditions.

In grid cells at which the rainfall depth P is lower than the initial abstraction
:math:`I_a`, infiltration is simulated following the equation below (Van Oost,
2003):

.. math::
    \begin{array}{ll}
        I=(I_a-P) \frac{D}{1440} & if & P<I_a
    \end{array}

with:

- :math:`I`: infiltration (:math:`m`)
- :math:`D`: duration of the rainfall event. (:math:`min`)

The total generated runoff is distributed over all timesteps proportional to the
rainfall distribution during the event. This results in a generated runoff value
for every grid cell at every time step.

Subsequently runoff redistribution through the landscape is modelled stepwise
through time. In the general, the following 2 steps are considered:

1) Calculating present runoff in a grid cell at certain timestep t:

.. math::
    R_{tot,t}=R_{r,t-1}+R_{P,t}+R_{in,t-1}

with:

- :math:`R_{tot,t}`: total present runoff at timestep t (:math:`m^3`)
- :math:`R_{r,t-1}`: runoff still present since previous timestep t-1 (:math:`m^3`)
- :math:`R_{P,t}`: runoff generated by rainfall during timestep t (:math:`m^3`)
- :math:`R_{in,t-1}`: upstream runoff generated at timestep t-1 (:math:`m^3`)

2) Calculating runoff leaving the grid cell to 1 or 2 neighboring downstream grid
cells based on the routing table:

.. math::
    R_{out,t}=R_{tot,t}  \alpha  \frac{v \Delta t}{d}

with:

- :math:`R_{out,t}`: runoff leaving the grid cell towards the neighboring
  downstream grid cell at timestep t (:math:`m^3`)
- :math:`R_{tot,t}`: total present runoff at timestep t (:math:`m^3`)
- :math:`\alpha`: routing fraction towards the downstream grid cell (:math:`-`)
- :math:`v`: flow velocity (:math:`m s^{-1}`)
- :math:`\Delta t`: time step duration (:math:`s`)
- :math:`d`: flow distance to the downstream grid cell (:math:`m`)

For buffer grid cells, the runoff outflow is calculated in a alternative way.
More information about the calculations in buffer can be found
:ref:`here <bufferbasins>`. If in the considered grid cell, a sewer inlet is
modelled, a fraction of the runoff will be transported through the sewers. More
information about the sewer functionality in the model can be found
:ref:`here <sewers>`.

During the stepwise calculation of the runoff in the catchment the following
variables are constantly updated for output at the end of the procedure:

- The amount of runoff that leaves the catchment during every time step
- Total amount of runoff leaving the catchment
- Total amount of runoff passing through each outlet
- The amount of runoff that enters every river segment during every time step
- The total amount of runoff that enters every river segment
- A map with the total amount of runoff for every grid cell for the entire event
- The total amount of spillover for each buffer
- The total amount of water leaving the system through the sewers

References
==========
Govers G., 1986, Mechanismen van akkererosie op lemige bodems, unpublished PhD
thesis, Faculteit Wetenschappen, KU Leuven.

NRCS,  2010,  Chapter  9  Hydrologic  Soil-Cover  Complexes,  National
Engineering  Handbook  Part  630 Hydrology, 20 pp. 
https://directives.sc.egov.usda.gov/OpenNonWebContent.aspx?content=17758.wba

Ponce  V.M. and Hawkins R.H.,  1996,  Runoff  Curve  Number:  has  it  reached
maturity?,  Journal  of Hydrologic Engineering, 1: 11-19. 
https://doi.org/10.1061/(ASCE)1084-0699(1996)1:1(11)

Van  Oost  K., 2003,  Spatial  modeling  of  soil  redistribution  processes
in  agricultural  landscapes, unpublished PhD thesis, Faculty of Sciences,
KU Leuven.