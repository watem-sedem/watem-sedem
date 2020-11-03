##########################
Runoff model: Curve Number
##########################

A rainfall-runoff module based on the curve number (CN) method is used to
calculate runoff rates in the simulation domain.


The original curve number method
================================

The original CN method has been developed in the mid-50’s by the former
American Soil Conservation Service (SCS) and has since then been revised more than once (NRCS, 2010).
It is a simplistic empirical method that allows the prediction of surface runoff with a limited amount of
input data. It is because of this simplicity the method has known widespread
use in water and soil management.

The main governing assumption of the CN method is the following:

.. math::
    \frac{Q}{P-I_a} = \frac{F}{S}

with:

- :math:`Q`: runoff depth (:math:`m`)
- :math:`P`: rainfall depth (:math:`m`)
- :math:`I_a`: initial abstractions (e.g. infiltration before runoff,
  evaporation, interception) (:math:`m`)
- :math:`S`: potential loss (maximum water storage after runoff starts) (:math:`m`)
- :math:`F`: actual loss (effective water storage) (:math:`m`)

Important to note is that those variables do not represent time series
variables. The CN method is an event-based model. All the variables are
cumulative depths over an event.

The mass balance implies following equation:

.. math::
    P = Q+F+I_a

Furthermore :math:`I_a` is expressed as follows:

.. math::
    I_a=cS

This constant c was originally set to 0.2, however, the quality of choice has been often questioned.
Nowadays it is often put at lower values (Ponce  V.M.  &  Hawkins, 1996), certainly in an urbanized context. 
Combining equations 1, 2 and 3 results in the following expression for :math:`Q`:

.. math::

    Q(P,S) =
        \Bigg\{
            \begin{array}{ll}
                (P-cS)^2 & \text{if} & P>I_a \\
                0   & \text{else} & P \leq I_a
            \end{array}

:math:`S`, finally, can be expressed in function of the curve number CN, an
empirical parameter ranging between 0 (everything infiltrates, e.g. a dry
very porous soil) and 100 (nothing infiltrates, e.g. a parking lot):

.. math::

    S = \frac{25400}{CN}-254

The CN values can be extracted from tables (NRCS, 2010), based on soil type, land use,
hydrologic condition, and initial moisture conditions. To conclude Q can
thus be expressed solely as a function of P and CN like depicted in the
graph below.

To conclude Q can thus be expressed solely as a function of P and CN like
depicted in the graph below.

FIGURE

The simplistic nature of the original CN method explains its widespread use.
It is important to note that original CN method is best suited for
applications in an agricultural context. Furthermore, one has to keep in mind that
this method spatio-temporarily lumpes output at the event-scale. Finally, it is
important to note that abstraction is made of certain processes like e.g. rainfall
intensity, surface crust formation, crop cover, antecedent conditions, etc.

The runoff module used in CNWS does not represent the original CN method. To
overcome some of the shortcomings mentioned above, some adjustments to the
original CN method have been made. On the one hand some processes are incorporated into the equations, 
on the other hand a spatio-temporal translation of the event-based output is implemented.
In the following section the current implementation of the CN-based runoff module is presented. 

The current runoff module
=========================

1)  For every grid cell the total event-based runoff is calculated based on the local CN-value and the total rainfall depth based on equation (*).
    A first adaptation suggested by Van Oost 2003 is pre-processing the tabulated CN values so that effects of crop cover and soil crusting are also accounted for:

.. math::
    CN = CN_Max  – (\frac{Cc,100} * c_1) + (\frac{Cr,5} * c_2)

with:

- :math: `CN_Max`: the maximum CN derived from the USDA SCS handbook (:math:`-`)
- :math:`Cc`:  the percentage of crop cover (:math:`-`)
- :math:`c_1`: coefficient where the value is set so that CN equals the CN_Min for a given crop-soil combination when the crop cover equals 100% (:math`-`)
- :math:`Cr`: the crusting stage (Govers et al, 1986) (:math:`-`)
- :math:`c_2`: coefficient where the value is set so that CN equals the value for a bare soil surface when the crop cover equals 0%. (:math:`-`)

    Furthermore, Van Oost 2003 also suggested a correction factor for the total simulated runoff value using rainfall intensity and antecedent rainfall depth:

.. math::
    Q = Q_CNII*\frac{IN_max10,10}^α  + \frac{AR5,10}*β

with:

- :math:`Q_CNII`: the estimated direct runoff using antecedent moisture condition II (:math:`m`)     
- :math:`IN_max10`: the maximum 10-minute rainfall intensity (:math:`m`)
- :math:`AR5`: the 5 days antecedent rainfall (:math:`m`)
- :math:`α, β`: tuning parameters (:math:`-`)

2)  In grid cells at which the rainfall depth P is lower than the initial abstraction I_a, infiltration is simulated following the equation below 
    (Van Oost,2003):

.. math::
    I = (Ia-P)* \frac{D,1440}        for P<Ia

with:

- :math:`I`: infiltration (:math:`m`)
- :math:`D`: duration of the rainfall event. (:math:`min`)

3)  For all buffers present in the modelled catchment 3 properties are calculated:

    - Maximal outflow:

.. math::
    Q_max=C_dam*A_opening (2×9.81×[H_dam-H_opening])^(0.5)

with:

- :math:`Q_max`: maximum outflow (:math:`m³s-1`)
- :math:`C_dam`: 
- :math:`A_opening`: opening area (:math:`m²`)
- :math:`H_dam`: dam height (:math:`m`)
- :math:`H_opening`: opening height (:math:`m`)

    - Dead volume:

.. math::
    V_dead=\frac{H_opening,H_dam} ×V

with:

- :math:`V_dead`: dead volume (:math:`m³`)
- :math:`V_buffer`: total buffer volume (:math:`m³`)

    - Buffer area:

.. math::
    A_buffer=\frac{V_buffer,H_dam} 

with:

- :math:`A_buffer`: buffer area (:math:`m²`)

4)  The total generated runoff is distributed over all timesteps proportional to the rainfall distribution during the event. This results in a generated 
    runoff value for every grid cell at every time step. 

5)  Subsequently runoff redistribution through the landscape is modelled stepwise through time. In the general, the following 2 steps are considered:

    - Calculating present runoff in a grid cell at certain timestep t:

.. math::
    R_(tot,t)=R_(r,t-1)+R_(P,t)+R_(in,t-1)

with:

- :math:`R_(tot,t)`: total present runoff at timestep t (:math:`m³`)
- :math:`R_(r,t-1)`: runoff still present since previous timestep t-1 (:math:`m³`)
- :math:`R_(P,t)`: runoff generated by rainfall during timestep t (:math:`m³`)
- :math:`R_(in,t-1)`: upstream runoff generated at timestep t-1 (:math:`m³`)

    - Calculating runoff leaving the grid cell to 1 or 2 neighboring downstream grid cells based on the routing table:

.. math::
    R_(out,t)=R_(tot,t)  α  \frac{v ∆t,dist}

with:

- :math:`R_(out,t)`: runoff leaving the grid cell towards the neighboring downstream grid cell at timestep t (:math:`m³`)
- :math:`R_(tot,t)`: total present runoff at timestep t (:math:`m³`)
- :math:`α`: routing fraction towards the downstream grid cell (:math:`-`)
- :math:`v`: flow velocity (:math:`ms-1`)
- :math:`∆t`: time step duration (:math:`s`)
- :math:`dist`: flow distance to the downstream grid cell (:math:`m`)

    For buffer grid cells, the runoff outflow is calculated in a alternative way, 3 cases are considered:

        - If the runoff volume present in the buffer is smaller than the dead volume. All the runoff is stored in the buffer, no downstream buffer outflow is generated.

        - The runoff volume present in the buffer lies between the dead volume and total buffer volume. A downstream buffer outflow volume is calculated:

.. math::
    R_(out,t)=Q_max (\frac{R_(tot,t),V_buffer-V_dead})^(0.5) ∆t

        - The runoff volume present in the buffer exceeds the total buffer volume. The downstream water flow is the sum of the maximum buffer outflow Q_max and the spill flow. The latter is calculated as:

.. math::
    spill=min⁡[C_dam w_dam 9.81^(0.5) [\frac{R_(tot,t)-V_buffer),A_buffer}]^1.5,\frac{R_(tot,t)-V_buffer),∆t}]

with:

- :math:`spill`: spill flow (:math:`m³s-1`)
- :math:`w_dam`: dam width (:math:`m`)

    If in the considered grid cell, a sewer inlet is modelled, a fraction of the runoff will be transported through the sewers.

    Remarks:
        The flow velocity v is dependent on the type of grid cell: overland flow is fixed to 0.3 ms-1 (Govers, 1992), sewer flow velocity at 1 ms-1 and river flow velocity is user-dependent.

6)  During the stepwise calculation of the runoff in the catchment the following variables are constantly updated for output at the end of the procedure:
    - The amount of runoff that leaves the catchment during every time step
    - Total amount of runoff leaving the catchment
    - Total amount of runoff passing through each outlet
    - The amount of runoff that enters every river segment during every time step
    - The total amount of runoff that enters every river segment
    - A map with the total amount of runoff for every grid cell for the entire event
    - The total amount of spillover for each buffer
    - The total amount of water leaving the system through the sewers



Titel
=====

References
==========
Govers G., 1986, Mechanismen van akkererosie op lemige bodems, unpublished PhD thesis, Faculteit Wetenschappen, KU Leuven.
NRCS,  2010,  Chapter  9  Hydrologic  Soil-Cover  Complexes,  National  Engineering  Handbook  Part  630 Hydrology, 20 pp.
Ponce  V.M.  &  Hawkins R.H.,  1996,  Runoff  Curve  Number:  has  it  reached  maturity?,  Journal  of Hydrologic Engineering, 1: 11-19.
Van  Oost  K., 2003,  Spatial  modeling  of  soil  redistribution  processes  in  agricultural  landscapes, unpublished PhD thesis, Faculty of Sciences, KU Leuven.

TO DO