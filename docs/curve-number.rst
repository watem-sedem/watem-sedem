##########################
Runoff model: Curve Number
##########################

A rainfall-runoff module based on the curve number (CN) method is used to
calculate runoff rates in the simulation domain.


Curve number method
===================

The original CN method has been developed in the mid-50’s by the former
American Soil Conservation Service (SCS). It is a simplistic empirical
method that allows the prediction of surface runoff with a limited amount of
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

This constant c was originally set to 0.2, however, recent research puts it
more towards 0.05, certainly in an urbanized context. Combining equations
1, 2 and 3 results in the following expression for :math:`Q`:

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

The CN values can be extracted from tables, based on soil type, land use,
hydrologic condition, and initial moisture conditions. To conclude Q can
thus be expressed solely as a function of P and CN like depicted in the
graph below.

To conclude Q can thus be expressed solely as a function of P and CN like
depicted in the graph below.

FIGURE

The simplistic nature of the original CN method explains its widespread use.
It is important to note that original CN method is best suited for
applications in an agricultural context. Thus, one has to keep in mind that
this method spatio-temporarily lumpes output at even-scale. Finally, it is
important to note that abstraction is made from processes like e.g. rainfall
intensity, surface crust formation, crop cover, antecedent conditions.

The runoff module used in CNWS does not represent the original CN method. To
overcome some of the shortcomings mentioned above, some adjustments to the
original CN method have been made. In the next section, these adjustments
are explained.

Titel
=====

References
==========

TO DO