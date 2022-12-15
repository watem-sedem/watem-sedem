.. _history:

A short history of CN-WS
========================

The sediment export model CN-WS was developed starting from the year 2013
until 2016 by KULeuven in partnership with `Antea Belgium
<https://anteagroup.be/>`_, commissioned by
the Flemish government (Antea, 2016). The aim of the development was to
create a tool that can quantify erosion, sediment transport and water run-off,
and the effect of erosion mitigation measures on erosion, sediment transport and
sediment delivery to the river.
CN-WS was developed on top of the original code of WaTEM/SEDEM.

The developments of WaTEM/SEDEM was initialised by the KULeuven
(`Department of Earth and Environmental Sciences <https://ees.kuleuven.be//>`_),
in 2000. A key component in the computation of sediment transport to the river was
the coupling of:

1. WaTEM (Van Oost et al. 2000): a spatially distributed model that
   computes erosion by rainfall and tillage.
2. SEDEM (Van Rompaey et al., 2001): a model simulating sediment-transport
   to the river.

Since 2016, the Government of Flanders, Department of Environment and Spatial
Development (Division VPO), and the
Flanders Environment Agency (VMM) commissioned further developments of
CN-WS to make the model operational for management and policy purposes.
Specifically, a number
of optimisations to the code were implemented to increase the model performance and
allow a roll-out on the scale of Flanders. In addition, the model was
recalibrated (Deproost et al., 2018) and a framework was developed for
processing CN-WS input, outputs and user choices. At that point, CN-WS is
submitted to versioning via git (https://github.com/cn-ws/cn-ws
). These optimization were executed by `Fluves <https://fluves.com/>`_.

Current roles are:

- Fluves: code developer and package maintenance.
- KU Leuven: code developer, advisor.
- VPO and VMM: supervisor.