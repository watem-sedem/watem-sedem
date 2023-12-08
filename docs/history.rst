.. _history:

A short history of CN-WS
========================

The developments of WaTEM/SEDEM was initialised by the KU Leuven
(`Department of Earth and Environmental Sciences <https://ees.kuleuven.be//>`_),
in 2000. A key component in the computation of sediment transport to the river was
the coupling of the two models:

1. WaTEM (Van Oost et al. 2000): a spatially distributed model that
   computes erosion by rainfall and tillage.
2. SEDEM (Van Rompaey et al., 2001): a model simulating sediment-transport
   to the river.

Since the original publications, the model has undergone numerous iterative developments
within published scientific literature. These led to numerous additional options being
available within the model which can typically selected as user-choice as options.

The sediment export model CN-WS was developed between 2013
and 2016 by KU Leuven in partnership with `Antea Belgium
<https://anteagroup.be/>`_, and was commissioned by
the Flemish government (Antea, 2016). The aim was to
develop a tool that can quantify erosion, sediment transport and water run-off,
as well as, the effect of erosion mitigation measures on these processes.
CN-WS was developed on top of the original code of WaTEM/SEDEM.

Since 2016, the Government of Flanders, Department of Environment and Spatial
Development (Division VPO), and the
Flanders Environment Agency (VMM) commissioned further developments of
CN-WS to make the model operational for management and policy purposes.
A number of optimisations to the code were implemented to increase the model performance and
allow a roll-out on the scale of Flanders. In addition, the model was
calibrated for the context of Flanders (Deproost et al., 2018) and a framework
was developed for processing CN-WS input, outputs and user choices.
At this point, CN-WS has been
submitted to versioning via git (https://github.com/cn-ws/cn-ws). 
These optimization tasks were executed by `Fluves <https://fluves.com/>`_.

Current roles are:

- Fluves: code developer and package maintenance.
- KU Leuven: intellectual property holders, model creator, code developer, advisor.
- VPO and VMM: supervisor.
