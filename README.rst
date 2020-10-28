#####
CN-WS
#####

The Curve Number-WaTEM/SEDEM (CN-WS) is a package that can be used tp
simulates soil erosion, sediment transport, water and sediment routing and
flow run-off in time and space. The package combines three existing models:

 - The curve number model for simulating flow run-off.
 - The WaTEM/SEDEM model for simulating erosion and over-land sediment
   transport.
 - The routing algorithm simulating water and sediment routing over land.

CN-WS is developed to assess sediment input to rivers, soil erosion on
agricultural fields and to evaluate soil erosion management measures.

A short history of CN-WS
========================

The sediment export model CN-WS was developed starting from the year 2013
until 2016 by KULeuven in partnership with `Antea Belgium
<https://anteagroup.be/>`_, commissioned by
the Flemish government (Antea, 2016). The aim of the development were to
create a tool that can quantify erosion, sediment/water run-off, and the
effect of erosion mitigation measures on erosion & run-off to the river.

The initial developments of WaTEM/SEDEM were initialised by KULeuven,
department (`Department of Earth and Environmental Sciences <https://ees
.kuleuven.be//>`_), before 2013. A key component in the computation of
sediment transport to the river was
the coupling of:

1. WaTEM (Van Oost et al. 2000): a spatially distributed model that
   computes erosion by rainfall and tillage.
2. SEDEM (Van Rompaey et al., 2001): a model simulating sediment-transport
   to the river.

Since 2016, the Flemish government, Department for Environment (VPO), and the
Flemisch Environment Agency (VMM) commissioned further developments for
CN-WS so it could become operational for management. Specifically, a number
optimisations to the code were implemented to increase model performance and
allow a roll-out on the scale of Flanders. In addition, the model was
recalibrated (Deproost et al., 2018) and a framework was developed for
processing CN-WS input, outputs and user choices. At that point, CN-WS is
submitted to versioning via git. These optimisation were executed by `Fluves
<https://fluves.com/>`_.

Model
=====

The CN-WS model described on this page consist of three models. In the
following figure, the main input, models and outputs are shown. The figure
above show which input data are required to develop a CN-WS model
. These input data are used to run the routing, erosion or run-off module.
The run-off and routing module is used in CN, whereas the erosion and
routing module are used in WS. The outputs at interest are stated at the
right of the diagram, whereas interesting state variables are shown in blue
in the CN-WS package block.

TO DO: expand explaination

.. image:: _static/png/diagram_cn_ws_package.png
    :width: 771px

.. note::
    The CN-WS package is still being optimized to make sure the CN, WS and
    routing model can be run separately.

Package
=======

CN-WS has thee modus operandi: CN_WS, CN_WS_console and CN_WS_LongTerm.
Typically the model is run in a command-line interface or a Graphical User
Interface (GUI). Yet, since 2016, the GUI is not maintained. Users are thus
advised to use the command line version.

1. The CN_WS model is used simulate the erosion and sediment run-off for one
   rainfall event. This model can be used through the GUI. To start the model,
   use the "CN_WS" shortcut in the current folder. To view the underlying
   source code, open the CN_WS folder and:

   - open the different .pas files in notepad
   - OR open Invoerform.lpi in lazarus

2. The CN_WS_console model is similar to CN_WS, but without GUI. This
   version can be used through WINDOWS commandline (see manual) (This model
   version is used by the long term version of the model to run each rainfall
   event). To view the underlying source code, open the CN_WS_console folder
   and:

   - open the different .pas files in notepad
   - OR open CN_WSmodel.lpi in lazarus

3. The CN_WS_LongTerm is the long term version of the model; including GUI.
   To start the model, use the "CN_WS_LongTerm" shortcut in the current
   folder. To view the underlying source code, open the CN_WS_LongTerm
   folder and:

   - open the different .pas files in notepad
   - OR open Invoerform.lpi in lazarus

License
=======

This project is licensed under the ??? License, see
`LICENSE <https://git.fluves.net/cn_ws/LICENSE>`_ for more information.

Documentation
=============

The documentation of this project is available at https://docs.fluves.net/cnws-pascal/.  In this documentation, a theoretical background of the model
and an overview of the model usage are given, and an installation
guide followerd by a tutorial.


References
==========
Antea, 2016. Modellering van de sedimentaanvoer naar de waterlopen, het
effect van erosiebestrijdingsmaatregelen en het transport van sediment in de
onbevaarbare waterlopen. Antea Belgium NV, Antwerpen, Belgium.
