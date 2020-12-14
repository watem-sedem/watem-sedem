#####
CN-WS
#####

The Curve Number-WaTEM/SEDEM (CN-WS) is a model that can be used to
simulates soil erosion, sediment transport, water and sediment routing and
flow run-off in time and space. The package combines three existing models:

 - The curve number model for simulating flow run-off.
 - The WaTEM/SEDEM model for simulating erosion and over-land sediment
   transport.
 - The routing algorithm simulating water and sediment routing over land.

CN-WS is developed to assess sediment input to rivers, soil erosion on
agricultural fields and to evaluate soil erosion management measures.

This documentation page explain how CN-WS works and how the model can be used. 
The official code for CNWS can be found at https://git.fluves.net/fluves/cn_ws. 


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
submitted to versioning via git (https://git.fluves.net/fluves/cn_ws
). These optimisation were executed by `Fluves
<https://fluves.com/>`_.

Model
=====

The CN-WS model described on this page consist of three models. In the
following figure, the main input, models and outputs are shown. In general,
information on the land cover, parcels, crops, soil, erosion control
measures, elevation and rainfall are needed to be able to use the models.
In addition, information on sewers and ditches can be used to refine the
models.

Both the :ref:`WS <WS>` and :ref:`CN <CN>` model make use of a routing table
simulated by the :ref:`routing model <routing>`. The use of this routing table
varies for both the CN and WS model. In WS, the routing table is used to
compute slopes, upstream areas and the sediment mass balance, whereas in the
CN model, the routing is used to spatially distribute the runoff. It is
important to note that the distribution of the runoff in the CN model is
computed sequentially for every time step :math:`dt`. This makes CN an
event-based model, whereas WS is a year-based model. Note that it is
technically possible to use WS as an event-based model, yet a year-based
assessment is found to be more robust.

.. image:: _static/png/diagram_cn_ws_package.png
    :width: 771px

The CN and WS model can also be used together, by using the
run-off to scale the yearly sediment load. This is explained :ref:`here
<CNWS>`.

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

For who is this documentation?
==============================

This page aims to explain how the CN-WS model can be run for a given case
study, and which output can be analysed to study the specific case. In
addition, it aims to explain how a specific run for case study can be set-up
(i.e. which input data are needed? in which format?). Finally, this page
is the reference for background information on model concepts, options, inputs
and outputs.

In order to run the CN-WS model, this set of minimal skills is required:

- Basic command line skills (know how to open your command line, and run an
  executable).
- Opening text files in a text editor.
- Opening raster data in QGIS, ArcGIS or your favorite GIS program.

__Note__:

The defined skills above allow you to run the CN-WS model with the input
files listed in the package (for an example, see :ref:`here<tutorial>`). For
generating your own input data for the model, for a self-defined case study, a
good knowledge of GIS is required.

License
=======

This project is licensed under the ??? License, see
`LICENSE <https://git.fluves.net/cn_ws/LICENSE>`_ for more information.

Documentation
=============

The documentation of this project is available at https://docs.fluves.net/cnws-pascal/.
In this documentation, a theoretical background of the model and an overview
of the model usage are given, and an installation guide followed by a
tutorial.


References
==========
Antea, 2016. Modellering van de sedimentaanvoer naar de waterlopen, het
effect van erosiebestrijdingsmaatregelen en het transport van sediment in de
onbevaarbare waterlopen. Antea Belgium NV, Antwerpen, Belgium.
