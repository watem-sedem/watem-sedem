#####
CN-WS
#####

The Curve Number-WaTEM/SEDEM (CN-WS) is a computer model that simulates soil
erosion and sediment transport in time and space. The model is combination
of two existing models: Curve Number (flow run-off model) and WaTEM/SEDEM
(water erosion and sediment transport model). CN-WS is developed to assess
sediment input to rivers, soil erosion on agricultural fields and to
evaluate soil erosion management measures.

A short history of CN-WS
=============================

The sediment export model CN-WS was developed starting from the year 2013
until 2016 by KULeuven in partnership with `Antea Belgium
<https://anteagroup.be/>`_, commissioned by
the Flemish government (Antea, 2016). The aim of the development were to
create a tool that can quantify erosion, sediment/water run-off, and the
effect of erosion mitigation measures on erosion & run-off to the river.

The initial developments of WaTEM/SEDEM were initialised by KULeuven,
department (`Departement of Earth and Environmental Sciences <https://ees
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


For who is this page?
=====================

This page aims to explain how the CN-WS model can be run for a given case
study, and which output can be analysed to study the specific case. In
addition, it aims to explain how a specific run for case study can be set-up
(i.e. which input data are needed? in which format?). Finally, this page is
the location for background information on model concepts, options, inputs
and outputs. In order to this, this set of minimal skills is required:

 - Basic command line skills (know how to open your command line, and run an
   executable).
 - Opening text files in a text editor.
 - Opening raster data in QGIS, ArcGIS or your favorite GIS program.

Model
==============

CN-WS has thee modus operandi: CN_WS, CN_WS_console and CN_WS_LongTerm.
Typically the model is run in a command-line interface,

1. The CN_WS model is used simulate the erosion and sediment run-off for one
rainfall event.
graphical user interface (GUI).
To start the model, use the "CN_WS" shortcut in the current folder. 
To view the underlying source code, open the CN_WS folder and:
- open the different *.pas files in notepad
- OR open Invoerform.lpi in lazarus

2. CN_WS_console = same model as CN_WS, but without GUI.
This version can be used through WINDOWS commandline (see manual).
(This model version is used by the long term version of the model to run each rainfall event).
To view the underlying source code, open the CN_WS_console folder and:
- open the different *.pas files in notepad
- OR open CN_WSmodel.lpi in lazarus
	
3. CN_WS_LongTerm = long term version of the model; including GUI.
To start the model, use the "CN_WS_LongTerm" shortcut in the current folder.
To view the underlying source code, open the CN_WS_LongTerm folder and:
- open the different *.pas files in notepad
- OR open Invoerform.lpi in lazarus

License
=======

This project is licensed under the ??? License, see
`LICENSE <https://git.fluves.net/cn_ws/LICENSE>`_ for more information.

Documentation
=============

The documentation of this project is available at https://docs.fluves
.net/cnws-pascal/. In this documentation, a theoretical background of the
model and an overview of the model usage are given (see :ref:`here <model>`
and :ref:`here <usage>`). Next, an :ref:`installation guide <installation>` and
:ref:`tutorial <tutorial>` is presented.


References
==========
Antea, 2016. Modellering van de sedimentaanvoer naar de waterlopen, het effect van erosiebestrijdingsmaatregelen en het transport van sediment in de onbevaarbare waterlopen. Antea Belgium NV, Antwerpen, Belgium.
