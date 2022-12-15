.. _intro:

#####
CN-WS
#####

CN-WS is a model that can be used to
simulate soil erosion, sediment transport, water and sediment routing and
flow run-off in time and space. The package combines three existing submodels:

 - The Curve Number model (CN) for simulating flow run-off.
 - The WaTEM/SEDEM model (WS) for simulating erosion and over-land sediment
   transport.
 - The routing algorithm for simulating water and sediment routing over land.

CN-WS is developed to assess soil erosion on
agricultural fields, sediment transport through the landscape and sediment
delivery to watercourses and sewage systems. As the impact of soil erosion
mitigation measures is incorporated, the model can also be used to evaluate soil
erosion management measures and compare mitigation strategies.

This documentation page explains how CN-WS works and how the model can be used.
The official code for CN-WS can be found at https://github.com/cn-ws/cn-ws .

Download / Clone
================

The executables of all releases can be found on 
`the release page <https://github.com/cn-ws/cn-ws/releases>`_. The source code 
can be cloned or downloaded via the main page. **Note that you need to clone
the directory via HTTPS or SSH to access the testfiles of the molenbeek 
(subdirectory ``testfiles``)**. The testfiles are not pulled correctly when 
downloading the package via ZIP due to using `Git Large File Storage 
<https://git-lfs.github.com/>`_. 

Package
=======

CN-WS has thee modus operandi: CN_WS, CN_WS_console and CN_WS_LongTerm.
Typically the model is run in a command-line interface or a Graphical User
Interface (GUI). Yet, since 2016, the GUI is not maintained. Users are thus
advised to use the command line version.

1. The CN_WS model is used simulate the erosion and sediment transport for one
   rainfall event. This model can be used through the GUI. To start the model,
   use the "CN_WS" shortcut in the current folder. To view the underlying
   source code, open the CN_WS folder and:

   - open the different .pas files in Notepad
   - OR open Invoerform.lpi in Lazarus

2. The CN_WS_console model is similar to CN_WS, but without GUI. This
   version can be used through WINDOWS commandline (see manual) (This model
   version is used by the long term version of the model to run each rainfall
   event). To view the underlying source code, open the CN_WS_console folder
   and:

   - open the different .pas files in Notepad
   - OR open CN_WSmodel.lpi in Lazarus

3. The CN_WS_LongTerm is the long term version of the model; including GUI.
   To start the model, use the "CN_WS_LongTerm" shortcut in the current
   folder. To view the underlying source code, open the CN_WS_LongTerm
   folder and:

   - open the different .pas files in Notepad
   - OR open Invoerform.lpi in Lazarus

For who is this documentation?
==============================

This page aims to explain how the CN-WS model can be run for a given case
study, and which output can be analysed to study the specific case. In
addition, it aims to explain how a specific run for a case study can be set-up
(i.e. which input data are needed? in which format?). Finally, this page
is the reference for background information on model concepts, options, inputs
and outputs.

In order to run the CN-WS model, this set of minimal skills is required:

- Basic command line skills (know how to open your command line, and run an
  executable).
- Opening text files in a text editor.
- Opening raster data in QGIS, ArcGIS or your favorite GIS program.

.. note::

    The defined skills above allow you to run the CN-WS model with the input
    files listed in the package (for an example, see :ref:`here<tutorial>`).
    For generating your own input data for the model, for a self-defined
    case study, a good knowledge of GIS is required.

License
=======

This project is licensed under the GNU General Public License v3.0, see
`LICENSE <https://github.com/cn-ws/cn-ws/blob/master/LICENSE>`_ for more information.

Documentation
=============

The documentation of this project is available at https://docs.fluves.net/cnws-pascal/.
In this documentation, a theoretical background of the model and an overview
of the model usage are given, as well as an installation guide followed by a
tutorial.


References
==========
Antea, 2016. Modellering van de sedimentaanvoer naar de waterlopen, het
effect van erosiebestrijdingsmaatregelen en het transport van sediment in de
onbevaarbare waterlopen. Departement Omgeving. Afdeling Gebiedsontwikkeling,
Omgevingsplannen en -projecten. Land en Bodembescherming, Brussel.
https://www.vlaanderen.be/publicaties/modellering-van-de-sedimentaanvoer-naar-de-waterlopen-het-effect-van-erosiebestrijdingsmaatregelen-en-het-transport-van-sediment-in-de-onbevaarbare-waterlopen

Deproost, P., Renders, D., Van de Wauw, J., Van Ransbeeck, N.,
Verstraeten, G., 2018, Herkalibratie van WaTEM/SEDEM met het DHMV-II als
hoogtemodel: eindrapport. Brussel.
https://archief.onderzoek.omgeving.vlaanderen.be/Onderzoek-1812384

