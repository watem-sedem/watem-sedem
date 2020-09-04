#####
CN-WS
#####

CN-WS is a model that predics soil erosion and sediment transport in time and space. 
The model is combination of two existing models: Curve Number (run-off model) and WaTEM-SEDEM (water erosion and sediment transport model). 
CN-WS is developed to assess sediment input to rivers, soil erosion on argricultaral fields and to evaluate soil erosion management measures. 

Model versions
==============

Three model versions exist:

CN_WS = CN_WS model to simulate one rainfall event; model version including graphical user interface (GUI).
To start the model, use the "CN_WS" shortcut in the current folder. 
To view the underlying source code, open the CN_WS folder and:
- open the different *.pas files in notepad
- OR open Invoerform.lpi in lazarus

CN_WS_console = same model as CN_WS, but without GUI.
This version can be used through WINDOWS commandline (see manual).
(This model version is used by the long term version of the model to run each rainfall event).
To view the underlying source code, open the CN_WS_console folder and:
- open the different *.pas files in notepad
- OR open CN_WSmodel.lpi in lazarus
	
CN_WS_LongTerm = long term version of the model; including GUI.
To start the model, use the "CN_WS_LongTerm" shortcut in the current folder.
To view the underlying source code, open the CN_WS_LongTerm folder and:
- open the different *.pas files in notepad
- OR open Invoerform.lpi in lazarus

License
=======

This project is licensed under the ??? License, see  `LICENSE <https://git.fluves.net/cn_ws/LICENSE>`_ for more information.

Documentation
=============

The documentation of this project is available at https://docs.fluves.net/cn_ws