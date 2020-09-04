############
Installation
############

Builing from source
===================

CN-WS is written in Free Pascal. 

To build the program from source you have to install the Lazarus package. 

Alternatively, on a linux machine, you can install the latest build of the console version in the master branch with

.. code-block:: shell
	sudo apt-install cn-ws

Running CN-WS
=============

When you want to use CN-WS from the command line you need a valid ini-file with all references to inputfiles and modelchoices. The model can be run with

.. code-block:: shell

	C:\cn_ws\CN_WSmodel.exe C:\modelinput\catchment.ini-file

The :doc: `tutorial` gives a practical example how to use the model and is the best place to continue from here.