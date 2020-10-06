############
Installation
############

Linux (debian/linux)
********************

Installing on linux (debian/linux)
==================================

Pre-built binary packages of cn-ws exist on apt.fluves.net . 

To install a specific version, go to https://apt.fluves.net/amd64/ and pick a
file. This file can be installed using:

.. code-block:: shell

	sudo dpkg -i https://apt.fluves.net/amd64/cn-ws_20200114-2~55.gbp527d54_amd64.deb


Building on linux
=================

First dependencies should be installed. This includes lazarus

.. code-block:: shell

	apt install build-essential lazarus


Then either open the project in lazarus or enter the project directory and run
`make`. This will build the binary CN_WS_model under the CN_WS_console directory.

Compiling a debian package
==========================

If you would like to make a package instead of a static binary, you can follow
these steps:

Make sure you have `debuild` and all build dependencies installed:

.. code-block:: shell

	sudo apt install devscripts build-essential lintian
	sudo apt install lazarus


Next, build and install from source:

.. code-block:: shell

	debuild -b 
	sudo dpkg -i ../cn_ws*deb


To test your installation, run CN_WS_model from terminal:

.. code-block:: shell

	CN_WS_model

.. code-block:: shell

	sudo apt-install cn-ws

Windows
*******

Building on Windows
===================

To build CN-WS on windows, you need to install the Lazarus IDE first.
You can download it on https://www.lazarus-ide.org/

Open the project in lazarus (open the .lpi file) and build the executable
(Shift + F9). The executable is stored in the same folder as the .lpi file and
can be run without further installation. 

Running CN-WS
*************

When you want to use CN-WS from the command line you need a valid ini-file with
all references to inputfiles and modelchoices. The model can be run with

.. code-block:: shell

	C:\cn_ws\CN_WSmodel.exe C:\modelinput\catchment.ini-file
	
It is possible to run the executable of CN-WS in every directory by adding its
location to the system variable PATH. The model was tested on dutch
("," as a decimal seperator) and english ("." as a decimal seperator) language
system computers. Both work, other language systems are not tested yet.

The :doc: `tutorial` gives a practical example how to use the model and is the
best place to continue from here.