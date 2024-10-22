.. _install:

############
Installation
############

Windows
********

Windows packages 
================
Windows binaries of watem_sedem can be found under `watem-sedem releases <https://github.com/watem-sedem/watem-sedem/releases>`_

Linux (debian/ubuntu)
********************

Installing on linux (debian/linux)
==================================

Pre-built binary packages of watem-sedem exist on https://github.com/watem-sedem/watem-sedem/releases .

Building on linux
=================

First build dependencies should be installed. The Lazarus dependency can be installed using:

.. code-block:: shell

	apt install build-essential lazarus


Then either open the project in Lazarus or enter the project directory and run
`make`. This will build the binary cn_ws under the cn_ws directory.

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


To test your installation, run cn_ws from terminal:

.. code-block:: shell

	cn_ws

.. code-block:: shell

	sudo apt-install cn-ws

Windows
*******

.. _buildwindows:

Building on Windows
===================

To build WaTEM/SEDEM on windows, you need to install the Lazarus IDE first.
This can be downloaded it from https://www.lazarus-ide.org/. Install it under `c:\lazarus` use the provided `make.bat` script to build. 

Open the project in Lazarus (open the .lpi file, see subfolder cn_ws/cn_ws/cn_ws.lpi)
and build the executable (Shift + F9). The executable is stored in the same folder as
the .lpi file and can be run without further installation. 

.. note::
    This step only has to be done if no cn_ws.exe file is present in the subfolder
    `cn_ws/cn_ws`

Running WaTEM/SEDEM
*******************

When you want to use WaTEM/SEDEM from the command line you need a valid ini-file with
all references to input files and model choices. The model can be run with:

.. code-block:: shell

	C:\watem-sedem\watem_sedem.exe C:\modelinput\catchment.ini-file
	
It is possible to run the executable of CN-WS in every directory by adding its
location to the system variable PATH. The model was tested on Dutch
("," as a decimal separator) and English ("." as a decimal separator) language
system computers. The model runs for these language systems, however other language systems yet to be tested.

The :ref:`tutorial <tutorial>` gives a practical example on how to use the model
and is the best place to continue from here.
