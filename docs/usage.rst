.. _usage:

###########
Model Usage
###########

The CN-WS model is a composite model simulating the hydrological and
sediment mass balance for a given area. The aim of this page is to
give a full overview of the possible model inputs, outputs and choices.
It is important to indicate that CN-WS is a raster-based model. Thus, inputs
and outputs are mainly defined by this raster-format (idrisi). In addition,
a number of text-files are used to define additional inputs and model
choices. It is noted that a lot of inputs, outputs and options are
available, and that - for a first introduction to the use of the CN-WS model
- one is referred to the tutorial (:ref:`click here <tutorial>`).

All user choices, input, outputs and options are defined in an ini-file. The
model reads the ini-file and uses this info to model a catchment according the
user choices. In the following chapters all possible keywords of the ini-file are
discussed.

.. toctree::
  
  choices
  input
  output
  calibration
  inifile
 