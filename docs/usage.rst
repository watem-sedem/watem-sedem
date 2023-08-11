.. _usage:

###########
Model Usage
###########

The CN-WS model is a composite model simulating the hydrological and
sediment mass balance for a given area. The aim of this page is to
give a full overview of the possible model choices, inputs and outputs.

It is important to indicate that CN-WS is a raster-based model. In-
and outputs are therefore mainly defined by this raster-format (idrisi). In addition,
a number of text-files are used to define additional inputs and model
choices. In CN-WS, a lot of inputs are required, and many outputs and options are
available. This is why, for a first introduction to the use of the CN-WS model,
one is referred to the tutorial (:ref:`click here <tutorial>`).

All user choices, input, outputs and options are defined in an ini-file. The
model reads the ini-file and uses this info to model the catchment according the
user choices. In the following chapters all possible keywords of the ini-file are
discussed. As the needed inputs and the generated outputs are largely dependent on the
defined model choices, we first explain all possible model choices. The model choices
also give an overview of all the possibilities of the model. 

.. toctree::

  rasterinfo
  choices
  input
  output
  calibration
  inifile
 
