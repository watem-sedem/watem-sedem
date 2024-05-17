.. _usage:

###########
Model Usage
###########

The aim of this page is to
give a full overview of the possible model choices, inputs and outputs.

It is important to indicate that WaTEM-SEDEM is a raster-based model. The in-
and outputs are therefore mainly defined by this raster-format (idrisi). In addition,
a number of text-files are used to define additional inputs and model
choices. WaTEM/SEDEM in its most simple implementation has a low number of model
parameters. However, a number of optional user choices (e.g. to include management
processes) or to consider parameters which are by-default fixed as variable will
naturally extend this parameter dimensionality. This is why, for a first introduction to
the use of WaTEM/SEDEM,
one is referred to the tutorial (:ref:`click here <tutorial>`).

All user choices, input, outputs and options are defined in an ini-file. The
model reads the ini-file and uses this info to model the catchment according to the
user choices. In the following chapters all possible keywords of the ini-file are
discussed. As the needed inputs and the generated outputs are largely dependent on the
defined model choices, we first explain all possible model choices. The model choices
also give an overview of all the possibilities of the model. 

.. toctree::

  rasterinfo
  choices
  model_extensions
  input
  output
  calibration
  inifile
 
