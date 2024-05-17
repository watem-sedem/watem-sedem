.. image:: https://zenodo.org/badge/DOI/10.5281/zenodo.10997287.svg
  :target: https://doi.org/10.5281/zenodo.10997287


###########
WaTEM/SEDEM
###########

WaTEM/SEDEM is a soil erosion and sediment delivery model used for catchment-scale
simulations. The model incorporates a Revised Universal Soil Loss Equation (RUSLE)
module to simulate gross erosion, extended with a spatially-explicit sediment routing
module to simulate sediment transport based on a cellular transport capacity. The model
therefore provides the user with predictions of gross and net soil erosion at the
catchment scale.

WaTEM/SEDEM has been applied for numerous applications considering both the on-site and
off-site impacts of soil erosion. These applications include the predicted soil loss
from fields, off-site deposition, predictions of catchment sediment yield. The model has
also been extended for the simulation of transported heavy metals, nutrients and organic
carbon. The default application of the model can be extended in several ways to simulate
the impact of soil erosion mitigation measures in a spatially-explicit way. For example,
additional options within the model allow for the consideration of other anthropogenic
factors such as roads, sewage systems, water and sediment buffer basins, dams and
ditches.

This documentation page explains how WaTEM/SEDEM works and how the model can
be used. The official code for WaTEM/SEDEM can be found at
https://github.com/watem-sedem/watem-sedem.


For who is this documentation?
==============================

This page aims to explain how the WaTEM/SEDEM can be run for a given case
study, and which output can be analysed to study the specific case. In
addition, it aims to explain how a specific run for a case study can be set-up
(i.e. which input data are needed? in which format?). Finally, this page
is the reference for background information on model concepts, options, inputs
and outputs.

In order to run WaTEM/SEDEM, a set of minimal skills is required:

- Basic command line skills (know how to open your command line, and run an
  executable).
- Opening text files in a text editor.
- Opening raster data in QGIS, ArcGIS, Idrisi or an equivalent GIS program.

.. note::

    The defined skills above allow you to run WaTEM/SEDEM with the input
    files listed in the package (for an example, see :ref:`here<tutorial>`).
    For generating your own input data for the model, for a self-defined
    case study, a good knowledge of GIS is required.

Download / Clone
================

The commandline interface of all releases can be found on
`the release page <https://github.com/watem-sedem/watem-sedem/releases>`_. The source code
can be cloned or downloaded via the main page. Note that you need to clone
the directory via HTTPS or SSH to access the testfiles of the molenbeek
(subdirectory ``testfiles``). The testfiles are not downloaded correctly when
the code is downloaded as a ZIP because they are stored via `Git Large File Storage
<https://git-lfs.github.com/>`_.
