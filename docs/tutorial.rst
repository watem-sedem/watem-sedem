.. _tutorial:

########
Tutorial
########

To use CNWS a .ini configuration file is needed. In this tutorial we will
explain how this configuration file is made and how options are enabled or
disabled.

0. Getting started
******************

Install a simple text editor like `notepad++ <https://notepad-plus-plus.org/>`_.
This editor is needed to edit the configuration file. A GIS program is also
needed to create the input rasters and to view the output rasters. We recommend
`QGIS <https://www.qgis.org>`_ or `Saga <http://www.saga-gis.org/>`_. In this
tutorial we will not explain how to use the gis software, nor how to make the
input rasters, these are basic GIS-tasks and are explained in several tutorials
on the net.

First, you need to download the latest release of the model. Pre-build binaries
exist for windows and linux. It is also possible to build the model from source.

See :ref:`the installation page <install>` for more information.

All example model runs described below make use of the testdataset which is
available in the repository under testfiles/molenbeek/modelinput.

1. A basic model run with WaTEM-SEDEM
*************************************

As a first exercise in the tutorial we will make a basic model run with the
WaTEM-SEDEM module of CN-WS. The basic model run includes only mandatory files
and input. This run disables all *advanced* modeloptions.

.. literalinclude:: tutorial_1.ini
    :language: ini

2. Get more model output!
*************************

The first tutorial described a very basic model run: only
:ref:`a txt file <totalsedimenttxt>` with a summary of the results is written as
output. If we want spatial information about where erosion and sedimentation
occurs, we can enable :ref:`some output options <modeloutput>`.

In this tutorial we change the output maps section in the ini-file a bit by
enabling the options :ref:`write upstream area <writeuparea>`,
:ref:`write sediment export <writesedexport>` and
:ref:`write water erosion <writerwatereros>`.

.. literalinclude:: tutorial_2.ini
    :language: ini
    :emphasize-lines: 38,39,42,43
    :lines: 34-45

When the model is ran with the this adapted ini-file

.. code-block::
    cn_ws tutorial_2.ini

We see the following output rasters emerge: :ref:`uparea.rst <upareamap>`,
:ref:`SediExport_kg.rst <sediexportrst>`, :ref:`SediOut_kg.rst <sedioutrst>`,
:ref:`SediIn_kg.rst <sediinrst>`,
:ref:`WATEREROS (kg per gridcel).rst <watereroskgrst>` and
:ref:`WATEREROS (mm per gridcel).rst <watererosmmrst>`.

These rasters can help us to identify the spatial patterns of the soil erosion
and sediment deposition in the model area. The
:ref:`SediExport_kg.rst <sediexportrst>` is a very useful raster when examining
the points in the river with large sediment inputs. If you want to see the paths
of the sediment transport through the landscape, you might have a look at
:ref:`SediOut_kg.rst <sedioutrst>` or :ref:`SediIn_kg.rst <sediinrst>`. These
rasters display how much sediment (in kg) is transported towards and outwards of
a cell. The paths seen in :ref:`SediOut_kg.rst <sedioutrst>` or
:ref:`SediIn_kg.rst <sediinrst>` can

3. Adding buffer basins
***********************



3. River routing?
*****************


4. Use the CN module
********************




