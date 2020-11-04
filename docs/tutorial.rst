.. _tutorial:

########
Tutorial
########

To use CNWS a .ini configuration file is needed. In this tutorial we will
explain how this configuration file is made and how model options are enabled or
disabled.

0. Getting started
******************

Install a simple text editor like `notepad++ <https://notepad-plus-plus.org/>`_.
This editor is needed to edit the configuration file. A GIS program is also
needed to create the input rasters and to view the output rasters. We recommend
`QGIS <https://www.qgis.org>`_ or `Saga <http://www.saga-gis.org/>`_. In this
tutorial we will not explain how to use the gis software, nor how to make the
input rasters, these are basic GIS-tasks and these are explained in several
tutorials on the net.

First, you need to download the latest release of the model. Pre-build binaries
exist for windows and linux. It is also possible to build the model from source.
See :ref:`the installation page <install>` for more information.

All example model runs described below make use of the testdataset which is
available in the repository under testfiles/molenbeek/modelinput. The ini-files
used in these tutorials can be found in the docs folder of the repository.

1. A basic model run with WaTEM-SEDEM
*************************************

As a first exercise in the tutorial we will make a basic model run with the
WaTEM-SEDEM module of CN-WS. The basic model run includes only mandatory files
and input. This run disables all *advanced* modeloptions.

.. literalinclude:: tutorial_1/tutorial_1.ini
    :language: ini

In the folder where you have build the model, or installed the binary, you can
run in your terminal

.. code-block:: bash

    $ cn_ws <path to cnws repository>/cn_ws/docs/tutorial_1/tutorial_1.ini

When the model run starts you will see::

    CN_WS model

    Inifile : <path to cnws repository>/cn_ws/docs/tutorial_1/tutorial_1.ini

After completion of the calculations the model reports the execution time::

    Calculations completed. Program Execution Time: 5.96 sec

Now, you can have a look in the modeloutput folder defined in the ini-file. A
txt file with a summary of the results is written:
:ref:`Total sediment.txt<totalsedimenttxt>`.

.. literalinclude:: tutorial_1/Total sediment
    :language: vim

This table contains the sum of all pixels with a negative mass balance (Total
erosion) and a positive mass balance (total deposition). It also reports how
much sediment enters the river pixels (Sediment leaving the catchment, via the
river). The sediment leaving the catchment, not via the river is (mostly) a small
fraction of the sediment that leaves the catchment via the borders (to nodata pixels).

In the ini-file of this tutorial we have not defined any outlets
(:ref:`Manual outlet selection = 0 <manualoutlet>`). Therefore, the
model made an outlet itself: the model looked for the lowest (river) pixel treated
in the routing algorithm. If you have a look in the model input folder, you will see
that :ref:`Outlet.rst <outletmap>` appeared. All pixels in this raster have
value '0' except one pixel with value 1. This pixel is the outlet of the model.

Congratulations! You just finished your first model calculation with CN-WS!

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

    cn_ws <path to cnws repository>/cn_ws/docs/tutorial_2/tutorial_2.ini

We see the following output rasters emerge in the outputfolder:

- :ref:`SediExport_kg.rst <sediexportrst>`,
- :ref:`SediOut_kg.rst <sedioutrst>`,
- :ref:`SediIn_kg.rst <sediinrst>`,
- :ref:`WATEREROS (kg per gridcel).rst <watereroskgrst>`,
- :ref:`WATEREROS (mm per gridcel).rst <watererosmmrst>`

These rasters can help us to identify the spatial patterns of the soil erosion
and sediment deposition in the model area. The
:ref:`SediExport_kg.rst <sediexportrst>` is a very useful raster when examining
the points in the river with large sediment inputs. We can see in this raster
how much sediment enters every river pixel. Remember, the total amount of
sediment entering the rivers can be found in
:ref:`Total sediment.txt <totalsedimenttxt>` (Sediment leaving the catchment,
via the river). This amount is in fact the sum of all riverpixels in
SediExport_kg.rst.

If you want to see the paths
of the sediment transport through the landscape, you might have a look at
:ref:`SediOut_kg.rst <sedioutrst>` or :ref:`SediIn_kg.rst <sediinrst>`. These
rasters display how much sediment (in kg) is transported towards and outwards of
a cell.

Making a spatial analysis of areas where a lot of erosion occurs, or where a lot
of depostion is possible, can be done with the WATEREROS rasters. These rasters
represent the result of the comparison between the total available sediment and
the trasport capacity of a pixel (for more information about this concept,
see :ref:`here <Concept>`). Positive values in these rasters indicate deposition,
negative values indicate erosion. 

3. Adding buffer basins
***********************



3. River routing?
*****************


4. Use the CN module
********************




