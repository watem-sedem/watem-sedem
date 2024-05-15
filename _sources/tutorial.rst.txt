.. _tutorial:

########
Tutorial
########

In this tutorial we will give a short overview on how the model can be configured for practical use.
On the one hand, we will explain how the configuration file (``.ini``-file) should be made, and on the other hand, how the prefered model options are enabled or
disabled.

0. Getting started
******************

First of all, an installation of a simple text editor like `notepad++ <https://notepad-plus-plus.org/>`_ is needed
in order to edit the configuration file (``.ini``-file). A GIS program is needed as well to create the necessary input rasters and to view the output rasters. We recommend
`QGIS <https://www.qgis.org>`_ or `Saga GIS <http://www.saga-gis.org/>`_. In this
tutorial we will not explain how to use GIS, nor how to make the
input rasters, these are basic GIS-tasks and these are explained in several
tutorials online. We encourage readers to start the tutorial by using
the tutorial_1.ini file, and adapt this file according to the exercise. This file can be found on the github page of CN-WS, under the folder: `docs/tutorials <https://github.com/watem-sedem/watem-sedem/tree/master/docs/tutorials>`_. The
other tutorial ini-files in the tutorials folder can be used to check your
adaptations.

Then, you need to download the latest release of the model. Pre-build binaries
exist for windows and linux. It is also possible to build the model from source.
See :ref:`the installation page <install>` for more information.

All example model runs described below make use of the test dataset which is
available in the repository under `testfiles/molenbeek/modelinput <https://github.com/watem-sedem/watem-sedem/tree/master/testfiles/molenbeek>`_.

.. _tutsection1:

1. A basic model run with WaTEM/SEDEM
*************************************

As a first exercise in the tutorial we will make a basic model run with
WaTEM/SEDEM. The basic model run includes only mandatory input files.
This run does not use advanced :ref:`model extensions <usage>`. All
keywords used in an ini-file are explained in the :ref:`reference <usage>`.

.. literalinclude:: tutorials/tutorial_1.ini
    :language: ini

After navigating to the folder where you have build the model, or installed the binary, you can
use your terminal to run the model, as follows: 

.. code-block:: bash

    $ watem_sedem "<path to watem-sedem repository>/watem_sedem/docs/tutorials/tutorial_1.ini"

When the model run starts you will see::

    WATEM/SEDEM model version: 5.0.0

    Inifile : <path to watem-sedem repository>/watem_sedem/docs/tutorials/tutorial_1.ini

After the completion of the calculations, the model reports the execution time::

    Calculations completed. Program Execution Time: 3.4 sec

Now, you can have a look in the model output folder defined in the ini-file. A
txt-file with a summary of the results is written:
:ref:`Total sediment.txt<totalsedimenttxt>`.

.. literalinclude:: tutorials/Total sediment 1.txt
    :language: vim

This table contains the sum of all pixels with a negative mass balance (Total
erosion) and a positive mass balance (total deposition). A negative mass balance
means that the available sediment (RUSLE + SediIn) in a pixel is smaller than the
transport capacity, and thus leads to a net erosion in this pixel. A positive mass balance means that the available sediment in
the pixel is larger than the transport capacity, and thus leads to a net sedimentation in this pixel. We refer to the
:ref:`WaTEM/SEDEM section <Concept>` where these concepts are explained more in
depth.

The total sediment table also reports how
much sediment enters the river pixels (Sediment leaving the catchment, via the
river). The sediment leaving the catchment, not via the river is (mostly) a small
fraction of the sediment that leaves the catchment via the borders (to nodata pixels).

It is important to note that the model made a catchment outlet itself: the model looked
for the lowest (river) pixel treated in the routing algorithm and assigned this pixel
as the model domain outlet. All the sediment is routed through the landscape to this pixel
If you have a look in the model input folder, you will see
that :ref:`Outlet.rst <outletmap>` appeared. All pixels in this raster have
value '0' except one pixel with value 1. This pixel is the outlet of the model.

Congratulations! You just finished your first model calculation with WaTEM/SEDEM!

2. Get more model output!
*************************

The first tutorial described a very basic model run: only
:ref:`a txt file <totalsedimenttxt>` with a summary of the results is written as
output. If we want spatial information about where erosion and sedimentation
occurs, we can enable :ref:`various output options <modeloutput>`. Therefore,
we need to add a section in the ini-file with the desired output:
:ref:`write sediment export <writesedexport>` and
:ref:`write water erosion <writerwatereros>`.

.. literalinclude:: tutorials/tutorial_2.ini
    :language: ini
    :lines: 24-26

To do so, you can make a copy of the `tutorial_1.ini`-file in a directory
of your choice, adapt the ini-file with the lines stated above, and rename the file to 'tutorial_2.ini'.
Then the adapted model can be ran using

.. code-block::

    watem_sedem $your_favorite_folder/tutorial_2.ini

If this is not working as it should, please check if the reference
'`your_favorite_folder`' is the folder where you saved the `tutorial_2.ini`-
file. If unsure, you can always check and run the reference `tutorial_2
.ini`-file present in the repository with:

.. code-block::

    watem_sedem <path to watem-sedem repository>/watem-sedem/docs/tutorials/tutorial_2.ini

The following output rasters will emerge in the output folder:

- :ref:`SediExport_kg.rst <sediexportrst>`,
- :ref:`SediOut_kg.rst <sedioutrst>`,
- :ref:`SediIn_kg.rst <sediinrst>`,
- :ref:`WATEREROS (kg per gridcel).rst <watereroskgrst>`,
- :ref:`WATEREROS (mm per gridcel).rst <watererosmmrst>`

These rasters can help us to identify the spatial patterns of the soil erosion
and sediment deposition in the model area. The
:ref:`SediExport_kg.rst <sediexportrst>` is a very useful raster when examining
the river locations with large sediment inputs. This raster shows
how much sediment enters every river pixel. Remember, the total amount of
sediment entering the rivers can be found in
:ref:`Total sediment.txt <totalsedimenttxt>` (Sediment leaving the catchment,
via the river). This amount is in fact the sum of all river pixels in
SediExport_kg.rst.

If you want to see the paths of the sediment transport through the landscape,
you can look at :ref:`SediOut_kg.rst <sedioutrst>` or
:ref:`SediIn_kg.rst <sediinrst>`. These rasters display how much sediment
(in kg) is transported into and out of a cell.

The WATEREROS rasters can help with making a spatial analysis of areas where a lot of erosion occurs, or where a lot
of deposition is possible. These rasters
represent the result of the comparison between the total available sediment and
the trasport capacity of a pixel (for more information about this concept,
see :ref:`here <Concept>`). Positive values in these rasters indicate deposition,
negative values indicate erosion. The
:ref:`Total sediment file <totalsedimenttxt>`, discussed in the tutorial '1. A basic model run with WaTEM/SEDEM',
contained the values 'total erosion' and 'total deposition'. These values are
calculated as the sum of all pixels in the WATEREROS raster where the pixel
value is larger (deposition) or smaller (erosion) than zero.

If you want more spatial output, have a look in :ref:`the reference <modeloutput>`
about all the possibilities!

3. Adding buffer basins
***********************

In the previous tutorials we learned how to run WaTEM-SEDEM and how to
enable or disable model output. In this tutorial we will explain how to enable
one of the *advanced* features of the model, the so-called model extensions.
The example will make use of the
:ref:`Include Buffers <includebuffers>` extension, but the same principles can be
used for all other extensions!

Buffer basins are infrastructural features that trap sediment. As described in
:ref:`the reference  <includebuffers>`. Two extra parameters are needed in the
ini-file if we want to enable this feature: the
:ref:`buffer map filename <buffermap>` and  the
:ref:`number of buffers <nrbuffers>`.

In this example we will add 31 buffers to the modelled catchment. After adding
the buffer map filename and the number of buffers the ini-file looks like this:

.. literalinclude:: tutorials/tutorial_3a.ini
    :language: ini
    :emphasize-lines: 12,27-31

If we run the model with this configuration (tutorial_3a.ini) we get::

    WaTEM/SEDEM model version: 5.0.0

    Inifile : <path to inifile>\docs\tutorials\tutorial_3a.ini

    Error in data input: Buffer 1 trapping efficiency value missing or wrong
    data format

Oh no, an error has occured. What does this mean? This error indicates that
the input data in the configuration file is not correct. We read that the
trapping efficiency value of Buffer 1 is missing or is given in a wrong data
format. On the :ref:`Model choices  <includebuffers>` page, more information can be found. 
There, you find that we need to define the trapping
efficiency, the extension id and buffer id for every buffer in the configuration file (see
:ref:`here <bufferdata>`). This has not yet been done here. So, we need to add 31 sections,
one for every buffer, with these data (see also tutorials/tutorial_3b.ini):

.. literalinclude:: tutorials/tutorial_3b.ini
    :language: ini

A buffer should trap sediment. So, after a successful model run with the above given ini-file, we
can look at the Total sediment file in the output folder, to see that the '*sediment trapped in buffers*' line is added.

.. literalinclude:: tutorials/Total sediment 3b.txt
    :language: vim
    :emphasize-lines: 5

This line gives us the total amount of sediment trapped in the 31
buffer basins added to the model. We also see that the amount of sediment
that reached the river (Sediment leaving the catchment, via the river) is less
in this model run than in the previous model run (see :ref:`section 1 <tutsection1>`). We can conclude that the addition
of buffers in this model run is a good measure to reduce the sediment input
in the rivers.

4. Use the CN extension
***********************

To use the CN-extension of WaTEM-SEDEM, we need to enable it with the
:ref:`curve number option <simple>`.

.. code-block:: ini

    [Extensions]
    Curve number = 1

Using the CN-extension implies we need to define additional input in the ini-file.
At the bottom of the :ref:`curve number <simple>`-section, the mandatory input is for running the
WaTEM-SEDEM with the CN model extension is described. We extend
the ini-file with this input:

.. code-block:: ini

    [Files]
    ...
    cn map filename = CNmap.rst
    rainfall filename = LS09_15B_N_event_1_dummy.txt
    ...
    [Parameters extensions]
    ...
    alpha = 0.4
    beta = 0.05
    stream velocity = 0.3
    5-day antecedent rainfall = 0
    desired timestep for model = 60
    endtime model = 2940
    ...

The CN module will create some additional :ref:`output <CNoutput>`. Most output is
automatically generated by enabling the CN module, however, it is posible to write
an extra output raster, namely, the 'rainfall excess' raster (Remap.rst).
This can be done by adding following line to the ini-file:

.. code-block:: ini

    [Output]
    ...
    write rainfall excess = 1
    ...

We refer to the documentation about CN for the interpretation of the output.

5. More examples?
*****************

Do you want to experiment even more with the options of WaTEM-SEDEM? The test files
in the repository contain an example project where following options are used:

- river routing
- include ditches
- include sewers
- create ktc map
- ...

Have a look at them, and using the principles explained above you should be able to get
these working!

