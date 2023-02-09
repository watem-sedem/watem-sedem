.. _rasterinfo:

#######
Rasters
#######

The input and outputfiles of CN-WS are based on geospatial rasters. Every raster
is defined with a cell size (e.g. 20x20m or 20x50m) and a number of rows and
columns. Geospatial rasters are placed within a reference system (e.g. WGS84 or
EPSG:31370) to situate these rasters in the right location.

.. _rasterformat:

Format
======

All input and output rasters are
`Idrisi-rasters <https://gdal.org/drivers/raster/Idrisi.html>`_  or
`Saga-rasters <https://gdal.org/drivers/raster/sdat.html>`_ and should have
the same amount of columns, rows and cell size.
If one of the  input rasters has a different spatial extent, the model will
raise an exception and will stop the execution.

Idrisi- and Saga-rasters are the native file format of respectivily
Idrisi GIS and `SAGA GIS <http://www.saga-gis.org/>`_, but can be opened,
edited and saved with almost every GIS-package like QGIS or SAGA-GIS,
with the aid of the `GDAL library <https://gdal.org>`_.

Both file formats contain a binary dump of the rasterdata (.rst and .sdat) and
an ascii metadata file (.rdc and .sgrid). Saga-rasters can also have a .prj-file
containing information on the projection of the raster.

To switch between
these two raster formats, use the :ref:`Saga grids <sagagrids>`-option.

.. _gridcoordinates:

Raster Coordinates
==================

The origin of the raster coordinatesytem in CN-WS lies in the upper left
rastercell. This cell has the raster coordinates (1,1).

