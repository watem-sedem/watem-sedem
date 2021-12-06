.. _rasters:

#######
Rasters
#######

The input and outputfiles of CN-WS are based on geospatial rasters. Every raster
is defined with a cell size (e.g. 20x20m or 20x50m) and a number of rows and
columns. Geospatial rasters are placed within a reference system (e.g. WGS84 or
EPSG:31370) to situate these rasters in the right location.

Format
======

In CN-WS all input and output rasters are idrisi-rasters. The rasterdata is
stored as binary data in the .rst file and an .rdc file contains the metadata
as an ascii-file. Idrisi raster can be created and edited in several gis-packages.
We recommend the gdal-library to handle these files.

