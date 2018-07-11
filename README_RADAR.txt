This is an adapted version of the CNWS model.

This version is specifically designed to use rainfall radar data as rainfall input for the model.
Only the console version of CNWS can be used to model with rainfall radar data.
An example inifile is provided here. In this file Use Radar rain should be set to 1 when you want to use radar data
and set to 0 when you want to use rain gauge data as rainfall input. Timestep Rain Radar data which is the timestep
between the radar images also needs to be provided in seconds and Radar directory, the directory in which the radar images
can be found, also need to be provided. Radar images need to be in chronological order and in .rst or Idrisi raster format.

Two versions of this adapted model have been created.
The first version found in the main map is the version for rainfall radar data which gives the rainfall rates 
at a particular moment in time.
The second version, which can be found in the mat CNWS_cumul, uses rainfall radar data which gives a cumulative rainfall 
over a particular time period.

Run_radar.py can be used with a python 3.6 environment with GDAL installed to convert radar images in HDF5 (.hdf, .h5) format to Idrisi raster (.rst).
The python script also clips the data so that only the necessary input around the catchment being modelled is contained in the .rst file.
The script also rescales the data to the resolution of the dtm used in the CNWS model.
The script needs as input a folder with the HDF5 files (name) and the dtm used in the CNWS model for the catchment being modelled (dtm).