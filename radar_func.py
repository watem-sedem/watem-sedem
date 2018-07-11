import time
import subprocess
import glob
import os
import shutil
from osgeo import gdal, gdalconst

def func(a,b):
    start_time = time.time()
    
    timestep = 5*60
    path = 'D:/Documents/1e_master_Geography_GIS/Thesis/Matlab rainfall/Radar_preparation/' + a
    DTM = 'D:/Documents/1e_master_Geography_GIS/Thesis/CNWS_runs/Radar/' + b
    
    os.chdir(path)
    filenames = []
    for file in glob.glob('*.hdf'):
        filenames.append(file)
    
    newnames = []
    timename = timestep
    for i in range(0,len(filenames)) :
        if i == 0 :
            newnames.append(0)
        else :
            newnames.append(timename)
            timename = timename + timestep
    
    if not os.path.exists(path + '/tmp'):
        os.makedirs(path + '/tmp')
    
    if not os.path.exists(path + '/RadarInput'):
        os.makedirs(path + '/RadarInput')
    
    
    # We want a section of source that matches this:
    match_ds = gdal.Open(DTM, gdalconst.GA_ReadOnly)
    match_proj = match_ds.GetProjection()
    match_geotrans = match_ds.GetGeoTransform()
    wide = match_ds.RasterXSize
    high = match_ds.RasterYSize
    
    
    for i in range(0,len(filenames)) :
    
    
        cmd_str= 'gdalwarp -overwrite -s_srs "+proj=aeqd +lat_0=49.9135 +lon_0=5.5044 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs" -t_srs "EPSG:31370" -of "GTiff" "%s" "%s"' %(path + '/' + filenames[i], path + '/tmp/' + str(newnames [i]) + '.tif')
        subprocess.check_call(cmd_str)
    
        # Source
        src_filename = path + '/tmp/' + str(newnames [i]) + '.tif'
        src = gdal.Open(src_filename, gdalconst.GA_ReadOnly)
        src_proj = src.GetProjection()
        src_geotrans = src.GetGeoTransform()
    
        # Output / destination
        dst_filename = path + '/tmp/' + str(newnames [i]) + 'clip.tif'
        dst = gdal.GetDriverByName('GTiff').Create(dst_filename, wide, high, 1, gdalconst.GDT_Float32)
        dst.SetGeoTransform( match_geotrans )
        dst.SetProjection( match_proj)
    
        # Do the work
        gdal.ReprojectImage(src, dst, src_proj, match_proj, gdalconst.GRA_NearestNeighbour)
    
        del src
        del dst # Flush
    
        cmd_str = 'gdalwarp -overwrite -s_srs "EPSG:31370" -t_srs "EPSG:31370" -of "RST" "%s" "%s"' % (path + '/tmp/' + str(newnames[i]) + 'clip.tif', path + '/RadarInput/' + str(newnames[i]) + '.rst')
        subprocess.check_call(cmd_str)
    
    del match_ds
    
    #shutil.rmtree(path + '/tmp')
    return

