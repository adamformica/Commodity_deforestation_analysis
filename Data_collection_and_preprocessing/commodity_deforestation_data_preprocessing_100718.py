import os
from qgis.utils import iface

# note: all spatial files possible to download
# and process with code below
# they were not uploaded to GitHub because of their
# large file size 

# download Hansen et al. 2013 tiles in R
# see source links in lossyear.txt

# store hansen tile filepaths and filenames in lists

inpath = "C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/Hansen_data/"
filepaths = []
for file in os.listdir(inpath):
	if file.endswith(".tif"):
		filepaths.append(os.path.join(inpath, file))

filenames = []
for filename in os.listdir(inpath):
	if filename.endswith(".tif"):
		filenames.append(filename[:-4])

# read and resample hansen tiles to 1km resolution

i = 1
for filepath in filepaths:
	iface.addRasterLayer(filepath,"hansen"+str(i))
	i = i + 1	

# if the input layer has a different resolution, 
# the values will be resampled with the nearest neighbor algorithm.

outpath = 'C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/Hansen_data_resample_nn/'
j = 1
k = 0
for filepath in filepaths:
	rlayer = QgsRasterLayer(filepath, "my layer")
	processing.run("qgis:rastercalculator", 
	{'EXPRESSION':'\"hansen'+str(j)+'@1\"',
    'LAYERS':'',
    'CELLSIZE':0.00833,
    'EXTENT':rlayer,
    'CRS':'EPSG:4326',
    'OUTPUT':outpath+filenames[k]+'_resample.tif'})
	j = j + 1
	k = k + 1
	
# reproject 1km hansen tiles to igh crs
	
inpath_resample = "C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/Hansen_data_resample_nn/"
filepaths_resample = []
for file in os.listdir(inpath_resample):
	if file.endswith(".tif"):
		filepaths_resample.append(os.path.join(inpath_resample, file))

outpath_reproject = 'C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/Hansen_data_reproject/'
m = 0
for filepath in filepaths_resample:
	processing.run("gdal:warpreproject",
	{'INPUT':filepath,
	'SOURCE_CRS':'EPSG:4326',
	'TARGET_CRS':'USER:100001',
	'RESAMPLING':0,
	'NODATA':None,
	'TARGET_RESOLUTION':None,
	'OPTIONS':'',
	'DATA_TYPE':5,
	'TARGET_EXTENT':None,
	'TARGET_EXTENT_CRS':None,
	'MULTITHREADING':True,
	'OUTPUT':outpath_reproject+filenames[m]+'_reproject.tif'})
	m = m + 1
	
# merge 1km reprojected hansen tiles
	
inpath_reproject = "C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/Hansen_data_reproject/"
filepaths_reproject = []
for file in os.listdir(inpath_reproject):
	if file.endswith(".tif"):
		filepaths_reproject.append(os.path.join(inpath_reproject, file))
		
processing.run("gdal:merge",
{'INPUT':filepaths_reproject,
'PCT':False,
'SEPARATE':False,
'NODATA_INPUT':None,
'NODATA_OUTPUT':None,
'OPTIONS':'',
'DATA_TYPE':5,
'OUTPUT':'C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/hansen_merge.tif'})

# assign igh projection to aau3445-data-s3.tif in R

# set values !=1 (commodity forest loss) to na in R and write output

# resample commodities mask to 1km resolution

commodities_mask_path = 'C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/commodities_mask.tif'
hansen_merge_path = 'C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/hansen_merge.tif'
hansen_merge = QgsRasterLayer(hansen_merge_path, "my layer")
processing.run("grass7:r.resample",
{'input':commodities_mask_path,
'output':'C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/commodities_mask_resample.tif',
'GRASS_REGION_PARAMETER':hansen_merge,
'GRASS_REGION_CELLSIZE_PARAMETER':934.736,
'GRASS_RASTER_FORMAT_OPT':'',
'GRASS_RASTER_FORMAT_META':''})

# apply commodities mask to hansen mosaic in r and write output

# apply zonal histogram to masked hansen mosaic

hansen_mask_path = 'C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/hansen_mask.tif'
countries_path = 'C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp'
processing.run("native:zonalhistogram",
{'INPUT_RASTER':hansen_mask_path,
'RASTER_BAND':1,
'INPUT_VECTOR':countries_path,
'COLUMN_PREFIX':'HISTO_',
'OUTPUT':'C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/hansen_mask_zonal.gpkg'})

# open hansen_mask_zonal.gpkg in QGIS
# select export -> save features as -> format csv
# save as "C:/Users/Sensonomic Admin/Desktop/Commodity_deforestation_analysis_data/hansen_mask_zonal.csv"
