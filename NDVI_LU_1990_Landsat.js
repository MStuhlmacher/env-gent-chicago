//GOAL: determine the amount and type of greenspace by census tract for 1990

//STEPS: 
//Calculate NDVI using 1990 Landsat imagery
//Threshold NDVI to isolate just green pixels (test 70th-90th)
//Group green pixels by their LU class
//Reduce regions using census tract boundaries and LU groupings
//Export census tracts for each NDVI threshold

//--Imports--//
var l5 = ee.ImageCollection("LANDSAT/LT05/C01/T1"),
    censusTracts = ee.FeatureCollection("users/mfstuhlmacher/EnvGentChicago/CensusTracts"), //from combinedCensusSHP.ipynb
    chicago = ee.FeatureCollection("users/mfstuhlmacher/EnvGentChicago/ChicagoMegatractBounds"), //from combinedCensusSHP.ipynb
    LU1990 = ee.FeatureCollection("users/mfstuhlmacher/EnvGentChicago/CMAPLandUse1990_MT"); //from CMAP

//--Calculate NDVI--//
var dateRange = ee.DateRange('1990-06-01', '1990-08-31'); //metrological summer

print(l5.filterBounds(chicago).filterDate(dateRange)); 

var chiL5 = ee.Algorithms.Landsat.simpleComposite({
  collection: l5.filterBounds(chicago).filterDate(dateRange),
  asFloat: true
});
Map.addLayer(chiL5,{bands: ['B3', 'B2', 'B1']},'chicago L5',false);

var ndvi = chiL5.normalizedDifference(['B4', 'B3']).rename('NDVI');
Map.addLayer(ndvi,{},'chicago NDVI',false);

//--Threshold NDVI--//
//Define scale and crs using a single Landsat image because reducing collections to a single image returns the default projection of WGS84
// var fc = l5.filterDate(dateRange).filterBounds(chicago);
// print(fc);
var sc = ee.Image('LANDSAT/LT05/C01/T1/LT05_022031_19900712').select('B2').projection().nominalScale();
//print(sc,"scale");
var crs = ee.Image('LANDSAT/LT05/C01/T1/LT05_022031_19900712').select('B2').projection().crs(); 
//print(crs,"crs");

//70th percentile
var pct70NDVI = ndvi.reduceRegion({
  reducer: ee.Reducer.percentile([70]), 
  geometry: chicago, 
  scale: sc,
  maxPixels: 1e9
}).get('NDVI');
//print(ee.Number(pct70NDVI),'70th NDVI threshold');

//75th percentile
var pct75NDVI = ndvi.reduceRegion({
  reducer: ee.Reducer.percentile([75]), 
  geometry: chicago, 
  scale: sc,
  maxPixels: 1e9
}).get('NDVI');

//80th percentile
var pct80NDVI = ndvi.reduceRegion({
  reducer: ee.Reducer.percentile([80]), 
  geometry: chicago, 
  scale: sc,
  maxPixels: 1e9
}).get('NDVI');

//85th percentile
var pct85NDVI = ndvi.reduceRegion({
  reducer: ee.Reducer.percentile([85]), 
  geometry: chicago, 
  scale: sc,
  maxPixels: 1e9
}).get('NDVI');

//90th percentile
var pct90NDVI = ndvi.reduceRegion({
  reducer: ee.Reducer.percentile([90]), 
  geometry: chicago, 
  scale: sc,
  maxPixels: 1e9
}).get('NDVI');
//print(ee.Number(pct90NDVI),'90th NDVI threshold');

var thresh70NDVI = ndvi.gte(ee.Number(pct70NDVI));
var thresh75NDVI = ndvi.gte(ee.Number(pct75NDVI));
var thresh80NDVI = ndvi.gte(ee.Number(pct80NDVI));
var thresh85NDVI = ndvi.gte(ee.Number(pct85NDVI));
var thresh90NDVI = ndvi.gte(ee.Number(pct90NDVI));

//--Sum greenspace by LU and census tract--//
//First, assign each pixel a land use value.
//Second, reduce the pixels to their census tract boundary but seperated out by LU codes

//Make an image out of the land use data
var rasterLU = LU1990
  .filter(ee.Filter.notNull(['LANDUSE']))
  .reduceToImage({
    properties: ['LANDUSE'],
    reducer: ee.Reducer.first()
}).rename('LANDUSE');
Map.addLayer(rasterLU,{min: 1110, max: 5300},'land use raster',false);

Map.addLayer(thresh70NDVI.mask(thresh70NDVI),{},'NDVI 70th percentile threshold',false);
Map.addLayer(thresh75NDVI.mask(thresh75NDVI),{},'NDVI 75th percentile threshold',false);
Map.addLayer(thresh80NDVI.mask(thresh80NDVI),{},'NDVI 80th percentile threshold',false);
Map.addLayer(thresh85NDVI.mask(thresh85NDVI),{},'NDVI 85th percentile threshold',false);
Map.addLayer(thresh90NDVI.mask(thresh90NDVI),{},'NDVI 90th percentile threshold',false);

//Multiply NDVI by area
var NDVI70area = thresh70NDVI.multiply(ee.Image.pixelArea());
var NDVI75area = thresh75NDVI.multiply(ee.Image.pixelArea());
var NDVI80area = thresh80NDVI.multiply(ee.Image.pixelArea());
var NDVI85area = thresh85NDVI.multiply(ee.Image.pixelArea());
var NDVI90area = thresh90NDVI.multiply(ee.Image.pixelArea());

//Add the landuse as a band to the NDVI area image
var NDVI70_LU = NDVI70area.addBands(rasterLU);
Map.addLayer(NDVI70_LU,{},"NDVI 70th percentile & land use with area",false);

var NDVI75_LU = NDVI75area.addBands(rasterLU);
Map.addLayer(NDVI75_LU,{},"NDVI 75th percentile & land use with area",false);

var NDVI80_LU = NDVI80area.addBands(rasterLU);
Map.addLayer(NDVI80_LU,{},"NDVI 80th percentile & land use with area",false);

var NDVI85_LU = NDVI85area.addBands(rasterLU);
Map.addLayer(NDVI85_LU,{},"NDVI 85th percentile & land use with area",false);

var NDVI90_LU = NDVI90area.addBands(rasterLU);
Map.addLayer(NDVI90_LU,{},"NDVI 90th percentile & land use with area",false);

//--Reduce the image using grouping--//
var table70th = NDVI70_LU.reduceRegions({
  collection:censusTracts,
  reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName:'LANDUSE'
  }),
  scale: sc,
  crs: crs,
  tileScale: 16});
print(table70th.first(), '70th percentile reduction');

var table75th = NDVI75_LU.reduceRegions({
  collection:censusTracts,
  reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName:'LANDUSE'
  }),
  scale: sc,
  crs: crs,
  tileScale: 10});
  
var table80th = NDVI80_LU.reduceRegions({
  collection:censusTracts,
  reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName:'LANDUSE'
  }),
  scale: sc,
  crs: crs,
  tileScale: 10});
  
var table85th = NDVI85_LU.reduceRegions({
  collection:censusTracts,
  reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName:'LANDUSE'
  }),
  scale: sc,
  crs: crs,
  tileScale: 10});
  
var table90th = NDVI90_LU.reduceRegions({
  collection:censusTracts,
  reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName:'LANDUSE'
  }),
  scale: sc,
  crs: crs,
  tileScale: 10});

//--Clean up table for export--//
//Remove geometry
var table70th_2 = table70th.map(function(feature) {
  return ee.Feature(feature.select([".*"], null, false));
});
var table75th_2 = table75th.map(function(feature) {
  return ee.Feature(feature.select([".*"], null, false));
});
var table80th_2 = table80th.map(function(feature) {
  return ee.Feature(feature.select([".*"], null, false));
});
var table85th_2 = table85th.map(function(feature) {
  return ee.Feature(feature.select([".*"], null, false));
});
var table90th_2 = table90th.map(function(feature) {
  return ee.Feature(feature.select([".*"], null, false));
});

//Subset columns
var table70th_3 = table70th_2.select(["cluster_id", "groups"]);
var table75th_3 = table75th_2.select(["cluster_id", "groups"]);
var table80th_3 = table80th_2.select(["cluster_id", "groups"]);
var table85th_3 = table85th_2.select(["cluster_id", "groups"]);
var table90th_3 = table90th_2.select(["cluster_id", "groups"]);

//Convert the list of dictionaries with the summed areas into a single dictionary
function toDictionary(cur, prev){
  var key = ee.String("LANDUSE").cat(ee.String(ee.Dictionary(cur).get('LANDUSE'))); //get the landuse class
  var value = ee.Dictionary(cur).get('sum'); //get the sum of the area 
  return ee.Dictionary(prev).set(key, value); //return as a property
}
 
var output70th = table70th_3.map(function(feature){
  var greenAreas = ee.List(feature.get("groups")).iterate(toDictionary, ee.Dictionary());
  return ee.Feature(null, ee.Dictionary(greenAreas).set("cluster_id",feature.get("cluster_id")));
});
//print(output70th, "output 70th percentile NDVI");
var output75th = table75th_3.map(function(feature){
  var greenAreas = ee.List(feature.get("groups")).iterate(toDictionary, ee.Dictionary());
  return ee.Feature(null, ee.Dictionary(greenAreas).set("cluster_id",feature.get("cluster_id")));
});
var output80th = table80th_3.map(function(feature){
  var greenAreas = ee.List(feature.get("groups")).iterate(toDictionary, ee.Dictionary());
  return ee.Feature(null, ee.Dictionary(greenAreas).set("cluster_id",feature.get("cluster_id")));
});
var output85th = table85th_3.map(function(feature){
  var greenAreas = ee.List(feature.get("groups")).iterate(toDictionary, ee.Dictionary());
  return ee.Feature(null, ee.Dictionary(greenAreas).set("cluster_id",feature.get("cluster_id")));
});
var output90th = table90th_3.map(function(feature){
  var greenAreas = ee.List(feature.get("groups")).iterate(toDictionary, ee.Dictionary());
  return ee.Feature(null, ee.Dictionary(greenAreas).set("cluster_id",feature.get("cluster_id")));
});

//--Export--//
Export.table.toDrive({
  collection: output70th,
  description: "LU1990_NDVI70thPer_L5",
  folder:"EnvGentChicago",
  fileNamePrefix: "LU1990_NDVI70thPer_L5",
  selectors:  ["cluster_id","LANDUSE1110","LANDUSE1120","LANDUSE1130","LANDUSE1140","LANDUSE1210","LANDUSE1220","LANDUSE1230",
  "LANDUSE1241","LANDUSE1242","LANDUSE1243","LANDUSE1250","LANDUSE1260","LANDUSE1311","LANDUSE1312","LANDUSE1313","LANDUSE1320",
  "LANDUSE1330","LANDUSE1340","LANDUSE1360","LANDUSE1370","LANDUSE1380","LANDUSE1390","LANDUSE1410","LANDUSE1420","LANDUSE1430",
  "LANDUSE1440","LANDUSE1510","LANDUSE1520","LANDUSE1530","LANDUSE1540","LANDUSE1550","LANDUSE1560","LANDUSE2000","LANDUSE3110",
  "LANDUSE3120","LANDUSE3130","LANDUSE3210","LANDUSE3220","LANDUSE4110","LANDUSE4120","LANDUSE4210","LANDUSE4220","LANDUSE4300",
  "LANDUSE5100","LANDUSE5200","LANDUSE5300","LANDUSE9999"],
  fileFormat:  "CSV"});

Export.table.toDrive({
  collection: output75th,
  description: "LU1990_NDVI75thPer_L5",
  folder:"EnvGentChicago",
  fileNamePrefix: "LU1990_NDVI75thPer_L5",
  selectors:  ["cluster_id","LANDUSE1110","LANDUSE1120","LANDUSE1130","LANDUSE1140","LANDUSE1210","LANDUSE1220","LANDUSE1230",
  "LANDUSE1241","LANDUSE1242","LANDUSE1243","LANDUSE1250","LANDUSE1260","LANDUSE1311","LANDUSE1312","LANDUSE1313","LANDUSE1320",
  "LANDUSE1330","LANDUSE1340","LANDUSE1360","LANDUSE1370","LANDUSE1380","LANDUSE1390","LANDUSE1410","LANDUSE1420","LANDUSE1430",
  "LANDUSE1440","LANDUSE1510","LANDUSE1520","LANDUSE1530","LANDUSE1540","LANDUSE1550","LANDUSE1560","LANDUSE2000","LANDUSE3110",
  "LANDUSE3120","LANDUSE3130","LANDUSE3210","LANDUSE3220","LANDUSE4110","LANDUSE4120","LANDUSE4210","LANDUSE4220","LANDUSE4300",
  "LANDUSE5100","LANDUSE5200","LANDUSE5300","LANDUSE9999"],
  fileFormat:  "CSV"});

Export.table.toDrive({
  collection: output80th,
  description: "LU1990_NDVI80thPer_L5",
  folder:"EnvGentChicago",
  fileNamePrefix: "LU1990_NDVI80thPer_L5",
  selectors:  ["cluster_id","LANDUSE1110","LANDUSE1120","LANDUSE1130","LANDUSE1140","LANDUSE1210","LANDUSE1220","LANDUSE1230",
  "LANDUSE1241","LANDUSE1242","LANDUSE1243","LANDUSE1250","LANDUSE1260","LANDUSE1311","LANDUSE1312","LANDUSE1313","LANDUSE1320",
  "LANDUSE1330","LANDUSE1340","LANDUSE1360","LANDUSE1370","LANDUSE1380","LANDUSE1390","LANDUSE1410","LANDUSE1420","LANDUSE1430",
  "LANDUSE1440","LANDUSE1510","LANDUSE1520","LANDUSE1530","LANDUSE1540","LANDUSE1550","LANDUSE1560","LANDUSE2000","LANDUSE3110",
  "LANDUSE3120","LANDUSE3130","LANDUSE3210","LANDUSE3220","LANDUSE4110","LANDUSE4120","LANDUSE4210","LANDUSE4220","LANDUSE4300",
  "LANDUSE5100","LANDUSE5200","LANDUSE5300","LANDUSE9999"],
  fileFormat:  "CSV"});

Export.table.toDrive({
  collection: output85th,
  description: "LU1990_NDVI85thPer_L5",
  folder:"EnvGentChicago",
  fileNamePrefix: "LU1990_NDVI85thPer_L5",
  selectors:  ["cluster_id","LANDUSE1110","LANDUSE1120","LANDUSE1130","LANDUSE1140","LANDUSE1210","LANDUSE1220","LANDUSE1230",
  "LANDUSE1241","LANDUSE1242","LANDUSE1243","LANDUSE1250","LANDUSE1260","LANDUSE1311","LANDUSE1312","LANDUSE1313","LANDUSE1320",
  "LANDUSE1330","LANDUSE1340","LANDUSE1360","LANDUSE1370","LANDUSE1380","LANDUSE1390","LANDUSE1410","LANDUSE1420","LANDUSE1430",
  "LANDUSE1440","LANDUSE1510","LANDUSE1520","LANDUSE1530","LANDUSE1540","LANDUSE1550","LANDUSE1560","LANDUSE2000","LANDUSE3110",
  "LANDUSE3120","LANDUSE3130","LANDUSE3210","LANDUSE3220","LANDUSE4110","LANDUSE4120","LANDUSE4210","LANDUSE4220","LANDUSE4300",
  "LANDUSE5100","LANDUSE5200","LANDUSE5300","LANDUSE9999"],
  fileFormat:  "CSV"});
  
Export.table.toDrive({
  collection: output90th,
  description: "LU1990_NDVI90thPer_L5",
  folder:"EnvGentChicago",
  fileNamePrefix: "LU1990_NDVI90thPer_L5",
  selectors:  ["cluster_id","LANDUSE1110","LANDUSE1120","LANDUSE1130","LANDUSE1140","LANDUSE1210","LANDUSE1220","LANDUSE1230",
  "LANDUSE1241","LANDUSE1242","LANDUSE1243","LANDUSE1250","LANDUSE1260","LANDUSE1311","LANDUSE1312","LANDUSE1313","LANDUSE1320",
  "LANDUSE1330","LANDUSE1340","LANDUSE1360","LANDUSE1370","LANDUSE1380","LANDUSE1390","LANDUSE1410","LANDUSE1420","LANDUSE1430",
  "LANDUSE1440","LANDUSE1510","LANDUSE1520","LANDUSE1530","LANDUSE1540","LANDUSE1550","LANDUSE1560","LANDUSE2000","LANDUSE3110",
  "LANDUSE3120","LANDUSE3130","LANDUSE3210","LANDUSE3220","LANDUSE4110","LANDUSE4120","LANDUSE4210","LANDUSE4220","LANDUSE4300",
  "LANDUSE5100","LANDUSE5200","LANDUSE5300","LANDUSE9999"],
  fileFormat:  "CSV"});