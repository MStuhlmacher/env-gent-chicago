//GOAL: determine the amount and type of greenspace by census tract for 2000

//STEPS: 
//Calculate NDVI using Landsat imagery
//Threshold NDVI to isolate just green pixels (test 70th-90th)
//Group green pixels by their LU class
//Reduce regions using census tract boundaries and LU groupings
//Export census tracts for each NDVI threshold

//--Imports--//
var l5 = ee.ImageCollection("LANDSAT/LT05/C01/T1"),
    censusTracts = ee.FeatureCollection("users/mfstuhlmacher/EnvGentChicago/CensusTracts"), //from combinedCensusSHP.ipynb
    chicago = ee.FeatureCollection("users/mfstuhlmacher/EnvGentChicago/ChicagoMegatractBounds"), //from combinedCensusSHP.ipynb
    LU2000 = ee.FeatureCollection("users/mfstuhlmacher/EnvGentChicago/CMAPLandUse2000_MT"); //from CMAP

//--Calculate NDVI--//
var dateRange = ee.DateRange('2000-06-01', '2000-08-31');

print(l5.filterBounds(chicago).filterDate(dateRange)); 

var chiL5 = ee.Algorithms.Landsat.simpleComposite({
  collection: l5.filterBounds(chicago).filterDate(dateRange),
  //cloudScoreRange: 5,
  asFloat: true
});
Map.addLayer(chiL5,{bands: ['B3', 'B2', 'B1'],min: 0, max: 0.5, gamma: [0.95, 1.1, 1]},'chicago L5',false);
Map.addLayer(chicago,{},'chicago',false);

var ndvi = chiL5.normalizedDifference(['B4', 'B3']).rename('NDVI');
var ndvi_palette = {min:-0.1, max:1, palette: ['FFFFFF','CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901', '66A000','529400',
                                              '3E8601', '207401', '056201', '004C00', '023B01', '012E01', '011D01', '011301']};
Map.addLayer(ndvi,ndvi_palette,'chicago NDVI',false);

//--Threshold NDVI--//
//Define scale and crs using a single Landsat image because reducing collections to a single image returns the default projection of WGS84
// var fc = l5.filterDate(dateRange).filterBounds(chicago);
// print(fc);
var sc = ee.Image('LANDSAT/LT05/C01/T1/LT05_022031_20010710').select('B2').projection().nominalScale();
//print(sc,"scale");
var crs = ee.Image('LANDSAT/LT05/C01/T1/LT05_022031_20010710').select('B2').projection().crs(); 
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
print(ee.Number(pct80NDVI),'80th NDVI threshold');

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

var thresh70NDVI = ndvi.gte(ee.Number(pct70NDVI));
var thresh75NDVI = ndvi.gte(ee.Number(pct75NDVI));
var thresh80NDVI = ndvi.gte(ee.Number(pct80NDVI));
var thresh85NDVI = ndvi.gte(ee.Number(pct85NDVI));
var thresh90NDVI = ndvi.gte(ee.Number(pct90NDVI));

//--Sum greenspace by LU and census tract--//
//First, assign each pixel a land use value.
//Second, reduce the pixels to their census tract boundary but seperated out by LU codes

//Make an image out of the land use data
var rasterLU = LU2000
  .filter(ee.Filter.notNull(['LANDUSE_n']))
  .reduceToImage({
    properties: ['LANDUSE_n'],
    reducer: ee.Reducer.first()
}).rename('LANDUSE_n');
Map.addLayer(rasterLU,{},'land use raster',false);

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

//Add the LANDUSE_n as a band to the NDVI area image
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
    groupName:'LANDUSE_n'
  }),
  scale: sc,
  crs: crs,
  tileScale: 16});
//print(table70th.first(), '70th percentile reduction');

var table75th = NDVI75_LU.reduceRegions({
  collection:censusTracts,
  reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName:'LANDUSE_n'
  }),
  scale: sc,
  crs: crs,
  tileScale: 10});
  
var table80th = NDVI80_LU.reduceRegions({
  collection:censusTracts,
  reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName:'LANDUSE_n'
  }),
  scale: sc,
  crs: crs,
  tileScale: 10});
  
var table85th = NDVI85_LU.reduceRegions({
  collection:censusTracts,
  reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName:'LANDUSE_n'
  }),
  scale: sc,
  crs: crs,
  tileScale: 10});
  
var table90th = NDVI90_LU.reduceRegions({
  collection:censusTracts,
  reducer: ee.Reducer.sum().group({
    groupField: 1,
    groupName:'LANDUSE_n'
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
  var key = ee.String("LANDUSE_n").cat(ee.String(ee.Dictionary(cur).get('LANDUSE_n'))); //get the LANDUSE_n class
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
  description: "LU2001_NDVI70thPer_L5",
  folder:"EnvGentChicago",
  fileNamePrefix: "LU2001_NDVI70thPer_L5",
  fileFormat:  "CSV",
  selectors: ["cluster_id","LANDUSE_n1110","LANDUSE_n1120","LANDUSE_n1130","LANDUSE_n1140","LANDUSE_n1211","LANDUSE_n1212",
  "LANDUSE_n1221","LANDUSE_n1222","LANDUSE_n1223","LANDUSE_n1231","LANDUSE_n1232","LANDUSE_n1240","LANDUSE_n1250","LANDUSE_n1310",
  "LANDUSE_n1320","LANDUSE_n1330","LANDUSE_n1340","LANDUSE_n1350","LANDUSE_n1360","LANDUSE_n1370","LANDUSE_n1410","LANDUSE_n1420",
  "LANDUSE_n1430","LANDUSE_n1440","LANDUSE_n1511","LANDUSE_n1512","LANDUSE_n1520","LANDUSE_n1530","LANDUSE_n1540","LANDUSE_n1550",
  "LANDUSE_n1560","LANDUSE_n2100","LANDUSE_n2200","LANDUSE_n3100","LANDUSE_n3200","LANDUSE_n3300","LANDUSE_n3400","LANDUSE_n3500",
  "LANDUSE_n3600","LANDUSE_n4110","LANDUSE_n4120","LANDUSE_n4210","LANDUSE_n4220","LANDUSE_n4300","LANDUSE_n5100","LANDUSE_n5200",
  "LANDUSE_n5300"]
});

Export.table.toDrive({
  collection: output75th,
  description: "LU2001_NDVI75thPer_L5",
  folder:"EnvGentChicago",
  fileNamePrefix: "LU2001_NDVI75thPer_L5",
  fileFormat:  "CSV",
  selectors: ["cluster_id","LANDUSE_n1110","LANDUSE_n1120","LANDUSE_n1130","LANDUSE_n1140","LANDUSE_n1211","LANDUSE_n1212",
  "LANDUSE_n1221","LANDUSE_n1222","LANDUSE_n1223","LANDUSE_n1231","LANDUSE_n1232","LANDUSE_n1240","LANDUSE_n1250","LANDUSE_n1310",
  "LANDUSE_n1320","LANDUSE_n1330","LANDUSE_n1340","LANDUSE_n1350","LANDUSE_n1360","LANDUSE_n1370","LANDUSE_n1410","LANDUSE_n1420",
  "LANDUSE_n1430","LANDUSE_n1440","LANDUSE_n1511","LANDUSE_n1512","LANDUSE_n1520","LANDUSE_n1530","LANDUSE_n1540","LANDUSE_n1550",
  "LANDUSE_n1560","LANDUSE_n2100","LANDUSE_n2200","LANDUSE_n3100","LANDUSE_n3200","LANDUSE_n3300","LANDUSE_n3400","LANDUSE_n3500",
  "LANDUSE_n3600","LANDUSE_n4110","LANDUSE_n4120","LANDUSE_n4210","LANDUSE_n4220","LANDUSE_n4300","LANDUSE_n5100","LANDUSE_n5200",
  "LANDUSE_n5300"]
});

Export.table.toDrive({
  collection: output80th,
  description: "LU2001_NDVI80thPer_L5",
  folder:"EnvGentChicago",
  fileNamePrefix: "LU2001_NDVI80thPer_L5",
  fileFormat:  "CSV",
  selectors: ["cluster_id","LANDUSE_n1110","LANDUSE_n1120","LANDUSE_n1130","LANDUSE_n1140","LANDUSE_n1211","LANDUSE_n1212",
  "LANDUSE_n1221","LANDUSE_n1222","LANDUSE_n1223","LANDUSE_n1231","LANDUSE_n1232","LANDUSE_n1240","LANDUSE_n1250","LANDUSE_n1310",
  "LANDUSE_n1320","LANDUSE_n1330","LANDUSE_n1340","LANDUSE_n1350","LANDUSE_n1360","LANDUSE_n1370","LANDUSE_n1410","LANDUSE_n1420",
  "LANDUSE_n1430","LANDUSE_n1440","LANDUSE_n1511","LANDUSE_n1512","LANDUSE_n1520","LANDUSE_n1530","LANDUSE_n1540","LANDUSE_n1550",
  "LANDUSE_n1560","LANDUSE_n2100","LANDUSE_n2200","LANDUSE_n3100","LANDUSE_n3200","LANDUSE_n3300","LANDUSE_n3400","LANDUSE_n3500",
  "LANDUSE_n3600","LANDUSE_n4110","LANDUSE_n4120","LANDUSE_n4210","LANDUSE_n4220","LANDUSE_n4300","LANDUSE_n5100","LANDUSE_n5200",
  "LANDUSE_n5300"]
});

Export.table.toDrive({
  collection: output85th,
  description: "LU2001_NDVI85thPer_L5",
  folder:"EnvGentChicago",
  fileNamePrefix: "LU2001_NDVI85thPer_L5",
  fileFormat:  "CSV",
  selectors: ["cluster_id","LANDUSE_n1110","LANDUSE_n1120","LANDUSE_n1130","LANDUSE_n1140","LANDUSE_n1211","LANDUSE_n1212",
  "LANDUSE_n1221","LANDUSE_n1222","LANDUSE_n1223","LANDUSE_n1231","LANDUSE_n1232","LANDUSE_n1240","LANDUSE_n1250","LANDUSE_n1310",
  "LANDUSE_n1320","LANDUSE_n1330","LANDUSE_n1340","LANDUSE_n1350","LANDUSE_n1360","LANDUSE_n1370","LANDUSE_n1410","LANDUSE_n1420",
  "LANDUSE_n1430","LANDUSE_n1440","LANDUSE_n1511","LANDUSE_n1512","LANDUSE_n1520","LANDUSE_n1530","LANDUSE_n1540","LANDUSE_n1550",
  "LANDUSE_n1560","LANDUSE_n2100","LANDUSE_n2200","LANDUSE_n3100","LANDUSE_n3200","LANDUSE_n3300","LANDUSE_n3400","LANDUSE_n3500",
  "LANDUSE_n3600","LANDUSE_n4110","LANDUSE_n4120","LANDUSE_n4210","LANDUSE_n4220","LANDUSE_n4300","LANDUSE_n5100","LANDUSE_n5200",
  "LANDUSE_n5300"]
});

Export.table.toDrive({
  collection: output90th,
  description: "LU2001_NDVI90thPer_L5",
  folder:"EnvGentChicago",
  fileNamePrefix: "LU2001_NDVI90thPer_L5",
  fileFormat:  "CSV",
  selectors: ["cluster_id","LANDUSE_n1110","LANDUSE_n1120","LANDUSE_n1130","LANDUSE_n1140","LANDUSE_n1211","LANDUSE_n1212",
  "LANDUSE_n1221","LANDUSE_n1222","LANDUSE_n1223","LANDUSE_n1231","LANDUSE_n1232","LANDUSE_n1240","LANDUSE_n1250","LANDUSE_n1310",
  "LANDUSE_n1320","LANDUSE_n1330","LANDUSE_n1340","LANDUSE_n1350","LANDUSE_n1360","LANDUSE_n1370","LANDUSE_n1410","LANDUSE_n1420",
  "LANDUSE_n1430","LANDUSE_n1440","LANDUSE_n1511","LANDUSE_n1512","LANDUSE_n1520","LANDUSE_n1530","LANDUSE_n1540","LANDUSE_n1550",
  "LANDUSE_n1560","LANDUSE_n2100","LANDUSE_n2200","LANDUSE_n3100","LANDUSE_n3200","LANDUSE_n3300","LANDUSE_n3400","LANDUSE_n3500",
  "LANDUSE_n3600","LANDUSE_n4110","LANDUSE_n4120","LANDUSE_n4210","LANDUSE_n4220","LANDUSE_n4300","LANDUSE_n5100","LANDUSE_n5200",
  "LANDUSE_n5300"]
});
