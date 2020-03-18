
#===========================================================================

# data geometry object for all functions except for the FROM_GeoJson_Schema

js_data = '{ "type": "MultiPolygon", "coordinates": [
  [[[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0], [102.0, 2.0]]],
  [[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]],
   [[100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2]]]
  ]
}'


# data for the 'FROM_GeoJson_Schema' function

schema_str = '{
                "name" : "example_name",
                "location" : {
                    "type" : "Point",
                    "coordinates" : [ -120.24, 39.21 ]
                  }
               }'


#===========================================================================


# data (geometry collection) -----------------------


Point = c(100, 1.01)

MultiPoint = list(c(100, 1.01), c(200, 2.01))

MultiLineString = list(list(c(100, 1.01), c(200, 2.01)),

                       list(c(100, 1.01), c(200, 2.01)))

LineString = list(c(100, 1.01), c(200, 2.01))

MultiLineString = list(list(c(100, 1.01), c(200, 2.01)),

                       list(c(100, 1.01), c(200, 2.01)))

Polygon = list(list(c(100, 1.01), c(200, 2.01), c(100, 1.01), c(200, 2.01)))

Polygon = list(list(c(100, 1.01), c(200, 2.01), c(100, 1.01), c(200, 2.01)),

               list(c(100, 1.01), c(200, 2.01), c(100, 1.01), c(200, 2.01)))

MultiPolygon = list(list(list(c(100, 1.0), c(200, 2.0), c(100, 1.0), c(200, 2.0))),

                    list(list(c(100, 1.0), c(200, 2.0), c(100, 1.0), c(200, 2.0)),

                         list(c(100, 1.0), c(200, 2.0), c(100, 1.0), c(200, 2.0))))


geometry_collection_dat = list(Point = Point, MultiPoint = MultiPoint,

                               MultiLineString = MultiLineString, LineString = LineString,

                               MultiLineString = MultiLineString, Polygon = Polygon,

                               Polygon = Polygon, MultiPolygon = MultiPolygon)



# data (Feature) -------------------------------------------


# Empty 'properties' list

feature_dat1 = list(id = 1, bbox = c(1,2,3,4), geometry = list(Point = c(100, 1.01)),

                    properties = list())


# Nested 'properties' list

feature_dat2 = list(id = "1", bbox = c(1,2,3,4), geometry = list(Point = c(100, 1.01)),

                    properties = list(prop0 = 'value0',

                                      prop1 = 0.0, vec = c(1,2,3), lst = list(a = 1, d = 2)))


# data (FeatureCollection) -----------------------------------


feature_col_dat = list(bbox = c(-10.01, -10.01, 10.01, 10.01),

                       features = list(Feature = feature_dat1, Feature = feature_dat2))


#===========================
context('geojson functions')
#===========================


#----------------------
# FROM_GeoJson function
#----------------------



testthat::test_that("in case that the 'url_file_string' parameter is not a character string it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( FROM_GeoJson(url_file_string = mt) )
})


testthat::test_that("in case that the 'Flatten_Coords' parameter is not a boolean it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( FROM_GeoJson(url_file_string = js_data, Flatten_Coords = mt) )
})


testthat::test_that("in case that the 'Average_Coordinates' parameter is not a boolean it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( FROM_GeoJson(url_file_string = js_data, Average_Coordinates = mt) )
})



testthat::test_that("in case that the 'To_List' parameter is not a boolean it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( FROM_GeoJson(url_file_string = js_data, To_List = mt) )
})


testthat::test_that("in case that the 'url_file_string' parameter is a geojson character string it returns a named list", {

  tmp = FROM_GeoJson(url_file_string = js_data)

  nams = sum(names(tmp) %in% c("type", "coordinates")) == 2

  obj = tmp$type == "MultiPolygon"

  len = length(tmp$coordinates) == 2

  testthat::expect_true(sum(c(nams, obj, len)) == 3)
})



testthat::test_that("in case that the 'url_file_string' parameter is a character string path to a file it returns a named list", {

  PATH = paste0(getwd(), path.expand("/file_data/Feature.geojson"))

  tmp = FROM_GeoJson(url_file_string = PATH)

  nams = sum(names(tmp) %in% c("bbox", "geometry", "id", "properties", "type")) == 5

  obj = tmp$type == "Feature"

  len = length(tmp$geometry$coordinates) == 2

  testthat::expect_true(sum(c(nams, obj, len)) == 3)
})


#-----------------------------
# FROM_GeoJson_Schema function
#-----------------------------



testthat::test_that("in case that the 'url_file_string' parameter is not a character string it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( FROM_GeoJson_Schema(url_file_string = mt) )
})


testthat::test_that("in case that the 'geometry_name' parameter is not a character string it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( FROM_GeoJson_Schema(url_file_string = schema_str, geometry_name = mt) )
})


testthat::test_that("in case that the 'Average_Coordinates' parameter is not a boolean it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( FROM_GeoJson_Schema(url_file_string = schema_str, Average_Coordinates = mt) )
})



testthat::test_that("in case that the 'To_List' parameter is not a boolean it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( FROM_GeoJson_Schema(url_file_string = schema_str, To_List = mt) )
})


testthat::test_that("in case that the 'url_file_string' parameter is a geojson character string it returns a named list", {

  tmp = FROM_GeoJson_Schema(url_file_string = schema_str, geometry_name = "location")

  nams = sum(names(tmp) %in% c("location", "name")) == 2

  obj = tmp$location$type == "Point"

  len = length(tmp$location$coordinates) == 2

  testthat::expect_true(sum(c(nams, obj, len)) == 3)
})


testthat::test_that("in case that the 'url_file_string' parameter is a character string path to a file it returns a named list", {

  PATH = paste0(getwd(), path.expand("/file_data/Schema_data.geojson"))

  tmp = FROM_GeoJson_Schema(url_file_string = PATH, geometry_name = "geometry")

  nams = sum(names(tmp) %in% c("_id", "geometry", "name")) == 3

  obj = tmp$geometry$type == "Polygon"

  len = sum(dim(tmp$geometry$coordinates) == c(4649, 2)) == 2

  testthat::expect_true(sum(c(nams, obj, len)) == 3)
})


#---------------------------
# Dump_From_GeoJson function
#---------------------------


testthat::test_that("in case that the 'url_file' parameter is not a character string it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( Dump_From_GeoJson(url_file = mt) )
})


testthat::test_that("in case that the 'url_file' parameter is a character string BUT not a path to a file or a valid url it returns an error", {

  PATH = 'INVALID'

  testthat::expect_error( Dump_From_GeoJson(url_file = PATH) )
})


testthat::test_that("in case that the 'url_file' parameter is a valid path to a file it returns a ", {

  PATH = paste0(getwd(), path.expand("/file_data/Feature.geojson"))

  tmp = Dump_From_GeoJson(url_file = PATH)

  tmp_OUT = FROM_GeoJson(url_file_string = tmp)

  nams = sum(names(tmp_OUT) %in% c("bbox", "geometry", "id", "properties", "type")) == 5

  obj = tmp_OUT$type == "Feature"

  len = length(tmp_OUT$geometry$coordinates) == 2

  testthat::expect_true(sum(c(nams, obj, len)) == 3)
})



#--------------------
# TO_GeoJson R6 class
#--------------------


#==================================== 'Point'

testthat::test_that("in case that the 'data' parameter is not a numeric vector it returns an error", {

  mt = matrix(runif(10), 2, 5)

  init = TO_GeoJson$new()

  testthat::expect_error( init$Point(mt, stringify = TRUE) )
})



testthat::test_that("in case that the 'stringify' parameter is not a boolean it returns an error", {

  init = TO_GeoJson$new()

  testthat::expect_error( init$Point(c(100, 200), stringify = 'TRUE') )
})



testthat::test_that("in case that both parameters are valid it returns a GeoJson object", {

  init = TO_GeoJson$new()

  res = init$Point(c(100, 200), stringify = TRUE)

  nams = sum(names(res) %in% c("json_dump", "type", "coordinates")) == 3

  TYPE = res$type == "Point"

  coords = is.vector(res$coordinates) && !length(res$coordinates) == 0

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})



#==================================== 'MultiPoint'

testthat::test_that("in case that the 'data' parameter is not a numeric list it returns an error", {

  mt = matrix(runif(10), 2, 5)

  init = TO_GeoJson$new()

  testthat::expect_error( init$MultiPoint(mt, stringify = TRUE) )
})



testthat::test_that("in case that the 'stringify' parameter is not a boolean it returns an error", {

  init = TO_GeoJson$new()

  testthat::expect_error( init$MultiPoint(list(c(100, 1.01), c(200, 2.01)), stringify = 'TRUE') )
})



testthat::test_that("in case that both parameters are valid it returns a GeoJson object", {

  init = TO_GeoJson$new()

  res = init$MultiPoint(list(c(100, 1.01), c(200, 2.01)), stringify = TRUE)

  nams = sum(names(res) %in% c("json_dump", "type", "coordinates")) == 3

  TYPE = res$type == "MultiPoint"

  coords = is.list(res$coordinates) && !length(res$coordinates) == 0

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})



#==================================== 'LineString'

testthat::test_that("in case that the 'data' parameter is not a numeric list it returns an error", {

  mt = matrix(runif(10), 2, 5)

  init = TO_GeoJson$new()

  testthat::expect_error( init$LineString(mt, stringify = TRUE) )
})



testthat::test_that("in case that the 'stringify' parameter is not a boolean it returns an error", {

  init = TO_GeoJson$new()

  testthat::expect_error( init$LineString(list(c(100, 1.01), c(200, 2.01)), stringify = 'TRUE') )
})



testthat::test_that("in case that both parameters are valid it returns a GeoJson object", {

  init = TO_GeoJson$new()

  res = init$LineString(list(c(100, 1.01), c(200, 2.01)), stringify = TRUE)

  nams = sum(names(res) %in% c("json_dump", "type", "coordinates")) == 3

  TYPE = res$type == "LineString"

  coords = is.list(res$coordinates) && !length(res$coordinates) == 0

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})




#==================================== 'MultiLineString'

testthat::test_that("in case that the 'data' parameter is not a numeric list it returns an error", {

  mt = matrix(runif(10), 2, 5)

  init = TO_GeoJson$new()

  testthat::expect_error( init$MultiLineString(mt, stringify = TRUE) )
})



testthat::test_that("in case that the 'stringify' parameter is not a boolean it returns an error", {

  init = TO_GeoJson$new()

  testthat::expect_error( init$MultiLineString(list(list(c(100, 1.01), c(200, 2.01)), list(c(100, 1.01), c(200, 2.01))), stringify = 'TRUE') )
})



testthat::test_that("in case that both parameters are valid it returns a GeoJson object", {

  init = TO_GeoJson$new()

  res = init$MultiLineString(list(list(c(100, 1.01), c(200, 2.01)), list(c(100, 1.01), c(200, 2.01))), stringify = TRUE)

  nams = sum(names(res) %in% c("json_dump", "type", "coordinates")) == 3

  TYPE = res$type == "MultiLineString"

  coords = is.list(res$coordinates) && !length(res$coordinates) == 0

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})


#==================================== 'Polygon (WITHOUT interior rings)'

testthat::test_that("in case that the 'data' parameter is not a numeric list it returns an error", {

  mt = matrix(runif(10), 2, 5)

  init = TO_GeoJson$new()

  testthat::expect_error( init$Polygon(mt, stringify = TRUE) )
})



testthat::test_that("in case that the 'stringify' parameter is not a boolean it returns an error", {

  init = TO_GeoJson$new()

  testthat::expect_error( init$Polygon(list(list(c(100, 1.01), c(200, 2.01), c(100, 1.01), c(200, 2.01))), stringify = 'TRUE') )
})



testthat::test_that("in case that both parameters are valid it returns a GeoJson object", {

  init = TO_GeoJson$new()

  res = init$Polygon(list(list(c(100, 1.01), c(200, 2.01), c(100, 1.01), c(200, 2.01))), stringify = TRUE)

  nams = sum(names(res) %in% c("json_dump", "type", "coordinates")) == 3

  TYPE = res$type == "Polygon"

  coords = is.list(res$coordinates) && !length(res$coordinates) == 0

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})



#==================================== 'Polygon (WITH interior rings)'

testthat::test_that("in case that the 'data' parameter is not a numeric list it returns an error", {

  mt = matrix(runif(10), 2, 5)

  init = TO_GeoJson$new()

  testthat::expect_error( init$Polygon(mt, stringify = TRUE) )
})



testthat::test_that("in case that the 'stringify' parameter is not a boolean it returns an error", {

  init = TO_GeoJson$new()

  testthat::expect_error( init$Polygon( list(list(c(100, 1.01), c(200, 2.01), c(100, 1.01), c(200, 2.01)),

                                             list(c(100, 1.01), c(200, 2.01), c(100, 1.01), c(200, 2.01))), stringify = 'TRUE') )
})



testthat::test_that("in case that both parameters are valid it returns a GeoJson object", {

  init = TO_GeoJson$new()

  res = init$Polygon( list(list(c(100, 1.01), c(200, 2.01), c(100, 1.01), c(200, 2.01)),

                           list(c(100, 1.01), c(200, 2.01), c(100, 1.01), c(200, 2.01))), stringify = TRUE)

  nams = sum(names(res) %in% c("json_dump", "type", "coordinates")) == 3

  TYPE = res$type == "Polygon"

  coords = is.list(res$coordinates) && !length(res$coordinates) == 0

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})



#==================================== 'MultiPolygon'

testthat::test_that("in case that the 'data' parameter is not a numeric list it returns an error", {

  mt = matrix(runif(10), 2, 5)

  init = TO_GeoJson$new()

  testthat::expect_error( init$MultiPolygon(mt, stringify = TRUE) )
})



testthat::test_that("in case that the 'stringify' parameter is not a boolean it returns an error", {

  init = TO_GeoJson$new()

  testthat::expect_error( init$MultiPolygon(list(list(list(c(100, 1.0), c(200, 2.0), c(100, 1.0), c(200, 2.0))),

                                                 list(list(c(100, 1.0), c(200, 2.0), c(100, 1.0), c(200, 2.0)),

                                                      list(c(100, 1.0), c(200, 2.0), c(100, 1.0), c(200, 2.0)))), stringify = 'TRUE') )
})



testthat::test_that("in case that both parameters are valid it returns a GeoJson object", {

  init = TO_GeoJson$new()

  res = init$MultiPolygon(list(list(list(c(100, 1.0), c(200, 2.0), c(100, 1.0), c(200, 2.0))),

                               list(list(c(100, 1.0), c(200, 2.0), c(100, 1.0), c(200, 2.0)),

                                    list(c(100, 1.0), c(200, 2.0), c(100, 1.0), c(200, 2.0)))), stringify = TRUE)

  nams = sum(names(res) %in% c("json_dump", "type", "coordinates")) == 3

  TYPE = res$type == "MultiPolygon"

  coords = is.list(res$coordinates) && !length(res$coordinates) == 0

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})



#==================================== 'GeometryCollection'

testthat::test_that("in case that the 'data' parameter is not a numeric list it returns an error", {

  mt = matrix(runif(10), 2, 5)

  init = TO_GeoJson$new()

  testthat::expect_error( init$GeometryCollection(mt, stringify = TRUE) )
})



testthat::test_that("in case that the 'stringify' parameter is not a boolean it returns an error", {

  init = TO_GeoJson$new()

  testthat::expect_error( init$GeometryCollection(geometry_collection_dat, stringify = 'TRUE') )
})



testthat::test_that("in case that both parameters are valid it returns a GeoJson object", {

  init = TO_GeoJson$new()

  res = init$GeometryCollection(geometry_collection_dat, stringify = TRUE)

  nams = sum(names(res) %in% c("json_dump", "type", "geometries")) == 3

  TYPE = res$type == "GeometryCollection"

  coords = is.list(res$geometries) && !length(res$geometries) == 0

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})



#==================================== 'Feature'

testthat::test_that("in case that the 'data' parameter is not a list it returns an error", {

  mt = matrix(runif(10), 2, 5)

  init = TO_GeoJson$new()

  testthat::expect_error( init$Feature(mt, stringify = TRUE) )
})



testthat::test_that("in case that the 'stringify' parameter is not a boolean it returns an error", {

  init = TO_GeoJson$new()

  testthat::expect_error( init$Feature(feature_dat2, stringify = 'TRUE') )
})



testthat::test_that("in case that both parameters are valid it returns a GeoJson object", {

  init = TO_GeoJson$new()

  res = init$Feature(feature_dat2, stringify = TRUE)

  nams = sum(names(res) %in% c("id", "bbox", "geometry", "properties", "json_dump", "type")) == 6

  TYPE = res$type == "Feature"

  coords = is.list(res$geometry) && !length(res$geometry) == 0

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})


#==================================== 'FeatureCollection'

testthat::test_that("in case that the 'data' parameter is not a list it returns an error", {

  mt = matrix(runif(10), 2, 5)

  init = TO_GeoJson$new()

  testthat::expect_error( init$FeatureCollection(mt, stringify = TRUE) )
})



testthat::test_that("in case that the 'stringify' parameter is not a boolean it returns an error", {

  init = TO_GeoJson$new()

  testthat::expect_error( init$FeatureCollection(feature_col_dat, stringify = 'TRUE') )
})



testthat::test_that("in case that both parameters are valid it returns a GeoJson object", {

  init = TO_GeoJson$new()

  res = init$FeatureCollection(feature_col_dat, stringify = TRUE)

  nams = sum(names(res) %in% c("bbox", "features", "type", "json_dump")) == 4

  TYPE = res$type == "FeatureCollection"

  coords = sum(unlist(lapply(res$features, function(x) is.list(x$geometry) && !length(x$geometry) == 0))) == 2

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})



#------------------------------
# Features_2Collection function
#------------------------------


testthat::test_that("in case that the 'Features_files_vec' parameter is not a character string it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( Features_2Collection(mt, bbox_vec = NULL) )
})


testthat::test_that("in case that the 'bbox_vec' parameter is not a numeric vector it returns an error", {

  PATH = paste0(getwd(), path.expand("/file_data/feature_multiple_files"))

  path_files = list.files(PATH, full.names = T)

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( Features_2Collection(path_files, bbox_vec = mt) )
})


testthat::test_that("in case that both parameters are valid it returns a named list", {

  PATH = paste0(getwd(), path.expand("/file_data/feature_multiple_files"))

  path_files = list.files(PATH, full.names = T)

  bb = c(-10.01, -10.01, 10.01, 10.01)

  res = Features_2Collection(path_files, bbox_vec = bb)

  tmp = FROM_GeoJson(url_file_string = res)

  nams = sum(names(tmp) %in% c("bbox", "features", "type")) == 3

  obj = tmp$type == "FeatureCollection"

  len = length(tmp$features) == 5

  testthat::expect_true(sum(c(nams, obj, len)) == 3)
})



#-------------------------
# shiny_from_JSON function
#-------------------------


testthat::test_that("in case that the 'input_file' parameter is not a character string it returns an error", {

  mt = matrix(runif(10), 2, 5)

  testthat::expect_error( shiny_from_JSON(mt) )
})


testthat::test_that("in case that the 'input_file' parameter is a valid path to a file it returns a named list", {

  PATH = paste0(getwd(), path.expand("/file_data/Feature.geojson"))

  res = shiny_from_JSON(PATH)

  nams = sum(names(res) %in% c("bbox", "geometry", "id", "properties", "type")) == 5

  TYPE = res$type == "Feature"

  coords = is.list(res$geometry) && !length(res$geometry) == 0

  testthat::expect_true( sum(c(nams, TYPE, coords)) == 3 )
})



#---------------------
# merge_files function
#---------------------

testthat::test_that("in case that the 'INPUT_FOLDER' parameter is not a character string it returns an error", {

  testthat::expect_error( merge_files(INPUT_FOLDER = NULL, OUTPUT_FILE = "/valid/file.json", CONCAT_DELIMITER = "\n", verbose = FALSE) )
})


testthat::test_that("in case that the 'OUTPUT_FILE' parameter is not a character string it returns an error", {

  testthat::expect_error( merge_files(INPUT_FOLDER = "/valid/path", OUTPUT_FILE = NULL, CONCAT_DELIMITER = "\n", verbose = FALSE) )
})


testthat::test_that("in case that the 'CONCAT_DELIMITER' parameter is not a character string it returns an error", {

  testthat::expect_error( merge_files(INPUT_FOLDER = "/valid/path", OUTPUT_FILE = "/valid/file.json", CONCAT_DELIMITER = NULL, verbose = FALSE) )
})


testthat::test_that("in case that the 'verbose' parameter is not a boolean it returns an error", {

  testthat::expect_error( merge_files(INPUT_FOLDER = "/valid/path", OUTPUT_FILE = "/valid/file.json", CONCAT_DELIMITER = "\n", verbose = NULL) )
})


testthat::test_that("in case that the 'INPUT_FOLDER' parameter does not end in slash it returns an error", {

  testthat::expect_error( merge_files(INPUT_FOLDER = "/valid/path", OUTPUT_FILE = "/valid/file.json", CONCAT_DELIMITER = "\n", verbose = FALSE) )
})


testthat::test_that("in case that the 'INPUT_FOLDER' parameter does not end in slash it returns an error", {

  PATH_file_exists = paste0(getwd(), path.expand("/file_exists.json"))

  testthat::expect_error( merge_files(INPUT_FOLDER = "/valid/path/", OUTPUT_FILE = PATH_file_exists, CONCAT_DELIMITER = "\n", verbose = FALSE) )
})


testthat::test_that("in case that the 'OUTPUT_FILE' already exists it returns a warning. Then it writes the content of the folder to a file and finally it removes the file from the directory)", {

  temporary_function = function() {

    PATH_folder_exists = paste0(getwd(), path.expand("/merge_folder/"))

    PATH_file_exists = paste0(getwd(), path.expand("/file_exists.json"))

    file.create(PATH_file_exists, showWarnings = F)

    merge_files(INPUT_FOLDER = PATH_folder_exists, OUTPUT_FILE = PATH_file_exists, CONCAT_DELIMITER = "\n", verbose = FALSE)

    file.remove(PATH_file_exists)
  }

  testthat::expect_warning( temporary_function() )        # at the same time it works as testthat::expect_true()
})


#===========================================================================


testthat::test_that("it throws an error in case that the json object includes an invalid json data type ( valid json-objects : 'string', 'number', 'a JSON object', 'array', 'boolean', 'null' )", {

  char_str = '{"invalid_data": NaN, "valid_data": 1}'              # NaN returns NULL

  testthat::expect_error( geojsonR:::export_From_JSON(char_str) )
})


#===========================================================================


#----------------------------------------------------
# 'save_R_list_Features_2_FeatureCollection' function
#----------------------------------------------------


testthat::test_that("the 'save_R_list_Features_2_FeatureCollection' function works as expected (Be aware that the sample input matrices MUST have 2 columns to resemble lat-lon values, otherwise an error will be raised)", {
  
  
  polygon_WITHOUT_interior = list(type ="Feature",
                                  id = 1L,
                                  properties = list(prop1 = 'polygon-without', prop2 = 1.0234),
                                  geometry = list(type = 'Polygon',
                                                  coordinates = matrix(runif(20), nrow = 10, ncol = 2)))
  
  polygon_WITH_interior = list(type ="Feature",
                                  id = 2L,
                                  properties = list(prop1 = 'polygon-with', prop2 = 4.892),
                                  geometry = list(type = 'Polygon',
                                                  coordinates = list(list(matrix(runif(20), nrow = 10, ncol = 2),
                                                                          matrix(runif(8), nrow = 4, ncol = 2)))))                  # a polygon with interior rings is a list of length 1 which includes 2 or more matrices
  
  multipolygon_WITHOUT_interior = list(type ="Feature",
                                       id = 3L,
                                       properties = list(prop1 = 'multipolygon-without', prop2 = 6.0987),
                                       geometry = list(type = 'MultiPolygon',
                                                       coordinates = list(matrix(runif(20), nrow = 10, ncol = 2),
                                                                          matrix(runif(20), nrow = 10, ncol = 2))))
  
  multipolygon_WITH_interior = list(type ="Feature",
                                    id = 4L,
                                    properties = list(prop1 = 'multipolygon-with', prop2 = 9.337),
                                    geometry = list(type = 'MultiPolygon',
                                                    coordinates = list(list(matrix(runif(20), nrow = 10, ncol = 2),                 # one or more polygons with interior rings
                                                                            matrix(runif(8), nrow = 4, ncol = 2)),
                                                                       matrix(runif(20), nrow = 10, ncol = 2),                      # one or more polygons without interior rings
                                                                       matrix(runif(20), nrow = 10, ncol = 2))))
  
  list_features = list(polygon_WITHOUT_interior,
                       polygon_WITH_interior,
                       multipolygon_WITHOUT_interior,
                       multipolygon_WITH_interior)
  
  path_feat_col = tempfile(fileext = '.geojson')
  
  res = save_R_list_Features_2_FeatureCollection(input_list = list_features,
                                                 path_to_file = path_feat_col,
                                                 verbose = TRUE)
  
  res_load = FROM_GeoJson_Schema(url_file_string = path_feat_col)
  
  prop_names = unlist(lapply(res_load$features, function(x) x$properties$prop1))
  
  feat_nams = c("geometry", "id", "properties", "type")
  
  plg_wo_int = all.equal(polygon_WITHOUT_interior[feat_nams], res_load$features[[which(prop_names == 'polygon-without')]][feat_nams], tol = 0.000001)      # add the tolerance parameter for differences that might arise due to save and load procedure
  plg_with_int = all.equal(polygon_WITH_interior[feat_nams], res_load$features[[which(prop_names == 'polygon-with')]][feat_nams], tol = 0.000001) 
  mlt_plg_wo_int = all.equal(multipolygon_WITHOUT_interior[feat_nams], res_load$features[[which(prop_names == 'multipolygon-without')]][feat_nams], tol = 0.000001) 
  mlt_plg_with_int = all.equal(multipolygon_WITH_interior[feat_nams], res_load$features[[which(prop_names == 'multipolygon-with')]][feat_nams], tol = 0.000001) 
  
  if (file.exists(path_feat_col)) file.remove(path_feat_col)

  testthat::expect_true( all(c(plg_wo_int, plg_with_int, mlt_plg_wo_int, mlt_plg_with_int)) )
})

