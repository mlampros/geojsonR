
# data (all tests) ----------------------------------


set.seed(1)
js_data = '{ "type": "MultiPolygon", "coordinates": [
  [[[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0], [102.0, 2.0]]],
  [[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]],
   [[100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2]]]
  ]
}'



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

  len = sum(dim(tmp$geometry$coordinates) == c(1, 2)) == 2

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

  len = sum(dim(tmp_OUT$geometry$coordinates) == c(1, 2)) == 2

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

