

#' reads GeoJson data
#'
#' @param url_file_string a string specifying the input path to a file OR a geojson object (in form of a character string) OR a valid url (beginning with 'http..') pointing to a geojson object
#' @param Flatten_Coords either TRUE or FALSE. If TRUE then the properties member of the geojson file will be omitted during parsing.
#' @param Average_Coordinates either TRUE or FALSE. If TRUE then additionally a geojson-dump and the average latitude and longitude of the geometry object will be returned.
#' @param To_List either TRUE or FALSE. If TRUE then the \emph{coordinates} of the geometry object will be returned in form of a list, otherwise in form of a numeric matrix.
#' @return a (nested) list
#' @details
#' The \emph{FROM_GeoJson} function is based on the 'RFC 7946' specification. Thus, geojson files/strings which include property-names other than the 'RFC 7946' specifies will return an error. To avoid errors of
#' that kind a user should take advantage of the \emph{FROM_GeoJson_Schema} function, which is not as strict concerning the property names.
#' @export
#' @examples
#'
#' \dontrun{
#'
#' library(geojsonR)
#'
#'
#' # INPUT IS A FILE
#'
#' res = FROM_GeoJson(url_file_string = "/myfolder/feature_collection.geojson")
#'
#'
#' # INPUT IS A GEOJSON (character string)
#'
#' tmp_str = '{ "type": "MultiPolygon", "coordinates": [
#'   [[[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0], [102.0, 2.0]]],
#'   [[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]],
#'    [[100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2]]]
#'   ]
#' }'
#'
#' res = FROM_GeoJson(url_file_string = tmp_str)
#'
#'
#' # INPUT IS A URL
#'
#' res = FROM_GeoJson(url_file_string = "http://www.EXAMPLE_web_page.geojson")
#' }
#'

FROM_GeoJson = function(url_file_string, Flatten_Coords = FALSE, Average_Coordinates = FALSE, To_List = FALSE) {

  if (!inherits(url_file_string, 'character') && length(url_file_string) != 1) { stop("the 'url_file_string' parameter should be of type character string", call. = F) }
  if (!inherits(Flatten_Coords, "logical")) { stop("the 'Flatten_Coords' parameter should be of type boolean", call. = F) }
  if (!inherits(Average_Coordinates, "logical")) { stop("the 'Average_Coordinates' parameter should be of type boolean", call. = F) }
  if (!inherits(To_List, "logical")) { stop("the 'To_List' parameter should be of type boolean", call. = F) }

  if (substring(url_file_string, 1, 4) == "http") {       # only url-addresses which start with 'http' will be considered as valid

    con = url(url_file_string, method = "libcurl")        # test url-output with : 'https://raw.githubusercontent.com/lyzidiamond/learn-geojson/master/geojson/cupcakes.geojson'

    url_json = readLines(con, warn = FALSE)

    url_file_string = paste(url_json, collapse = "\n")

    close(con); gc()
  }

  res = export_From_geojson(url_file_string, Flatten_Coords, Average_Coordinates, To_List)

  return(res)
}



#' reads GeoJson data using a one-word-schema
#'
#' @param url_file_string a string specifying the input path to a file OR a geojson object (in form of a character string) OR a valid url (beginning with 'http..') pointing to a geojson object
#' @param geometry_name a string specifying the geometry name in the geojson string/file. The \emph{geometry_name} functions as a one-word schema and can significantly speed up the parsing of the data.
#' @param Average_Coordinates either TRUE or FALSE. If TRUE then additionally a geojson-dump and the average latitude and longitude of the geometry object will be returned.
#' @param To_List either TRUE or FALSE. If TRUE then the \emph{coordinates} of the geometry object will be returned in form of a list, otherwise in form of a numeric matrix.
#' @return a (nested) list
#' @details
#' This function is appropriate when the property-names do not match exactly the 'RFC 7946' specification ( for instance if the \emph{geometry} object-name appears as \emph{location} as is the case sometimes in mongodb queries ).
#' The user can then specify the \emph{geometry_name} as it exactly appears in the .geojson string/file (consult the example for more details). If no \emph{geometry_name} is given then recursion will be used, which increases the processing time.
#' In case that the input .geojson object is of \emph{type} : \emph{Point}, \emph{LineString}, \emph{MultiPoint}, \emph{Polygon}, \emph{GeometryCollection}, \emph{MultiLineString}, \emph{MultiPolygon},
#'  \emph{Feature} or \emph{FeatureCollection} with a second attribute name : \emph{coordinates}, then the \emph{geometry_name} parameter is not necessary.
#' @export
#' @examples
#'
#' library(geojsonR)
#'
#'
#' # INPUT IS A GEOJSON (character string)
#'
#' tmp_str = '{
#'             "name" : "example_name",
#'             "location" : {
#'                 "type" : "Point",
#'                 "coordinates" : [ -120.24, 39.21 ]
#'               }
#'            }'
#'
#' res = FROM_GeoJson_Schema(url_file_string = tmp_str, geometry_name = "location")
#'

FROM_GeoJson_Schema = function(url_file_string, geometry_name = "", Average_Coordinates = FALSE, To_List = FALSE) {

  if (!inherits(url_file_string, 'character') && length(url_file_string) != 1) { stop("the 'url_file_string' parameter should be of type character string", call. = F) }
  if (!inherits(geometry_name, "character")) { stop("the 'geometry_name' parameter should be of type character", call. = F) }
  if (!inherits(Average_Coordinates, "logical")) { stop("the 'Average_Coordinates' parameter should be of type boolean", call. = F) }
  if (!inherits(To_List, "logical")) { stop("the 'To_List' parameter should be of type boolean", call. = F) }

  if (substring(url_file_string, 1, 4) == "http") {       # only url-addresses which start with 'http' will be considered as valid

    con = url(url_file_string, method = "libcurl")        # test url-output with : 'https://raw.githubusercontent.com/lyzidiamond/learn-geojson/master/geojson/cupcakes.geojson'

    url_json = readLines(con, warn = FALSE)

    url_file_string = paste(url_json, collapse = "\n")

    close(con); gc()
  }

  res = export_From_geojson_schema(url_file_string, geometry_name, Average_Coordinates, To_List)

  return(res)
}



#' returns a json-dump from a geojson file
#'
#' @param url_file either a string specifying the input path to a file OR a valid url (beginning with 'http..') pointing to a geojson object
#' @return a character string (json dump)
#' @export
#' @examples
#'
#' \dontrun{
#'
#' library(geojsonR)
#'
#' res = Dump_From_GeoJson("/myfolder/point.geojson")
#' }
#'

Dump_From_GeoJson = function(url_file) {

  if (!inherits(url_file, 'character') && length(url_file) != 1) {

    stop("the 'url_file' parameter should be of type character string", call. = F)
  }

  if (substring(url_file, 1, 4) == "http") {       # only url-addresses which start with 'http' will be considered as valid

    con = url(url_file, method = "libcurl")        # test url-output with : 'https://raw.githubusercontent.com/lyzidiamond/learn-geojson/master/geojson/hackspots.geojson'

    url_json = readLines(con, warn = FALSE)

    res = paste(url_json, collapse = "\n")

    close(con); gc()}

  else if (file.exists(url_file)) {

    res = dump_geojson(url_file)}

  else {

    stop("the input shoud be either a valid url (beginning with 'http..') OR a valid path to a geojson file", call. = F)
  }

  return(res)
}



#' converts data to a GeoJson object
#'
#' @param data a list specifying the geojson geometry object
#' @param stringify either TRUE or FALSE, specifying if the output should also include a geojson-dump (as a character string)
#' @return a List
#' @export
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods:
#'
#' \describe{
#'  \item{\code{TO_GeoJson$new()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Point(data, stringify = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{MultiPoint(data, stringify = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{LineString(data, stringify = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{MultiLineString(data, stringify = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Polygon(data, stringify = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{MultiPolygon(data, stringify = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{GeometryCollection(data, stringify = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Feature(data, stringify = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{FeatureCollection(data, stringify = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'  }
#'
#' @usage # utl <- TO_GeoJson$new()
#' @examples
#'
#' library(geojsonR)
#'
#'
#'# initialize class
#'
#' init = TO_GeoJson$new()
#'
#'
#' # Examples covering all geometry-objects
#'
#'
#' # Point
#'
#' point_dat = c(100, 1.01)
#'
#' point = init$Point(point_dat, stringify = TRUE)
#' point
#'
#'
#' # MultiPoint
#'
#' multi_point_dat = list(c(100, 1.01), c(200, 2.01))
#'
#' multi_point = init$MultiPoint(multi_point_dat, stringify = TRUE)
#' multi_point
#'
#'
#' # LineString
#'
#' linestring_dat = list(c(100, 1.01), c(200, 2.01))
#'
#' line_string = init$LineString(linestring_dat, stringify = TRUE)
#' line_string
#'
#'
#' # MultiLineString
#'
#' multilinestring_dat = list(list(c(100, 0.0), c(101, 1.0)), list(c(102, 2.0), c(103, 3.0)))
#'
#' multiline_string = init$MultiLineString(multilinestring_dat, stringify = TRUE)
#' multiline_string
#'
#'
#' # Polygon (WITHOUT interior rings)
#'
#' polygon_WITHOUT_dat = list(list(c(100, 1.01), c(200, 2.01), c(100, 1.0), c(100, 1.01)))
#'
#' polygon_without = init$Polygon(polygon_WITHOUT_dat, stringify = TRUE)
#' polygon_without
#'
#'
#' # Polygon (WITH interior rings)
#'
#' polygon_WITH_dat = list(list(c(100, 1.01), c(200, 2.01), c(100, 1.0), c(100, 1.01)),
#'
#'                    list(c(50, 0.5), c(50, 0.8), c(50, 0.9), c(50, 0.5)))
#'
#' polygon_with = init$Polygon(polygon_WITH_dat, stringify = TRUE)
#' polygon_with
#'
#'
#' # MultiPolygon
#'
#' # the first polygon is without interior rings and the second one is with interior rings
#'
#' multi_polygon_dat = list(list(list(c(102, 2.0), c(103, 2.0), c(103, 3.0), c(102, 2.0))),
#'
#'                          list(list(c(100, 0.0), c(101, 1.0), c(101, 1.0), c(100, 0.0)),
#'
#'                               list(c(100.2, 0.2), c(100.2, 0.8), c(100.8, 0.8), c(100.2, 0.2))))
#'
#' multi_polygon = init$MultiPolygon(multi_polygon_dat, stringify = TRUE)
#' multi_polygon
#'
#'
#'
#' # GeometryCollection (named list)
#'
#'
#' Point = c(100, 1.01)
#'
#' MultiPoint = list(c(100, 1.01), c(200, 2.01))
#'
#' MultiLineString = list(list(c(100, 0.0), c(101, 1.0)),
#'
#'                   list(c(102, 2.0), c(103, 3.0)))
#'
#' LineString = list(c(100, 1.01), c(200, 2.01))
#'
#' MultiLineString = list(list(c(100, 0.0), c(101, 1.0)),
#'
#'                   list(c(102, 2.0), c(103, 3.0)))
#'
#' Polygon = list(list(c(100, 1.01), c(200, 2.01), c(100, 1.0), c(100, 1.01)))
#'
#' Polygon = list(list(c(100, 1.01), c(200, 2.01), c(100, 1.0), c(100, 1.01)),
#'
#'                list(c(50, 0.5), c(50, 0.8), c(50, 0.9), c(50, 0.5)))
#'
#' MultiPolygon = list(list(list(c(102, 2.0), c(103, 2.0), c(103, 3.0), c(102, 2.0))),
#'
#'                     list(list(c(100, 0.0), c(101, 1.0), c(101, 1.0), c(100, 0.0)),
#'
#'                     list(c(100.2, 0.2), c(100.2, 0.8), c(100.8, 0.8), c(100.2, 0.2))))
#'
#'
#' geometry_collection_dat = list(Point = Point, MultiPoint = MultiPoint,
#'
#'                                MultiLineString = MultiLineString, LineString = LineString,
#'
#'                                MultiLineString = MultiLineString, Polygon = Polygon,
#'
#'                                Polygon = Polygon, MultiPolygon = MultiPolygon)
#'
#'
#' geometry_col = init$GeometryCollection(geometry_collection_dat, stringify = TRUE)
#' geometry_col
#'
#'
#' # Feature (named list)
#'
#'
#' # Empty 'properties' list
#'
#' feature_dat1 = list(id = 1, bbox = c(1,2,3,4), geometry = list(Point = c(100, 1.01)),
#'
#'                     properties = list())
#'
#'
#' # Nested 'properties' list
#'
#' feature_dat2 = list(id = "1", bbox = c(1,2,3,4), geometry = list(Point = c(100, 1.01)),
#'
#'                     properties = list(prop0 = 'value0',
#'
#'                                       prop1 = 0.0, vec = c(1,2,3), lst = list(a = 1, d = 2)))
#'
#'
#' feature_obj = init$Feature(feature_dat2, stringify = TRUE)
#' feature_obj
#' cat(feature_obj$json_dump)
#'
#'
#'
#' # FeatureCollection (named list)
#'
#'
#' # takes as input the previously created 'feature_dat1', 'feature_dat2'
#'
#' feature_col_dat = list(bbox = c(-10.01, -10.01, 10.01, 10.01),
#'
#'                        features = list(Feature = feature_dat1, Feature = feature_dat2))
#' feature_col_dat
#'
#'
#' feature_collection_obj = init$FeatureCollection(feature_col_dat, stringify = TRUE)
#' feature_collection_obj
#' cat(feature_collection_obj$json_dump)
#'

TO_GeoJson <- R6::R6Class("TO_GeoJson",

                              public = list(

                                initialize = function() {

                                  private$empty_vec = numeric(0)

                                },

                                Point = function(data, stringify = FALSE) {

                                  if (!inherits(data, c('numeric', 'vector'))) { stop("the 'data' parameter should be a numeric vector", call. = F) }

                                  if (!inherits(stringify, 'logical')) { stop("the 'stringify' parameter should be of type boolean", call. = F) }

                                  res = export_To_GeoJson("Point", data, private$empty_vec, private$empty_vec, private$empty_vec, stringify)

                                  return(res)
                                },

                                MultiPoint = function(data, stringify = FALSE) {

                                  if (!inherits(data, c('numeric', 'list'))) { stop("the 'data' parameter should be a numeric list", call. = F) }

                                  if (!inherits(stringify, 'logical')) { stop("the 'stringify' parameter should be of type boolean", call. = F) }

                                  res = export_To_GeoJson("MultiPoint", private$empty_vec, data, private$empty_vec, private$empty_vec, stringify)

                                  return(res)
                                },

                                LineString = function(data, stringify = FALSE) {

                                  if (!inherits(data, c('numeric', 'list'))) { stop("the 'data' parameter should be a numeric list", call. = F) }

                                  if (!inherits(stringify, 'logical')) { stop("the 'stringify' parameter should be of type boolean", call. = F) }

                                  res = export_To_GeoJson("LineString", private$empty_vec, data, private$empty_vec, private$empty_vec, stringify)

                                  return(res)
                                },

                                MultiLineString = function(data, stringify = FALSE) {

                                  if (!inherits(data, c('numeric', 'list'))) { stop("the 'data' parameter should be a numeric list", call. = F) }

                                  if (!inherits(stringify, 'logical')) { stop("the 'stringify' parameter should be of type boolean", call. = F) }

                                  res = export_To_GeoJson("MultiLineString", private$empty_vec, private$empty_vec, data, private$empty_vec, stringify)

                                  return(res)
                                },

                                Polygon = function(data, stringify = FALSE) {

                                  if (!inherits(data, c('numeric', 'list'))) { stop("the 'data' parameter should be a numeric list", call. = F) }

                                  if (!inherits(stringify, 'logical')) { stop("the 'stringify' parameter should be of type boolean", call. = F) }

                                  res = export_To_GeoJson("Polygon", private$empty_vec, private$empty_vec, data, private$empty_vec, stringify)

                                  return(res)
                                },

                                MultiPolygon = function(data, stringify = FALSE) {

                                  if (!inherits(data, c('numeric', 'list'))) { stop("the 'data' parameter should be a numeric list", call. = F) }

                                  if (!inherits(stringify, 'logical')) { stop("the 'stringify' parameter should be of type boolean", call. = F) }

                                  res = export_To_GeoJson("MultiPolygon", private$empty_vec, private$empty_vec, private$empty_vec, data, stringify)

                                  return(res)
                                },

                                GeometryCollection = function(data, stringify = FALSE) {

                                  if (!inherits(data, c('numeric', 'list'))) { stop("the 'data' parameter should be a numeric list", call. = F) }

                                  if (!inherits(stringify, 'logical')) { stop("the 'stringify' parameter should be of type boolean", call. = F) }

                                  geometry_names = names(data)

                                  res = Geom_Collection(geometry_names, data, stringify)

                                  return(res)
                                },

                                Feature = function(data, stringify = FALSE) {

                                  if (!inherits(data, 'list')) { stop("the 'data' parameter should be of type list", call. = F) }

                                  if (!inherits(stringify, 'logical')) { stop("the 'stringify' parameter should be of type boolean", call. = F) }

                                  feature_names = names(data)

                                  res = Feature_Obj(feature_names, data, stringify)

                                  return(res)
                                },

                                FeatureCollection = function(data, stringify = FALSE) {

                                  if (!inherits(data, 'list')) { stop("the 'data' parameter should be of type list", call. = F) }

                                  if (!inherits(stringify, 'logical')) { stop("the 'stringify' parameter should be of type boolean", call. = F) }

                                  feature_col_names = names(data)

                                  res = Feature_collection_Obj(feature_col_names, data, stringify)

                                  return(res)
                                }
                              ),

                              private = list(

                                empty_vec = NULL
                              )
)



#' creates a FeatureCollection dump from multiple Feature geojson objects
#'
#' @param Features_files_vec a character vector specifying paths to files (Feature geojson objects)
#' @param bbox_vec either NULL or a numeric vector
#' @param write_path either NULL or a character string specifying a valid path to a file ( preferably with a \emph{.geojson extension} ) where the output data will be saved
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return a FeatureCollection dump
#' @details
#' The \emph{Features_2Collection} function utilizes internally a for-loop. In case of an error set the \emph{verbose} parameter to TRUE to find out which file leads to this error.
#' @export
#' @examples
#'
#' \dontrun{
#'
#' library(geojsonR)
#'
#' vec_files = c("/myfolder/Feature1.geojson", "/myfolder/Feature2.geojson",
#'               "/myfolder/Feature3.geojson", "/myfolder/Feature4.geojson",
#'               "/myfolder/Feature5.geojson")
#'
#' res = Features_2Collection(vec_files, bbox_vec = NULL)
#' }
#'

Features_2Collection = function(Features_files_vec, bbox_vec = NULL, write_path = NULL, verbose = FALSE) {

  if (!inherits(Features_files_vec, c('vector', 'character'))) {

    stop("the 'Features_files_vec' parameter should be a character vector", call. = F)
  }

  if (is.null(bbox_vec)) {

    bbox_vec = numeric(0)}

  else {

    if (!inherits(bbox_vec, c('vector', 'numeric'))) {

      stop("the 'bbox' parameter should be a numeric vector", call. = F)
    }
  }

  tmp_feat = Features_TO_Collection(Features_files_vec, bbox_vec, verbose)

  if (!is.null(write_path)) {
    fileConn = file(write_path)
    writeLines(tmp_feat, fileConn)
    close(fileConn)
  }

  return(tmp_feat)
}




#' creates a FeatureCollection from R list objects ( see the details section about the limitations of this function )
#'
#'
#' @param input_list a list object that includes 1 or more geojson R list Features
#' @param path_to_file either an empty string ("") or a valid path to a file where the output FeatureCollection will be saved
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @return a FeatureCollection in form of a character string
#' @return a FeatureCollection saved in a file
#' @details
#'
#' \itemize{
#'     \item it allows the following attributes: \emph{'type'}, \emph{'id'}, \emph{'properties'} and \emph{'geometry'}
#'     \item it allows only coordinates of type \emph{'Polygon'} or \emph{'MultiPolygon'} to be processed. In case of a \emph{'Polygon'} there are 2 cases: (a.) Polygon WITHOUT interior rings (a numeric matrix is expected) and (b.) Polygon WITH interior rings (a list of numeric matrices is expected). See the test-cases if you receive an error for the correct format of the input data. In case of a \emph{'MultiPolygon'} both Polygons with OR without interior rings can be included. Multipolygons are of the form: list of lists where each SUBLIST can be either a numeric matrix (Polygon without interior rings) or a list (Polygon with interior rings)
#'     \item the \emph{properties} attribute must be a list that can take only \emph{character strings}, \emph{numeric} and \emph{integer} values of SIZE 1. In case that any of the input properties is of SIZE > 1 then it will throw an error.
#' }
#'
#' The \emph{input_list} parameter can be EITHER created from scratch OR GeoJson Features (in form of a FeatureCollection) can be loaded in R and modified so that this list can be processed by this function
#'
#' @export
#' @examples
#'
#' \dontrun{
#'
#' library(geojsonR)
#'
#' #------------------------------------------------
#' # valid example that will save the data to a file
#' #------------------------------------------------
#'
#' Feature1 = list(type ="Feature",
#'                 id = 1L,
#'                 properties = list(prop1 = 'id', prop2 = 1.0234),
#'                 geometry = list(type = 'Polygon',
#'                                 coordinates = matrix(runif(20), nrow = 10, ncol = 2)))
#'
#' Feature2 = list(type ="Feature",
#'                 id = 2L,
#'                 properties = list(prop1 = 'non-id', prop2 = 6.0987),
#'                 geometry = list(type = 'MultiPolygon',
#'                                 coordinates = list(matrix(runif(20), nrow = 10, ncol = 2),
#'                                                   matrix(runif(20), nrow = 10, ncol = 2))))
#'
#' list_features = list(Feature1, Feature2)
#'
#' path_feat_col = tempfile(fileext = '.geojson')
#'
#' res = save_R_list_Features_2_FeatureCollection(input_list = list_features,
#'                                                path_to_file = path_feat_col,
#'                                                verbose = TRUE)
#'
#' #-------------------------------------
#' # validate that the file can be loaded
#' #-------------------------------------
#'
#' res_load = FROM_GeoJson_Schema(url_file_string = path_feat_col)
#' str(res_load)
#'
#'
#' #----------------------------------------------------
#' # INVALID data types such as NA's will throw an ERROR
#' #----------------------------------------------------
#'
#'
#' Feature1 = list(type ="Feature",
#'                 id = 1L,
#'                 properties = list(prop1 = NA, prop2 = 1.0234),
#'                 geometry = list(type = 'Polygon',
#'                                 coordinates = matrix(runif(20), nrow = 10, ncol = 2)))
#'
#' list_features = list(Feature1, Feature2)
#'
#' path_feat_col = tempfile(fileext = '.geojson')
#'
#' res = save_R_list_Features_2_FeatureCollection(input_list = list_features,
#'                                                path_to_file = path_feat_col,
#'                                                verbose = TRUE)
#' }
#'

save_R_list_Features_2_FeatureCollection = function(input_list,
                                                    path_to_file = "",
                                                    verbose = FALSE) {

  if (length(input_list) < 1) {
    stop("The 'input_list' parameter must be at least of length 1!", call. = F)
  }

  res = SAVE_R_list_Features_2_FeatureCollection(x = input_list,
                                                 path_to_file = path_to_file,
                                                 verbose = verbose)
  return(res)
}




#' secondary function for shiny Applications
#'
#' @param input_file a character string specifying a path to a file
#' @return a (nested) list
#' @details
#' This function is meant for \emph{shiny Applications}. To read a GeoJson file use either the \emph{FROM_GeoJson} or \emph{FROM_GeoJson_Schema} function.
#' @export

shiny_from_JSON = function(input_file) {              # shiny apps should be in a separate package if Rcpp::class(es) are present

  if (!inherits(input_file, 'character') && length(input_file) != 1) {

    stop("the 'input_file' parameter should be a character string", call. = F)
  }

  return(export_From_JSON(input_file))
}




#' merge json files (or any kind of text files) from a directory
#'
#' @param INPUT_FOLDER a character string specifying a path to the input folder
#' @param OUTPUT_FILE a character string specifying a path to the output file
#' @param CONCAT_DELIMITER a character string specifying the delimiter to use when merging the files
#' @param verbose either TRUE or FALSE. If TRUE then information will be printed in the console.
#' @details
#' This function is meant for json files but it can be applied to any kind of text files. It takes an input folder (\emph{INPUT_FOLDER}) and an output file
#' (\emph{OUTPUT_FILE}) and merges all files from the \emph{INPUT_FOLDER} to a single \emph{OUTPUT_FILE} using the concatenation delimiter (\emph{CONCAT_DELIMITER}).
#' @export
#' @examples
#'
#' \dontrun{
#' library(geojsonR)
#'
#' merge_files(INPUT_FOLDER = "/my_folder/", OUTPUT_FILE = "output_file.json")
#' }

merge_files = function(INPUT_FOLDER, OUTPUT_FILE, CONCAT_DELIMITER = "\n", verbose = FALSE) {

  if (!inherits(INPUT_FOLDER, 'character') && length(INPUT_FOLDER) != 1) stop("the 'INPUT_FOLDER' parameter should be a character string", call. = F)
  if (!inherits(OUTPUT_FILE, 'character') && length(OUTPUT_FILE) != 1) stop("the 'OUTPUT_FILE' parameter should be a character string", call. = F)
  if (!inherits(CONCAT_DELIMITER, 'character') && length(CONCAT_DELIMITER) != 1) stop("the 'CONCAT_DELIMITER' parameter should be a character string", call. = F)
  if (!inherits(verbose, 'logical')) stop("the 'verbose' parameter should be of type boolean", call. = F)

  str_SPL = strsplit(INPUT_FOLDER, "")[[1]]
  if (!str_SPL[nchar(INPUT_FOLDER)] %in% c("/", "\\")) stop('the "INPUT_FOLDER" parameter should end in slash', call. = F)

  if (file.exists(OUTPUT_FILE)) warning(paste("the '", OUTPUT_FILE, "' file already exists. New data will be added to the end of '", OUTPUT_FILE, "' !", sep = ""), call. = F)
  if (!dir.exists(INPUT_FOLDER)) stop("the path to the 'INPUT_FOLDER' parameter does not exist", call. = F)

  merge_json(INPUT_FOLDER, OUTPUT_FILE, CONCAT_DELIMITER, verbose)

  invisible()
}

