

#' reads GeoJson data
#'
#' @param url_file_string a string specifying the input path to a file OR a geojson object (in form of a character string) OR a valid url (beginning with 'http..') pointing to a geojson object
#' @param ... See details for the \emph{ellipsis} (...).
#' @return a (nested) list
#' @details
#' The \emph{FROM_GeoJson} function can take two more parameters : \emph{flatten_coords} and \emph{average_coordinates}. Both parameters are boolean.
#' If \emph{flatten_coords} is TRUE then the properties member of the geojson file will be omitted. If \emph{average_coordinates} is TRUE then additionally a geojson-dump and the average
#' latitude and longitude of the geometry object will be returned.
#' @export
#' @examples
#'
#' library(geojsonR)
#'
#'
#' # INPUT IS A FILE
#'
#' # Do not run
#'
#' # res = FROM_GeoJson(url_file_string = "/myfolder/feature_collection.geojson")
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
#' # Do not run
#'
#' # res = FROM_GeoJson(url_file_string = "http://www.EXAMPLE_web_page.geojson")
#'

FROM_GeoJson = function(url_file_string, ...) {

  if (!inherits(url_file_string, 'character') && length(url_file_string) != 1) {

    stop("the 'url_file_string' parameter should be of type character string", call. = F)
  }

  if (substring(url_file_string, 1, 4) == "http") {       # only url-addresses which start with 'http' will be considered as valid

    con = url(url_file_string, method = "libcurl")        # test url-output with : 'https://raw.githubusercontent.com/lyzidiamond/learn-geojson/master/geojson/cupcakes.geojson'

    url_json = readLines(con, warn = FALSE)

    url_file_string = paste(url_json, collapse = "\n")

    rm(con); gc()
  }

  res = export_From_geojson(url_file_string, ...)

  return(res)
}



#' returns a json-dump from a geojson file
#'
#' @param url_file either a string specifying the input path to a file OR a valid url (beginning with 'http..') pointing to a geojson object
#' @return a character string (json dump)
#' @export
#' @examples
#'
#' library(geojsonR)
#'
#' # Do not run
#'
#' # res = Dump_From_GeoJson("/myfolder/point.geojson")
#'

Dump_From_GeoJson = function(url_file) {

  if (!inherits(url_file, 'character') && length(url_file) != 1) {

    stop("the 'url_file' parameter should be of type character string", call. = F)
  }

  if (substring(url_file, 1, 4) == "http") {       # only url-addresses which start with 'http' will be considered as valid

    con = url(url_file, method = "libcurl")        # test url-output with : 'https://raw.githubusercontent.com/lyzidiamond/learn-geojson/master/geojson/hackspots.geojson'

    url_json = readLines(con, warn = FALSE)

    res = paste(url_json, collapse = "\n")

    rm(con); gc()}

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
#' @return a FeatureCollection dump
#' @export
#' @examples
#'
#' library(geojsonR)
#'
#' # Do not run
#'
#' # vec_files = c("/myfolder/Feature1.geojson", "/myfolder/Feature2.geojson",
#' #               "/myfolder/Feature3.geojson", "/myfolder/Feature4.geojson",
#' #               "/myfolder/Feature5.geojson")
#'
#' # res = Features_2Collection(vec_files, bbox_vec = NULL)
#'

Features_2Collection = function(Features_files_vec, bbox_vec = NULL) {

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

  tmp_feat = Features_TO_Collection(Features_files_vec, bbox_vec)

  return(tmp_feat)
}




#' secondary function for shiny Applications
#'
#' @param input_file a character string specifying a path to a file
#' @return a (nested) list
#' @details
#' This function is meant for \emph{shiny Applications}. To read a GeoJson file use the \emph{FROM_GeoJson} function.
#' @export

shiny_from_JSON = function(input_file) {              # shiny apps should be in a separate package if Rcpp::class(es) are present

  if (!inherits(input_file, 'character') && length(input_file) != 1) {

    stop("the 'input_file' parameter should be a character string", call. = F)
  }

  return(export_From_JSON(input_file))
}


