
/**
 * Copyright (C) 2017 Lampros Mouselimis
 *
 * @file TO_geojson.cpp
 *
 * @author Lampros Mouselimis
 *
 * @date February - March 2017
 *
 * @Notes: converts data to a GeoJson object
 *
 * @last_modified: March 2017
 *
 **/


# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(cpp11)]]


#define NDEBUG                              // disable assertions completely, due to '__assert_fail' errors when checking package [ add flag also in Makevars file ]

#include <iostream>
#include <sstream>
#include <string>
#include <fstream>

#include <R.h>
#include <Rinternals.h>

#include "json11.h"



// Classes to build GeoJson-Geometries
//

template<class T>
class GeoJson_Geometries {

public:

  GeoJson_Geometries() { }


  // inner geometry object (json11)
  //

  json11::Json Inner_GeoJson(std::string geometry_object, T data) {

    json11::Json::array OBJ;

    for (unsigned int i = 0; i < data.size(); i++) {

      OBJ.push_back(data[i]);
    }

    json11::Json geo_json = json11::Json::object {

      { "type", geometry_object },

      { "coordinates", OBJ },
    };

    return geo_json;
  }


  // geometry-object : "Point", "MultiPoint", "LineString", "MultiLineString", "Polygon", "MultiPolygon"
  //

  Rcpp::List To_Geom_Obj(std::string geometry_object, T data, bool stringify = false) {

    Rcpp::List RES;

    if (stringify) {

      json11::Json geo_json = Inner_GeoJson(geometry_object, data);

      std::string json_str =  geo_json.dump();

      RES["json_dump"] = json_str;
    }

    RES["type"] = geometry_object;

    RES["coordinates"] = data;

    return RES;
  }

  ~GeoJson_Geometries() { }
};




// Class to build : 'Geometry-Collection', 'Feature', and 'Feature-Collection'
//


class GeoJson_Collections {

public:

  // array of geometry-objects
  //

  json11::Json::array array_geometry_collection(std::vector<std::string> geometry_object_names, Rcpp::List geometry_objects) {

    json11::Json::array array_geometries;

    for (unsigned int i = 0; i < geometry_object_names.size(); i++) {

      json11::Json geo_json;

      if (geometry_object_names[i] == "Point") {

        GeoJson_Geometries<std::vector<double>> gjg;

        std::vector<double> tmp_dat = geometry_objects[i];

        geo_json = gjg.Inner_GeoJson(geometry_object_names[i], tmp_dat);
      }

      else if (geometry_object_names[i] == "MultiPoint" || geometry_object_names[i] == "LineString") {

        GeoJson_Geometries<std::vector<std::vector<double>>> gjg;

        std::vector<std::vector<double>> tmp_dat = geometry_objects[i];

        geo_json = gjg.Inner_GeoJson(geometry_object_names[i], tmp_dat);
      }

      else if (geometry_object_names[i] == "MultiLineString" || geometry_object_names[i] == "Polygon") {

        GeoJson_Geometries<std::vector<std::vector<std::vector<double>>>> gjg;

        std::vector<std::vector<std::vector<double>>> tmp_dat = geometry_objects[i];

        geo_json = gjg.Inner_GeoJson(geometry_object_names[i], tmp_dat);
      }

      else if (geometry_object_names[i] == "MultiPolygon") {

        GeoJson_Geometries<std::vector<std::vector<std::vector<std::vector<double>>>>> gjg;

        std::vector<std::vector<std::vector<std::vector<double>>>> tmp_dat = geometry_objects[i];

        geo_json = gjg.Inner_GeoJson(geometry_object_names[i], tmp_dat);
      }

      else {

        Rcpp::stop("invalid GeoJson geometry object --> array_geometry_collection() function");
      }

      array_geometries.push_back(geo_json);
    }

    return array_geometries;
  }


  // 'GeometryCollection' object
  //

  Rcpp::List geometry_collection(std::vector<std::string> geometry_object_names, Rcpp::List geometry_objects, bool stringify = false) {

    Rcpp::List RES;

    std::string tmp_nam = "GeometryCollection";

    if (stringify) {

      json11::Json::array ARRAY = array_geometry_collection(geometry_object_names, geometry_objects);

      json11::Json geom_OBJECTS = json11::Json::object {

        { "type", tmp_nam },

        { "geometries", ARRAY },

      };

      std::string json_str =  geom_OBJECTS.dump();

      RES["json_dump"] = json_str;
    }

    RES["type"] = tmp_nam;

    RES["geometries"] = geometry_objects;

    return RES;
  }


  // recursive function for the 'properties' member of the 'Feature' object [ if stringify = TRUE ]
  // For, the 'TYPEOF()', 'LENGTH', 'REALSXP', 'LGLSXP etc. SEE :
  // http://adv-r.had.co.nz/C-interface.html, https://github.com/hadley/pryr/blob/master/src/typename.cpp,
  // https://github.com/hadley/r-internals/blob/master/vectors.md, http://gallery.rcpp.org/articles/rcpp-wrap-and-recurse/
  //

  json11::Json typeof_item(Rcpp::List rec_prop) {

    json11::Json::object properties_OBJ;

    std::vector<std::string> prop_nams = rec_prop.attr("names");

    unsigned int REC_SIZE = rec_prop.size();

    for (unsigned int f = 0; f < REC_SIZE; f++) {

      std::string tmp_nam = prop_nams[f];

      int length_item = LENGTH(rec_prop[f]);                          // distinction between 'float' and 'vector of floats' using LENGTH()

      if (length_item == 1) {

        if (TYPEOF(rec_prop[f]) == REALSXP) {

          double tmp_dbl = Rcpp::as<double>(rec_prop[f]);

          properties_OBJ[tmp_nam] = json11::Json(tmp_dbl);}
        
        else if (TYPEOF(rec_prop[f]) == INTSXP) {
          
          int tmp_dbl_int = Rcpp::as<int>(rec_prop[f]);
          
          properties_OBJ[tmp_nam] = json11::Json(tmp_dbl_int);}

        else if (TYPEOF(rec_prop[f]) == LGLSXP) {

          bool tmp_bool = rec_prop[f];

          properties_OBJ[tmp_nam] = json11::Json(tmp_bool);}

        else if (TYPEOF(rec_prop[f]) == STRSXP) {

          std::string tmp_str = Rcpp::as<std::string>(rec_prop[f]);

          properties_OBJ[tmp_nam] = json11::Json(tmp_str);}

        else if (TYPEOF(rec_prop[f]) == NILSXP) {

          std::string tmp_str = "null";

          properties_OBJ[tmp_nam] = json11::Json(tmp_str);}

        else if (TYPEOF(rec_prop[f]) == VECSXP) {

          Rcpp::List tmp_lst = rec_prop[f];

          properties_OBJ[tmp_nam] = typeof_item(tmp_lst);}

        else {

          Rcpp::stop("invalid Json object of length == 1 --> typeof_item() function");
        }
      }

      else {

        if (TYPEOF(rec_prop[f]) == REALSXP) {

          std::vector<double> arr_vec = Rcpp::as<std::vector<double>>(rec_prop[f]);

          properties_OBJ[tmp_nam] = json11::Json(arr_vec);}
        
        else if (TYPEOF(rec_prop[f]) == INTSXP) {
          
          std::vector<int> arr_vec_int = Rcpp::as<std::vector<int>>(rec_prop[f]);
          
          properties_OBJ[tmp_nam] = json11::Json(arr_vec_int);}

        else if (TYPEOF(rec_prop[f]) == VECSXP) {

          Rcpp::List tmp_lst = rec_prop[f];

          properties_OBJ[tmp_nam] = typeof_item(tmp_lst);}

        else {

          Rcpp::stop("invalid Json object of length > 1 --> typeof_item() function");
        }
      }
    }

    return properties_OBJ;
  }


  // inner 'json11::Json::object' function for the 'properties' member of the 'Feature' object [ if stringify = TRUE ]
  //

  json11::Json::object inner_Feature(std::vector<std::string> geometry_object_names, Rcpp::List geometry_objects, std::string Feature_name) {

    json11::Json::object feature_OBJ;                                      // build the Json::object incrementally

    feature_OBJ["type"] = Feature_name;

    for (unsigned int i = 0; i < geometry_object_names.size(); i++) {

      if (geometry_object_names[i] == "id") {

        if (TYPEOF(geometry_objects[i]) == STRSXP) {

          std::string tmp_id = Rcpp::as<std::string>(geometry_objects["id"]);

          feature_OBJ["id"] = json11::Json(tmp_id);}

        else if (TYPEOF(geometry_objects[i]) == REALSXP) {

          double tmp_id = Rcpp::as<double>(geometry_objects["id"]);

          feature_OBJ["id"] = json11::Json(tmp_id);}

        else {

          Rcpp::stop("the 'id' member should be either a character string or a numeric value --> inner_Feature() function");
        }
      }

      else if (geometry_object_names[i] == "bbox") {

        std::vector<double> bbox_vec = geometry_objects["bbox"];

        feature_OBJ["bbox"] = json11::Json(bbox_vec);}

      else if (geometry_object_names[i] == "geometry") {

        Rcpp::List tmp_lst = geometry_objects[i];

        std::vector<std::string> tmp_lst_nams = tmp_lst.attr("names");

        json11::Json::array tmp_lst_array = array_geometry_collection(tmp_lst_nams, geometry_objects[i]);      // std::vector, Rcpp::List as input

        feature_OBJ["geometry"] = json11::Json(tmp_lst_array[0]);}                                              // extract the object from the array [ first array element ]

      else if (geometry_object_names[i] == "properties") {

        json11::Json::object properties_OBJ;                               // initialize second json-object for properties;

        Rcpp::List tmp_prop = geometry_objects[i];

        if (tmp_prop.size() == 0) {

          feature_OBJ["properties"] = json11::Json(properties_OBJ);}      // empty object if Rcpp::List is empty

        else {

          feature_OBJ["properties"] = typeof_item(tmp_prop);              // recursive function
        }
      }

      else {

        Rcpp::stop("invalid member of the Feature geometry object --> inner_Feature() function");
      }
    }

    return feature_OBJ;
  }


  // 'Feature' geometry object
  //

  Rcpp::List feature_OBJECT(std::vector<std::string> geometry_object_names, Rcpp::List geometry_objects, bool stringify = false) {

    Rcpp::List RES = geometry_objects;

    std::string tmp_nam = "Feature";

    if (stringify) {

      json11::Json::object feature_OBJ = inner_Feature(geometry_object_names, geometry_objects, tmp_nam);

      json11::Json res_json = feature_OBJ;

      std::string json_str = res_json.dump();

      RES["json_dump"] = json_str;
    }

    RES["type"] = tmp_nam;

    return RES;
  }


  // 'FeatureCollection' geometry object
  //

  Rcpp::List feature_collection(std::vector<std::string> geometry_object_names, Rcpp::List geometry_objects, bool stringify = false) {

    Rcpp::List RES = geometry_objects;

    std::string feat_col_nam = "FeatureCollection";

    RES["type"] = feat_col_nam;

    if (stringify) {

      json11::Json::object feature_col_OBJ;

      feature_col_OBJ["type"] = feat_col_nam;

      for (unsigned int i = 0; i < geometry_object_names.size(); i++) {

        if (geometry_object_names[i] == "bbox") {

          std::vector<double> bbox_vec = geometry_objects["bbox"];

          feature_col_OBJ["bbox"] = json11::Json(bbox_vec);}

        else if (geometry_object_names[i] == "features") {

          json11::Json::array multiple_features_OBJ;                                // initialize second json-object for multiple-feature-objects;

          Rcpp::List tmp_feat = geometry_objects[i];

          if (tmp_feat.size() == 0) {

            feature_col_OBJ["features"] = json11::Json(multiple_features_OBJ);}      // empty object if Rcpp::List is empty

          else {

            std::vector<std::string> outer_lst_nams = tmp_feat.attr("names");

            for (unsigned int j = 0; j < outer_lst_nams.size(); j++) {

              Rcpp::List inner_feat_lst = tmp_feat[j];

              std::vector<std::string> inner_lst_nams = inner_feat_lst.attr("names");

              std::string tmp_inner_nam = outer_lst_nams[j];

              json11::Json::object inner_loop_json11 = inner_Feature(inner_lst_nams, inner_feat_lst, tmp_inner_nam);

              multiple_features_OBJ.push_back(inner_loop_json11);
            }
          }

          feature_col_OBJ["features"] = multiple_features_OBJ;
        }

        else {

          Rcpp::stop("invalid object for the member 'features' of the 'FeatureCollection' object --> feature_collection() function");
        }
      }

      json11::Json inner_res_json = feature_col_OBJ;

      std::string json_str = inner_res_json.dump();

      RES["json_dump"] = json_str;
    }

    return RES;
  }

};




//----------------------------------
// TO-GeoJson functions [ exported ]
//----------------------------------


// Returns one of the following geometry-objects : "Point", "MultiPoint", "LineString", "MultiLineString", "Polygon", "MultiPolygon"
// either the 'vector' OR the 'nested-vector' of the objects must be an empty object [ empty = numeric(0) ]
//

// [[Rcpp::export]]
Rcpp::List export_To_GeoJson(std::string geometry_object, std::vector<double> data_POINTS, std::vector<std::vector<double>> data_ARRAYS, std::vector<std::vector<std::vector<double>>> data_ARRAY_ARRAYS,

                             std::vector<std::vector<std::vector<std::vector<double>>>> data_POLYGON_ARRAYS, bool stringify = false) {


  if (geometry_object == "Point") {

    GeoJson_Geometries<std::vector<double>> tgj;

    return tgj.To_Geom_Obj(geometry_object, data_POINTS, stringify);
  }

  else if (geometry_object == "MultiPoint" || geometry_object == "LineString") {

    GeoJson_Geometries<std::vector<std::vector<double>>> tgj;

    return tgj.To_Geom_Obj(geometry_object, data_ARRAYS, stringify);}

  else if (geometry_object == "MultiLineString" || geometry_object == "Polygon") {

    GeoJson_Geometries<std::vector<std::vector<std::vector<double>>>> tgj;

    return tgj.To_Geom_Obj(geometry_object, data_ARRAY_ARRAYS, stringify);}

  else if (geometry_object == "MultiPolygon") {

    GeoJson_Geometries<std::vector<std::vector<std::vector<std::vector<double>>>>> tgj;

    return tgj.To_Geom_Obj(geometry_object, data_POLYGON_ARRAYS, stringify);}

  else {

    Rcpp::stop("invalid geometry object --> export_To_GeoJson() function");
  }
}



// Geometry-collection object
//

// [[Rcpp::export]]
Rcpp::List Geom_Collection(std::vector<std::string> geometry_object_names, Rcpp::List geometry_objects, bool stringify = false) {

  GeoJson_Collections gjc_geometry;

  return gjc_geometry.geometry_collection(geometry_object_names, geometry_objects, stringify);
}



// Feature object
//

// [[Rcpp::export]]
Rcpp::List Feature_Obj(std::vector<std::string> geometry_object_names, Rcpp::List geometry_objects, bool stringify = false) {

  GeoJson_Collections gjc_feature;

  return gjc_feature.feature_OBJECT(geometry_object_names, geometry_objects, stringify);
}



// FeatureCollection object
//

// [[Rcpp::export]]
Rcpp::List Feature_collection_Obj(std::vector<std::string> geometry_object_names, Rcpp::List geometry_objects, bool stringify = false) {

  GeoJson_Collections gjc_feature_collection;

  return gjc_feature_collection.feature_collection(geometry_object_names, geometry_objects, stringify);
}


