
/**
 * Copyright (C) 2017 Lampros Mouselimis
 *
 * @file FROM_geojson.cpp
 *
 * @author Lampros Mouselimis
 *
 * @date February - March 2017
 *
 * @Notes: reads GeoJson from file / url / character-string
 *
 * @last_modified: March 2020
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
#include <dirent.h>
#include <map>

#include <R.h>
#include <Rinternals.h>

#include "json11.h"



// class to parse geojson geometries [ except for multi-polygon ]
//


class From_GeoJson_geometries {

private:

  arma::rowvec flatten_coords_pr;


public:

  From_GeoJson_geometries() { }


  // switch (if-else) function for the geometry-objects
  //

  Rcpp::List geom_OBJ(std::string geom_OBJECT, json11::Json input_obj, int polygon_size = 1, bool average_coordinates = false, bool to_list = false) {       // 'average_coordinates' of geojson object is needed (indirectly) in leaflet's "setView()"

    Rcpp::List switch_OBJ;

    if (geom_OBJECT == "Point") {

      json11::Json::array tmp_point = input_obj["coordinates"].array_items();

      Rcpp::NumericVector res_point(2);                                             // in case of 'Point' by default return an Rcpp::NumericVector

      res_point[0] = tmp_point[0].number_value();

      res_point[1] = tmp_point[1].number_value();

      switch_OBJ["unlist_OBJ"] = res_point;

      if (average_coordinates && !to_list) {

        flatten_coords_pr.set_size(2);

        flatten_coords_pr(0) = tmp_point[0].number_value();

        flatten_coords_pr(1) = tmp_point[1].number_value();
      }
    }

    else if (geom_OBJECT == "LineString" || geom_OBJECT == "MultiPoint" || (geom_OBJECT == "Polygon" && polygon_size == 1)) {

      json11::Json::array tmp_array;

      if (geom_OBJECT == "Polygon") {

        tmp_array = input_obj["coordinates"][0].array_items();}                 // array_item[0] in case of polygon-without-interior-rings

      if (geom_OBJECT == "LineString" || geom_OBJECT == "MultiPoint") {

        tmp_array = input_obj["coordinates"].array_items();
      }

      unsigned int size_array = tmp_array.size();

      arma::mat tmp_RES_(size_array, 2);                                  // initialize to_list = F

      Rcpp::List tmp_RES_LIST(size_array);                                // initialize to_list = T

      for (unsigned int i = 0; i < size_array; i++) {

        json11::Json::array inner_array = tmp_array[i].array_items();

        arma::rowvec inner_vec(inner_array.size());                       // initialize to_list = F

        Rcpp::NumericVector res_LMP_sizeONE(2);                           // initialize to_list = T

        for (unsigned int j = 0; j < inner_array.size(); j++) {

          if (to_list) {

            res_LMP_sizeONE[j] = inner_array[j].number_value();}

          else {

            inner_vec(j) = inner_array[j].number_value();
          }
        }

        if (to_list) {

          tmp_RES_LIST[i] = res_LMP_sizeONE;}

        else {

          tmp_RES_.row(i) = inner_vec;
        }
      }

      if (average_coordinates && !to_list) {

        flatten_coords_pr = arma::conv_to< arma::rowvec >::from(arma::mean(tmp_RES_, 0));
      }

      if (to_list) {

        switch_OBJ["unlist_OBJ"] = tmp_RES_LIST;
      }

      else {

        switch_OBJ["unlist_OBJ"] = tmp_RES_;
      }
    }

    else if (geom_OBJECT == "MultiLineString" || (geom_OBJECT == "Polygon" && polygon_size > 1)) {

      json11::Json::array poly_tmp = input_obj["coordinates"].array_items();

      unsigned int inner_poly_size = poly_tmp.size();

      Rcpp::List tmp_RES_poly_interior;

      arma::mat outer_avg;

      if (average_coordinates && !to_list) {

        outer_avg.set_size(inner_poly_size, 2);
      }

      for (unsigned int k = 0; k < inner_poly_size; k++) {

        json11::Json::array tmp_array = poly_tmp[k].array_items();

        unsigned int size_array = tmp_array.size();

        arma::mat coord_mat(size_array, 2);

        Rcpp::List tmp_RES_LIST_MP(size_array);

        for (unsigned int i = 0; i < size_array; i++) {

          json11::Json::array inner_array = tmp_array[i].array_items();

          arma::rowvec inner_vec(inner_array.size());

          Rcpp::NumericVector res_LMP_sizeMULTI(2);

          for (unsigned int j = 0; j < inner_array.size(); j++) {

            if (to_list) {

              res_LMP_sizeMULTI[j] = inner_array[j].number_value();}

            else {

              inner_vec(j) = inner_array[j].number_value();
            }
          }

          if (to_list) {

            tmp_RES_LIST_MP[i] = res_LMP_sizeMULTI;}

          else {

            coord_mat.row(i) = inner_vec;
          }
        }

        if (average_coordinates && !to_list) {

          outer_avg.row(k) = arma::conv_to< arma::rowvec >::from(arma::mean(coord_mat, 0));
        }

        if (to_list) {

          tmp_RES_poly_interior.push_back(tmp_RES_LIST_MP);}

        else {

          tmp_RES_poly_interior.push_back(coord_mat);
        }
      }

      if (average_coordinates && !to_list) {

        flatten_coords_pr = arma::conv_to< arma::rowvec >::from(arma::mean(outer_avg, 0));
      }

      switch_OBJ.push_back(tmp_RES_poly_interior);
    }

    else if (geom_OBJECT == "MultiPolygon") {

      json11::Json::array tmp_mlpol = input_obj["coordinates"].array_items();

      unsigned int outer_size = tmp_mlpol.size();

      arma::mat outer_avg;

      if (average_coordinates && !to_list) {

        outer_avg.set_size(outer_size, 2);
      }

      for (unsigned int k = 0; k < outer_size; k++) {

        json11::Json array_item = tmp_mlpol[k];

        int tmp_size_array = array_item.array_items().size();

        if (tmp_size_array == 1) {

          json11::Json::array tmp_array = array_item[0].array_items();                         // array_item[0] in case of polygon-without-interior-rings  [ multi-polygon --version ]

          unsigned int size_array = tmp_array.size();

          arma::mat tmp_RES_(size_array, 2);

          Rcpp::List tmp_RES_LIST_MPoly(size_array);

          for (unsigned int i = 0; i < size_array; i++) {

            json11::Json::array inner_array = tmp_array[i].array_items();

            arma::rowvec inner_vec(inner_array.size());

            Rcpp::NumericVector res_MPoly(2);

            for (unsigned int j = 0; j < inner_array.size(); j++) {

              if (to_list) {

                res_MPoly[j] = inner_array[j].number_value();}

              else {

                inner_vec(j) = inner_array[j].number_value();
              }
            }

            if (to_list) {

              tmp_RES_LIST_MPoly[i] = res_MPoly;}

            else {

              tmp_RES_.row(i) = inner_vec;
            }
          }

          if (average_coordinates && !to_list) {

            outer_avg.row(k) = arma::conv_to< arma::rowvec >::from(arma::mean(tmp_RES_, 0));
          }

          if (to_list) {

            switch_OBJ.push_back(tmp_RES_LIST_MPoly);}

          else {

            switch_OBJ.push_back(tmp_RES_);
          }
        }

        if (tmp_size_array > 1) {

          json11::Json::array poly_tmp = array_item.array_items();                             // multi-polygon [ polygon WITH interior --version ]

          unsigned int inner_poly_size = poly_tmp.size();

          Rcpp::List tmp_RES_poly_interior;

          arma::mat outer_avg_sec;

          if (average_coordinates && !to_list) {

            outer_avg_sec.set_size(inner_poly_size, 2);
          }

          for (unsigned int k1 = 0; k1 < inner_poly_size; k1++) {

            json11::Json::array tmp_array = poly_tmp[k1].array_items();

            unsigned int size_array = tmp_array.size();

            arma::mat coord_mat(size_array, 2);

            Rcpp::List tmp_RES_LIST_MPolyMULTI(size_array);

            for (unsigned int i = 0; i < size_array; i++) {

              json11::Json::array inner_array = tmp_array[i].array_items();

              arma::rowvec inner_vec(inner_array.size());

              Rcpp::NumericVector res_MPolyMULTI(2);

              for (unsigned int j = 0; j < inner_array.size(); j++) {

                if (to_list) {

                  res_MPolyMULTI[j] = inner_array[j].number_value();}

                else {

                  inner_vec(j) = inner_array[j].number_value();
                }
              }

              if (to_list) {

                tmp_RES_LIST_MPolyMULTI[i] = res_MPolyMULTI;}

              else {

                coord_mat.row(i) = inner_vec;
              }
            }

            if (average_coordinates && !to_list) {

              outer_avg_sec.row(k1) = arma::conv_to< arma::rowvec >::from(arma::mean(coord_mat, 0));
            }

            if (to_list) {

              tmp_RES_poly_interior.push_back(tmp_RES_LIST_MPolyMULTI);}

            else {

              tmp_RES_poly_interior.push_back(coord_mat);
            }
          }

          if (average_coordinates && !to_list) {

            outer_avg.row(k) = arma::conv_to< arma::rowvec >::from(arma::mean(outer_avg_sec, 0));
          }

          switch_OBJ.push_back(tmp_RES_poly_interior);
        }
      }

      if (average_coordinates && !to_list) {

        flatten_coords_pr = arma::conv_to< arma::rowvec >::from(arma::mean(outer_avg, 0));
      }
    }

    else {

      Rcpp::stop("invalid GeoJson geometry object --> geom_OBJ() function");
    }

    return switch_OBJ;
  }


  // return average_coordinates
  //

  arma::rowvec return_COORDS() {

    return flatten_coords_pr;
  }


  // geometry-collection
  //

  Rcpp::List geom_collection_OBJ(json11::Json parse_geom, bool average_coordinates = false, bool to_list = false) {              // 'average_coordinates' of geojson object is needed (indirectly) in leaflet's "setView()"

    Rcpp::List RES_col;

    json11::Json::array col_array = parse_geom["geometries"].array_items();

    RES_col["type"] = parse_geom["type"].string_value();

    unsigned int SIZE = col_array.size();

    Rcpp::List geoms_tmp;

    arma::mat outer_avg;

    if (average_coordinates && !to_list) {

      outer_avg.set_size(SIZE, 2);
    }

    for (unsigned int f = 0; f < SIZE; f++) {

      json11::Json iter = col_array[f];

      std::string res_type = iter["type"].string_value();

      Rcpp::List RES_inner;

      RES_inner["type"] = res_type;

      int polygon_size = iter["coordinates"].array_items().size();

      if (res_type == "Point" || res_type == "LineString" || res_type == "MultiPoint" || (res_type == "Polygon" && polygon_size == 1)) {

        RES_inner["coordinates"] = geom_OBJ(res_type, iter, polygon_size, average_coordinates, to_list)["unlist_OBJ"];         // unlist object
      }

      else {

        RES_inner["coordinates"] = geom_OBJ(res_type, iter, polygon_size, average_coordinates, to_list);
      }

      if (average_coordinates && !to_list) {

        outer_avg.row(f) = flatten_coords_pr;

        flatten_coords_pr.clear();                  // each time it's called : it first assigns to arma::mat then clears the private-variable [ due to "inside-class-loop" ]
      }

      geoms_tmp.push_back(RES_inner);
    }

    if (average_coordinates && !to_list) {

      flatten_coords_pr = arma::conv_to< arma::rowvec >::from(arma::mean(outer_avg, 0));
    }

    RES_col["geometries"] = geoms_tmp;

    return RES_col;
  }



  // secondary function for the "Feature" GeoJson object [ to process the "properties" member recursively ]
  //

  SEXP recursive_switch(json11::Json json) {             // json["properties"][ITEM]  AND use by default ITEM = "NULL" only in case of an array

    if (json.is_string()) {

      return Rcpp::wrap(json.string_value());}

    else if (json.is_bool()) {

      return Rcpp::wrap(json.bool_value());}

    else if (json.is_null()) {

      return Rcpp::wrap(R_NilValue);}                                                  // assign R's NULL value

    else if (json.is_number()) {

      return Rcpp::wrap(json.number_value());}

    else if (json.is_array()) {

      Rcpp::List recurs_out_array;

      for (auto& second_item : json.array_items()) {

        recurs_out_array.push_back(Rcpp::wrap(recursive_switch(second_item)));          // recursion of 'arrays' for geojson files is limited only to the "properties" member (unknown depth of array AND unknown type of item of each array). This because recursion is slower than a for loop
      }

      return recurs_out_array;
    }

    else if (json.is_object()) {

      Rcpp::List recurs_out;

      for (auto& second_item : json.object_items()) {

        std::string SEC_ITEM = second_item.first;

        recurs_out[SEC_ITEM] = Rcpp::wrap(recursive_switch(json[SEC_ITEM]));             // recursion for properties [ unknown depth ]
      }

      return recurs_out;
    }

    else {

      Rcpp::stop("invalid Json object --> recursive_switch() function");
    }
  }


  // geojson object : "Feature"
  //

  Rcpp::List feature_OBJ(json11::Json input_obj, bool flatten_coords = false, bool average_coordinates = false, bool to_list = false) {     // 'flatten_coords' and 'average_coordinates' of geojson object is needed (indirectly) in leaflet's "setView()"

    Rcpp::List RES_feat;

    for (auto& iter : input_obj.object_items()) {

      if (iter.first == "type") {

        RES_feat["type"] = iter.second.string_value();}

      else if (iter.first == "id" || iter.first == "_id") {                 // exception : some geojson feature-files have the 'id' member with an underscore [ in any case create an 'id' in the resulted list ]

        if (iter.second.is_number()) {

          RES_feat["id"] = iter.second.number_value();}

        else if (iter.second.is_string()) {

          RES_feat["id"] = iter.second.string_value();}

        else {

          Rcpp::stop("invalid type for the 'id' member --> feature_geojson_sequential() function");
        }
      }

      else if (iter.first == "bbox") {

        std::vector<json11::Json> tmp_arr = iter.second.array_items();

        std::vector<double> bbox_vec;

        for (auto& it : tmp_arr) {

          bbox_vec.push_back(it.number_value());
        }

        RES_feat["bbox"] = bbox_vec;
      }

      else if (iter.first == "geometry") {

        json11::Json tmp_geom = input_obj["geometry"];

        std::string res_type = tmp_geom["type"].string_value();

        int polygon_size = tmp_geom["coordinates"].array_items().size();

        Rcpp::List RES_OUT_feat;

        RES_OUT_feat["type"] = res_type;

        if (res_type == "Point" || res_type == "LineString" || res_type == "MultiPoint" || (res_type == "Polygon" && polygon_size == 1)) {

          RES_OUT_feat["coordinates"] = geom_OBJ(res_type, tmp_geom, polygon_size, average_coordinates, to_list)["unlist_OBJ"];}                           // unlist object

        else {

          RES_OUT_feat["coordinates"] = geom_OBJ(res_type, tmp_geom, polygon_size, average_coordinates, to_list);
        }

        RES_feat["geometry"] = RES_OUT_feat;
      }

      else if (iter.first == "properties" && !flatten_coords) {

        Rcpp::List tmp_prop;

        for (auto& item_prop : input_obj["properties"].object_items()) {

          std::string first_item = item_prop.first;

          tmp_prop[first_item] = recursive_switch(input_obj["properties"][first_item]);
        }

        RES_feat["properties"] = tmp_prop;
      }
    }

    return RES_feat;
  }



  // geojson object : "Feature"  [ used in the 'schema' function ]
  //

  Rcpp::List feature_OBJ_schema(json11::Json input_obj, bool average_coordinates = false, bool to_list = false) {     // 'flatten_coords' and 'average_coordinates' of geojson object is needed (indirectly) in leaflet's "setView()"

    Rcpp::List RES_feat;

    for (auto& iter : input_obj.object_items()) {

      if (iter.first == "geometry") {                                                           // In 'Feature' the property-name of each geometry-object is 'geometry'

        json11::Json tmp_geom = input_obj["geometry"];

        std::string res_type = tmp_geom["type"].string_value();

        int polygon_size = tmp_geom["coordinates"].array_items().size();

        Rcpp::List RES_OUT_feat;

        RES_OUT_feat["type"] = res_type;

        if (res_type == "Point" || res_type == "LineString" || res_type == "MultiPoint" || (res_type == "Polygon" && polygon_size == 1)) {

          RES_OUT_feat["coordinates"] = geom_OBJ(res_type, tmp_geom, polygon_size, average_coordinates, to_list)["unlist_OBJ"];}                           // unlist object

        else {

          RES_OUT_feat["coordinates"] = geom_OBJ(res_type, tmp_geom, polygon_size, average_coordinates, to_list);
        }

        RES_feat["geometry"] = RES_OUT_feat;
      }

      else {

        RES_feat[iter.first] = recursive_switch(iter.second);
      }
    }

    return RES_feat;
  }


  // geojson object : "FeatureCollection"
  //

  Rcpp::List feature_collection_geojson(json11::Json input_obj, bool flatten_coords = false, bool average_coordinates = false, bool to_list = false) {         // 'flatten_coords' and 'average_coordinates' of geojson object is needed (indirectly) in leaflet's "setView()"

    Rcpp::List RES_feat_col;

    for (auto& iter : input_obj.object_items()) {

      if (iter.first == "type") {

        RES_feat_col["type"] = iter.second.string_value();}

      else if (iter.first == "bbox") {

        std::vector<double> bbox_vec;

        for (auto& num : iter.second.array_items()) {

          bbox_vec.push_back(num.number_value());
        }

        RES_feat_col["bbox"] = bbox_vec;
      }

      else if (iter.first == "features") {

        Rcpp::List lst_feats;

        json11::Json::array tmp_arr = input_obj["features"].array_items();

        unsigned int ITER_AVG = 0;

        arma::mat outer_avg;

        if (average_coordinates && !to_list) {

          outer_avg.set_size(tmp_arr.size(), 2);
        }

        for (auto& itf : tmp_arr) {

          lst_feats.push_back(feature_OBJ(itf, flatten_coords, average_coordinates, to_list));

          if (average_coordinates && !to_list) {

            outer_avg.row(ITER_AVG) = flatten_coords_pr;

            flatten_coords_pr.clear();                       // each time it's called : it first assigns to arma::mat then clears the private-variable [ due to "inside-class-loop" ]

            ITER_AVG++;
          }
        }

        if (average_coordinates && !to_list) {

          flatten_coords_pr = arma::conv_to< arma::rowvec >::from(arma::mean(outer_avg, 0));
        }

        RES_feat_col["features"] = lst_feats;
      }
    }

    return RES_feat_col;
  }



  // geojson object : "FeatureCollection"     [ used in the 'schema' function ]
  //

  Rcpp::List feature_collection_geojson_schema(json11::Json input_obj, bool average_coordinates = false, bool to_list = false) {         // 'flatten_coords' and 'average_coordinates' of geojson object is needed (indirectly) in leaflet's "setView()"

    Rcpp::List RES_feat_col;

    for (auto& iter : input_obj.object_items()) {

      if (iter.first == "features") {

        Rcpp::List lst_feats;

        json11::Json::array tmp_arr = input_obj["features"].array_items();

        unsigned int ITER_AVG = 0;

        arma::mat outer_avg;

        if (average_coordinates && !to_list) {

          outer_avg.set_size(tmp_arr.size(), 2);
        }

        for (auto& itf : tmp_arr) {

          lst_feats.push_back(feature_OBJ_schema(itf, average_coordinates, to_list));        // In 'FeatureCollection' the property-name of each geometry-object is 'geometry'

          if (average_coordinates && !to_list) {

            outer_avg.row(ITER_AVG) = flatten_coords_pr;

            flatten_coords_pr.clear();                       // each time it's called : it first assigns to arma::mat then clears the private-variable [ due to "inside-class-loop" ]

            ITER_AVG++;
          }
        }

        if (average_coordinates && !to_list) {

          flatten_coords_pr = arma::conv_to< arma::rowvec >::from(arma::mean(outer_avg, 0));
        }

        RES_feat_col[iter.first] = lst_feats;
      }

      else {

        RES_feat_col[iter.first] = recursive_switch(iter.second);
      }
    }

    return RES_feat_col;
  }



  // check if input-file exists
  //

  bool file_exists(std::string fileName) {

    std::ifstream infile(fileName);

    return infile.good();
  }


  // parse the geo-json objects
  //

  json11::Json parse_geojson_objects(std::string input_data) {

    // if (!file_exists(input_data)) {                                                           # redundant, see the Rcpp-exported function
    //
    //   Rcpp::stop("the input file does not exist --> parse_geojson_objects() function");
    // }

    std::string data_in;

    std::fstream myfile(input_data, std::fstream::in);

    char chs;

    while (myfile >> std::noskipws >> chs) {

      data_in += chs;
    }

    std::string Error_Message;

    json11::Json json_input = json11::Json::parse(data_in, Error_Message, json11::JsonParse::COMMENTS);

    if (json_input.is_null()) {

      Rcpp::stop("The output json object is NULL! See if any of the input data objects is not a valid json data type!");

      if (!Error_Message.empty()) {

        Rcpp::Rcout << Error_Message << std::endl;
      }
    }

    return json_input;
  }


  // input a geojson character string (rather than a path to a file)
  //

  json11::Json parse_geojson_string(std::string character_string) {

    std::string Error_Message;

    json11::Json json_input = json11::Json::parse(character_string, Error_Message, json11::JsonParse::COMMENTS);

    if (json_input.is_null()) {

      Rcpp::stop("The output json object is NULL! See if any of the input data objects is not a valid json data type!");

      if (!Error_Message.empty()) {

        Rcpp::Rcout << Error_Message << std::endl;
      }
    }

    return json_input;
  }


  // helper function [ for 'export_From_geojson' and 'export_From_geojson_schema' ]
  //

  Rcpp::List helper_geom_objects(From_GeoJson_geometries prs, json11::Json tmp_prs, bool flatten_coords = false,

                                 bool average_coordinates = false, bool schema = false, bool to_list = false) {

    Rcpp::List RES_OUT;

    std::string res_type = tmp_prs["type"].string_value();

    int polygon_size = tmp_prs["coordinates"].array_items().size();

    if (res_type == "GeometryCollection") {

      RES_OUT = prs.geom_collection_OBJ(tmp_prs, average_coordinates, to_list);}

    else if (res_type == "Feature") {

      if (schema) {

        RES_OUT = prs.feature_OBJ_schema(tmp_prs, average_coordinates, to_list);}                              // use the modified version of 'feature_OBJ' function [ 'feature_OBJ_schema' ], which is not as strict concerning the 'RFC 7946'

      else {

        RES_OUT = prs.feature_OBJ(tmp_prs, flatten_coords, average_coordinates, to_list);                      // setting 'flatten_coords' to TRUE avoids 'properties' member recursive calculation
      }
    }

    else if (res_type == "FeatureCollection") {

      if (schema) {

        RES_OUT = prs.feature_collection_geojson_schema(tmp_prs, average_coordinates, to_list);}               // use the modified version of 'feature_collection_geojson' function [ 'feature_collection_geojson_schema' ], which is not as strict concerning the 'RFC 7946'

      else {

        RES_OUT = prs.feature_collection_geojson(tmp_prs, flatten_coords, average_coordinates, to_list);
      }
    }

    else if (res_type == "Point" || res_type == "LineString" || res_type == "MultiPoint" || (res_type == "Polygon" && polygon_size == 1)) {

      RES_OUT["type"] = res_type;

      RES_OUT["coordinates"] = prs.geom_OBJ(res_type, tmp_prs, polygon_size, average_coordinates, to_list)["unlist_OBJ"];}      // unlist object

    else if (res_type == "MultiLineString" || res_type == "MultiPolygon" || (res_type == "Polygon" && polygon_size > 1)) {

      RES_OUT["type"] = res_type;

      RES_OUT["coordinates"] = prs.geom_OBJ(res_type, tmp_prs, polygon_size, average_coordinates, to_list);}

    else {

      Rcpp::stop("Give a valid path to a '.geojson' file or a 'GeoJson' character string. Valid GeoJson objects are : 'Point', 'LineString', 'MultiPoint', 'Polygon', 'MultiLineString', 'MultiPolygon', 'GeometryCollection', 'Feature' and 'FeatureCollection' --> helper_geom_objects() function");
    }

    if (average_coordinates && !to_list) {

      RES_OUT["geometry_dump"] = tmp_prs.dump();

      RES_OUT["leaflet_view_coords"] = prs.return_COORDS();
    }

    return RES_OUT;
  }


  ~From_GeoJson_geometries() { }
};




//========================
// Rcpp-exported functions
//========================


//-----------------------
// FROM-GeoJson function
//-----------------------


// geometry-objects [ 'flatten_coords' AND 'average_coordinates' are used in leaflet's "setView()"  ['longitude' and 'latitude'] ]
//

// [[Rcpp::export]]
Rcpp::List export_From_geojson(std::string input_file, bool flatten_coords = false, bool average_coordinates = false, bool to_list = false) {

  From_GeoJson_geometries prs;

  json11::Json tmp_prs;

  if (prs.file_exists(input_file)) {                                                                                     // check if file exists

    tmp_prs = prs.parse_geojson_objects(input_file);}                                                                    // input is a path to a file

  else {

    tmp_prs = prs.parse_geojson_string(input_file);                                                                      // input is a geojson character string
  }

  Rcpp::List RES_OUT = prs.helper_geom_objects(prs, tmp_prs, flatten_coords, average_coordinates, false, to_list);                    // schema = false

  return RES_OUT;
}




// fully recursive extraction of data [ used in 'address_geocoding_nominatim()' AND 'reverse_geocoding_nominatim()' functions ]
//

// [[Rcpp::export]]
SEXP export_From_JSON(std::string input_file) {

  From_GeoJson_geometries prs;

  json11::Json tmp_prs;

  if (prs.file_exists(input_file)) {                        // check if file exists

    tmp_prs = prs.parse_geojson_objects(input_file);}       // input is a path to a file

  else {

    tmp_prs = prs.parse_geojson_string(input_file);         // input is a json character string
  }

  SEXP tmp = prs.recursive_switch(tmp_prs);

  return tmp;
}



// dump a geojson object
//

// [[Rcpp::export]]
std::string dump_geojson(std::string input_data) {

  From_GeoJson_geometries fgj;

  json11::Json tmp_gj = fgj.parse_geojson_objects(input_data);

  return tmp_gj.dump();
}


// FeatureCollection from geojson Feature files
//

// [[Rcpp::export]]
std::string Features_TO_Collection(std::vector<std::string> feat_files_lst,
                                   std::vector<double> bbox_vec,
                                   bool verbose = false) {

  From_GeoJson_geometries prs;

  json11::Json::array feat_col_array;

  for (unsigned int i = 0; i < feat_files_lst.size(); i++) {

    std::string input_file = feat_files_lst[i];

    if (verbose) Rcpp::Rcout << "File '" << input_file << "' will be processed ..." << std::endl;

    json11::Json tmp_prs = prs.parse_geojson_objects(input_file);

    feat_col_array.push_back(tmp_prs);
  }

  json11::Json Geom_Coll_OBJ = json11::Json::object {

    { "type", "FeatureCollection" },

    { "bbox", bbox_vec},

    { "features", feat_col_array },

  };

  return Geom_Coll_OBJ.dump();
}



// processes any geometry-object using a one-word-schema  --- appropriate for cases where the property-names do not match (exactly) the 'RFC 7946' specification [ such as in mongodb queries ]
//

// [[Rcpp::export]]
Rcpp::List export_From_geojson_schema(std::string input_file, std::string GEOMETRY_OBJECT_NAME = "", bool average_coordinates = false, bool to_list = false) {

  Rcpp::List RES_ALL;

  From_GeoJson_geometries prs;

  json11::Json tmp_prs;

  if (prs.file_exists(input_file)) {                                                                 // check if file exists

    tmp_prs = prs.parse_geojson_objects(input_file);}                                                // input is a path to a file

  else {

    tmp_prs = prs.parse_geojson_string(input_file);                                                  // input is a geojson character string
  }

  std::string type_col = tmp_prs["type"].string_value();                                             // check initially if the object is a 'Feature' OR a 'Feature-Collection'

  if (type_col == "Point" || type_col == "LineString" || type_col == "MultiPoint" ||

      type_col == "Polygon" || type_col == "GeometryCollection" ||

      type_col == "MultiLineString" || type_col == "MultiPolygon" ||

      type_col == "Feature" || type_col == "FeatureCollection") {

    RES_ALL = prs.helper_geom_objects(prs, tmp_prs, false, average_coordinates, true, to_list);}                  // first check that the .geojson object is not one of 'Point', 'Linestring', etc. [ here use 'schema' = true ]

  else {                                                                                             // otherwise [ if 'GEOMETRY_OBJECT_NAME' != "" ]:

    for (auto& ITEMS : tmp_prs.object_items()) {                                                     // loop over the property-names and if 'GEOMETRY_OBJECT_NAME' exists then use the 'vectorized-armadillo' version, ....

      if (ITEMS.first == GEOMETRY_OBJECT_NAME) {

        json11::Json INNER_ITEM = ITEMS.second;

        Rcpp::List RES_OUT;

        std::string res_type = INNER_ITEM["type"].string_value();

        int polygon_size = INNER_ITEM["coordinates"].array_items().size();

        if (res_type == "GeometryCollection") {

          RES_OUT = prs.geom_collection_OBJ(INNER_ITEM, average_coordinates, to_list);}

        else if (res_type == "Point" || res_type == "LineString" || res_type == "MultiPoint" || (res_type == "Polygon" && polygon_size == 1)) {

          RES_OUT["type"] = res_type;

          RES_OUT["coordinates"] = prs.geom_OBJ(res_type, INNER_ITEM, polygon_size, average_coordinates, to_list)["unlist_OBJ"];}      // unlist object

        else if (res_type == "MultiLineString" || res_type == "MultiPolygon" || (res_type == "Polygon" && polygon_size > 1)) {

          RES_OUT["type"] = res_type;

          RES_OUT["coordinates"] = prs.geom_OBJ(res_type, INNER_ITEM, polygon_size, average_coordinates, to_list);}

        else {

          Rcpp::stop("Give a valid path to a '.geojson' file or a 'GeoJson' character string. Valid GeoJson objects are : 'Point', 'LineString', 'MultiPoint', 'Polygon', 'MultiLineString', 'MultiPolygon', 'GeometryCollection' --> export_From_geojson_schema() function");
        }

        if (average_coordinates && !to_list) {

          RES_OUT["geometry_dump"] = INNER_ITEM.dump();

          RES_OUT["leaflet_view_coords"] = prs.return_COORDS();
        }

        RES_ALL[ITEMS.first] = RES_OUT;
      }

      else {

        RES_ALL[ITEMS.first] = prs.recursive_switch(ITEMS.second);                           // .... , otherwise [ which means for all other property-names ] do full recursion
      }
    }
  }

  return RES_ALL;
}



//==================================================================================== merge multiple json files to a single file


// returns the paths of files in a folder
// http://www.cplusplus.com/forum/unices/3548/
// (much faster than the base R list.files() function with default settings)

// [[Rcpp::export]]
std::vector<std::string> list_files( const std::string& path, bool full_path = true) {

  std::vector <std::string> result;

  dirent* de;

  DIR* dp;

  errno = 0;

  dp = opendir( path.empty() ? "." : path.c_str() );

  if (dp) {

    while (true) {

      errno = 0;

      de = readdir( dp );

      if (de == NULL) break;

      std::string tmp = std::string( de->d_name );

      int count = std::count_if(tmp.begin(), tmp.end(),[](char c){ return (std::isalnum(c)); });

      if (count > 0) {

        if (full_path) {

          std::string full_str = path + tmp;

          result.push_back( full_str );}

        else {

          result.push_back( tmp );
        }
      }
    }

    closedir( dp );

    std::sort( result.begin(), result.end() );
  }

  return result;
}



// read 'json-files' (or any kind of .txt file) from a directory and append it to an 'output-file'
// [ use concat to specify the position of each appended file ( newline, empty space etc. ) ]
//

// [[Rcpp::export]]
void merge_json(const std::string& input_folder, std::string output_file, std::string concat_delimiter = "\n", bool verbose = false) {

  arma::wall_clock timer;

  if (verbose) {

    timer.tic(); Rprintf("\n");
  }

  std::vector<std::string> all_files = list_files(input_folder, true);        // by default return full-paths

  if (all_files.empty()) {

    Rcpp::stop("the folder is empty");
  }

  std::ofstream out;

  out.open(output_file, std::ios::app);

  for (unsigned int i = 0; i < all_files.size(); i++) {

    std::string data_in;

    std::fstream myfile(all_files[i], std::fstream::in);

    char chs;

    while (myfile >> std::noskipws >> chs) {

      data_in += chs;
    }

    if (i == 0) {

      out << data_in;}

    else {

      out << concat_delimiter + data_in;
    }

    if (verbose) {

      std::string tns = std::to_string(all_files.size());

      const char* in_rpr = "\rnumber of files processed: %2d of ";

      char const* pchar = tns.c_str();

      char * RutaFinal = new char[strlen(in_rpr) + strlen(pchar) + 1];    // concatenation of const char* : https://stackoverflow.com/questions/8487337/how-to-concat-two-const-char
      strcpy(RutaFinal, in_rpr);
      strcat(RutaFinal, pchar);

      Rprintf(RutaFinal, i + 1);
    }
  }

  if (verbose) {

    double n = timer.toc();

    Rprintf("\ttotal.time.in.minutes: %.5f", n / 60.0);
  }

  out.close();
}





// This function takes advantage of the 'C-structures' in Rcpp to extract the data type of an object
// ( somehow difficult to do this currenty in C++11 )
// It works the same way as the "typeof_item" function (see 'TO_geojson.cpp' file for more info)
//
// https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json
// Those are C-structures [ http://adv-r.had.co.nz/C-interface.html ]
//

// [[Rcpp::export]]
std::string DATA_TYPE(SEXP sublist) {

  int length_item = LENGTH(sublist);

  std::string res;

  if (length_item == 1) {

    if (TYPEOF(sublist) == REALSXP) {                              // double
      res = "double";
    }
    else if (TYPEOF(sublist) == LGLSXP) {                          // boolean
      res = "boolean";
    }
    else if (TYPEOF(sublist) == STRSXP) {                          // std::string
      res = "string";
    }
    else if (TYPEOF(sublist) == NILSXP) {                          // NULL
      res = "null";
    }
    else if (TYPEOF(sublist) == INTSXP) {                          // integer
      res = "int";
    }
    else if (TYPEOF(sublist) == VECSXP) {                          // list
      res = "list";
    }
    else {
      Rcpp::stop("In case that the object is of length EQUAL to 1 it must be of type 'double', 'boolean', 'string', 'list' or 'null'");
    }
  }
  else {
    if (TYPEOF(sublist) == VECSXP) {
      res = "list";
    }
    else if (TYPEOF(sublist) == REALSXP) {              // normally this is an numeric-vector but matrices and arrays are recognized as such types too
      res = "NUMERIC_matrix_array_vector";
    }
    else if (TYPEOF(sublist) == INTSXP) {               // normally this is an integer-vector but matrices and arrays are recognized as such types too
      res = "INTEGER_matrix_array_vector";
    }
    else {
      Rcpp::stop("In case that the object is of length greater than 1 it must be either an Rcpp::List or a Matrix!");
    }
  }

  return res;
}




// inner function for 'SAVE_R_list_Features_2_FeatureCollection'
// which concatenates the coordinates
//

// [[Rcpp::export]]
std::string inner_coords(Rcpp::List geom_lst, int z, bool polygon_interior = false) {

  Rcpp::NumericMatrix geom_mt = Rcpp::as<Rcpp::NumericMatrix>(geom_lst[z]);             // this matrix will have 2 columns (longitude and latitude)
  std::string geom_str_poly;

  for (int w = 0; w < geom_mt.nrow(); w++) {

    Rcpp::NumericVector tmp_inner = geom_mt.row(w);

    if (tmp_inner.size() != 2) {
      Rcpp::stop("The input data MUST have 2 columns ('latitude' and 'longitude')!");
    }

    double first_item = tmp_inner[0];
    double second_item = tmp_inner[1];

    if (w == 0) {
      if (polygon_interior) {
        geom_str_poly += "[";
      }
      else {
        geom_str_poly += "[[";
      }
    }

    geom_str_poly += "[" + std::to_string(first_item) + "," + std::to_string(second_item) + "]";

    if (w < geom_mt.nrow() - 1) {
      geom_str_poly += ",";
    }
    if (w == geom_mt.nrow() - 1) {
      if (polygon_interior) {
        geom_str_poly += "]";
      }
      else {
        geom_str_poly += "]]";
      }
    }
  }

  return geom_str_poly;
}




// inner function for 'SAVE_R_list_Features_2_FeatureCollection' which
// deals with Polygons with interior rings
//

// [[Rcpp::export]]
std::string Polygon_with_interior_rings(Rcpp::List geom_lst, int i, bool verbose) {

  if (verbose) Rcpp::Rcout << "Input Feature: " << i+1 << "  --  POLYGON with INTERIOR Rings!" << std::endl;
  Rcpp::List Poly_interior = geom_lst[0];
  std::vector<std::string> all_interiors;
  std::string coords;

  for (unsigned int m = 0; m < Poly_interior.size(); m++) {

    std::string geom_str_poly = inner_coords(Poly_interior, m, true);
    all_interiors.push_back(geom_str_poly);

    if (m != (Poly_interior.size() - 1)) {
      all_interiors.push_back(", ");
    }
  }

  coords += "[";

  for (unsigned int n = 0; n < all_interiors.size(); n++) {
    coords += all_interiors[n];
  }

  coords += "]";

  return coords;
}




// This function takes GeoJson Features in form of R lists and write those Features to a 'FeatureCollection'
//

// [[Rcpp::export]]
std::string SAVE_R_list_Features_2_FeatureCollection(Rcpp::List x, std::string path_to_file = "", bool verbose = false) {

  std::vector<std::string> allowed_attributes = {"type", "id", "properties", "geometry"};
  std::sort(allowed_attributes.begin(), allowed_attributes.end());                                // sort both the 'allowed_attributes' and 'input_attrubutes' so that 'std::equal' can function properly
  std::map<int, std::map<std::string, std::string> > OUTER_map;

  for (unsigned int i = 0; i < x.size(); i++) {                                                   // 'x' is a list of 'Features'

    Rcpp::List sublist = x[i];

    Rcpp::CharacterVector nams_sublist = sublist.names();
    std::vector<std::string> input_attrubutes = Rcpp::as<std::vector<std::string> >(nams_sublist);
    std::sort(input_attrubutes.begin(), input_attrubutes.end());
    bool result = std::equal(input_attrubutes.begin(), input_attrubutes.end(), allowed_attributes.begin());
    if (!result) Rcpp::stop("The input lists / sublists must include the following names: 'type', 'id', 'properties' and 'geometry'!");

    std::map<std::string, std::string> INNER_map;

    for (unsigned int j = 0; j < nams_sublist.size(); j++) {

      std::string inner_name = Rcpp::as<std::string>(nams_sublist[j]);
      SEXP sexp_obj = sublist[inner_name];

      if (verbose) Rcpp::Rcout << "Input Feature: " << i+1 << "  --  Attribute: '" << inner_name << "'" << "  --  Data type: '" << DATA_TYPE(sexp_obj) << "'" << std::endl;

      if (inner_name == "type") {
        if (DATA_TYPE(sexp_obj) == "string") {

          std::string inner_data = sublist[inner_name];

          if (inner_data != "Feature") {
            Rcpp::stop("In case that the name of the input object equals to 'type' then it MUST be a 'Feature' attribute! Sublist:" + std::to_string(i+1) + " type:" + std::to_string(j+1));
          }
          else {
            INNER_map["type"] = "\"type\":\"Feature\"";                                           // add quotes to an std::string [ https://stackoverflow.com/a/12338826/8302386 ]
          }
        }
        else {
          Rcpp::stop("In case that the name of the input object equals to 'type' then it MUST be a character string! Sublist:" + std::to_string(i+1) + " type:" + std::to_string(j+1));
        }
      }
      else if (inner_name == "id") {

        std::string ID;

        if (DATA_TYPE(sexp_obj) == "string") {
          std::string inner_data = sublist[inner_name];
          ID = inner_data;
          INNER_map["id"] = "\"id\":\"" + inner_data + "\"";
        }
        else if (DATA_TYPE(sexp_obj) == "int") {
          int inner_data = sublist[inner_name];
          std::string conv_to_str = std::to_string(inner_data);
          ID = conv_to_str;
          INNER_map["id"] = "\"id\":" + conv_to_str;
        }
        else {
          Rcpp::stop("In case that the name of the input object equals to 'id' then it MUST be either a character string OR an integer! Sublist:" + std::to_string(i+1) + " id:" + std::to_string(j+1));
        }
        if (verbose) Rcpp::Rcout << "Input Feature: " << i+1 << "  --  'id' attribute-NAME: '" << ID << "'" << std::endl;
      }
      else if (inner_name == "properties") {

        if (DATA_TYPE(sexp_obj) == "list") {

          Rcpp::List inner_props = sublist[inner_name];
          Rcpp::CharacterVector nams_props = inner_props.names();
          std::vector<std::string> props_vec;

          for (unsigned int k = 0; k < nams_props.size(); k++) {

            std::string inner_name_props = Rcpp::as<std::string>(nams_props[k]);
            SEXP sexp_prop = inner_props[inner_name_props];
            std::string tmp_prop;

            if (DATA_TYPE(sexp_prop) == "string") {
              std::string inner_prop_str = inner_props[inner_name_props];
              tmp_prop = "\"" + inner_name_props + "\"" + ":" + "\"" + inner_prop_str + "\"";
            }
            else if (DATA_TYPE(sexp_prop) == "double") {
              double tmp_double = inner_props[inner_name_props];
              tmp_prop = "\"" + inner_name_props + "\"" + ":" + std::to_string(tmp_double);
            }
            else if (DATA_TYPE(sexp_prop) == "int") {
              int tmp_int = inner_props[inner_name_props];
              tmp_prop = "\"" + inner_name_props + "\"" + ":" + std::to_string(tmp_int);
            }
            else {
              Rcpp::stop("In case that the name of the input object equals to 'properties' then each item in 'properties' MUST be of type: 'string', 'double' or 'int'! Sublist:" + std::to_string(i+1) + " properties:" + std::to_string(j+1) + " properties-item:" + std::to_string(k+1));
            }

            if (k != nams_props.size() - 1) {                                // add trailing comma except for the last item
              tmp_prop += ", ";
            }
            props_vec.push_back(tmp_prop);
          }

          std::string props_out;
          for (unsigned int t = 0; t < props_vec.size(); t++) {
            if (t == 0) {
              props_out += "{";                                              // add an opening curly brace at the beginning
            }

            props_out += props_vec[t];                                       // in any case add the item to the string

            if (t == props_vec.size() - 1) {
              props_out += "}";                                              // add a closing curly brace at the end
            }
          }
          INNER_map["properties"] = "\"properties\":" + props_out;
        }
        else {
          Rcpp::stop("In case that the name of the input object equals to 'properties' then it MUST be a list object! Sublist:" + std::to_string(i+1) + " properties:" + std::to_string(j+1));
        }
      }
      else if (inner_name == "geometry") {

        std::string type_geom;
        std::string coords;

        if (DATA_TYPE(sexp_obj) == "list") {
          Rcpp::List inner_geom = sublist[inner_name];
          Rcpp::CharacterVector nams_geom = inner_geom.names();

          for (unsigned int s = 0; s < nams_geom.size(); s++) {

            std::string inner_name_geom = Rcpp::as<std::string>(nams_geom[s]);
            SEXP sexp_geom = inner_geom[inner_name_geom];

            if (nams_geom[s] == "type") {
              std::string tmp_geom_type = inner_geom[inner_name_geom];

              if (DATA_TYPE(sexp_geom) == "string") {                       // account for all cases: 'Polygon', 'POLYGON', 'MultiPolygon', 'MULTIPOLYGON'
                if (tmp_geom_type == "Polygon") {
                  type_geom = "\"type\":\"Polygon\"";
                }
                else if (tmp_geom_type == "POLYGON") {
                  type_geom = "\"type\":\"POLYGON\"";
                }
                else if (tmp_geom_type == "MultiPolygon") {
                  type_geom = "\"type\":\"MultiPolygon\"";
                }
                else if (tmp_geom_type == "MULTIPOLYGON") {
                  type_geom = "\"type\":\"MULTIPOLYGON\"";
                }
                else {
                  Rcpp::stop("In case that the name of the input object equals to 'geometry' then the 'type' MUST be either a 'Polygon' or a 'MultiPolygon'! Sublist:" + std::to_string(i+1) + " geometry:" + std::to_string(j+1));
                }
              }
              else {
                Rcpp::stop("In case that the name of the input object equals to 'geometry' then the 'type' MUST be either a character string! Sublist:" + std::to_string(i+1) + " geometry:" + std::to_string(j+1));
              }
            }
            else if (nams_geom[s] == "coordinates") {

              if (DATA_TYPE(sexp_geom) == "list") {

                Rcpp::List geom_lst = inner_geom[inner_name_geom];

                if (geom_lst.size() == 1) {                                                                      // 1st. case: If the list is of length 1, I expect that the Geometry is a Polygon with Interior rings (such as Countries with enclaves, see South Africa & Lesotho or Italy & (Vatican, San Marino)). This list will include 2 or more numeric matrices.

                  coords = Polygon_with_interior_rings(geom_lst, i, verbose);
                }
                else {                                                                                           // 2nd. case: Here I expect that the Geometry is a Multipolygon (a list of length > 1, where each sublist is a numeric matrix)

                  if (verbose) Rcpp::Rcout << "Input Feature: " << i+1 << "  --  MULTIPOLYGON!!" << std::endl;

                  for (unsigned int z = 0; z < geom_lst.size(); z++) {

                    SEXP sexp_inner_geom = geom_lst[z];

                    std::string geom_str_poly;

                    if (DATA_TYPE(sexp_inner_geom) == "list") {

                      geom_str_poly = Polygon_with_interior_rings(geom_lst, i, verbose);
                    }
                    else if (DATA_TYPE(sexp_inner_geom) == "NUMERIC_matrix_array_vector") {

                      geom_str_poly = inner_coords(geom_lst, z, false);
                    }
                    else {
                      Rcpp::stop("Invalid Geometry object in case of MULTIPOLYGON! It must be either of type 'list' or 'numeric matrix'!");
                    }

                    if (z == 0) {
                      coords += "[";
                    }
                    if (z != geom_lst.size() - 1) {
                      coords += geom_str_poly + ",";
                    }
                    else {
                      coords += geom_str_poly;
                    }
                    if (z == geom_lst.size() - 1) {
                      coords += "]";
                    }
                  }
                }
              }
              else if (DATA_TYPE(sexp_geom) == "NUMERIC_matrix_array_vector") {

                if (verbose) Rcpp::Rcout << "Input Feature: " << i+1 << "  --  POLYGON WITHOUT INTERIOR Rings!" << std::endl;

                Rcpp::NumericMatrix geom_mt_single = Rcpp::as<Rcpp::NumericMatrix>(inner_geom[inner_name_geom]);             // this matrix will have 2 columns (longitude and latitude)
                std::string geom_str_poly_single;

                for (int w1 = 0; w1 < geom_mt_single.nrow(); w1++) {

                  Rcpp::NumericVector tmp_inner = geom_mt_single.row(w1);

                  if (tmp_inner.size() != 2) {
                    Rcpp::stop("The input data MUST have 2 columns ('latitude' and 'longitude')!");
                  }

                  double first_item1 = tmp_inner[0];
                  double second_item1 = tmp_inner[1];

                  if (w1 == 0) {
                    coords += "[[";
                  }

                  coords += "[" + std::to_string(first_item1) + "," + std::to_string(second_item1) + "]";

                  if (w1 < geom_mt_single.nrow() - 1) {
                    coords += ",";
                  }
                  if (w1 == geom_mt_single.nrow() - 1) {
                    coords += "]]";
                  }
                }
              }
              else {
                Rcpp::stop("In case that the name of the input object equals to 'geometry' then the 'coordinates' attribute MUST be either a list OR a NUMERIC matrix (of type double because the coordinates are normally latitudes and longitudes)! Sublist:" + std::to_string(i+1) + " geometry:" + std::to_string(j+1));
              }
            }
            else {
              Rcpp::stop("In case that the name of the input object equals to 'geometry' then it MUST be a list object! Sublist:" + std::to_string(i+1) + " geometry:" + std::to_string(j+1));
            }
          }
        }
        else {
          Rcpp::stop("In case that the name of the input object equals to 'geometry' then it MUST be a list object! Sublist:" + std::to_string(i+1) + " geometry:" + std::to_string(j+1));
        }

        INNER_map["geometry"] = "\"geometry\":{" + type_geom + ", \"coordinates\":" + coords + "}";
      }
    }

    OUTER_map[i] = INNER_map;
  }

  std::string feature_collection_str = "{\"type\":\"FeatureCollection\",\"features\":[\n";

  for(unsigned int iter = 0; iter < OUTER_map.size(); iter++) {

    std::map<std::string, std::string> this_map = OUTER_map[iter];

    std::string this_inner_map = "{";

    unsigned int count = 0;

    for(auto iter_in : this_map) {

      if (count < this_map.size() - 1) {
        this_inner_map += iter_in.second + ", ";
      }
      else {
        this_inner_map += iter_in.second;
      }
      count += 1;
    }

    this_inner_map += "}";

    if (iter != OUTER_map.size() - 1) {
      this_inner_map += ",\n";
    }

    feature_collection_str += this_inner_map;
  }

  feature_collection_str += "]}";


  if (path_to_file != "") {

    std::ofstream out(path_to_file);
    out << feature_collection_str;
    out.close();
  }

  return feature_collection_str;
}


