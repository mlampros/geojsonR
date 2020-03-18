#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _geojsonR_DATA_TYPE(SEXP);
extern SEXP _geojsonR_dump_geojson(SEXP);
extern SEXP _geojsonR_export_From_geojson(SEXP, SEXP, SEXP, SEXP);
extern SEXP _geojsonR_export_From_geojson_schema(SEXP, SEXP, SEXP, SEXP);
extern SEXP _geojsonR_export_From_JSON(SEXP);
extern SEXP _geojsonR_export_To_GeoJson(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _geojsonR_Feature_collection_Obj(SEXP, SEXP, SEXP);
extern SEXP _geojsonR_Feature_Obj(SEXP, SEXP, SEXP);
extern SEXP _geojsonR_Features_TO_Collection(SEXP, SEXP, SEXP);
extern SEXP _geojsonR_Geom_Collection(SEXP, SEXP, SEXP);
extern SEXP _geojsonR_inner_coords(SEXP, SEXP, SEXP);
extern SEXP _geojsonR_list_files(SEXP, SEXP);
extern SEXP _geojsonR_merge_json(SEXP, SEXP, SEXP, SEXP);
extern SEXP _geojsonR_Polygon_with_interior_rings(SEXP, SEXP, SEXP);
extern SEXP _geojsonR_SAVE_R_list_Features_2_FeatureCollection(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_geojsonR_DATA_TYPE",                                (DL_FUNC) &_geojsonR_DATA_TYPE,                                1},
    {"_geojsonR_dump_geojson",                             (DL_FUNC) &_geojsonR_dump_geojson,                             1},
    {"_geojsonR_export_From_geojson",                      (DL_FUNC) &_geojsonR_export_From_geojson,                      4},
    {"_geojsonR_export_From_geojson_schema",               (DL_FUNC) &_geojsonR_export_From_geojson_schema,               4},
    {"_geojsonR_export_From_JSON",                         (DL_FUNC) &_geojsonR_export_From_JSON,                         1},
    {"_geojsonR_export_To_GeoJson",                        (DL_FUNC) &_geojsonR_export_To_GeoJson,                        6},
    {"_geojsonR_Feature_collection_Obj",                   (DL_FUNC) &_geojsonR_Feature_collection_Obj,                   3},
    {"_geojsonR_Feature_Obj",                              (DL_FUNC) &_geojsonR_Feature_Obj,                              3},
    {"_geojsonR_Features_TO_Collection",                   (DL_FUNC) &_geojsonR_Features_TO_Collection,                   3},
    {"_geojsonR_Geom_Collection",                          (DL_FUNC) &_geojsonR_Geom_Collection,                          3},
    {"_geojsonR_inner_coords",                             (DL_FUNC) &_geojsonR_inner_coords,                             3},
    {"_geojsonR_list_files",                               (DL_FUNC) &_geojsonR_list_files,                               2},
    {"_geojsonR_merge_json",                               (DL_FUNC) &_geojsonR_merge_json,                               4},
    {"_geojsonR_Polygon_with_interior_rings",              (DL_FUNC) &_geojsonR_Polygon_with_interior_rings,              3},
    {"_geojsonR_SAVE_R_list_Features_2_FeatureCollection", (DL_FUNC) &_geojsonR_SAVE_R_list_Features_2_FeatureCollection, 3},
    {NULL, NULL, 0}
};

void R_init_geojsonR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
