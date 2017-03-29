#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP geojsonR_dump_geojson(SEXP);
extern SEXP geojsonR_export_From_geojson(SEXP, SEXP, SEXP);
extern SEXP geojsonR_export_From_JSON(SEXP);
extern SEXP geojsonR_export_To_GeoJson(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP geojsonR_Feature_collection_Obj(SEXP, SEXP, SEXP);
extern SEXP geojsonR_Feature_Obj(SEXP, SEXP, SEXP);
extern SEXP geojsonR_Features_TO_Collection(SEXP, SEXP);
extern SEXP geojsonR_Geom_Collection(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"geojsonR_dump_geojson",           (DL_FUNC) &geojsonR_dump_geojson,           1},
    {"geojsonR_export_From_geojson",    (DL_FUNC) &geojsonR_export_From_geojson,    3},
    {"geojsonR_export_From_JSON",       (DL_FUNC) &geojsonR_export_From_JSON,       1},
    {"geojsonR_export_To_GeoJson",      (DL_FUNC) &geojsonR_export_To_GeoJson,      6},
    {"geojsonR_Feature_collection_Obj", (DL_FUNC) &geojsonR_Feature_collection_Obj, 3},
    {"geojsonR_Feature_Obj",            (DL_FUNC) &geojsonR_Feature_Obj,            3},
    {"geojsonR_Features_TO_Collection", (DL_FUNC) &geojsonR_Features_TO_Collection, 2},
    {"geojsonR_Geom_Collection",        (DL_FUNC) &geojsonR_Geom_Collection,        3},
    {NULL, NULL, 0}
};

void R_init_geojsonR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
