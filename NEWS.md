
## geojsonR 1.0.8

* I've added the *save_R_list_Features_2_FeatureCollection* function and I've modified the *typeof_item* method of the *GeoJson_Collections* C++ class to accept also input objects of type integer.


## geojsonR 1.0.7

* I've added the *write_path* and *verbose* parameters to the *Features_2Collection* function
* I've added error handling in the *parse_geojson_string* and *parse_geojson_objects* methods of the *From_GeoJson_geometries* C++ class to account for cases where the output of the *json11::Json::parse* function is a NULL object ( I've included also a test case for demonstration )


## geojsonR 1.0.6

I fixed minor typos in the .cpp files


## geojsonR 1.0.5

I modified the R functions which made a connection to Url's  (previously the connection was not closed in an appropriate way producing that way a warning)


## geojsonR 1.0.4

I modified the *Makevars* files to allow *OpenMP* usage. The *geojsonR* package is not parallelized, however the *Armadillo* library uses *OpenMP* internally to improve the execuction time of the functions.


## geojsonR 1.0.3

I added the *merge_files* function.


## geojsonR 1.0.2

I added the *FROM_GeoJson_Schema* function. This function is appropriate when the property-names do not match exactly the *RFC 7946* specification ( for instance if the *geometry-object-name* appears as *location*, as is the case sometimes in mongodb queries ). This way one can avoid unnecessary errors when reading *geojson* files/strings.


## geojsonR 1.0.1

I added a Vignette and corrected mistakes of the examples in the documentation


## geojsonR 1.0.0

