
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

