
[![tic](https://github.com/mlampros/geojsonR/workflows/tic/badge.svg?branch=master)](https://github.com/mlampros/geojsonR/actions)
[![codecov.io](https://codecov.io/github/mlampros/geojsonR/coverage.svg?branch=master)](https://codecov.io/github/mlampros/geojsonR?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geojsonR)](http://cran.r-project.org/package=geojsonR)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/geojsonR?color=blue)](http://www.r-pkg.org/pkg/geojsonR)
<a href="https://www.buymeacoffee.com/VY0x8snyh" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>
[![Dependencies](https://tinyverse.netlify.com/badge/geojsonR)](https://cran.r-project.org/package=geojsonR)


## geojsonR
<br>

The **geojsonR** package includes functions for processing [GeoJson objects](https://en.wikipedia.org/wiki/GeoJSON) relying on [RFC 7946](https://datatracker.ietf.org/doc/html/rfc7946). The geojson encoding is based on [json11](https://github.com/dropbox/json11), a tiny JSON library for C++11. Furthermore, the source code is exported in R through the *Rcpp* and *RcppArmadillo* packages. More details on the functionality of geojsonR can be found in the [blog-post](http://mlampros.github.io/2017/03/29/geojsonR_package/) and in the package Vignette.
<br><br>

To install the package from CRAN use, 

```R

install.packages("geojsonR")


```
<br>

and to download the latest version from Github use the *install_github* function of the *remotes* package,
<br><br>

```R

remotes::install_github('mlampros/geojsonR')


```
<br>

Use the following link to report bugs/issues,
<br><br>

[https://github.com/mlampros/geojsonR/issues](https://github.com/mlampros/geojsonR/issues)

<br>

### **Citation:**

If you use the code of this repository in your paper or research please cite both **geojsonR** and the **original articles / software** `https://CRAN.R-project.org/package=geojsonR`:

<br>

```R
@Manual{,
  title = {{geojsonR}: A GeoJson Processing Toolkit},
  author = {Lampros Mouselimis},
  year = {2021},
  note = {R package version 1.1.2},
  url = {https://CRAN.R-project.org/package=geojsonR},
}
```

<br>
