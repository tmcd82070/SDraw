SDraw 2.1.13 (Published: 2020-07-03)
==============

Changes: 
* Added 'xz' compression of .RDA data frames


SDraw 2.1.12 (Published: 2020-06-30)
==============

Changes:
* Bug fix in test_that code. Version 2.1.11 failed on 
Linux systems after change in GEOS package. 
New version resolves: 1. Failure: bas.point() 
operates appropriately (@test-bas.point.R#19)
testSamp <- bas.point(spdf, 20) produced warnings.
  

SDraw 2.1.11 (Published: 2020-03-31)
==============

Changes:

* Resolved 'stringsAsFactors' issue

SDraw 2.1.10 (Unpublished: 2020-03-27)
==============

Changes:

* Fixed time-out of grts.line and grts.polygon 
examples. 
* Updated .yml file 
* Bug fix in voronoi.polygons
* Updated link in WY documentation


SDraw 2.1.9 (Unpublished: 2019-02-20; commit 31c270)
==============

* Stefan Emmons added many unit tests
* Added Stefan Emmons to author string
* Bug fixes
* Added bounding box arg to voronoi.polygons

SDraw 2.1.8 (Released: 2019-03-07)
==============

Changes:

* Fixed DOI references in DESCRIPTION

SDraw 2.1.7 (Unpublished: 2019-03-05; commit 870e7e08fa)
==============

Changes:

* Added references to DESCRIPTION
* Fixed author field in DESCRIPTION

SDraw 2.1.6 (Unpublished: 2019-03-05)
==============

Changes:

* Fixed uncompressed data issue.


SDraw 2.1.5 (Unpublished: 2019-03-04)
==============

Changes:

* Added Halton Iterative Partitioning (HIP) sampling capability
