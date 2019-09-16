appFrame 0.1.8
==============
* removed banner content and set banner switch to FALSE (don't
  display).
  
appFrame 0.1.7
==============
* added optional banner display to headers immediately beneath NWFSC, NOAA logos.

* modified appFrameHeaderFixed() and appFrameHeaderScrolling() to accept boolean switch controlling banner display.

* added banner.html containing HTTP-HTTPS banner content.

* changed footer links to https where currently available.

appFrame 0.1.6
==============
* specified customer site survey URL in footers.

appFrame 0.1.5
==============
* increased z-index of non-scrolling headers and footers to avoid some Shiny elements overlaying them.

* moved header class styles from footer.css to header.css

appFrame 0.1.4
==============

* added contactURL and displayAppsURL arguments to footer creation functions to allow for specifying contact and app-list links.

appFrame 0.1.3
==============

* added overlapHeight arguments to header creation functions to provide control over where the top of the content begins.

appFrame 0.1.2
==============

* Replaced NOAA logo with recommended NOAA/NWFSC image.


appFrame 0.1.1
==============

* Adjusted headers and footers to extend completely across browser window rather than aligning with Shiny app boundaries (thus leaving whitespace at left and right ends).

* Added dataexplorer.nwfsc.noaa.gov link to "Display apps" footer link.

* Added www.nwfsc.noaa.gov/contact/feedback.cfm link to "Problems with site?" footer link.

* Added README.md

* Added NEWS.md
