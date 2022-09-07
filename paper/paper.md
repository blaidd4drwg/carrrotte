---
title: 'carrrotter: An R package to interactively inspect and process Avaatech XRF Corescanner data'
tags:
  - R
  - limnogeology
  - sedimentology
  - XRF core scanning
  - paleoclimate
authors:
  - name: Remo L. Roethlin
    orcid: 0000-0001-8449-9600
    equal-contrib: true
    corresponding: true
    affiliation: "1, 2"
  - name: Ronald Lloren
    orcid: 0000-0003-3001-6647
    affiliation: "1, 2"
  - name: Nathalie Dubois
    orcid: 0000-0003-2349-0826
    affiliation: "1, 2"
affiliations:
 - name: Department of Surface Waters Research and Management, Eawag, Überlandstrasse 133, Dübendorf, 8600, Zurich, Switzerland
   index: 1
 - name: Department of Earth Sciences, ETH Zurich, Sonneggstrasse 5, Zurich, 8092, Zurich, Switzerland
   index: 2
date: 6 September 2022
bibliography: paper.bib
---

# Summary

![](carrrotter.svg){ width=120px }

Marine and lacustrine sediment cores are used to a great extent in environmental sciences to reconstruct past environmental conditions, such as climate, ecological changes or pollution histories [@ellis:2019; @longman:2019; @jones:2019]. A few of the many biogeochemical analyses routinely used on sediment samples specifically aim to study whole sediment cores. One of these techniques is X-ray fluorescence core-scanning [@croudace:2019]. XRF core scanning is a non-destructive method to analyse the elementary composition of sediment core half surfaces over the whole length. Mainly two types of these devices are used in research: One produced by the company Avaatech [@richter:2006; @hennekam:2019] and one by the company Itrax [@lowemark:2019].

This package aims to facilitate parsing the export files the Avaatech bAXIL software produces (one per tube voltage usually) and to convert them into one tidy data frame [@wickham:2019].
Additionally, this package includes an R Shiny web app that allows for interactive analyses of the export data. It also allows checking the goodness of fit and the relative error of the fitting process and removing outliers if necessary. The resulting data (with an overview of removed data) can then be exported.


# Statement of need

`carrrotter` is an R package that lets scientists parse their Avaatech XRF data and inspect it quickly. 
One common issue with core scanner data is the sheer size of information generated: Depending on the number of sediment cores (or sections) scanned and models used, scientists quickly end up with many untidy files. Checking the quality of XRF core scanning data manually is tedious and error-prone.
The web app allows scientists to check the quality of the measured data instantly by inspecting every element for a core at once.
 
While a free software to process and calibrate Avaatech XRF data exists under the name Xelerate [@lowemark:2019], it hasn't been updated in years and is not open-source.
Moreover, GNU R is widely used in environmental and life sciences for statistical analyses and visualisation, and the `carrrotter` package allows scientists to follow a reproducible workflow. By bundling the interactive web app with the parser function, the package can be used by scientists that do not have previous experience in R and those that do alike.

The web app contained in this package has previously been tested and used informally by scientists working with an Avaatech XRF core scanner.


# Acknowledgements

This software would not be possible without the helpful inputs of scientists at Eawag and the limnogeology community. We also thank Andreas Halter for his artwork/logo.

# References


