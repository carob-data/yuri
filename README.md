# yuri

"yuri" is an *R* package to download research data for use in reproducible workflows. The data are refreferred to with a [URI](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier) persistent identfier such as a [DOI](https://en.wikipedia.org/wiki/Digital_object_identifier) or [HDL](https://en.wikipedia.org/wiki/Handle_System). Repositories supported include those based on the [Harvard Dataverse](https://dataverse.org/) and [CKAN](https://ckan.org/) systems, and [Dryad](https://datadryad.org/) and [Zenodo](https://zenodo.org/). Let us know if you would like us to include this functionality for additional data repositories. And please [raise an issue](https://github.com/carob-data/yuri/issues) if you find a bug or would like to request a new feature.

You can install "yuri" with: 

```
remotes::install_github("carob-data/yuri")
```

And you can use it like this. 

```{r}
path <- "d:/yuri"
uri <- "doi:10.7910/DVN/HDKKAL"
ff <- yuri::dataURI(uri, path)
ff
#[1] "d:/yuri/doi_10.7910_DVN_HDKKAL/AgMIP_Wheat_Avignon_full_vPublish.xlsx" 
#[2] "d:/yuri/doi_10.7910_DVN_HDKKAL/AgMIP_Wheat_Bushland_full_vPublish.xlsx"
```

You can now use these files in your workflow. To make sure that they are accessible across different computers (with different path names) you can refer to them like this:
 

```
f1 <- ff[basename(ff) == "AgMIP_Wheat_Avignon_full_vPublish.xlsx"]
f2 <- ff[basename(ff) == "AgMIP_Wheat_Bushland_full_vPublish.xlsx"]
```

You can extract some of the metadata with 

```
m <- yuri::extract_metadata(uri, path)
t(m)
#                  [,1]
#uri               "doi:10.7910/DVN/HDKKAL"
#dataset_id        "doi_10.7910_DVN_HDKKAL"
#license           "CC-BY-4.0"
#title             "Wheat crop water use in semi-arid and Mediterranean environments"
#authors           "Cooke, Diane; White, Jeff; Chanzy, André; Evett, Steve; Webber, Heidi"
#publication       "NA"
#data_published    "2025-04-30"
#data_organization "Leibniz Centre for Agricultural Landscape Research (ZALF) e.V.; Agricultural and Biological Engineering Dept., University of Florida, Gainesville, USA; UMR EMMAH, INRAE- Avignon Université, Avignon, France; USDA ARS, Conservation and Production Research Laboratory, USDA-ARS, Bushland, USA; Leibniz Centre for Agricultural Landscape Research (ZALF)"
#data_publisher    "Harvard Dataverse"
#version           "1.0"
#description       "Two sets of wheat field experimental data and associated management practices from Bushland, Texas, USA and Avignon, France are provided. These datasets were selected based on completeness of the data and availability of data providers to clarify interpretation of data and obtain additional information, especially as related to crop management and soil characteristics. The field experiments differ in their climate, soil and individual study designs. The first is a lysimeter experiment from Bushland, Texas, U.S.A. The site has a semi-arid climate and Pullman Clay Loam soil. Data are available from three years, with two treatment fields planted each year, characterized by either full or partial irrigation (irrigated for crop establishment). Different winter bread wheat (Triticum aestivum) cultivars were planted each year, but remained the same between treatments within the same year. The second dataset is from Avignon, France, a site with a Mediterranean climate. The soil is a calcaric fluvisol. Crops were largely rainfed, though some years received small supplemental irrigation to ensure crop establishment. The field was sown with a succession of winter and summer crops over twelve years, including six durum wheat (T. turgidum ssp. durum) crops. Cultivar mostly varied between years, with four cultivars used over the course of the experiment. We selected data from 4 of these years, having excluded two years data due to poor emergence and incompatibility of the phenology data collected with that required for model calibration."
#design            NA                
#data_citation     "Cooke, Diane; White, Jeff; Chanzy, André; Evett, Steve; Webber, Heidi (2025). Wheat crop water use in semi-arid and Mediterranean environments. Harvard Dataverse. Version 1.0. doi:10.7910/DVN/HDKKAL"       
```

To be able to read the description.

```
cat(stringr::str_wrap(m$description, 100))
#Two sets of wheat field experimental data and associated management practices from Bushland,
#Texas, USA and Avignon, France are provided. These datasets were selected based on completeness
#of the data and availability of data providers to clarify interpretation of data and obtain
#additional information, especially as related to crop management and soil characteristics. The field
#experiments differ in their climate, soil and individual study designs. The first is a lysimeter
#experiment from Bushland, Texas, U.S.A. The site has a semi-arid climate and Pullman Clay Loam soil.
#Data are available from three years, with two treatment fields planted each year, characterized by
#either full or partial irrigation (irrigated for crop establishment). Different winter bread wheat
#(Triticum aestivum) cultivars were planted each year, but remained the same between treatments
#within the same year. The second dataset is from Avignon, France, a site with a Mediterranean
#climate. The soil is a calcaric fluvisol. Crops were largely rainfed, though some years received
#small supplemental irrigation to ensure crop establishment. The field was sown with a succession of
#winter and summer crops over twelve years, including six durum wheat (T. turgidum ssp. durum) crops.
#Cultivar mostly varied between years, with four cultivars used over the course of the experiment.
#We selected data from 4 of these years, having excluded two years data due to poor emergence and
#incompatibility of the phenology data collected with that required for model calibration.> 
```
 
