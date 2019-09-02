<p align="center"><a href="http://193.146.75.235/sample-apps/final_apps/portada"><img src="https://github.com/ordanovich/images/blob/master/webportal_overview.gif?raw=true"></a></p>
<p align="center"><a href="http://longpop-itn.eu/"><img src="https://github.com/ordanovich/images/blob/master/logo3inline_small.png?raw=true"></a></p>

## Interactive application for multivariate mapping based on the data retrieved from the [INEbase](https://www.ine.es/dyngs/INEbase/listaoperaciones.htm)

Please refer to the source to get more information on how the [Web Services](https://www.ine.es/dyngs/DataLab/en/manual.html?cid=45) function and how the [requests for data retrieval](https://www.ine.es/dyngs/DataLab/en/manual.html?cid=48) should be pulled together. This application is built upon pre-processed data to speed up rendering of visuals, but you can build your own procedure for data download and mapping by refering to the process commented [here](https://github.com/ordanovich/downloadINE).

### Nomenclature

- :rocket: [app.R](https://github.com/ordanovich/SpainMultivariateMapping/blob/master/app.R): Shiny app combining **UI** and **server** parts.
- :round_pushpin: [.RData](https://github.com/ordanovich/SpainMultivariateMapping/blob/master/spatial_data_all.RData): spatial data (administrative division of Spain) to be used by the [app.R](https://github.com/ordanovich/SpainMultivariateMapping/blob/master/app.R).
- :file_folder: [.sqlite](https://github.com/ordanovich/SpainMultivariateMapping/blob/master/demo_phenom_app.sqlite): SQLite database to be used by the [app.R](https://github.com/ordanovich/SpainMultivariateMapping/blob/master/app.R) with the minimum dataset for the application to function.

The application itself (use `shiny::runApp()` from the cloned repository or open the <a href="http://193.146.75.235/sample-apps/final_apps/demographic_phenomena/"  rel="noopener noreferrer" target="_blank">online version</a> to preview the app) pursues the goal to allow users to select a combination of variables (one variable for a simple choropleth map, two variables for biscale mapping and 3 variables to create a ternary composition map) and map it at selected administrative level for a chosen year. 

For more information on how bivariate maps are made using the [biscale package in R](https://github.com/slu-openGIS/biscale) or [ArcGIS online](https://www.esri.com/arcgis-blog/products/arcgis-online/mapping/how-to-make-a-relationship-map-in-arcgis-online/) please refer to the links provided. To learn more about the ternary compositions in R visit the [ticolore package repository](https://github.com/jschoeley/tricolore).
