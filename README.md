# Elections-2022-Visual-Data-Analysis
My country goes to a General Election in less than two months. I found it necessary to have an application that relays candidates' popularity, predictions, or even the real end results.
This application can be of use to media houses (television) and popularity polls/research firms/ companies. The data used in this application is self generating as at of now. However, definite data can be used in a case where it is fully available. A general case could be an analysis of 2013 and 2017 presidential elections.

Here is a snippet of the home page of the application.
![image](https://user-images.githubusercontent.com/100840448/174190601-f66d11db-12f2-4cca-ac47-3a9db9ff57b8.png)

The user can then select a county to view per county analysis.
![Screenshot 2022-06-17 015411](https://user-images.githubusercontent.com/100840448/174191452-9875a7d2-d0b7-4c6c-8779-744405f4a8be.png)

The user is allowed to navigate a candidates popularity regions.
![Screenshot 2022-06-17 015411](https://user-images.githubusercontent.com/100840448/174194321-2b628b3a-f0c6-41ba-ab3b-5d9bb739bfec.png)

Packages used in the making include:
> shiny - the mother of the interactive board
> 
> leaflet - create the basic live map
> 
> rgdal -to read the geographical data with the bounderies. 
> 
> tidyverse- for general dplyr  
> 
> flipdownr - a count down of days to election day
> 
> echarts4r - plot a live/animated and interactive graphs
> 
> shinyjqui-hide/show elements whenever required 
> 
> CSS - styling elements

Interact with the [live App here](https://ndekejefferson.shinyapps.io/Elections-2022-Visual-Data-Analysis/)


UPDATE: A new version include 2013 and 2017 General Elections data. 
![app](https://user-images.githubusercontent.com/100840448/175614268-d9ba552d-369f-426a-818e-6335d4dda2b2.png)


Data source: [IEBC Kenya](https://www.iebc.or.ke/resources/).
Other demographic data from [Knbs](https://www.knbs.or.ke/).

