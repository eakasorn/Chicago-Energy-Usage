# Information
This application makes use of electricity and gas usage data obtained from the City of Chicago website. The link to the original data can be accessed here https://www.kaggle.com/chicago/chicago-energy-usage-2010 or https://data.cityofchicago.org/Environment-Sustainable-Development/Energy-Usage-2010/8yq3-m6wp. The file consists of data from 2010 in the city of Chicago, in the form of .csv file. Another source of data is the Chicago Metropolitan Agency for Planning website which provides information about the demography of the residents in 2010. The link to the data can be found at https://datahub.cmap.illinois.gov/dataset/5700ba1a-b173-4391-a26e-48b198e830c8/resource/b30b47bf-bb0d-46b6-853b-47270fb7f626/download/CCASF12010CMAP.xlsx in the form of an .xlsx file.

The data is read into the application as is with data manipulation being done within the application itself. The main attributes that are focused on are the community area, census block, tracts, types of property, properties of the building, population living in the area, electricity and gas usage by the month. Also, the number of people by the age groups are also considered later on. The final number of attributes remaining is about 50.

# Instruction
To run this application, R language and RStudio version 1.4.1103 is recommended. In this application, the library packages required are shiny, shinydashboard, readxl, ggplot2, DT, and leaflet, tigris, mapview and reshape2.

To install R, click the link https://www.r-project.org/ and choose the version 4.0.4.
To install RStudio, click the link https://rstudio.com/products/rstudio/download/ and choose the RStudio Desktop version.
After downloading and installing R and RStudio, to install the required packages to run the code, in RStudio application on the bottom left section named "Console", run the 'code install.packages()' where inside the brackets, put in the name of the library to install. Note that double quotation marks are required. (For example, to install shiny package use the code 'install.packages("shiny")')
Set the working directory to the folder path where the csv file is located. This can be done using the code 'setwd()' where the path to the folder is inside the bracket.
To run the code open the app.R file in RStudio and select Run App in the top middle section of the application.