################################################################################
#                       Sweden                                                 #
#         Max-Planck Institute for Demographic Research                        #
#               Subnational Birth Squeezes                                     #
################################################################################

### Preperations -------------------------------------------------------------

  # Load the packages
  source("Functions/Packages.R")
  source("Functions/Graphics.R")
  source("Functions/Functions.R")

### Genesis Statistisches Bundesamt ------------------------------------------

    
    # Part 1: account details
    IHRE_KENNUNG <- "DEVZ8FSF85"
    IHR_PASSWORT <- "Schweden!2018"
    login <- paste0("username=", IHRE_KENNUNG, "&password=", IHR_PASSWORT)
    
    # Part 2: set url
    website <- "https://www-genesis.destatis.de/genesisWS/rest/2020/"
    
    # Part 3: define tasks
    tasks <- c("find/find?", "catalogue/results?", "catalogue/tables?", "catalogue/timeseries/", "data/table?")
    names <- c("find", "results", "tables", "timeseries", "data")
    end <- c("&term=Population&category=all&pagelength=100&language=de", "&selection=*12411*&area=all&pagelength=100&language=de",
             "&selection=12411-0018&area=all%searchcriterion=&pagelength=100&language=de",
              "&selection=12411-0018&area=all&pagelength=20&language=de", 
              "&name=11111-0001&area=all&compress=false&transpose=false&startyear=&endyear=&timeslices=&regionalvariable=&regionalkey=&classifyingvariable1=&classifyingkey1=&classifyingvariable2=&classifyingkey2=&classifyingvariable3=&classifyingkey3=&job=false&stand=01.01.1970&language=de")
    
    # Part 4: set number of data
    data_nr <- "12411-0018"
    
    # combine the different insertations
    request <- cbind(names, tasks, end)  %>% as.data.frame()
    
    # Load the data from genesis
    res <- genesis_api(task = "results", request)
    
    # create a data frame
    variable_list <- res$Tables %>% do.call(rbind, .)
    
    # Get the code
    Code <- "12411-0007"


### Variable manual ---------------------------------------------------

  # Set the path
  path <- paste0("https://www-genesis.destatis.de/genesisWS/web/RechercheService_2010?method=MerkmalsKatalog&kennung=", IHRE_KENNUNG ,"&passwort=", IHR_PASSWORT, 
                  "&filter=", filter, "&kriterium=", Code ,"&typ=alle&bereich=Alle&listenLaenge=99&sprache=de")
    
  # Load the data
  resp <- read_html(path)
  
  # Variablenliste
  resp %>% html_element("sections")


### Load the data series ----------------------------------------------
  
  
  #
  fromJSON(content(resp, "text"))
  
  # Extract the results
  results <- results$Object
  results %>% str_split(pattern = "\nmännlich") %>%  colsplit(.,  ";", names = c("Age", "Kreis", "Pop"))
  
#### load entire data frame ------------------------------------------------
  
  # load the table
  r <- load_table_DE()
  
  
  # Test, get the IP adress
  path <- "helloworld/whoami"
  
  # Load the information
  genesis_api(path)
  
  path <- paste0("https://www-genesis.destatis.de/genesisWS/rest/2020/", path = path)
  
  resp <- GET(url)
  
#### Extracting information using jsonlite ---------------------------------
  
  #save url
  url <- paste0("https://www-genesis.destatis.de/genesisWS/rest/2020/helloworld/logincheck?username=", IHRE_KENNUNG, "&password=", IHR_PASSWORT, "&language=de")
  
  r <- GET(url)
  r$content
  http_type(r)
  
  #
  jsonlite::fromJSON(content(r, "text"), simplifyVector = FALSE)
  
  
  # Prepare request
  url <- "https://www-genesis.destatis.de/genesisWS/rest/2020/" 
  task <- "find/find?"
 # login <- 
  parameters <- "&term=population&category=all&pagelength=100&language=en"
  
  
  # Using httr code
  GET(paste0(url, task, login, parameters))
  
  # Get the information from the website
  r <- GET(url)
  r <- fromJSON(content(r, "text"), simplifyVector =  T)
  pop_data <- r$Tables
  table <- pop_data %>% filter(Code == data_nr)
  
  ### Set the parameters
  
  # Number of the dataset: Bevölkerung, Kreise, Geschlecht, Altersgruppen
  data_nr <- "12411-0018"
  start_year <- 2010
  end_year <- 2011
  data_nr <- "12411"
  
  
  # Set the url for time-series download
  url <- paste0("https://www-genesis.destatis.de/genesisWS/rest/2020/data/table?username=",
         IHRE_KENNUNG, "&passwort=", IHR_PASSWORT, "&name", data_nr, "&area=all&compress=false&transpose=false&startyear=",
         start_year, "&endyear=", end_year, "&timeslices=", end_year-start_year,
  "&regionalvariable=&regionalkey=&classifyingvariable1=KREISE&classifyingkey1=1001&classifyingvariable2=Geschlecht&classifyingkey2=GES&classifyingvariable3=Altersgruppen&classifyingkey3=ALTX20&job=false&stand=01.01.2021&language=de")
  
  # Load the data
  data <- GET(url)
  
  # Set the url
  url <- paste0("https://www-genesis.destatis.de/genesisWS/rest/2020/data/timeseries?username=", IHRE_KENNUNG ,"&password=", IHR_PASSWORT, "&name=", data_nr,
                "&area=all&compress=false&transpose=false&contents=&startyear=", start_year, "&endyear=", end_year, "&timeslices=&regionalvariable=&regionalkey=&regionalkeycode=&classifyingvariable1=&classifyingkey1=&classifyingkeycode1=&classifyingvariable2=&classifyingkey2=&classifyingkeycode2=&classifyingvariable3=&classifyingkey3=&classifyingkeycode3=&job=false&stand=&language=de")
  
  # Using jsonData
  jsonData <- fromJSON(url)
  
  jsonData
  
  # get the website informaiton
  r <- GET(url)
  
  # get status of request 
  http_status(r)

  
  