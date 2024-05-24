################################################################################
#                       Estonia                                                #
#         Max-Planck Institute for Demographic Research                        #
#               Subnational Birth Squeezes                                     #
################################################################################

### Preperations -------------------------------------------------------------

# Load the packages
source("Functions/Packages.R")
source("Functions/Graphics.R")
source("Functions/Functions.R")


### Download Estonian data ---------------------------------------------------

# PXWEB query 
# Elukoht = municipality, Sugu = sex, Haridustase = education
pxweb_query_list <- 
  list("Elukoht"=c("H2","H3","H4","H5","37","140","H8","H9","198","245","296","304","337","353","424","H17","H18","H19","446","651","653","718","727","H25","H26","784","176","298","339","387","482","524","596","614","890","39","H38","H39","H40","44","H42","251","H44","H45","322","120","265","340","553","747","437","H53","H54","H55","511","513","H58","H59","735","802","49","248","H64","H65","H66","H67","H68","616","H70","H71","51","H73","566","H75","H76","835","H78","H79","57","183","H82","H83","H84","907","59","190","272","663","662","790","H92","H93","H94","900","902","H97","H98","926","65","285","619","H103","H104","707","H106","H107","67","213","303","H111","H112","H113","H114","625","H116","H117","710","H119","H120","808","H122","H123","70","292","317","504","669","H129","H130","74","478","689","H134","H135","H136","78","H138","H139","H140","282","H142","432","528","587","H146","H147","795","H149","H150","794","82","636","H154","H155","H156","H157","H158","H159","H160","H161","84","H163","H164","H165","H166","H167","H168","H169","H170","H171","897","H173","86","143","H176","H177","697","H179","919","918"),
       "Aasta"=c("2018","2019","2020","2021"),
       "Sugu"=c("2","3"),
       "Haridustase"=c("2","3","4","5"))

# Download data 
px_data <- pxweb_get(url = "https://andmed.stat.ee/api/v1/en/stat/rahvastik/rahvastikunaitajad-ja-koosseis/rahvaarv-ja-rahvastiku-koosseis/RV0232U.PX",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Get pxweb data comments 
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)

# Cite the data as 
pxweb_cite(px_data)


############          END       #########################################
