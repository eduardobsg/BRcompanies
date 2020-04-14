download_reports <- function(){
  require(rvest)
  require(httr)
  require(dplyr)
  # list of individual urls
  url_DRE <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/DRE/DADOS/'
  url_DMPL <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/DMPL/DADOS/'
  url_ATIVO <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/BPA/DADOS/'
  url_PASSIVO <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/BPP/DADOS/'
  url_FCdireto <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/DFC_MD/DADOS/'
  url_FCindireto <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/DFC_MI/DADOS/'
  url_ITR <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/ITR/DADOS/'
  url_DVA <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/DVA/DADOS/'

  # list with all urls together
  url <-    c(url_DRE, url_DMPL, url_ATIVO,
              url_PASSIVO, url_FCdireto, url_FCindireto,
              url_ITR, url_DVA)

  # create a folder to hold all zips that will be downloaded
  dir.create('downloads')
  #pastas = c('dre', 'dmpl', 'balanco', 'fluxocaixa', 'itr', 'dva')
  #for (i in 1:length(pastas)){dir.create(pastas[i])}

  # scrape each url, getting its html info
  for (url in url){
    txt <- GET(url)
    html <- read_html(txt)
    # what matters is the content of the 'href' attribute
    links <- html %>% html_nodes("a") %>% html_attr("href")
    # corce into a dataframe
    links <- as.data.frame(links, stringsAsFactors=FALSE)

    # determine the kind of download to do
    links2 <- unlist(c(links))  # easier to stringr-r
    # identify lines with years
    links2 <- str_extract(string = links2, pattern = '.+[0-9]+')
    # get rid of NA's, then get the first row
    links2 <- links2[!is.na(links2)][1]
    # get only the first characteres
    links2 <- substr(links2, 1,7)
    # remove rows that do not contain 'links2'
    links = dplyr::filter(links, grepl(links2, links))

    # construct the full url
    full_url = paste0(url, links$links)

    # the loop goes through each link, verifies if the file is in the folder and if it is not
    # then it begins the download
    for (i in 1:length(full_url)){
      print(basename(full_url[i]))
      url = full_url[i]
      download.file(url, destfile = paste0("downloads/",basename(full_url[i])),
                    method = "libcurl", mode = "wb")
    }
  }
}

unzip_files <- function(){
  # unzips all files
  files <- list.files('downloads/')
  for (file in files){
    unzip(paste0('downloads/',file), exdir = 'sheets')
  }
}

join_files <- function(){
  require(data.table)
  files <- list.files('sheets/')
  dfp <- NULL
  temp <- NULL
  for (file in files){
    temp <- read.csv2(paste0('sheets/', file))
    dfp <- rbindlist(list(dfp, temp), fill = TRUE)
    dfp
  }
}
