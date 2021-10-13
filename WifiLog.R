# https://github.com/Salamek/huawei-lte-api/blob/master/examples/data_dump.py

library(httr)
library(rvest)
library(dplyr)
library(readr)

textlogpath <- "WifiLog.csv"
confpath <- "wc_conf.txt"


getConf <- function(settingStr){
  
  settingSearch  <- settingStr
  
  conf_tbl <- read_csv(confpath,
                       col_names = T,
                       col_types = cols(
                                         SETTING = col_character(),
                                         VALUE = col_double()
                                       )
                         )
  
  valueOut <- conf_tbl %>% filter(SETTING == settingSearch) %>% pull(VALUE)
  
  valueOut
}

getStats <- function()
{
  r <- GET("http://192.168.1.1/api/monitoring/traffic-statistics")
  
  
  rdoc <- read_html(r)
  
  pdo_node <- html_node(rdoc, "response")
  
  output_tib <- tibble( 
    `Download` = as.numeric(html_text(html_nodes(pdo_node,"currentdownload"))),
    `Upload` = as.numeric(html_text(html_nodes(pdo_node,"currentupload"))),
    `Time` = Sys.time()
    
  )
  
  output_tib
}

sendMsg <- function(par_txt) {
  msg <- URLencode(par_txt)
  httr::GET(paste0("https://api.telegram.org/bot714765088:AAGMUv8e0wXZUK094u4eXYX6QPttHOg1Bj8/sendMessage?chat_id=822736555&text=",msg))
  
  
}

getRate <- function(par_tbl,x) {
  
  print(max(par_tbl$Download)/(1024^2))
  
  outputtbl <- par_tbl %>% tail(x) %>% select(1:3) %>%
    mutate(prev_down = lag(Download),
           prev_time = lag(Time),
           slice_down = ((Download - prev_down)/(1024^2)),
           slice_time = Time - prev_time,
           down_rate = round(
                                slice_down/(as.numeric(slice_time, units="secs")),
                                digits = 2
                                )
                                        
    )
  
    
    
  outputtbl
}

checkHighRate <- function(par_vec, max_rate, max_msg_time) {
  
  curr_avg <- mean(par_vec,na.rm = T)
  
  print(paste0('Avg download rate for last 3 minutes: ',curr_avg,'mb/s'))

  tesettime <- max_msg_time
  
  if (curr_avg >= max_rate) {
    if (as.numeric(Sys.time() - max_msg_time, units="mins") >= 2 ) {
        if (getConf(settingStr = "send_notify") == 1) {
          sendMsg(paste("Avegare Mb/s download is over",max_rate,"Mb/s. Actual avg rate is",curr_avg,"Mb/s"))
          tesettime <- Sys.time()
        }
    }
    
    
  }
  tibble(msg_time = tesettime)
  
}

i <- 1

stst_tbl <- getStats()

msg_sent <- tibble(msg_time = Sys.time())

#----------------------------------

repeat{
  
    
    
    temp_tbl <- getStats()
    
    stst_tbl <- stst_tbl %>% union(temp_tbl)
    
    # getRate(stst_tbl,5)
    
    # getRate(stst_tbl,5)$down_rate
    
    pollingrate <- getConf(settingStr = "poll_minutes")
    minutesAvg <- getConf(settingStr = "minutes_avg")
    
    rateRecords <- minutesAvg / pollingrate
      
    
    
    msg_sent <- checkHighRate(par_vec = getRate(stst_tbl,rateRecords)$down_rate,
                              max_rate = getConf(settingStr = "download_thresh"),
                              max_msg_time = max(msg_sent$msg_time))
    
    print(max(msg_sent$msg_time))

    
    write_csv(temp_tbl,textlogpath,append = T,col_names = F)
    
    Sys.sleep(pollingrate*60)

}


# stst_tbl %>% mutate(Download/(1024^2)) %>% View()

