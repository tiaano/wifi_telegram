# install.packages("telegram.bot")
library(telegram.bot)
library(dplyr)

telegramBotTok <- "714765088:AAGMUv8e0wXZUK094u4eXYX6QPttHOg1Bj8"

bot <- Bot(token = telegramBotTok)

# updates <- bot$getUpdates()

chat_id <- "822736555" # you can retrieve it from bot$getUpdates() after sending a message to the bot
# bot$sendMessage(chat_id = chat_id, text = "TestReply")

#  Misc Functions ----

confpath <- "wc_conf.txt"

setConf <- function(settingStr,parvalue){
  
  settingSearch  <- settingStr
  
  conf_tbl <- read_csv(confpath,
                       col_names = T,
                       col_types = cols(
                         SETTING = col_character(),
                         VALUE = col_double()
                       )
  )
  
  conf_tbl_out <- conf_tbl %>%
    mutate(VALUE = case_when(
      SETTING %in% c(settingSearch)                            ~ parvalue,
      TRUE                                                     ~ VALUE
    ))
  
  write_csv(conf_tbl_out,confpath,col_names = T,append = F
           )
  
  
}


# Controllers ----

start <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Hello %s!", update$message$from$first_name))
}

showCmds <- function(bot,update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Commands\nSet threshold: /setT\n/Set Notification: setN"))
}

setThresh <- function(bot, update, arg){
  
  if (length(arg > 0L)){
    new_thresh <- arg
    
    setConf(settingStr = "download_thresh",parvalue = as.numeric(new_thresh))
    
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste("Download threshold set to ",new_thresh))
  }
  

}

updater <- Updater(token = telegramBotTok)

start_handler <- CommandHandler("start", start)
cmdList_handler <- CommandHandler("cmds", showCmds)
setThresh_handler <- CommandHandler("sett", setThresh, pass_args = TRUE)

updater <- updater + start_handler 
updater <- updater +  cmdList_handler
updater <- updater + setThresh_handler

updater$start_polling()
