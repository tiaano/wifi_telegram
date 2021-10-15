# install.packages("telegram.bot")
library(telegram.bot)
library(dplyr)
library(readr)

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
  
  print("Command received: /start")
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Hello %s!", update$message$from$first_name))
}

showCmds <- function(bot,update){
  print("Command received: /cmds")
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Commands\nSet threshold: /setT\n/Set Notification: setN"))
}

setThresh <- function(bot, update, args){
  print("Command received: /sett",args)
  
  if (length(args > 0L)){
    new_thresh <- args
    
    setConf(settingStr = "download_thresh",parvalue = as.numeric(new_thresh))
    
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste("Download threshold set to ",new_thresh))
  }
  

}

RKM <- ReplyKeyboardMarkup(
  keyboard = list(
    list(KeyboardButton("Yes, they certainly are!")),
    list(KeyboardButton("I'm not quite sure")),
    list(KeyboardButton("No..."))
  ),
  resize_keyboard = FALSE,
  one_time_keyboard = TRUE
)

echo <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id, text = update$message$text, reply_markup = RKM)
}



updater <- Updater(token = telegramBotTok)

start_handler <- CommandHandler("start", start)
cmdList_handler <- CommandHandler("cmds", showCmds)
setThresh_handler <- CommandHandler("sett", setThresh, pass_args = TRUE)

updater <- updater + start_handler 
updater <- updater +  cmdList_handler
updater <- updater + setThresh_handler
updater <- updater + MessageHandler(echo, MessageFilters$text)

updater$start_polling()
