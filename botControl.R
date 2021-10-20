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

work_state <- "none"

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
                  text = sprintf("Commands\nSet threshold: /sett\nSet Notification: /setn"))
}

setThresh <- function(bot, update, args){
  print("Command received: /sett",args)
  
  work_state <<- "threshold"
  
  bot$sendMessage(chat_id = update$message$chat_id,
                                    text = paste("What should the new threshold be?"))
  

}

setNotify <- function(bot, update){
  print("Command received: /setn")
  
  work_state <<- "notify"
  
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = paste("Set notify flag 1 or 0"), reply_markup = RKM_notify)
  
  
}

RKM_notify <- ReplyKeyboardMarkup(
  keyboard = list(
    list(KeyboardButton("1")),
    list(KeyboardButton("0"))
  
  ),
  resize_keyboard = FALSE,
  one_time_keyboard = TRUE
)

echo <- function(bot, update){
  txt <- update$message$text
  # bot$sendMessage(chat_id = update$message$chat_id, text = update$message$text, reply_markup = RKM)
  
  if ( tolower(txt) == "hi") {
    bot$sendMessage(chat_id = update$message$chat_id, text = paste("Bot is active"))
  }
  
  
  if ( tolower(txt) == "state") {
    bot$sendMessage(chat_id = update$message$chat_id, text = paste("Current state is:",work_state))
  }
  
  
  
  if (work_state == "threshold" & !is.na(suppressWarnings(as.numeric(txt))) ){
    new_thresh <- txt
    setConf(settingStr = "download_thresh",parvalue = as.numeric(new_thresh))
    
    bot$sendMessage(chat_id = update$message$chat_id,
                                      text = paste("Download threshold set to ",new_thresh))
    
    work_state <<- "none"
  }
  
  if (work_state == "notify" & !is.na(suppressWarnings(as.numeric(txt))) ){
    new_thresh <- txt
    setConf(settingStr = "send_notify",parvalue = as.numeric(new_thresh))
    
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste("Notify flag set to ",new_thresh))
    
    work_state <<- "none"
  }
  
}



updater <- Updater(token = telegramBotTok)

start_handler <- CommandHandler("start", start)
cmdList_handler <- CommandHandler("cmds", showCmds)
setThresh_handler <- CommandHandler("sett", setThresh, pass_args = TRUE)
setNotify_handler <- CommandHandler("setn", setNotify)

updater <- updater + start_handler 
updater <- updater +  cmdList_handler
updater <- updater + setThresh_handler
updater <- updater + setNotify_handler
updater <- updater + MessageHandler(echo, MessageFilters$text)

updater$start_polling()
