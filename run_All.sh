#!/bin/bash
cd /Users/Shared/Development/RStudio/WifiControl/
(trap 'kill 0' SIGINT; ./run_Log.sh & ./run_Bot.sh)

