# Data retrieval and cleaning
# Restart session first!

# Remove lock folders if they exist
unlink("C:/Users/tshul/AppData/Local/R/win-library/4.3/00LOCK*", recursive = TRUE)

# Install dependencies
install.packages(c("janitor","tidyverse"), type = "binary")

# setting Up my BER API
remotes::install_github("Bureau-for-Economic-Research/berdata")


usethis::edit_r_environ() # using my global environment to collect data using my Bureau of Economic Research (BER) API key


# Getting the Data
#K: Quartely and J:Monthly
#Total employment in the Public sector KBP7002 - (K1,J1)
#Total employment in the private sector KBP7008 - (K1,J1)
#Official unemployment rate KBP7019 - (K1,J1)




