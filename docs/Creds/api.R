

token <- "ddms5s0l3gejlz2z42ydt0bnwmf6ssqd62bdxteu7t8sumv5ii"


# library(httr)
# library(curl)
# 
# 
# headers <- c(
#   'Content-Type' = 'application/x-www-form-urlencoded'
# )
# 
# 
# id <- '0ef99166-64ac-11e8-b9f8-f23c91df28e9'
# user <- 'ivanbalabanic@gmail.com'  
# pass <- 'Vicmaher2012'
# url <-  'https://login.metricom.eu/auth/realms/metricom/protocol/openid-connect/token'
# body = list(
#   grant_type = 'password', # set to password
# #  scope = "offline_access", 
#   client_id  = id, # set to your assigned client ID
#   username = user, # e-mail of user you are trying to authorize
#   password = pass)  
# 
# 
# res <- POST(url = url,
#             body = body,
#             add_headers(headers))
# content(res)
# 
# 
# 
# 
# library(httr)
# 
# id <- '0ef99166-64ac-11e8-b9f8-f23c91df28e9'
# user <- 'ivanbalabanic@gmail.com'  
# pass <- 'Vicmaher2012'
# url <-  'https://login.metricom.eu/auth/realms/metricom/protocol/openid-connect/token'
# res <- POST(url = url,
#             body = list(
#               grant_type = 'password', # set to password
#               client_id  = id, # set to your assigned client ID
#               username = user, # e-mail of user you are trying to authorize
#               password = pass),
#               encode = c("form"))
# content(res)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## PROBAJ ----
# res <- GET(url = "https://login.metricom.eu/auth/realms/metricom", body = body,
#             add_headers(headers))
# 
# 
# 
# 
# ## CURL ----
# 
# curl -s -X POST
# --data-urlencode "username=ivanbalabanic@gmail.com"
# --data-urlencode "client_id=0ef99166-64ac-11e8-b9f8-f23c91df28e9"
# --data-urlencode "password=Vicmaher2012"
# --data-urlencode "grant_type=password"
# https://login.metricom.eu/auth/realms/metricom/protocol/openid-connect/token
# 
# 
# curl \
# -d "client_id=0ef99166-64ac-11e8-b9f8-f23c91df28e9" \
# -d "username=ivanbalabanic@gmail.com" \
# -d "password=Vicmaher2012" \
# -d "grant_type=password" \
# "https://login.metricom.eu/auth/realms/metricom/protocol/openid-connect/token"
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# curl -X POST \
# https://login.metricom.eu/auth/realms/metricom/protocol/openid-connect/token \
# -H 'Content-Type: application/x-www-form-urlencoded' \
# -d '{
#   "grant_type": "password",
#   "client_id": "0ef99166-64ac-11e8-b9f8-f23c91df28e9",
#   "username": "ivanbalabanic@gmail.com",
#   "password": "Vicmaher2012"
# }'
# 
# 
# curl -X POST 'https://login.metricom.eu/auth/realms/metricom/protocol/openid-connect/token'
# --header "Content-Type: application/x-www-form-urlencoded" \
# --data '{"grant_type": "password", "client_id": "0ef99166-64ac-11e8-b9f8-f23c91df28e9", "username": "ivanbalabanic@gmail.com", "password": "Vicmaher2012"}' 
# 
# 
# curl -X POST \
# https://login.metricom.eu/auth/realms/metricom/protocol/openid-connect/token \
# -H 'Content-Type: application/x-www-form-urlencoded' \
# -d 'username=ivanbalabanic@gmail.com&password=Vicmaher2012&grant_type=password&client_id=0ef99166-64ac-11e8-b9f8-f23c91df28e9'
# 
# 
# curl -d "username=ivanbalabanic@gmail.com&password=Vicmaher2012&grant_type=password&client_id=0ef99166-64ac-11e8-b9f8-f23c91df28e9" -X POST https://login.metricom.eu/auth/realms/metricom/protocol/openid-connect/token
# 
# 
# 





