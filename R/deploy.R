
rsconnect::setAccountInfo(name = Sys.getenv("shinyappsio_user"), 
                          token = Sys.getenv("shinyappsio_token"), 
                          secret = Sys.getenv("shinyappsio_secret"))

rsconnect::deployApp(appName = "attainment_scotland", 
                     account = Sys.getenv("shinyappsio_user"),
                     'app/')
