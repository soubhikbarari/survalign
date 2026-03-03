gss <- readRDS('data/gss.rds'); 
gss <- labelled::unlabelled(gss)
gss$year <- as.numeric(as.character(gss$year))
attr(gss$year, "label") <- "Survey Year"
save(gss, file = 'data/gss.rda') 

ces <- readRDS('data/ces.rds'); 
ces <- labelled::unlabelled(ces)
ces$year <- as.numeric(as.character(ces$year))
attr(ces$year, "label") <- "Survey Year"
save(ces, file = 'data/ces.rda')
