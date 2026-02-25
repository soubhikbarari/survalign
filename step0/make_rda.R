gss <- readRDS('data/gss.rds'); 
gss <- labelled::unlabelled(gss)
save(gss, file = 'data/gss.rda') 

ces <- readRDS('data/ces.rds'); 
ces <- labelled::unlabelled(ces)
save(ces, file = 'data/ces.rda')
