for( podsiec in GP ){ 
    zlozon <- union(zlozon, entropy(toString(as_adjacency_matrix(podsiec))))
    zlozon <- union(zlozon, entropy(toString(laplacian_matrix(podsiec))))
    zlozon <- union(zlozon, entropy(toString(degree(podsiec))))
    zlozon <- union(zlozon, entropy(toString(degree_distribution(podsiec))))

    ks <- toString(as_adjacency_matrix(podsiec))
    sm <- unlist(strsplit(gsub("(.{10})", "\\1 ",gsub(" ", "", ks, fixed = TRUE))," "))
    cl = makeCluster(8)
    clusterExport(cl,"local_complexity")
    smc <- sm[-c(length(sm))]
    dh4 <- parLapply(cl, smc, function(i)  local_complexity(toString(i), span=10))
    stopCluster(cl)
    n_occur <- table(unlist(as.data.frame.complex(dh4)))
    no <- as.data.frame(n_occur)
    k = 0
    for ( value in no$Freq ){ k = k + (as.numeric(log(value,9)))}
    ddf <- as.data.frame(unlist(dh4))
    ddf <- ddf[complete.cases(ddf),]
    zlozon <- union(zlozon,sum(ddf) + k)

    ks <- toString(laplacian_matrix(podsiec))
    sm <- unlist(strsplit(gsub("(.{10})", "\\1 ",gsub(" ", "", ks, fixed = TRUE))," "))
    cl = makeCluster(8)
    clusterExport(cl,"local_complexity")
    smc <- sm[-c(length(sm))]
    dh4 <- parLapply(cl, smc, function(i)  local_complexity(toString(i), span=10))
    stopCluster(cl)
    n_occur <- table(unlist(as.data.frame.complex(dh4)))
    no <- as.data.frame(n_occur)
    k = 0
    for ( value in no$Freq ){ k = k + (as.numeric(log(value,9)))}
    ddf <- as.data.frame(unlist(dh4))
    ddf <- ddf[complete.cases(ddf),]
    zlozon <- union(zlozon,sum(ddf) + k)

    ks <- toString(degree(podsiec))
    sm <- unlist(strsplit(gsub("(.{10})", "\\1 ",gsub(" ", "", ks, fixed = TRUE))," "))
    cl = makeCluster(8)
    clusterExport(cl,"local_complexity")
    smc <- sm[-c(length(sm))]
    dh4 <- parLapply(cl, smc, function(i)  local_complexity(toString(i), span=10))
    stopCluster(cl)
    n_occur <- table(unlist(as.data.frame.complex(dh4)))
    no <- as.data.frame(n_occur)
    k = 0
    for ( value in no$Freq ){ k = k + (as.numeric(log(value,9)))}
    ddf <- as.data.frame(unlist(dh4))
    ddf <- ddf[complete.cases(ddf),]
    zlozon <- union(zlozon,sum(ddf) + k)

    ks <- toString(degree_distribution(podsiec))
    sm <- unlist(strsplit(gsub("(.{10})", "\\1 ",gsub(" ", "", ks, fixed = TRUE))," "))
    cl = makeCluster(8)
    clusterExport(cl,"local_complexity")
    smc <- sm[-c(length(sm))]
    dh4 <- parLapply(cl, smc, function(i)  local_complexity(toString(i), span=10))
    stopCluster(cl)
    n_occur <- table(unlist(as.data.frame.complex(dh4)))
    no <- as.data.frame(n_occur)
    k = 0
    for ( value in no$Freq ){ k = k + (as.numeric(log(value,9)))}
    ddf <- as.data.frame(unlist(dh4))
    ddf <- ddf[complete.cases(ddf),]
    zlozon <- union(zlozon,sum(ddf) + k)
 }