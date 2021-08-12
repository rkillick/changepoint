#This function generates a user an (n-1)x(p+2) design matrix for their data and own choice of AR(p) model. This can be used directly with cpt.reg() but is unnecessary for cpt.ar() as it will be generated internally. 
design <- function(data,p=1){
    if(p>0.2*length(data)){
        warning("Due to the order of this model, there is a high risk of overfitting the data")
    }
    n = length(data)
    generic <- cbind(data[-c(1:p)],rep(1,n-p))
    plist <- list()
    for(i in 0:(p-1)){
        plist[[i+1]] = data[(p-i):(n-(i+1))]
    }
    arpspecific = cbind()
    for(j in 1:length(plist)){
        arpspecific = cbind(arpspecific, plist[[j]])
    }
    output <- cbind(generic,arpspecific)
    output
}
