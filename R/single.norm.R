single.mean.norm.calc <-
  function(data, extrainf = TRUE, minseglen) {
    singledim = function(data, extrainf = TRUE, minseglen) {
      n = length(data)
      y = c(0, cumsum(data))
      y2 = c(0, cumsum(data ^ 2))
      null = y2[n + 1] - y[n + 1] ^ 2 / n
      taustar = minseglen:(n - minseglen + 1)
      tmp = y2[taustar + 1] - y[taustar + 1] ^ 2 / taustar + (y2[n + 1] -
                                                                y2[taustar + 1]) - ((y[n + 1] - y[taustar + 1]) ^ 2) / (n - taustar)
      
      tau = which(tmp == min(tmp, na.rm = T))[1]
      taulike = tmp[tau]
      tau = tau + minseglen - 1 # correcting for the fact that we are starting at minseglen
      if (extrainf == TRUE) {
        out = c(tau, null, taulike)
        names(out) = c('cpt', 'null', 'alt')
        return(out)
      }
      else{
        return(tau)
      }
    }
    
    
    if (is.null(dim(data)) == TRUE) {
      # single data set
      cpt = singledim(data, extrainf, minseglen)
      return(cpt)
    }
    else{
      rep = nrow(data)
      n = ncol(data)
      cpt = NULL
      if (extrainf == FALSE) {
        for (i in 1:rep) {
          cpt[i] = singledim(data[i,], extrainf, minseglen)
        }
      }
      else{
        cpt = matrix(0, ncol = 3, nrow = rep)
        for (i in 1:rep) {
          cpt[i,] = singledim(data[i,], extrainf, minseglen)
        }
        colnames(cpt) = c('cpt', 'null', 'alt')
      }
      return(cpt)
    }
  }

single.mean.norm <-
  function(data,
           penalty = "MBIC",
           pen.value = 0,
           class = TRUE,
           param.estimates = TRUE,
           minseglen) {
    if (is.null(dim(data)) == TRUE) {
      # single dataset
      n = length(data)
    }
    else{
      n = ncol(data)
    }
    if (n < 2) {
      stop('Data must have atleast 2 observations to fit a changepoint model.')
    }
    if (n < (2 * minseglen)) {
      stop('Minimum segment legnth is too large to include a change in this data')
    }
    
    pen.value = penalty_decision(
      penalty,
      pen.value,
      n,
      diffparam = 1,
      asymcheck = "mean.norm",
      method = "AMOC"
    )
    if (is.null(dim(data)) == TRUE) {
      # single dataset
      tmp = single.mean.norm.calc(coredata(data), extrainf = TRUE, minseglen)
      if (penalty == "MBIC") {
        tmp[3] = tmp[3] + log(tmp[1]) + log(n - tmp[1] + 1)
      }
      ans = decision(tmp[1], tmp[2], tmp[3], penalty, n, diffparam = 1, pen.value)
      
      if (class == TRUE) {
        return(
          class_input(
            data,
            cpttype = "mean",
            method = "AMOC",
            test.stat = "Normal",
            penalty = penalty,
            pen.value = ans$pen,
            minseglen = minseglen,
            param.estimates = param.estimates,
            out = c(0, ans$cpt)
          )
        )
      }
      else{
        alogn = (2 * log(log(n))) ^ (-(1 / 2))
        blogn = (alogn ^ (-1)) + (1 / 2) * alogn * log(log(log(n)))  # Chen & Gupta (2000) pg10
        out = c(ans$cpt, exp(-2 * (pi ^ (1 / 2)) * exp(
          -alogn * sqrt(abs(tmp[2] - tmp[3])) + (alogn ^ {
            -1
          }) * blogn
        )) - exp(-2 * (pi ^ (1 / 2)) * exp((alogn ^ {
          -1
        }) * blogn)))
        names(out) = c('cpt', 'conf.value')
        return(out)
      }
    }
    else{
      tmp = single.mean.norm.calc(data, extrainf = TRUE, minseglen)
      if (penalty == "MBIC") {
        tmp[, 3] = tmp[, 3] + log(tmp[, 1]) + log(n - tmp[, 1] + 1)
      }
      ans = decision(tmp[, 1], tmp[, 2], tmp[, 3], penalty, n, diffparam = 1, pen.value)
      if (class == TRUE) {
        rep = nrow(data)
        out = list()
        for (i in 1:rep) {
          out[[i]] = class_input(
            data,
            cpttype = "mean",
            method = "AMOC",
            test.stat = "Normal",
            penalty = penalty,
            pen.value = ans$pen,
            minseglen = minseglen,
            param.estimates = param.estimates,
            out = c(0, ans$cpt[i])
          )
        }
        return(out)
      }
      else{
        alogn = (2 * log(log(n))) ^ (-(1 / 2))
        blogn = (alogn ^ (-1)) + (1 / 2) * alogn * log(log(log(n)))  # Chen & Gupta (2000) pg10
        out = cbind(ans$cpt, exp(-2 * (pi ^ (1 / 2)) * exp(
          -alogn * sqrt(abs(tmp[, 2] - tmp[, 3])) + (alogn ^ {
            -1
          }) * blogn
        )) - exp(-2 * (pi ^ (1 / 2)) * exp((alogn ^ {
          -1
        }) * blogn)))
        colnames(out) = c('cpt', 'conf.value')
        rownames(out) = NULL
        return(out)
      }
    }
  }





single.var.norm.calc <-
  function(data, mu, extrainf = TRUE, minseglen) {
    n = length(data)
    y = c(0, cumsum((data - mu) ^ 2))
    null = n * log(y[n + 1] / n)
    taustar = minseglen:(n - minseglen + 1)
    sigma1 = y[taustar + 1] / taustar
    neg = sigma1 <= 0
    sigma1[neg == TRUE] = 1 * 10 ^ (-10)
    sigman = (y[n + 1] - y[taustar + 1]) / (n - taustar)
    neg = sigman <= 0
    sigman[neg == TRUE] = 1 * 10 ^ (-10)
    tmp = taustar * log(sigma1) + (n - taustar) * log(sigman)
    
    tau = which(tmp == min(tmp, na.rm = T))[1]
    taulike = tmp[tau]
    tau = tau + minseglen - 1 # correcting for the fact that we are starting at minseglen
    if (extrainf == TRUE) {
      out = c(tau, null, taulike)
      names(out) = c('cpt', 'null', 'alt')
      return(out)
    }
    else{
      return(tau)
    }
  }


single.var.norm <-
  function(data,
           penalty = "MBIC",
           pen.value = 0,
           know.mean = FALSE,
           mu = NA,
           class = TRUE,
           param.estimates = TRUE,
           minseglen) {
    if (is.null(dim(data)) == TRUE) {
      # single dataset
      n = length(data)
      mu = mu[1]
    }
    else{
      n = ncol(data)
    }
    if (n < 4) {
      stop('Data must have atleast 4 observations to fit a changepoint model.')
    }
    if (n < (2 * minseglen)) {
      stop('Minimum segment legnth is too large to include a change in this data')
    }
    
    pen.value = penalty_decision(
      penalty,
      pen.value,
      n,
      diffparam = 1,
      asymcheck = "var.norm",
      method = "AMOC"
    )
    
    if (is.null(dim(data)) == TRUE) {
      if ((know.mean == FALSE) & (is.na(mu))) {
        mu = mean(coredata(data))
      }
      tmp = single.var.norm.calc(coredata(data), mu, extrainf = TRUE, minseglen)
      if (penalty == "MBIC") {
        tmp[3] = tmp[3] + log(tmp[1]) + log(n - tmp[1] + 1)
      }
      ans = decision(tmp[1], tmp[2], tmp[3], penalty, n, diffparam = 1, pen.value)
      if (class == TRUE) {
        out = class_input(
          data,
          cpttype = "variance",
          method = "AMOC",
          test.stat = "Normal",
          penalty = penalty,
          pen.value = pen.value,
          minseglen = minseglen,
          param.estimates = param.estimates,
          out = c(0, ans$cpt)
        )
        param.est(out) = c(param.est(out), mean = mu)
        return(out)
      }
      else{
        alogn = sqrt(2 * log(log(n)))
        blogn = 2 * log(log(n)) + (log(log(log(n)))) / 2 - log(gamma(1 / 2))
        out = c(ans$cpt, exp(-2 * exp(-alogn * sqrt(
          abs(tmp[2] - tmp[3])
        ) + blogn)) - exp(-2 * exp(blogn)))  # Chen & Gupta (2000) pg27
        names(out) = c('cpt', 'conf.value')
        return(out)
      }
    }
    else{
      rep = nrow(data)
      tmp = matrix(0, ncol = 3, nrow = rep)
      if (length(mu) != rep) {
        mu = rep(mu, rep)
      }
      for (i in 1:rep) {
        if ((know.mean == FALSE) & (is.na(mu[i]))) {
          mu = mean(coredata(data[i,]))
        }
        tmp[i,] = single.var.norm.calc(data[i,], mu[i], extrainf = TRUE, minseglen)
      }
      
      if (penalty == "MBIC") {
        tmp[, 3] = tmp[, 3] + log(tmp[, 1]) + log(n - tmp[, 1] + 1)
      }
      ans = decision(tmp[, 1], tmp[, 2], tmp[, 3], penalty, n, diffparam = 1, pen.value)
      if (class == TRUE) {
        out = list()
        for (i in 1:rep) {
          out[[i]] = class_input(
            data,
            cpttype = "variance",
            method = "AMOC",
            test.stat = "Normal",
            penalty = penalty,
            pen.value = ans$pen,
            minseglen = minseglen,
            param.estimates = param.estimates,
            out = c(0, ans$cpt[i])
          )
          param.est(out[[i]]) = c(param.est(out[[i]]), mean = mu[i])
        }
        return(out)
      }
      else{
        alogn = sqrt(2 * log(log(n)))
        blogn = 2 * log(log(n)) + (log(log(log(n)))) / 2 - log(gamma(1 / 2))
        out = cbind(ans$cpt, exp(-2 * exp(-alogn * sqrt(
          abs(tmp[, 2] - tmp[, 3])
        ) + blogn)) - exp(-2 * exp(blogn)))  # Chen & Gupta (2000) pg27
        colnames(out) = c('cpt', 'conf.value')
        rownames(out) = NULL
        return(out)
      }
    }
  }






single.meanvar.norm.calc <-
  function(data, extrainf = TRUE, minseglen) {
    singledim = function(data, extrainf = TRUE, minseglen) {
      n = length(data)
      y = c(0, cumsum(data))
      y2 = c(0, cumsum((data) ^ 2))
      null = n * log((y2[n + 1] - (y[n + 1] ^ 2 / n)) / n)
      taustar = minseglen:(n - minseglen + 1)
      sigma1 = ((y2[taustar + 1] - (y[taustar + 1] ^ 2 / taustar)) / taustar)
      neg = sigma1 <= 0
      sigma1[neg == TRUE] = 1 * 10 ^ (-10)
      sigman = ((y2[n + 1] - y2[taustar + 1]) - ((y[n + 1] - y[taustar + 1]) ^
                                                   2 / (n - taustar))) / (n - taustar)
      neg = sigman <= 0
      sigman[neg == TRUE] = 1 * 10 ^ (-10)
      tmp = taustar * log(sigma1) + (n - taustar) * log(sigman)
      
      tau = which(tmp == min(tmp, na.rm = T))[1]
      taulike = tmp[tau]
      tau = tau + minseglen - 1 # correcting for the fact that we are starting at minseglen
      if (extrainf == TRUE) {
        out = c(tau, null, taulike)
        names(out) = c('cpt', 'null', 'alt')
        return(out)
      }
      else{
        return(tau)
      }
    }
    
    
    if (is.null(dim(data)) == TRUE) {
      # single data set
      cpt = singledim(data, extrainf, minseglen)
      return(cpt)
    }
    else{
      rep = nrow(data)
      n = ncol(data)
      cpt = NULL
      if (extrainf == FALSE) {
        for (i in 1:rep) {
          cpt[i] = singledim(data[i,], extrainf, minseglen)
        }
      }
      else{
        cpt = matrix(0, ncol = 3, nrow = rep)
        for (i in 1:rep) {
          cpt[i,] = singledim(data[i,], extrainf, minseglen)
        }
        colnames(cpt) = c('cpt', 'null', 'alt')
      }
      return(cpt)
    }
  }

single.meanvar.norm <-
  function(data,
           penalty = "MBIC",
           pen.value = 0,
           class = TRUE,
           param.estimates = TRUE,
           minseglen) {
    if (is.null(dim(data)) == TRUE) {
      # single dataset
      n = length(data)
    }
    else{
      n = ncol(data)
    }
    if (n < 4) {
      stop('Data must have atleast 4 observations to fit a changepoint model.')
    }
    if (n < (2 * minseglen)) {
      stop('Minimum segment legnth is too large to include a change in this data')
    }
    
    pen.value = penalty_decision(
      penalty,
      pen.value,
      n,
      diffparam = 1,
      asymcheck = "meanvar.norm",
      method = "AMOC"
    )
    
    if (is.null(dim(data)) == TRUE) {
      tmp = single.meanvar.norm.calc(coredata(data), extrainf = TRUE, minseglen)
      if (penalty == "MBIC") {
        tmp[3] = tmp[3] + log(tmp[1]) + log(n - tmp[1] + 1)
      }
      ans = decision(tmp[1], tmp[2], tmp[3], penalty, n, diffparam = 2, pen.value)
      if (class == TRUE) {
        return(
          class_input(
            data,
            cpttype = "mean and variance",
            method = "AMOC",
            test.stat = "Normal",
            penalty = penalty,
            pen.value = ans$pen,
            minseglen = minseglen,
            param.estimates = param.estimates,
            out = c(0, ans$cpt)
          )
        )
      }
      else{
        alogn = sqrt(2 * log(log(n)))
        blogn = 2 * log(log(n)) + (log(log(log(n))))
        out = c(ans$cpt, exp(-2 * exp(-alogn * sqrt(
          abs(tmp[2] - tmp[3])
        ) + blogn)) - exp(-2 * exp(blogn)))   # Chen & Gupta (2000) pg54
        names(out) = c('cpt', 'conf.value')
        return(out)
      }
    }
    else{
      tmp = single.meanvar.norm.calc(data, extrainf = TRUE, minseglen)
      if (penalty == "MBIC") {
        tmp[, 3] = tmp[, 3] + log(tmp[, 1]) + log(n - tmp[, 1] + 1)
      }
      ans = decision(tmp[, 1], tmp[, 2], tmp[, 3], penalty, n, diffparam = 2, pen.value)
      if (class == TRUE) {
        rep = nrow(data)
        out = list()
        for (i in 1:rep) {
          out[[i]] = class_input(
            data[i,],
            cpttype = "mean and variance",
            method = "AMOC",
            test.stat = "Normal",
            penalty = penalty,
            pen.value = ans$pen,
            minseglen = minseglen,
            param.estimates = param.estimates,
            out = c(0, ans$cpt[i])
          )
        }
        return(out)
      }
      else{
        alogn = sqrt(2 * log(log(n)))
        blogn = 2 * log(log(n)) + (log(log(log(n))))
        out = cbind(ans$cpt, exp(-2 * exp(-alogn * sqrt(
          abs(tmp[, 2] - tmp[, 3])
        ) + blogn)) - exp(-2 * exp(blogn)))   # Chen & Gupta (2000) pg54
        colnames(out) = c('cpt', 'conf.value')
        rownames(out) = NULL
        return(out)
      }
    }
  }
