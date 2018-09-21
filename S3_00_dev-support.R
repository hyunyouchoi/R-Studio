
### General Purpose Utilities ##################################################

zip_to_list = function(a,b) {
  z = list()
  i = 1
  for (element in a) {
    z[[element]] = b[i]
    i = i + 1
  }
  z
}

rm_blanks = function(u) {

   i = which(u=="")
   if (length(i) == 0) {
     out = u
   } else {
     out = u[-i]
   }
  out
}

concat = function(...) {
  paste(..., sep="")
}

get_excel = function(file, sheet) {
  # needs openxlsx package
  read.xlsx(
      file
    , sheet=sheet
    , colNames=TRUE
    , startRow=1
    , skipEmptyRows=FALSE
    , skipEmptyCols=FALSE
    , detectDates=TRUE
    , check.names=TRUE
    , na.strings="NA"
  )
}

stack = function(..., labels=NA) {


  df_list = list(...)
  m = length(df_list)
  n = 0

  df_n_list = list()
  for (i in 1:m) {
    k = dim(df_list[[i]])[1]
    df_n_list[[i]] = k
    n = n + k
  }

  # Get all column names
  is_numeric_list = list()
  name_vec = c()
  for (i in 1:m) {
    test_df = df_list[[i]]
    for (name in names(test_df)) {
      is_numeric_list[name] = is.numeric(test_df[name])
    }
    name_vec = c(names(test_df), name_vec)
  }

  name_vec = unique(name_vec)

  # add missing columns to each data frame
  for (i in 1:m) {
    col_names = names(is_numeric_list)
    for (name in col_names) {
      if (!(name %in% names(df_list[[i]]))) {
        if (is_numeric_list[[name]]) {
          df_list[[i]][name] = NA
        } else {
          df_list[[i]][name] = "<NA>"
        }
      }
    }
  }

  # allocate stacked df space
  out_df = data.frame(dummy=numeric(n))
  for(name in name_vec) {
    if (is_numeric_list[[name]]) {
      out_df[name] = numeric(n)
    } else {
      out_df[name] = character(n)
    }
  }

  out_df = out_df[, !(names(out_df) %in% "dummy")]
  out_df["label"] = "<NA>"

  #populate data
  start_row = 0
  for (i in 1:m) {
    rows = df_n_list[[i]]
    curr_df = df_list[[i]]
    for (j in 1:rows) {
      k = start_row + j
      out_df[k, name_vec] = curr_df[j, name_vec]
      if (length(labels) == m) {
        out_df[k, "label"] = labels[i]
      }
    }
    start_row = start_row + rows
  }

  out_df

}

### Transformation #############################################################

delta = function(y, lag=1) {
  # needs zoo package
  rollapply(y, lag+1, function(x) {diff(x, lag=lag)}, fill=NA, align="right")
}

log_diff = function(y, lag=1) {
  # needs zoo package
  delta(log(y), lag=lag)
}

ma = function(y, n=1) {
  # needs zoo package
  rollapply(y, n, mean, fill=NA, align="right")
}

gr = function(y, lag=1) {
  100 * (delta(y, lag=lag)/shift(y, n=lag))
}


### Error Calculations #########################################################

calc_rsq = function(y, yhat) {

   mu = mean(y)
   sse = sum((y - yhat)^2)
   sst = sum((y - mu)^2)

   rsq_cost = 1 - sse/sst;

   rsq_cost
}

calc_rmse = function(y, yhat) {

  mse = mean((y - yhat)^2)
  rmse = sqrt(mse)
  rmse

}

calc_mad = function(y, yhat) {

  mad = mean(abs(y - yhat))
  mad

}

calc_mxad = function(y, yhat) {
  
  mxad = max(abs(y - yhat))
  mxad
  
}


calc_mape = function(y, yhat) {

  mape = mean(abs(yhat/y - 1))
  mape

}

### Binning ####################################################################

bin_interval_variable <- function(u, n=10) {

  if (!is.vector(u)) {
    u <- u[[1]]
  }

  if (100 %% n == 0) {
    pct_increment = n/100
  } else {
    print("Adjusted requested bin count to be a factor on 100.")
    new_n <- n - (100 %% n)
  }

  pct_list <- seq(from=pct_increment, to =1 - pct_increment, by=pct_increment)
  quantile_list <- unique(unname(quantile(u, pct_list)))

  # calculate bin midpoints with real data
  lo <- min(u)
  hi <- max(u)

  bin_values <- c(lo, quantile_list, hi)
  bin_len <- length(bin_values)

  midpoints <- rep(NA, bin_len - 1)

  for (i in 2:bin_len) {
    midpoints[i-1] <- mean(bin_values[(i-1):i])
  }


  breaks <- c(-Inf, quantile_list, Inf)
  bin_info <- list(
      breaks=breaks
    , midpoints=midpoints
  )

  bin_info

}

### Model Specific Functions ###################################################
get_bal_forecast = function(start_bal, scores) {
  start_bal * exp(cumsum(scores))
}

### Variable Selection #########################################################

calc_bic = function(y, yhat, k, family="binomial") {

   n = length(y)

   if (family == "binomal") {

      log_like = sum( y * log(yhat) + (1 - y) * log(1 - yhat))
      bic_cost = -2 * log_like + k * log(n)

   } else if (family == "gaussian") {

      sg2 = sum((y-yhat)^2)/n
      sg = sqrt(sg2)

      log_like = sum(log(dnorm(y, mean=yhat, sd=sg)))
      # k + 1 since we also had to estimate sigma
      bic_cost = -2 * log_like + (k + 1) * log(n)

   }



   return(bic_cost)

}

cv_step = function(data, resp="log_diff", test_var="", model="", from_yr=2007, to_yr=2016) {

  mod = c(test_var, model)
  mod = rm_blanks(mod)
  input_data = data[, c(resp, "year","target", mod), with=FALSE]
  setnames(input_data, resp, "resp")

  full_fit = lm(data=input_data, resp ~ .-year-target)
  # full_fit = lm(data=input_data, resp ~ ENTITY_NAME)
  params = data.frame(summary(full_fit)$coefficients)

  names(params) = c("est", "se", "t", "p_value")
  params$var = row.names(params)
  params = data.table(params)
  params = params[var != "(Intercept)",]

  worst_p_value = max(params[["p_value"]])
  if (test_var != "") {
    coefficient = params[var == test_var,"est"][[1]]
  } else {
    coefficient = 0
  }

  p = full_fit$rank
  n = length(full_fit$residuals)

  if (p > 2) {
    
    c1 <- try(worst_vif <- max(vif(full_fit)),silent = TRUE)
    if(is(c1,"try-error")){worst_vif<-NA}

  } else {
    worst_vif = 0
  }

  j = 1
  
  for (yr in from_yr:to_yr) {
    hold_out_yr = input_data[year == yr,]
    test_data = input_data[year != yr,]
    hold_out_test = hold_out_yr[, c("target","resp")]
    head(test_data)
    hold_out_fit = lm(data=test_data, resp ~ .-year-target)
    hold_out_test[["est"]] = predict(hold_out_fit, hold_out_yr)
    hold_out_test[["est.full"]] = predict(full_fit, hold_out_yr)
    if (test_var != "") {
      cv_coef = hold_out_fit$coefficients[[test_var]]
    } else {
      cv_coef = 0
    }

    if (j == 1) {
      cv_data = hold_out_test
      coef_data = cv_coef

    } else {
      cv_data = rbind(cv_data, hold_out_test)
      coef_data = rbind(cv_coef, coef_data)
    }
    j = j + 1
  }
  
  
  y       = cv_data[["resp"]]
  y_est   = cv_data[["est"]]
  y_est_f = cv_data[["est.full"]]
  
  bic = calc_bic(y, y_est, p, family="gaussian")
  rsq = calc_rsq(y, y_est)
  mape = calc_mape(y, y_est)
  rmse = calc_rmse(y, y_est)
  mad = calc_mad(y, y_est)
  mxad = calc_mxad(y, y_est)
  shapiro.pvalue=shapiro.test(y-y_est)$p.value

  bic.f  = calc_bic(y, y_est_f, p, family="gaussian")
  rsq.f  = calc_rsq(y, y_est_f)
  mape.f = calc_mape(y, y_est_f)
  rmse.f = calc_rmse(y, y_est_f)
  mad.f  = calc_mad(y,  y_est_f)
  mxad.f  = calc_mxad(y, y_est_f)
  shapiro.pvalue.f=shapiro.test(y-y_est_f)$p.value
  

  y       = cv_data[target==1,][["resp"]]
  y_est   = cv_data[target==1,][["est"]]
  y_est_f = cv_data[target==1,][["est.full"]]
  
  bic.sub = calc_bic(y, y_est, p, family="gaussian")
  rsq.sub = calc_rsq(y, y_est)
  mape.sub = calc_mape(y, y_est)
  rmse.sub = calc_rmse(y, y_est)
  mad.sub = calc_mad(y, y_est)
  mxad.sub = calc_mxad(y, y_est)
  shapiro.pvalue.sub=shapiro.test(y-y_est)$p.value
  
  bic.f.sub  = calc_bic(y, y_est_f, p, family="gaussian")
  rsq.f.sub  = calc_rsq(y, y_est_f)
  mape.f.sub = calc_mape(y, y_est_f)
  rmse.f.sub = calc_rmse(y, y_est_f)
  mad.f.sub  = calc_mad(y,  y_est_f)
  mxad.f.sub  = calc_mxad(y, y_est_f)
  shapiro.pvalue.f.sub=shapiro.test(y-y_est_f)$p.value  
  
  data.table(
      var=test_var
    , coefficient=round(coefficient,6)
    , rsq=round(rsq,6)
    , rsq.f=round(rsq.f,6)
    , rsq.sub=round(rsq.sub,6)
    , rsq.f.sub=round(rsq.f.sub,6)
    , rmse=round(rmse,6)
    , rmse.f=round(rmse.f,6)
    , rmse.sub=round(rmse.sub,6)
    , rmse.f.sub=round(rmse.f.sub,6)
    , mad=round(mad,6)
    , mad.f=round(mad.f,6)
    , mad.sub=round(mad.sub,6)
    , mad.f.sub=round(mad.f.sub,6)
    # , mxad=round(mxad,6)
    # , mxad.f=round(mxad.f,6)
    , shapiro.pvalue=shapiro.pvalue
    , shapiro.pvalue.f=shapiro.pvalue.f
    , shapiro.pvalue.sub=shapiro.pvalue.sub
    , shapiro.pvalue.f.sub=shapiro.pvalue.f.sub
    , worst_vif=worst_vif
    , n=n
    , p=p
    # , bic=bic
    # , mape=mape
    , worst_p_value=worst_p_value
    , coefficient_mapd=mean(abs(coef_data/coefficient - 1))
  )
}

cv_select = function(data, info, criteria="rsq", resp="log_diff", modl="", iter=2, from_yr=2007, to_yr=2016, vif_tol=10, sig_tol=0.10) {

  
  
  
  mdl.base<-info[name%in%modl,unique(base),]
  
  info_table = info[!base%in%mdl.base,,]
  selections = modl
  selection_rsq = NA
  summary_out = list()
  for (i in 1:iter) {

    # collect info on each variable
    if (dim(info_table)[1] != 0) {
      var_tuples = zip_to_list(info_table[["name"]],info_table[["sign"]])
      cat_tuples = zip_to_list(info_table[["name"]],info_table[["base"]])
      j = 1
      for (var in names(var_tuples)) {
          step_result = cv_step(
              data
            , resp=resp
            , test_var=var
            , model=selections
            , from_yr=from_yr
            , to_yr=to_yr
          )
        step_result[["sign"]] = var_tuples[[var]]
        step_result[["base"]] = cat_tuples[[var]]
        if (j == 1) {
          step_collection = step_result

        } else {
          step_collection = rbind(step_collection, step_result)
        }
        j = j + 1
      }

      # pick a variable
      if (criteria == "rsq") {
        step_collection = step_collection[order(-rsq)]
      } else if (criteria == "bic") {
        step_collection = step_collection[order(bic)]
      } else if (criteria == "mape") {
        step_collection = step_collection[order(mape)]
      } else if (criteria == "coefficient_mapd") {
        step_collection = step_collection[order(coefficient_mapd)]
      }

      summary_out[[concat("iteration-",i)]] = step_collection
      step_filter = step_collection[
          ((sign(coefficient) == sign) & (worst_vif <= vif_tol) & (worst_p_value <= sig_tol))
      ,]

      if (dim(step_filter)[1] != 0 ) {
        selection = step_filter[1, var][[1]]
        selection_base = step_filter[1, base][[1]]
        selections = c(selections, selection)
        selection_base = step_filter[1, base][[1]]
        selection_rsq = na.remove(c(selection_rsq, step_filter[1, rsq][[1]]))

        # remove variable from consideration
        info_table = info_table[!(name == selection | base == selection_base),,]
      } else {
        break
      }

    }
  }

  list(summary=summary_out, selections=rm_blanks(selections), selection_rsq=selection_rsq)

}





