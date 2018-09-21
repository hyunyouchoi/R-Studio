
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

calc_mape = function(y, yhat) {

  mape = mean(abs(yhat/y - 1))
  mape

}


calc_smape = function(y, yhat) {

  num_i = 2 * abs(y - yhat)
  den_i = abs(y) + abs(yhat)
  smape = mean(num_i/den_i)
  smape

}

calc_maape = function(y, yhat) {

  aape = atan(abs(y - yhat)/abs(yhat))
  maape = mean(aape)
  maape
}

calc_under_rate = function(y, yhat, pct_tol=0.025) {
  pct_less_than_y = -(yhat/y - 1)

  under_rate = mean(ifelse(pct_less_than_y > pct_tol, 1, 0))
  under_rate
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

get_ldiff_forecast = function(start_ldiff, scores) {
  ldiff = start_ldiff + cumsum(scores)

  ldiff
}

### PSI Series #################################################################
get_psi_from_vector_amts = function(snap_shot, data, cols=NA, const=1e-6) {

  snap_raw = apply(as.matrix(snap_shot[,cols, with=FALSE]), 2, sum)
  snap_row_sums = sum(snap_raw)
  snap_dist = snap_raw/snap_row_sums

  raw = as.matrix(data[, cols, with=FALSE])
  row_sums = apply(raw, 1, sum)
  dist = raw/row_sums

  str = dim(dist)
  I = str[1]
  J = str[2]

  psi_series = sapply(1:I,
    function(i) {
      psi = 0
      for (j in 1:J) {
        psi = psi + (dist[i, j] - snap_dist[j]) * log(dist[i, j]/snap_dist[j])
      }
      psi
    }
  )
  names(psi_series) = NULL
  psi_series

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

#### outlier flag ##############################################################
is_outlier_ia = function(x) {

  # Iglewicz and Hoaglin modififed z-score
  K = 0.6745
  p50 = median(x, na.rm=TRUE)
  mdd = median(abs(x - p50), na.rm=TRUE)
  mod_z = K * (x - p50)/mdd

  ifelse(abs(mod_z) > 3.5, 1, 0)

}


################################################################################

cv_step_bal = function(data, bal="bal", resp="log_diff", use_cochrane_orcutt=FALSE, test_var="", model="", from_yr=2007, to_yr=2016) {
  # need orcutt package.

  mod = c(test_var, model)
  mod = rm_blanks(mod)
  input_data = data[, c(bal, resp, "year", mod), with=FALSE]
  mod_stuff = c("resp", mod)

  setnames(input_data, c(resp, bal), c("resp", "bal"))

  test_fit = lm(data=input_data[,mod_stuff, with=FALSE], resp ~ .)
  if (use_cochrane_orcutt == TRUE) {
    full_fit = cochrane.orcutt(lm(data=input_data[,mod_stuff, with=FALSE], resp ~ .))
  } else {
    full_fit = test_fit
  }

  params = data.frame(summary(full_fit)$coefficients)

  names(params) = c("est", "se", "t", "p_value")
  params$var = row.names(params)
  params = data.table(params)
  params = params[var != "(Intercept)",]

  worst_p_value = max(params[["p_value"]])
  if (test_var != "") {
    coefficient = params[var == test_var,"est"][[1]]
    correlation = cor(input_data[, c("resp", test_var), with=FALSE])["resp", test_var]
  } else {
    coefficient = 0
    correlation = 0
  }

  p = full_fit$rank
  n = length(full_fit$residuals)

  if (p > 2) {
    worst_vif = max(vif(test_fit)) #needs car package
  } else {
    worst_vif = 0
  }


  j = 1
  for (yr in from_yr:to_yr) {
    hold_out_yr = input_data[year == yr,]
    test_data = input_data[year != yr,]
    hold_out_test = hold_out_yr[, c("resp","bal")]


    if (yr == from_yr) {
      start_bal = input_data[year == yr,.SD[1]][["bal"]]
    } else {
      start_bal = input_data[ year == (yr - 1),.SD[.N]][["bal"]]
    }

    hold_out_fit = lm(data=test_data[, mod_stuff, with=FALSE], resp ~ .)
    hold_out_test[["est"]] = predict(hold_out_fit, hold_out_yr)
    hold_out_test[["bal_est"]] = get_bal_forecast(start_bal, hold_out_test[["est"]])

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

  y = cv_data[["resp"]]
  y_est = cv_data[["est"]]

  bal_y = cv_data[["bal"]]
  bal_y_est = cv_data[["bal_est"]]

  bic = calc_bic(y, y_est, p, family="gaussian")
  rsq = calc_rsq(y, y_est)
  mape = calc_mape(y, y_est)
  smape = calc_smape(y, y_est)
  maape = calc_maape(y, y_est)

  bmape = calc_mape(bal_y, bal_y_est)
  bsmape = calc_smape(bal_y, bal_y_est)

  data.table(
      var=test_var
    , n=n
    , p=p
    , bic=bic
    , rsq=rsq
    , mape=mape
    , smape=smape
    , maape=maape
    , bmape=bmape
    , bsmape=bsmape
    , worst_p_value=worst_p_value
    , worst_vif=worst_vif
    , coefficient=coefficient
    , correlation=correlation
    , coefficient_mapd=mean(abs(coef_data/coefficient - 1))
  )
}


cv_select_bal = function(data, info, criteria="rsq", bal="bal", resp="log_diff", use_cochrane_orcutt=FALSE, modl="", iter=2, from_yr=2007, to_yr=2016, vif_tol=10, sig_tol=0.10) {

  info_table = info
  selections = modl
  selection_rsq = NA
  selection_mape = NA
  selection_smape = NA
  selection_bmape = NA
  selection_bsmape = NA
  summary_out = list()
  for (i in 1:iter) {

    # collect info on each variable
    if (dim(info_table)[1] != 0) {
      var_tuples = zip_to_list(info_table[["name"]],info_table[["sign"]])
      cat_tuples = zip_to_list(info_table[["name"]],info_table[["base"]])
      j = 1
      for (var in names(var_tuples)) {
          step_result = cv_step_bal(
              data
            , bal=bal
            , resp=resp
            , use_cochrane_orcutt=use_cochrane_orcutt
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
      } else if (criteria == "smape") {
        step_collection = step_collection[order(smape)]
      } else if (criteria == "bmape") {
        step_collection = step_collection[order(bmape)]
      } else if (criteria == "bsmape") {
        step_collection = step_collection[order(bsmape)]
      } else if (criteria == "maape") {
        step_collection = step_collection[order(maape)]
      } else if (criteria == "coefficient_mapd") {
        step_collection = step_collection[order(coefficient_mapd)]
      }

      summary_out[[concat("iteration-",i)]] = step_collection

      step_filter = step_collection[
          ((sign(coefficient) == sign) & (sign(correlation) == sign) & (worst_vif <= vif_tol) & (worst_p_value <= sig_tol))
      ,]

      if (dim(step_filter)[1] != 0 ) {
        selection = step_filter[1, var][[1]]
        selection_base = step_filter[1, base][[1]]
        selections = c(selections, selection)
        selection_base = step_filter[1, base][[1]]

        selection_rsq = na.remove(c(selection_rsq, step_filter[1, rsq][[1]]))
        selection_mape = na.remove(c(selection_mape, step_filter[1, mape][[1]]))
        selection_smape = na.remove(c(selection_smape, step_filter[1, smape][[1]]))
        selection_bmape = na.remove(c(selection_bmape, step_filter[1, bmape][[1]]))
        selection_bsmape = na.remove(c(selection_bmape, step_filter[1, bsmape][[1]]))

        # remove variable from consideration
        info_table = info_table[!(name == selection | base == selection_base),]
      } else {
        break
      }

    }
  }

  list(
      summary=summary_out
    , selections=rm_blanks(selections)
    , selection_rsq=selection_rsq
    , selection_mape=selection_mape
    , selection_smape=selection_smape
    , selection_bmape=selection_bmape
    , selection_bsmape=selection_bsmape
  )


}

cv_step_qr_bal = function(data, bal="bal", resp="log_diff", tau=0.50, test_var="", model="", from_yr=2007, to_yr=2016) {

  # needs quantreg package

  mod = c(test_var, model)
  mod = rm_blanks(mod)
  input_data = data[, c(bal, resp, "year", mod), with=FALSE]
  setnames(input_data, c(resp, bal), c("resp", "bal"))
  mod_stuff = c("resp", mod)


  full_fit = rq(data=input_data[,mod_stuff, with=FALSE], resp ~ ., tau=tau)
  fit_dummy = lm(data=input_data[,mod_stuff, with=FALSE], resp ~ .)

  params = data.frame(summary(full_fit)$coefficients)

  names(params) = c("est", "lower_bd", "upper_bd")
  params$var = row.names(params)
  params = data.table(params)
  params = params[var != "(Intercept)",]

  if (test_var != "") {
    coefficient = params[var == test_var,"est"][[1]]
    correlation = cor(input_data[, c("resp", test_var), with=FALSE])["resp", test_var]
  } else {
    coefficient = 0
    correlation = 0
  }

  p = fit_dummy$rank
  n = length(full_fit$residuals)

  if (p > 2) {
    worst_vif = max(vif(fit_dummy)) #needs car package
  } else {
    worst_vif = 0
  }

  j = 1
  for (yr in from_yr:to_yr) {
    hold_out_yr = input_data[year == yr,]
    test_data = input_data[year != yr,]
    hold_out_test = hold_out_yr[, c("resp","bal")]


    if (yr == from_yr) {
      start_bal = input_data[year == yr,.SD[1]][["bal"]]
    } else {
      start_bal = input_data[ year == (yr - 1),.SD[.N]][["bal"]]
    }


    hold_out_fit = rq(data=test_data[,mod_stuff, with=FALSE], resp ~ ., tau=tau)
    hold_out_test[["est"]] = predict(hold_out_fit, hold_out_yr)
    hold_out_test[["bal_est"]] = get_bal_forecast(start_bal, hold_out_test[["est"]])

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

  y = cv_data[["resp"]]
  y_est = cv_data[["est"]]

  bal_y = cv_data[["bal"]]
  bal_y_est = cv_data[["bal_est"]]

  bic = calc_bic(y, y_est, p, family="gaussian")
  rsq = calc_rsq(y, y_est)
  mape = calc_mape(y, y_est)
  smape = calc_smape(y, y_est)
  maape = calc_maape(y, y_est)

  bmape = calc_mape(bal_y, bal_y_est)
  bsmape = calc_smape(bal_y, bal_y_est)

  data.table(
      var=test_var
    , n=n
    , p=p
    , bic=bic
    , rsq=rsq
    , mape=mape
    , smape=smape
    , maape=maape
    , bmape=bmape
    , bsmape=bsmape
    , worst_vif=worst_vif
    , coefficient=coefficient
    , correlation=correlation
    , coefficient_mapd=mean(abs(coef_data/coefficient - 1))
  )
}
