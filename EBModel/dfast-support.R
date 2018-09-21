
get_hold_out_perf = function(history, train_data, bal_var="all", dt_var="qtr_dt", resp="log_diff", model="", tau=0.50, type="smp", ar_term="log_diff_lag1") {

  ### make fit object
  keep_vars = c(resp, model, dt_var, bal_var)
  hst = history[, c(dt_var, bal_var), with=FALSE]
  setnames(hst, c(dt_var, bal_var), c("dt", "bal"))
  hst[["year"]] = year(hst[["dt"]])

  hst_desc = hst[order(-dt),.SD[1], keyby="year"]
  last_qtr_bal = hst_desc[,.SD[1], keyby="year"][, c("year", "bal")]
  prior_year_end_bal = last_qtr_bal[, year := year + 1]

  ### make test data
  keep_vars = c(resp, model, dt_var, bal_var)
  train = train_data[, keep_vars, with=FALSE]
  setnames(train, c(dt_var, resp, bal_var), c("dt", "resp", "bal"))
  train[["year"]] = year(train[["dt"]])

  if (type == "arx") {
    model = replace(model, model == ar_term, "resp_lag1")
    setnames(train, c(ar_term), c("resp_lag1"))
  }


  train_years = unique(train[["year"]])
  j = 1
  for (yr in train_years) {

    hold_out_yr = train[year == yr, ]
    input_data = train[year != yr, ]

    if (type == "qrg") {
      hold_out_fit_obj = rq(resp~., tau=tau, data=input_data[, c("resp", model), with=FALSE])
    } else {
      hold_out_fit_obj = lm(resp~., data=input_data[, c("resp", model), with=FALSE])
    }

    coefs = data.frame(summary(hold_out_fit_obj)$coefficients)

    if (type == "qrg") {

      names(coefs) = c("est", "lower_bd", "uppder_bd")
      coefs$var = row.names(summary(hold_out_fit_obj)$coefficients)
      coefs$yr_out = yr

    } else {

      names(coefs) = c("est", "se", "t", "p_value")
      coefs$var = row.names(summary(hold_out_fit_obj)$coefficients)
      coefs$yr_out = yr

    }

    if (j == 1) {
      hold_out_coefs = coefs
    } else {
      hold_out_coefs = rbind(hold_out_coefs, coefs)
    }

    start_bal = prior_year_end_bal[year == yr, "bal"][[1]]
    target_data = hold_out_yr
    predictions = predict(hold_out_fit_obj, hold_out_yr)
    target_data[["resp_hat"]] = predictions
    target_data[["bal_est"]] = get_bal_forecast(start_bal, predictions)

    if (j == 1) {
      target_hold_data = target_data[, c("dt", "year", "bal", "bal_est", "resp", "resp_hat"), with=FALSE]
    } else {
      target_hold_data = rbind(target_hold_data, target_data[, c("dt","year", "bal", "bal_est", "resp", "resp_hat")])
    }
    j = j + 1
  }

  coef_data = data.table(hold_out_coefs)
  coef_data = coef_data[order(var),]
  list(hold_out_data=target_hold_data,hold_out_coefs=coef_data)

}


### out of time testing
get_oot_data = function(train_data, test_data, bal_var="all", dt_var="qtr_dt", resp="log_diff", model="", tau=0.50, type="smp", ar_term="log_diff_lag1", max_train_dt="2016-09-30",ma_n=2) {

  ### make fit object
  keep_vars = c(resp, model, dt_var, bal_var)
  train_ind = ifelse(train_data[[dt_var]] <= as.Date(max_train_dt), TRUE, FALSE)
  train = train_data[train_ind, keep_vars, with=FALSE]

  ### make test data
  keep_vars = c(resp, model, dt_var, bal_var)
  test = test_data[, keep_vars, with=FALSE]
  setnames(train, c(dt_var, resp, bal_var), c("dt", "resp", "bal"))
  setnames(test, c(dt_var, resp, bal_var), c("dt", "resp", "bal"))

  test[["bal_ma"]] = ma(test[["bal"]], n=ma_n)


  if (type == "arx") {
    model = replace(model, model == ar_term, "resp_lag1")
    setnames(train, c(ar_term), c("resp_lag1"))
    setnames(test, c(ar_term), c("resp_lag1"))
  }

  if (type == "qrg") {
    fit_obj = rq(resp~., tau=tau, data=train[, c("resp", model), with=FALSE])
  } else {
    fit_obj = lm(resp~., data=train[, c("resp", model), with=FALSE])
  }

  test[["resp_hat"]] = predict(fit_obj, test)

  if (type == "arx") {
    ### use calculated resp_lag1
    test[dt >= as.Date(max_train_dt),][["resp_hat"]] = predict_arx(fit_obj, test[dt >= as.Date(max_train_dt),], ar_term="resp_lag1")
  }

  test[["start_bal"]] = test[dt == as.Date(max_train_dt),][["bal_ma"]]
  test[["cum_resp_hat"]] = 0
  test[, `:=`(cum_resp_hat = cumsum(ifelse(dt > as.Date(max_train_dt), resp_hat, 0)))]
  test[["is_oot"]] = ifelse(test[["dt"]] > as.Date(max_train_dt), 1, 0)
  test[["bal_est"]] = test[["start_bal"]] * exp(test[["cum_resp_hat"]])

  if (type == "arx") {
    setnames(test, "resp_lag1", ar_term)
  }

  test
}



### Forecasts ##################################################################
get_forecasts = function(train_data, base, adv, sev, bal_var="all", dt_var="qtr_dt", model="", model_obj=NA, resp="log_diff", type="smp", ar_term="log_diff_lag1", ma_n=2) {

  ### get historical data
  actuals = train_data
  if (type == "arx") {
    keep_vars_for_actuals = c(resp, dt_var, bal_var, ar_term)
  } else{
    keep_vars_for_actuals = c(resp, dt_var, bal_var)
  }
  actuals = actuals[, keep_vars_for_actuals, with=FALSE]
  setnames(actuals, c(dt_var, resp, bal_var), c("dt", "resp", "bal"))

  ### set up scenario data

  keep_vars = c(model, dt_var)
  baseline = baseline[, keep_vars, with=FALSE]
  adverse = adverse[, keep_vars, with=FALSE]
  severe = severe[, keep_vars, with=FALSE]
  setnames(baseline, c(dt_var), c("dt"))
  setnames(adverse, c(dt_var), c("dt"))
  setnames(severe, c(dt_var), c("dt"))

  first_actual_record = actuals[order(dt), .SD[1]]
  first_actual_dt = first_actual_record[["dt"]]

  baseline = baseline[dt >= first_actual_dt,]
  adverse = adverse[dt >= first_actual_dt,]
  severe = severe[dt >= first_actual_dt,]

  ### place actuals into scenario data
  if (type == "arx") {
    outcomes = actuals[, c("dt", "resp", "bal", ar_term), with=FALSE]
  } else {
    outcomes = actuals[, c("dt", "resp", "bal")]
  }

  outcomes[["bal_ma"]] = ma(outcomes[["bal"]], n=ma_n)

  baseline = outcomes[baseline, on="dt"]
  adverse = outcomes[adverse, on="dt"]
  severe = outcomes[severe, on="dt"]

  last_actual_record = outcomes[order(-dt), .SD[1]]
  last_actual_dt = last_actual_record[["dt"]]
  last_actual_bal = last_actual_record[["bal_ma"]]

  baseline[["is_fcst"]] = ifelse(baseline[["dt"]] > last_actual_dt, 1, 0)
  adverse[["is_fcst"]] = ifelse(adverse[["dt"]] > last_actual_dt, 1, 0)
  severe[["is_fcst"]] = ifelse(severe[["dt"]] > last_actual_dt, 1, 0)

  if (type != "qrg") {
    baseline[, c("fcst", "pi_lo","pi_hi")] = data.frame(predict(model_obj, baseline, interval="predict"))
    adverse[, c("fcst", "pi_lo","pi_hi")] = data.frame(predict(model_obj, adverse, interval="predict"))
    severe[, c("fcst", "pi_lo","pi_hi")] = data.frame(predict(model_obj, severe, interval="predict"))
  } else {
    baseline[, c("fcst", "pi_lo","pi_hi")] = data.frame(predict(model_obj, baseline, interval="confidence"))
    adverse[, c("fcst", "pi_lo","pi_hi")] = data.frame(predict(model_obj, adverse, interval="confidence"))
    severe[, c("fcst", "pi_lo","pi_hi")] = data.frame(predict(model_obj, severe, interval="confidence"))
  }

  if (type == "arx") {
    ### use calculated resp_lag1
    baseline[dt >= as.Date(fcst_start_dt),][["fcst"]] = predict_arx(model_obj, baseline[qtr_dt >= as.Date(last_actual_dt),], ar_term=ar_term)
    adverse[dt >= as.Date(fcst_start_dt),][["fcst"]] = predict_arx(model_obj, adverse[qtr_dt >= as.Date(last_actual_dt),], ar_term=ar_term)
    severe[dt >= as.Date(fcst_start_dt),][["fcst"]] = predict_arx(model_obj, severe[qtr_dt >= as.Date(last_actual_dt),], ar_term=ar_term)
  }

  baseline[["xfcst"]] = ifelse(baseline[["is_fcst"]] == 1, baseline[["fcst"]], 0)
  adverse[["xfcst"]] = ifelse(adverse[["is_fcst"]] == 1, adverse[["fcst"]], 0)
  severe[["xfcst"]] = ifelse(severe[["is_fcst"]] == 1, severe[["fcst"]], 0)

  baseline[["fcst_bal"]] = get_bal_forecast(last_actual_bal, baseline[["xfcst"]])
  adverse[["fcst_bal"]] = get_bal_forecast(last_actual_bal, adverse[["xfcst"]])
  severe[["fcst_bal"]] = get_bal_forecast(last_actual_bal, severe[["xfcst"]])

  baseline[["fcst_bal"]] = ifelse(baseline[["is_fcst"]] == 1, baseline[["fcst_bal"]], baseline[["bal"]])
  adverse[["fcst_bal"]] = ifelse(adverse[["is_fcst"]] == 1, adverse[["fcst_bal"]], adverse[["bal"]])
  severe[["fcst_bal"]] = ifelse(severe[["is_fcst"]] == 1, severe[["fcst_bal"]], severe[["bal"]])

  ### output balance forecasts
  baseline_fcst_bal = baseline[, c("dt", "fcst_bal", "bal")]
  adverse_fcst_bal = adverse[, c("dt", "fcst_bal")]
  severe_fcst_bal = severe[, c("dt", "fcst_bal")]

  ### output ldiff forecasts
  baseline_fcst_ldiff = baseline[, c("dt", "resp", "fcst", "pi_lo", "pi_hi")]
  adverse_fcst_ldiff = adverse[, c("dt", "resp", "fcst", "pi_lo", "pi_hi")]
  severe_fcst_ldiff = severe[, c("dt", "resp", "fcst", "pi_lo", "pi_hi")]

  setnames(baseline_fcst_bal, "fcst_bal", "baseline")
  setnames(adverse_fcst_bal, "fcst_bal", "adverse")
  setnames(severe_fcst_bal, "fcst_bal", "severe")

  setnames(baseline_fcst_ldiff, c("fcst", "pi_lo", "pi_hi"), c("baseline" ,"baseline_pi_lo", "baseline_pi_hi"))
  setnames(adverse_fcst_ldiff, c("fcst", "pi_lo", "pi_hi"), c("adverse", "adverse_pi_lo", "adverse_pi_hi"))
  setnames(severe_fcst_ldiff,  c("fcst", "pi_lo", "pi_hi"), c("severe", "severe_pi_lo", "severe_pi_hi"))


  fcst_bal_data = severe_fcst_bal[adverse_fcst_bal[baseline_fcst_bal, on="dt"], on="dt"]
  key = c("dt", "resp")
  fcst_ldiff_data = severe_fcst_ldiff[adverse_fcst_ldiff[baseline_fcst_ldiff, on=key], on=key]
  fcst_ldiff_data[!is.na(resp), ns_resp_hat := baseline]

  list(balance=fcst_bal_data, ldiff=fcst_ldiff_data)

}
