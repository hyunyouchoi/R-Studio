StepFun <- function(
  var_info,
  loan_in,        #input loan level data
  macro_in,       #input macro data
  tier=1,         #indicate which tier of variables to consider
  y='FLR_nco_diff~',       #indicate response variable
  thresh=c(0.05, 0.01, 0.001),    #significance level for SE based p-value and LR test based p-value for each tier
  criteria='bic', #variable selection criteria; other values='bic', 'LR.p', 'SE.p', 'rsq'
  vars0 = c('1'),          #model 0 variables
  fix_vars0,    #indicate which variables are fixed
  out.print=T         #indicate wheter intermediate output will be printed 
){
  
  # loan_in <- loan
  # macro_in <- macro
  # tier <- 2
  # thresh <- c(0.05, 0.01, 0.001)
  # y <- 's2_qd~'
  # criteria <- 'rsq'
  # vars0 <- c("ngdp_grw_lag4", "tr10y_qd")
  # fix_vars0 <- c(1)
  # out.print <- T
  
  use_info <- var_info[, c('var', 'base', 'sign', 'tier'), with=F]
  setnames(use_info, c('Variable', 'Base', 'Sign', 'Tier'))
  
  base0 <- unique(use_info[Variable%in%vars0, ]$Base)
  candidate <- copy(use_info[!Base%in%base0 & Tier==tier, ])
  
  
  vars <- vars0 #vars will change as more variables added (dropped), vars0 is the initial variable list
  vars_nm <- all.vars(as.formula(paste(y, paste(vars, collapse='+'), sep='')))
  
  ## Define initial model0
  # macro_in = macro_all
  data0 <- merge(loan_in, macro_in, by='Date')
  data0 <- data0[complete.cases(data0[,vars_nm, with=F])]
  model0 <- lm(as.formula(paste(y, paste(vars, collapse='+'), sep='')),
               data=data0)
  sum_0 <- summary(model0)
  aic_0 <- -2*as.numeric(logLik(model0))+2*attr(logLik(model0), 'df')
  bic_0 <- -2*as.numeric(logLik(model0))+log(nrow(data0))*attr(logLik(model0), 'df')
  rsq_0 <- sum_0$adj.r.squared
  
  
  add_var <- 0
  k <- 0
  result_list <- list()
  while(length(add_var)>0){
    k <- k+1
    ## Add 1 variable to model0
    add_list <- list()
    add_table <- NULL
    system.time({
      for(var in candidate$Variable){
        formula <- as.formula(paste(y, paste(c(vars, var), collapse='+'), sep=''))
        vars_nm <- all.vars(formula)
        
        model_k <- lm(formula, data=data0[,vars_nm, with=F])
        sum_k <- summary(model_k)
        add_list[[length(add_list)+1]] <- sum_k
        gc();gc();
        
        loglik <- as.numeric(logLik(model_k))
        df <- attr(logLik(model_k), 'df')
        coef <- sum_k$coefficients[nrow(sum_k$coefficients), 1]
        SE.p=sum_k$coefficients[nrow(sum_k$coefficients), 4]*ifelse(candidate[Variable==var,]$Sign==0, 1, 0.5)
        aic <- -2*loglik+2*df
        bic <- -2*loglik+log(nrow(data0))*df
        LR.p <- pchisq(2*(loglik-as.numeric(logLik(model0))),1,lower.tail=F)*ifelse(candidate[Variable==var,]$Sign==0, 1, 0.5)
        rsq <- sum_k$adj.r.squared
        if(length(add_table)==0){
          add_table <- data.table(add.var=var, coef, SE.p, aic, bic, LR.p, rsq)
        }else{
          add_table <- rbind(add_table, data.frame(add.var=var, coef, SE.p, aic, bic, LR.p, rsq))
        }
      }
      names(add_list) <- candidate$Variable
    })
    
    
    add_table <- add_table[, ':='(
      expected_sign=candidate$Sign
      ,sign_correct=sign(coef)*candidate$Sign>=0
      ,SE.p.sig=SE.p<=thresh[candidate[Variable==add.var,]$Tier]
      ,aic_correct=aic<=aic_0
      ,bic_correct=bic<=bic_0
      ,LR.sig=LR.p <= thresh[candidate[Variable==add.var,]$Tier]
      ,rsq_correct=rsq>rsq_0
    ),]
    
    cat('\n\n\n -----------------------Step ', k, '-----------------------\n\n',sep='')
    setorderv(add_table, criteria, ifelse(criteria=='rsq', -1, 1))
    if(out.print==T){
      print(add_table[sign_correct==T, unique(c('add.var', 'coef','sign_correct','SE.p', 'rsq', 'SE.p.sig', criteria)), with=F], digits=2)
      }
    #choose the variable to add
    if(criteria=='aic'){
      add_var <- add_table[aic_correct==T&sign_correct==T, ]$add.var[1]
    }else if(criteria=='bic'){
      add_var <- add_table[bic_correct==T&sign_correct==T, ]$add.var[1]
    }else if(criteria=='LR.p'){
      add_var <- add_table[LR.sig==T&sign_correct==T, ]$add.var[1]
    }else if (criteria =='rsq'){
      setorderv(add_table, criteria, -1)
      add_var <- add_table[rsq_correct==T&sign_correct==T&SE.p.sig==T, ]$add.var[1]
    }else{
      add_var <- add_table[SE.p.sig==T&sign_correct==T, ]$add.var[1]
    }
    add_var <- as.character(add_var)
    
    add_results <- list(all_models=add_table, add_var=add_var)
    drop_results <- list()
    if(length(add_var)>0&!is.na(add_var)){
      
      vars <- c(vars, add_var)
      vars_nm <- all.vars(as.formula(paste(y, paste(vars, collapse='+'), sep='')))
      
      
      ##Update candidate list
      add_base <- candidate[Variable==add_var,]$Base
      candidate <- candidate[Base!=add_base, ]
      sum_0 <- add_list[[add_var]]
      aic_0 <- add_table[add.var==add_var, ]$aic
      bic_0 <- add_table[add.var==add_var, ]$bic
      rsq_0 <- sum_0$adj.r.squared
      
      cat('\n ---Add variable: ', add_var, '---\n', sep='')
      if(out.print){cat('\n ---Selected model: ---\n')
        print(add_list[[add_var]])}
      
      ## Remove insignificant coefficient if any
      sig_levels <- unlist(lapply(setdiff(vars, c("1")), function(x){
        if(x%in%use_info$Variable){
          thresh[use_info[Variable==x, ]$Tier]*ifelse(use_info[Variable==x, ]$Sign==0, 1, 2)
        }else{
          thresh[1]
        }
      }))
      insig_ind0 <- which(pmax(add_list[[add_var]]$coefficients[-1,4]-sig_levels, 0)>0)
      insig_ind <- setdiff(insig_ind0, insig_ind0[which(vars[insig_ind0]%in%fix_vars0)])
      
      if(length(insig_ind)>0){
        cat('\n ---Remove insignificant variable(s):---\n',sep='')
        while(length(insig_ind)>0){
          drop_list <- list()
          drop_table <- NULL
          for(x in insig_ind){
            formula_drop <- as.formula(paste(y, paste(vars[-x], collapse='+'),sep=''))
            vars_nm <- all.vars(formula_drop)
            model_drop <- lm(formula_drop, data=data0)
            drop_list[[length(drop_list)+1]] <- summary(model_drop)
            gc();gc();
            
            drop.var <- vars[x]
            aic <- -2*as.numeric(logLik(model_drop))+2*attr(logLik(model_drop), 'df')
            bic <- -2*as.numeric(logLik(model_drop))+log(nrow(data0))*attr(logLik(model_drop), 'df')
            LR.p <- pchisq(aic-aic_0+2,1,lower.tail=F)
            rsq <- summary(model_drop)$adj.r.squared
            
            if(length(drop_table)==0){
              drop_table <- data.table(drop.var, aic, bic, LR.p, rsq)
            }else{
              drop_table <- rbind(drop_table, data.table(drop.var, aic, bic, LR.p, rsq))
            }
          }
          names(drop_list) <-vars[insig_ind]
          drop_table <- drop_table[, ':='(
            aic_correct=aic<=aic_0
            ,bic_correct=bic<=bic_0
            ,LR.sig=LR.p<=sig_levels[x]
            , rsq_correct=rsq>=rsq_0
          )]
          
          if(out.print){print(drop_table)}
          if(criteria=='aic'){
            setorderv(drop_table, criteria, c(1))
            drop_var <- drop_table[aic_correct==T, ]$drop.var[1]
          }else if(criteria=='bic'){
            setorderv(drop_table, criteria, c(1))
            drop_var <- drop_table[bic_correct==T, ]$drop.var[1]
          }else if(criteria=='LR.p'){
            setorderv(drop_table, criteria, c(-1))
            drop_var <- drop_table[LR.sig==F, ]$drop.var[1]
          }else if(criteria=='rsq'){
            setorderv(drop_table, criteria, c(-1))
            drop_var <- drop_table[rsq_correct==T,]$drop.var[1]
          }else{
            insig_pval <- add_list[[add_var]]$coefficients[-1,4]-sig_levels
            drop_var <- vars[intersect(which(insig_pval>0 &insig_pval==pmax(insig_pval)), insig_ind)]
          }
          
          drop_results[[length(drop_results)+1]] <- list(drop_table=drop_table, drop_var=drop_var)
          
          if(is.na(drop_var)){drop_var <- NULL}
          if(length(drop_var)>0){
            if(drop_var%in%fix_vars0){
              cat('\n No variable is dropped because initial list is fixed.')
              insig_ind <- insig_ind[-which(names(insig_ind)==drop_var)]
              
            }else{
              cat('\n Drop variable: ', drop_var, '\n\n', sep='')
              ## Update candidate list:
              
              drop_base <- use_info[Variable==drop_var,]$Base
              candidate <- rbind(candidate, use_info[Base==drop_base, ])
              
              vars <- setdiff(vars, drop_var)
              sig_levels <- unlist(lapply(vars, function(x){
                if(x%in%use_info$Variable){
                  thresh[use_info[Variable==x, ]$Tier]*ifelse(use_info[Variable==x, ]$Sign==0, 1, 2)
                }else{
                  thresh[1]
                }
              }))
              insig_ind <-  which(pmax(drop_list[[drop_var]]$coefficients[-1,4]-sig_levels, 0)>0)
              sum_0 <- drop_list[[drop_var]]
              aic_0 <- drop_table[drop.var==drop_var,]$aic
              bic_0 <- drop_table[drop.var==drop_var,]$bic
            }
          }else{
            cat('\n No variable is dropped because dropping will result in higher AIC (or BIC) or significant drop in likelihood.')
            insig_ind <- NULL
          }
          
        }#end while
      }
      
    }else{
      add_var <- NULL
      cat('No variable added.')
    }
    
    result_list[[k]] <- list(add_results=add_results, drop_results=drop_results, final_model=sum_0)
    gc();gc();
  }
  
  return(result_list)
}