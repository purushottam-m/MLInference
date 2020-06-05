
############################################################################
#  
#  Description: Treatment Effect Heterogeneity
#
#  Author: Purushottam Mohanty (M2, APE)
#  Date Modified: 08/04/2020
#
############################################################################


rm(list=ls(all=TRUE))

# Load Packages
if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
pacman::p_load(tidyverse, stats, readstata13, foreach, data.table, lmtest, sandwich, stargazer, xtable)

# setting main directory 
paths = c("/Users/purushottam/Documents/pse_local/coursework/machine_learning/MachineLearningHW", 
          "C://user/Wilma", 
          "C://Some/other/path")
names(paths) = c("purushottam", "mridul", "ucindami")
user.path <- paths[Sys.info()[['user']]]
setwd(user.path)

maindir <- paste0(getwd())
datadir <- paste0(maindir, "/1_Data")
outputdir <- paste0(maindir, "/3_Output")


###############   Replication ################## 

# import dataset
rep_df <- as.data.table(read.dta13(paste0(datadir, "/TurkeyPublicUseData.dta")))

# Values for categorical treatment variable
rep_df[maxTreat == "Control", Treat_assign := 0][maxTreat == "Treat", Treat_assign := 1]
rep_df[, maxTreat := NULL][, maxTreat := Treat_assign]

# Standardized employment outcome (of variables in table)
# Note this is missing if employed variable is missing, but otherwise is average of whatever variables are not missing
for(var in rbind("FF_work4wks1", "FF_work20hrs", "FF_numhrswk", "FF_inc_mth", "FF_ihs_inc_mth","occstatus", "FF_workSS", "FF_inc_frm")) {
  
  thismean <- rep_df[maxTreat == 0, mean(eval(parse(text = var)), na.rm = TRUE)]
  thissd <- rep_df[maxTreat == 0, sd(eval(parse(text = var)), na.rm = TRUE)]
  rep_df[, paste0("z1_", var) := ((eval(parse(text = var)) - thismean) / thissd)]  

}

rep_df[, employedindex2 := rowMeans(rep_df[, .(z1_FF_work4wks1, z1_FF_work20hrs, z1_FF_numhrswk, z1_FF_inc_mth, z1_FF_ihs_inc_mth, z1_FF_workSS, z1_occstatus)])] 
rep_df[is.na(FF_work4wks1), employedindex2 := NA] 


########################### TABLE 5: INDIVIDUAL HETEROGENEITY by age/gender strata ###########################

rep_df[, maxTreat_my := maxTreat*MM_my] %>%
  .[, maxTreat_mo := maxTreat*MM_mo] %>%
  .[, maxTreat_fy := maxTreat*MM_fy] %>%
  .[, maxTreat_fo := maxTreat*MM_fo]


a <- NULL

for(var in rbind("FF_work4wks1", "FF_work20hrs", "FF_numhrswk", "FF_inc_mth", "FF_ihs_inc_mth", "occstatus", "FF_workSS", "FF_inc_frm", "employedindex2")) {
  
  stratadum <- paste0("stratadum", 1:457)
  form <- as.formula(paste0(var, "~", "maxTreat_my + maxTreat_mo + maxTreat_fy + maxTreat_fo +", paste(stratadum, collapse = " + ")))
  # reg <- lm_robust(form, se_type = "stata", data = rep_df)

  reg <- lm(form, data = rep_df[eval_course == 1,])
  se <- coeftest(reg, vcov = vcovHC(reg, "HC1"))
  
  assign(paste0("ITT_emp4_", var), reg)
  a <- cbind(a, se[,2])
  
}

# exporting table
stargazer(ITT_emp4_FF_work4wks1, ITT_emp4_FF_work20hrs, ITT_emp4_FF_numhrswk, ITT_emp4_FF_inc_mth, ITT_emp4_FF_ihs_inc_mth, ITT_emp4_occstatus, ITT_emp4_FF_workSS, ITT_emp4_FF_inc_frm, 
          se = list(a), report = "vcs", keep.stat = "n",
          dep.var.labels = c("Working at all", "Working 20$+$ hrs", "Weekly hrs", "Monthly inc", "Trans. monthly inc", "Occ. status", "Formal work", "Formal inc"),
          keep = c("maxTreat_my", "maxTreat_mo", "maxTreat_fy", "maxTreat_fo"),
          covariate.labels = c("ITT for males under 25", "ITT for males over 25", "ITT for females under 25", "ITT for females over 25"),
          no.space = TRUE, float = FALSE, out = paste0(outputdir, "/table5.tex"))



# *************** Table 6 -heterogeneity with respect to these characteristics ******************

finaltable <- NULL

for (indvar in rbind(c("expectedbenefit", "posths", "prevcourse", "childunder6", "empoweredtowork", "raven", "numerate", "workcentral", "tenacity", "longterm"))) {
  
  rep_df[, paste0("maxTreat_", indvar) := maxTreat * eval(parse(text = indvar))]
  table <- data.frame(1:2)
  
  for (depvar in rbind("FF_work20hrs", "employedindex2", "employjan12", "formallyemployedAug2013")) {
    
    stratadum <- paste0("stratadum", 1:457)
    form <- as.formula(paste0(depvar, "~ maxTreat + ", paste0("maxTreat_", indvar, " + ", indvar, " + "), paste(stratadum, collapse = " + ")))
    
    reg <- lm(form, data = rep_df[eval_course == 1,])
    se <- coeftest(reg, vcov = vcovHC(reg, "HC1"))
    
    coeftable <- data.frame(c(round(se[[3, 1]], digits = 3), paste0("(", round(se[[3, 2]], digits = 3), ")")))
    names(coeftable) <- c(depvar)
    table <- cbind(table, coeftable)
    
  }
  
  table <- cbind(c(indvar, indvar), c("coef", "se"), table)
  names(table)[[1]] <- "indvar"
  names(table)[[2]] <- "type"
  
  finaltable <- rbind(finaltable, table)
  
}

# format Latex table
a <- xtable(
  finaltable %>%
    mutate(indvar = as.character(indvar)) %>%
    mutate(indvar = ifelse(type == "se", "", indvar)) %>%
    select(-X1.2, -type) %>%
    
    rename("Employed 20+ Hours" = FF_work20hrs) %>%
    rename("Aggregate Employment Index" = employedindex2) %>%
    rename("Ever Formal by Jan 12" = employjan12) %>%
    rename("Currently Formal Aug 13" = formallyemployedAug2013)
  )

align(a) <- "llcccc"

# export latex table
print(a, booktabs = TRUE, floating = FALSE, latex.environments = "center", include.rownames = FALSE,
      file = paste0(outputdir, "/table6.tex"))

















