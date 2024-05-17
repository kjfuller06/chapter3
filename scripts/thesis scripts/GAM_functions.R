gamstatfun = function(modelnom, response){
  setwd("C:/chapter3/outputs")
  gam1 = readRDS(paste0(modelnom, ".rds"))
  
  capture.output(
    paste0("AIC = ", AIC(logLik.gam(gam1))),
    file = paste0(modelnom, "_modeloutputs.txt"))
  capture.output(
    print("**************************summary****************************"),summary(gam1),
    file = paste0(modelnom, "_modeloutputs.txt"),
    append = T)
  capture.output(
    print("**************************anova******************************"), anova(gam1),
    file = paste0(modelnom, "_modeloutputs.txt"),
    append = T)
  capture.output(
    print("***************************gam.check**************************"), gam.check(gam1),
    file = paste0(modelnom, "_modeloutputs.txt"),
    append = T)
  capture.output(
    print("***************************k.check**************************"), k.check(gam1),
    file = paste0(modelnom, "_modeloutputs.txt"),
    append = T)
  
  jpeg(paste0(modelnom, "_diagnosticplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
  par(mfrow = c(2, 2))
  gam.check(gam1)
  dev.off()
  
  jpeg(paste0(modelnom, "_partialeffectplots.jpeg"), width = 3000, height = 2000, res = 200, units = "px")
  plot(gam1, pages = 1,
       all.terms = T,
       rug = T,
       se = T,
       shade = T,
       shift = coef(gam1)[1])
  dev.off()
  
  setwd("E:/chapter3/for GAMs")
  test = read.csv("testingset_isochrons.csv")
  test$stringybark.5 = as.factor(test$stringybark.5)
  test$ribbonbark.5 = as.factor(test$ribbonbark.5)
  
  preds<-predict(gam1, type="response", newdata=test,
                 se.fit=TRUE)
  
  test$prediction = preds$fit
  test$pred.se = preds$se.fit
  
  names(test)[names(test) == response] = "response"
  
  test$error = test$prediction - test$response
  R2 = 1 - (sum((test$response - test$prediction)^2)/sum((test$response - mean(test$response))^2))
  R2adj <- 1- ((1 - R2) * (length(test$response) - 1)/
                 (length(test$response) - length(gam1$coefficients) - 1))
  nrmse = sqrt(mean((test$error) ^ 2))/sd(test$response)
  rmse = sqrt(mean((test$error) ^ 2))
  
  setwd("C:/chapter3/outputs")
  capture.output(
    print("************************** R^2 ****************************"), print(paste0("test R^2 = ", R2)),
    file = paste0(modelnom, "_modeloutputs.txt"),
    append = T)
  
  capture.output(
    print("************************** adjusted R^2 ****************************"), print(paste0("test adjR^2 = ", R2adj)),
    file = paste0(modelnom, "_modeloutputs.txt"),
    append = T)
  
  capture.output(
    print("************************** NRMSE ****************************"), print(paste0("NRMSE = ", nrmse)),
    file = paste0(modelnom, "_modeloutputs.txt"),
    append = T)
  
  capture.output(
    print("************************** RMSE ****************************"), print(paste0("RMSE = ", rmse)),
    file = paste0(modelnom, "_modeloutputs.txt"),
    append = T)
}
gamplotdatafun = function(modelnom){
  setwd("C:/chapter3/outputs")
  gam1 = readRDS(paste0(modelnom, ".rds"))
  rugdat = gam1$model
  
  vars.cont = data.frame(variable = substr(names(summary(gam1)$chi.sq), 3, nchar(names(summary(gam1)$chi.sq)) - 1),
                         type = "continuous",
                         default = NA,
                         alternative = NA)
  vars.cat = names(summary(gam1)$p.coeff)[2:length(names(summary(gam1)$p.coeff))]
  vars.cat = substr(vars.cat, 1, nchar(vars.cat)-1)
  vars.cat = data.frame(variable = vars.cat,
                        type = "categorical",
                        default = "0",
                        alternative = "1")
  vars = rbind(vars.cont, vars.cat)
  
  plot.temp.a = list()
  cond = list()
  if(nrow(vars |> filter(type == "categorical")) > 0){
    for(i in vars$variable[vars$type == "categorical"]){
      pred = list()
      pred[[i]] = as.character(vars[,c("default", "alternative")][vars$variable == i,])
      set.seed(5)
      plot.temp = plot_parametric(gam1, pred = pred)
      plot.temp = plot.temp$fv
      plot.temp$var = i
      plot.temp$model = modelnom
      plot.temp.a[[i]] = plot.temp
      
      cond[[i]] = as.character(vars[,c("default")][vars$variable == i])
    }
    
  } else {
    cond = list()
  }
  
  for(cat in vars$variable[vars$type == "continuous"]){
    set.seed(5)
    plot.temp = plot_smooth(gam1, view = cat, n.grid = 100, cond = cond)
    plot.temp = plot.temp$fv
    plot.temp$var = cat
    plot.temp$model = modelnom
    if(cat == "cover_z_1" | cat == "cover" | cat == "over_cover"){
      lab = names(plot.temp)[grepl("cover", names(plot.temp))]
      names(plot.temp)[grepl("cover", names(plot.temp))] = "cover"
      plot.temp$cover = plot.temp$cover*100
      names(plot.temp)[grepl("cover", names(plot.temp))] = lab
    }
    
    plot.temp.a[[cat]] = plot.temp
  }
  
  dat = bind_rows(plot.temp.a)
  write.csv(dat, paste0(modelnom, "_plotdata.csv"), row.names = F)
}
gamplotfun = function(modelnom, response){
  setwd("C:/chapter3/outputs")
  g1 = read.csv(paste0(modelnom, "_plotdata.csv"))
  names(g1)[names(g1) == response] = "response"
  
  df = g1[ ,sapply(g1, is.integer)]
  if(ncol(df) > 0){
    g2 = g1[,names(g1) %in% names(df)]
    g2$fit = exp(g1$fit)
    g2$ul = exp(g1$fit + g1$CI)
    g2$ll = exp(g1$fit - g1$CI)
    g2$var = g1$var
    g1 = g1[,!names(g1) %in% names(df)]
    g1 = g1 |> filter(!(var %in% names(df)))
    g1$fit = exp(g1$fit)
    g1$ul = exp(g1$ul)
    g1$ll = exp(g1$ll)
  }
  continuous = names(g1)
  categorical = names(g2)
  
  g1mod = readRDS(paste0(modelnom, ".rds"))
  rugdat = g1mod$model
  names(rugdat)[names(rugdat) == response] = "fit"
  rugdat$fit = exp(rugdat$fit)
  if(any(grepl("cover", names(rugdat)))){
    lab = names(rugdat)[grepl("cover", names(rugdat))]
    names(rugdat)[grepl("cover", names(rugdat))] = "cover"
    rugdat$cover = rugdat$cover*100
    names(rugdat)[grepl("cover", names(rugdat))] = lab
  }
  
  for(i in 1:(ncol(g1)-6)){
    g1.temp = g1 |> 
      filter(var == continuous[i])
    rug.temp = rugdat |> 
      dplyr::select(all_of(names(g1)[1:(length(names(g1))-5)])) |> 
      filter(fit <= max(g1.temp$fit))
    gplot1 =
      ggplot(g1.temp, aes(x = g1.temp[,i], y = fit)) +
      geom_line(lwd = 1.5) +
      geom_ribbon(aes(ymin=ll, ymax=ul), alpha = 0.15) +
      geom_abline(slope = 0, intercept = 0, lty = 2) +
      geom_abline(slope = 0, intercept = max(g1.temp$fit), lty = 2) +
      coord_cartesian(ylim = c(0, max(g1.temp$fit)),
                      xlim = c(min(rug.temp[,i]), max(rug.temp[,i]))) +
      ylab(response) +
      theme_bw() +
      geom_rug(data = rug.temp, aes(x = rug.temp[,i], y = fit), sides = "b") +
      xlab(continuous[i]) +
      theme(legend.position = "none")
    ggsave(paste0(modelnom, "_", continuous[i], "_plot.jpg"), gplot1, device = "jpeg", height = 5, width = 5)
  }
  for(i in 1:(ncol(g2)-4)){
    g1.temp = g2 |> 
      filter(var == categorical[i]) |> 
      dplyr::select(categorical[i], fit, ul, ll)
    names(g1.temp)[1] = "var"
    g1.temp$var = as.factor(g1.temp$var)
    gplot1 =
      ggplot(g1.temp, aes(x = var, ymin = ll, lower = ll, middle = fit, upper = ul, ymax = ul)) +
      geom_boxplot(stat = "identity") +
      ylab(response) +
      theme_bw() +
      xlab(categorical[i]) +
      theme(legend.position = "none")
    ggsave(paste0(modelnom, "_", categorical[i], "_plot.jpg"), gplot1, device = "jpeg", height = 5, width = 5)
  }
}
