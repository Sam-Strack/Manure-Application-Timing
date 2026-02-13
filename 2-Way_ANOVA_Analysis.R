#Script for doing statistical analysis on manure timing experimental data. This
#will focus on performing 2-way ANOVA to determine effect of manure application timing
#and manure type on plant/soil N content and crop yield/biomass
#Written By: Sam Strack :)
#Last Updated: 12/29/2025

install.packages("car")
install.packages("EnvStats")

library(car)
library(ggplot2)
library(EnvStats)
#------------------------------------------------------------------------------------#
#HYPOTHESES

#Main effect of manure application timing on yield/biomass/soilN/PlantN:
  #H0: manure application timing has no effect on yield/biomass/soilN/PlantN
  #H1: manure application timing does effect yield/biomass/soilN/PlantN

#Main effect manure type on yield/biomass/soilN/PlantN:
  #H0: manure type has no effect on yield/biomass/soilN/PlantN
  #H1: manure type does effect yield/biomass/soilN/PlantN

#Interaction between manure application timing and type:
  #H0: there is no interaction between manure application timing and type,
       #meaning the relationship between manure application timing and yield/biomass/soilN/PlantN is the same for all manure types
  #H1: there is an interaction between manure application timing and type,
       #meaning that the relationship between manure application timing and yield/biomass/soilN/plantN is different for solid and liquid manure
       #(similarly, the relationship between manure type and yield/biomass/soilN/plantN depends on the application timing)

#------------------------------------------------------------------------------------#
#LOAD IN DATA: Remove data from Urea treatments and make plots/data summaries for later

#Variable Format 2-Way ANOVA-> [Dependent - Independent1 - Independent2]

#Rosemount_Plant_N
#Waseca_Plant_N

Rosemount_Yield <- data.frame(Yield = Growing_Season_2025$Rosemount_R6$Grain_Yield_Bu.acre,
                              Type = Growing_Season_2025$Rosemount_R6$Manure_Source,
                              Timing = Growing_Season_2025$Rosemount_R6$Manure_Timing)
Rosemount_Yield <- Rosemount_Yield[!grepl('PPI Urea', Rosemount_Yield$Timing), ]

Rosemount_Yield_Summary <- Rosemount_Yield %>%
                           group_by(Timing, Type) %>%
                           summarise(Mean_Yield = mean(Yield),
                                     St_Dev_Yield = sd(Yield),
                                     Count = n())

Waseca_Yield <- data.frame(Yield = Growing_Season_2025$Waseca_R6$Grain_Yield_Bu.acre,
                           Type = Growing_Season_2025$Waseca_R6$Manure_Source,
                           Timing = Growing_Season_2025$Waseca_R6$Manure_Timing)
Waseca_Yield <- Waseca_Yield[!grepl('PPI Urea', Waseca_Yield$Timing), ]

Waseca_Yield_Summary <- Waseca_Yield %>%
  group_by(Timing, Type) %>%
  summarise(Mean_Yield = mean(Yield),
            St_Dev_Yield = sd(Yield),
            Count = n())

Rosemount_Soil_N <- data.frame(SoilN = Growing_Season_2025$`Rosemount_In-Season_Soil`$Soil_NO3_0_12,
                               Type = Growing_Season_2025$Rosemount_R6$Manure_Source,
                               Timing = Growing_Season_2025$Rosemount_R6$Manure_Timing)
Rosemount_Soil_N <- Rosemount_Soil_N[!grepl('PPI Urea', Rosemount_Soil_N$Timing), ]

Rosemount_Soil_N_Summary <- Rosemount_Soil_N %>%
  group_by(Timing, Type) %>%
  summarise(Mean_Soil_N = mean(SoilN),
            St_Dev_Soil_N = sd(SoilN),
            Count = n())


Waseca_Soil_N <- data.frame(SoilN = Growing_Season_2025$`Waseca_In-Season_Soil`$Soil_NO3_0_12,
                           Type = Growing_Season_2025$Waseca_R6$Manure_Source,
                            Timing = Growing_Season_2025$Waseca_R6$Manure_Timing)
Waseca_Soil_N <- Waseca_Soil_N[!grepl('PPI Urea', Waseca_Soil_N$Timing), ]

Waseca_Soil_N_Summary <- Waseca_Soil_N %>%
  group_by(Timing, Type) %>%
  summarise(Mean_Soil_N = mean(SoilN),
            St_Dev_Soin_N = sd(SoilN),
            Count = n())

#Naming Variables
sites <- c("Rosemount","Waseca")
analyses <- c("Rosemount_Yield","Waseca_Yield","Rosemount_Soil_N","Waseca_Soil_N")

#make list of raw data and data summaries
data_list <- list(Rosemount_Yield,Waseca_Yield,Rosemount_Soil_N,Waseca_Soil_N)
data_summary_list <- list(Rosemount_Yield_Summary,Waseca_Yield_Summary,Rosemount_Soil_N_Summary,Waseca_Soil_N_Summary)

#Run Models(ANOVA type III SS since design is unbalanced for both sites), put into list
Rosemount_Yield_Mod <- aov(Yield ~ Timing * Type,
                  data = Rosemount_Yield)
Waseca_Yield_Mod <- aov(Yield ~ Timing * Type,
                           data = Waseca_Yield)

Rosemount_Soil_N_Mod <- aov(SoilN ~ Timing * Type,
                            data = Rosemount_Soil_N)
Waseca_Soil_N_Mod <- aov(SoilN ~ Timing * Type,
                            data = Waseca_Soil_N)

mod_list <- list(Rosemount_Yield_Mod,Waseca_Yield_Mod,Rosemount_Soil_N_Mod,Waseca_Soil_N_Mod)

#------------------------------------------------------------------------------------#
#ASSUMPTIONS & TRANSFORMATIONS

#initialize results list
Assumptions_Results <- list()

for (x in 1 : as.numeric(length(data_list))) {

  Boxplot <- ggplot(data_list[[x]]) +
    aes(Type, data_list[[x]][[1]], fill = Timing) +
    ylab(analyses[[x]]) +
    geom_boxplot()
  plot(Boxplot)

#Normality: QQ-Plot -> Histogram of Residuals -> Shapiro-Wilks-Test
#Transformations are applied if S-Wilks test determines non-normal distribution (p > 0.05)
  #QQ
  plot(mod_list[[x]], which = 2)
  QQ_Plot <- recordPlot()

  #H.o.R
  Residual_Histogram <- hist(mod_list[[x]]$residuals)

  #S-W Test
  S_Wilks <- shapiro.test(mod_list[[x]]$residuals)

  #Checks result of S-Wilks test applies transformations is not normally distributed
  if (S_Wilks$p.value > 0.05) {

    Normality <- "Normally Distributed"

    #Print result
    print(paste0(analyses[[x]]," is ",Normality))

  } else if (S_Wilks$p.value <= 0.05) {

    Normality <- "Not Normally Distributed"
    Transformations <- list()

    #Print result
    print(paste0(analyses[[x]]," is ",Normality,", Transformations Applied"))

    #Apply transformations to data, re-run models and S-wilks test to find best transformations to use
      #boxcox
      boxcox = MASS :: boxcox(mod_list[[x]])
      boxcox_lambda = boxcox$x[which(boxcox$y == max(boxcox$y))]

      if (boxcox_lambda >= -0.75 & boxcox_lambda < -0.25) {
        Suggestion = "Boxcox Suggests y^-0.5 Transformation"
        print(Suggestion)

      } else if (boxcox_lambda >= -0.25 & boxcox_lambda < 0.25) {
        Suggestion = "Boxcox Suggests ln(y) Transformation"
        print(Suggestion)

      } else if (boxcox_lambda >= 0.25 & boxcox_lambda < 0.75) {
        Suggestion = "Boxcox Suggests y^0.5 Transformation"
        print(Suggestion)

      } else if (boxcox_lambda >= 0.75 & boxcox_lambda < 1.5) {
        Suggestion = "Boxcox Suggests no transformation"
        print(Suggestion) }

     #apply transformations to data ##CHANGE HERE TO INCLUDE BOXCOX
     Transformations <- data_list[[x]] %>%
          mutate(log_trans = log10(data_list[[x]][[1]]),
                 ln_trans = log(data_list[[x]][[1]]),
                 `y^-0.5_trans` = data_list[[x]][[1]]^-0.5,
                 `y^-0.3_trans` = data_list[[x]][[1]]^-0.3,
                 `y^0.3_trans` = data_list[[x]][[1]]^0.3,
                 `y^0.5_trans` = data_list[[x]][[1]]^0.5,)
                #boxcox_trans = data_list[[x]][[1]]^boxcox_lambda

     #initialize new list for transformation info, S-Wilks test results
     trans_list <- list()
     trans_S_wilks_results <- list()

     #run models on transformed data, run S-Wilks tests and put all the info in a list ##CHANGE HERE TO INCLUDE BOXCOX(4-10)
     for (y in 4:9) {

       transformed_mod <- aov(Transformations[[y]] ~ Timing * Type,
                              data = Transformations)

       #rerun S-Wilks on models
       `transformed_S-wilks` <- shapiro.test(transformed_mod$residuals)

       #transformation naming variable
       trans_name = paste0(analyses[[x]],"_",colnames(Transformations)[y])

       #make list for this transformation and resluts
       trans_info <- list(trans_name,`transformed_S-wilks`,transformed_mod)

       #make new variable for transformed list
       trans_list[[length(trans_list) + 1]] <- assign(paste0(trans_name),trans_info)
       trans_S_wilks_results[[length(trans_S_wilks_results) + 1]] <- assign(paste0(trans_name),`transformed_S-wilks`[[2]])

       }

      #Print results ##CHANGE HERE TO INCLUDE BOXCOX
      SW_Results <- (c(paste0("Transformed ",analyses[[x]]," Shapero-Wilks Test Results(p-value):"),
                       paste0("log: ",trans_S_wilks_results[[1]]),
                       paste0("ln: ",trans_S_wilks_results[[2]]),
                       paste0("y^-0.5: ",trans_S_wilks_results[[3]]),
                       paste0("y^-0.3: ",trans_S_wilks_results[[4]]),
                       paste0("y^0.3: ",trans_S_wilks_results[[5]]),
                       paste0("y^0.5: ",trans_S_wilks_results[[6]])))
                       #paste0("Boxcox: ",trans_S_wilks_results[[7]])

      writeLines(SW_Results)

      #Select the transformation with the largest S-Wilks p-value to use for analysis, rename list elements
      transformation_model_pick <- trans_list[[which.max(unlist(trans_S_wilks_results))]][[3]]
      transformation_model_pick[[12]][[2]] <- trans_list[[which.max(unlist(trans_S_wilks_results))]][[1]]

      transformation_data_pick <- trans_list[[which.max(unlist(trans_S_wilks_results))]][[3]][[13]]
      transformation_data_pick <- transformation_data_pick %>%
        rename("{trans_list[[which.max(unlist(trans_S_wilks_results))]][[1]]}" := `Transformations[[y]]`)

      #reinsert transformed data into data list and rerun model into model list
       mod_list[[x]] <- transformation_model_pick
       data_list[[x]] <- transformation_data_pick
       analyses[[x]] <- trans_list[[which.max(unlist(trans_S_wilks_results))]][[1]]

       #Print Results
       print(paste0(trans_list[[which.max(unlist(trans_S_wilks_results))]][[1]], " Chosen"))

      #QQ
      plot(mod_list[[x]], which = 2)
      QQ_Plot <- recordPlot()

      #H.o.R
      Residual_Histogram <- hist(mod_list[[x]]$residuals)

      #S-W Test
      S_Wilks <- shapiro.test(mod_list[[x]]$residuals)

  }

#Homogeneity of Variances: H.o.V Plot -> Levene Test
  #H.o.V
  plot(mod_list[[x]], which = 3)
  H_o_V_Plot <- recordPlot()

  #LeveneTest
  Levene_Test <- leveneTest(mod_list[[x]])

  if (Levene_Test$`Pr(>F)`[[1]] > 0.05) {

    Homogeneity_of_Variance <- "Levenes Test has Determined Equal Variance"

  } else if (Levene_Test$`Pr(>F)`[[1]] <= 0.05) {

    Homogeneity_of_Variance <- "Levenes Test has Determined Non Equal Variance"

  }

  #Print result
  print(paste0(Homogeneity_of_Variance, " in ", analyses[[x]]))

#Outliers: Boxplot -> Percentiles -> Rosner's test(if dataset is normally distributed)
  #Boxplots
  Boxplot_Timing <- ggplot(data_list[[x]]) +
    aes(data_list[[x]][[3]], data_list[[x]][[1]]) +
    ggtitle(analyses[[x]]) +
    geom_boxplot()

  Boxplot_Type <- ggplot(data_list[[x]]) +
    aes(data_list[[x]][[2]], data_list[[x]][[1]]) +
    ggtitle(analyses[[x]]) +
    geom_boxplot()

  #Percentiles(2.5%,97.5%)
  lower_bound <- quantile(data_list[[x]][[1]], 0.025)
  upper_bound <- quantile(data_list[[x]][[1]], 0.975)

  outlier_index <- which(data_list[[x]][[1]] < lower_bound | data_list[[x]][[1]] > upper_bound)
  Outliers <- data_list[[x]][outlier_index,1]

  #Print results
  print(paste0("Potential Outlier at ",as.character(Outliers)))

  #Rosners Test(On Normal Datasets)
  Rosner_Test <- rosnerTest(data_list[[x]][[1]],
                            k = as.numeric(length(Outliers)))

  #Print Results
  Outlier_Results <- paste0("Rosners Test has determined there are ", as.character(Rosner_Test$n.outliers), " outliers in ",analyses[[x]])
  print(Outlier_Results)

#Combine results into list
Assumptions <- list(Name =analyses[[x]], Normality = Normality, Homogeneity_of_Variance = Homogeneity_of_Variance, Outlier_Results = Outlier_Results,
                   General_Boxplot = Boxplot, Data_Summary = data_summary_list[[x]], QQ_Plot = QQ_Plot, Residual_Histogram = Residual_Histogram, H_o_V_Plot = H_o_V_Plot, Levene_Test = Levene_Test,
                   Boxplot_Timing = Boxplot_Timing ,Boxplot_Type = Boxplot_Type, percentiles = c(lower_bound,upper_bound),
                   potential_outliers = Outliers, Rosner_Test = Rosner_Test,
                   model = mod_list[[x]], data = data_list[[x]])

#make named list for each set of results
assign(paste0(analyses[[x]],"_Assumptions"),Assumptions)

#add analysis lists to list
Assumptions_Results[[length(Assumptions_Results) + 1]] <- assign(paste0("Results"),Assumptions)

}

#------------------------------------------------------------------------------------#

#Print results of assumptions tests to determine if transformations are necessary
print(paste0("Performing 2-Way ANOVA on ", analyses, " ~ Timing x Type"))

for (z in 1: length(mod_list)) {

}

