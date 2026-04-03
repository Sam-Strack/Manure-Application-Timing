Treatment_index <- function() {

  Trt_Index <- list(Groups = list(c("Liquid","Liquid","Liquid","Solid","Solid","Solid","Control","Urea","Urea","Urea","Urea"),
                                  c("Liquid","Liquid","Liquid","Liquid","Solid","Solid","Solid","Control","Urea","Urea","Urea","Urea")),
                    Arrange = list(c(8,11,12,9,10,3,1,4,7,5,6),
                                   c(8,11,12,9,10,3,1,2,4,7,5,6)),
                    Trt_Num = list(c(1,3,4,5,6,7,8,9,10,11,12),
                                   c(1,2,3,4,5,6,7,8,9,10,11,12)),
                    Names =  list(c("Liquid Dairy Early Fall","Liquid Dairy Spring","Liquid Dairy Sidedress","Solid Beef Early Fall","Solid Beef Late Fall",
                                    "Solid Beef Spring","Control","Urea 98lbsN","Urea 146lbsN","Urea 195lbsN","Urea 293lbsN"),
                                  c("Liquid Swine Early Fall","Liquid Swine Late Fall","Liquid Swine Spring","Liquid Swine Sidedress","Solid Turkey Early Fall","Solid Turkey Late Fall",
                                    "Solid Turkey Spring","Control","Urea 75lbsN","Urea 112lbsN","Urea 150lbsN","Urea 225lbsN"),
                                  "Control"),
                    Yield_Types = list("Grain_Yield_Bu.acre",
                                       "Silage_Yield_65Percent_Moisture_Ton.acre",
                                       "Grain_Yield_Bu.acre"),
                    Y_labels = list("Grain Yield(Bu/Acre)",
                                    "Silage Yield(65%Moisture)(Ton/Acre)",
                                    "Grain Yield(Bu/Acre)"),
                    Titles = list("Grain Yield",
                                  "Silage Yield",
                                  "Grain Yield"),
                    Sample_Timings = list("In Season(V4)",
                                          "Post Harvest",
                                          "Pre-Plant"),
                    Sample_Depths = list('0-12in',
                                         '12-24in',
                                         '24-36in'),
                    Sites = list("Rosemount",
                                 "Waseca")
                    )

  return(Trt_Index)

}

