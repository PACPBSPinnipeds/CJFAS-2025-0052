# this script will generate all analysis results and figures contained in manuscript
# except: Fig 1 & 2 - maps
#         Fig 7- basic pivot from raw hardpart data
#         tables S2 and S3 - generated from the original data files that came off the instrument
#         tables 3 - reproduced from Trzcinski et al. 2024 MEPS


##########################
# DNA diet analysis
#########################
source("1.0_DNA diet_data_compilation.R") # Tables 1 and S1
source("1.1_DNA diet_prevalence.R")  # Fig 3  
source("1.2_DNA diet_bootstrap and raincloud plot.R") # Table 2 & Fig 4 
source("1.3_DNA diet_hurdle models.R") # Table 4a&b, Fig 5 & Fig S1 
source("1.4_DNA diet_FO_hurdle models_sex effect.R") # Fig 6a&b, Fig S4, tables 5 and 6
source("1.5_alluvial plots_ salmon hake herring co-occurrence.R") # Fig9
source("1.6_DNA diet_bootstrap_annual contrasts.R") # Fig S2 $ S3

########################
# Hard part analysis
#######################
source("2.0_hardparts_ analysis.R") # Fig 7, Fig 8, Fig S5, Fig 10, Table 8, Table S4

