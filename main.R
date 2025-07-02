## set up ----------------------------------------------------------------------

source("scripts/package_list.R")

# create directories
dir.create("output")
dir.create("output/univar")
dir.create("output/multivar")
dir.create("output/base")

# run for all variables or run a shorter example?
# if you run for all variables please note that the code takes a long time to run
run <- "short"


## read in data ----------------------------------------------------------------

# entomology trap locations
coords<-read.csv("data/trap_coordinates.csv")
xy <-coords[,c(4:3)] 

# shapefile
shapefile<-readRDS("data/gadm36_ITA_1_sp.rds")

# estimated WNV prevalence and collated meteorological/ avian/ land use data
# the data is collated ready for INLA models, see data description in paper for methods
data<-readRDS("data/data_for_models.RDS")


## set up and run baseline models ----------------------------------------------

source("scripts/setup_models.R")
plot_mesh(new_shapefile, mesh_n1, xy)

# run baseline model
source("scripts/run_baseline_model.R")
summary(baseline_model)
visualise_field_means(res=baseline_model, stk.yz=stk.yz, mesh = mesh_n1, summary="median")
plot_field_parameters(res=baseline_model)  
    

## run univariable models ------------------------------------------------------

# run an example showing the quantile method to determine which variables were linear versus non-linear
# can skip this step
source("scripts/run_quantile_models.R")

# run the univariable model analysis - either for a short run of 2 variables or all the variables
source("scripts/run_univariable_models.R")
source("R/model_building.R")
best_univariable <- best_univar(univar_nl = readRDS("output/univar/univar_nonlinear.RDS"),
                                univar_l = readRDS("output/univar/univar_linear.RDS"))
print(paste0("best univariable model = ", best_univariable[1]))


# plot univariable outputs
source("scripts/plot_univariable_model_output.R")
ggsave(plot=plots_linear, "output/plot_univar_linear.png")
ggsave(plot=plots_nonlinear, "output/plot_univar_nonlinear.png")


## multivariable models --------------------------------------------------------

source("scripts/run_multivariable_models.R")


## run the final model ---------------------------------------------------------

# this is pre-set to be the variables chosen in the analysis in the paper
source("scripts/run_final_model.R")
summary(final_model)
visualise_field_means(res=final_model, stk.yz=stk.yz, mesh = mesh_n1, summary="median")
plot_field_parameters(res=final_model)  
saveRDS(final_model, "output/final_model.RDS")


## assessment of model fit -----------------------------------------------------
source("R/model_fit_functions.R")

# receiver operating characteristic curve to assess binary classifier of hurdle model
ROC_curve(res=final_model, stk.yz, z)

# estimated prevalence versus calculated prevalence in data
model_fit(res=final_model, stk.yz, z, data)

# mean absolute error averaged over time, shown for each trap
model_mae_map(res=final_model, stk.yz, z, data, shapefile= new_shapefile)

# mean absolute error averaged over space, shown for each year and week number of the transmission season
model_mae_annual(res=final_model, stk.yz, data)
