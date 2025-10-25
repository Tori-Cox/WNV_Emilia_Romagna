###########################################################
## sensitivity: running models without the spatial field ##
###########################################################

## read in data ----------------------------------------------------------------

# shapefile
shapefile <- readRDS("data/gadm36_ITA_1_sp.rds")

# estimated WNV prevalence and collated meteorological/ avian/ land use data
# the data is collated ready for INLA models, see data description in paper for methods
data <- readRDS("data/hurdle_data.RDS")

# SPDE model components
mesh_n1 <- readRDS("data/mesh.RDS")
spde <- readRDS("data/spde.RDS")
A <- readRDS("data/A_spde.RDS")


## set up and run baseline models ----------------------------------------------

source("scripts/setup_models.R")
plot_mesh(new_shapefile, mesh_n1)

# run baseline model
source("scripts/sensitivity_without_spatial/run_baseline_model_xsp.R")
source("R/plot_models.R")
summary(baseline_model)


####################################################################################
########################## Univariable analysis ####################################
####################################################################################

# run the univariable model analysis - either for a short run of 2 variables or all the variables
source("scripts/sensitivity_without_spatial/run_univariable_models_xsp.R")
source("R/model_building.R")

best_univariable <- best_univar(univar_nl = readRDS("scripts/sensitivity_without_spatial/output/univar/univar_nonlinear.RDS"),
                                univar_l = readRDS("scripts/sensitivity_without_spatial/output/univar/univar_linear.RDS"))
print(paste0("best univariable model = ", best_univariable[1] |> str_remove("_y") |> str_remove("_z")))


# plot univariable outputs
source("scripts/plot_univariable_model_output.R")
plots_linear
plots_nonlinear


####################################################################################
########################## Multivariable analysis ##################################
####################################################################################

## run the final model ---------------------------------------------------------

# this is pre-set 
source("scripts/sensitivity_without_spatial/run_final_model_xsp.R")
summary(final_model)
print(final_model_plots)

saveRDS(final_model, "scripts/sensitivity_without_spatial/output/final_model.RDS")


## assessment of model fit -----------------------------------------------------
source("R/model_fit_functions.R")

# receiver operating characteristic curve to assess binary classifier of hurdle model
ROC_curve(res=final_model, stk.yz, z)

# estimated prevalence versus calculated prevalence in data
model_fit(res=final_model, stk.yz, z, data=new)

# mean absolute error averaged over space, shown for each year and week number of the transmission season
model_mae_annual(res=final_model, stk.yz, data=new)
