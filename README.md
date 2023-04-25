# NetworkMonitoring
Code for the simulation study of the metric-based multivariate monitoring strategies presented in Flossdorf et al. (2023). For all details see the instructions and the description in the paper

#' Short Overview of the files:
LinkChanges.R - contains data generating functions for dynamic networks with link changes (GLC, LLC)
NodeChanges.R - contains data generating functions for dynamic networks with node changes (GNC, LNC)
ControlCharts.R - contains the implementation and setup of the used univariate control charts for the simulation study
NetworkMeasures.R - contains the implementation of the extraction of the considered metrics
helpers.R - some helping functions for the evaluation of the simulation study
simulationStudy.R - the actual simulation study of the paper with the described settings
MultivariateBootstrap.R - the simulation study with multivariate performances using the non-parametric bootstrap control chart
F_version.R - the simulation study with multivariate performances using the parametric Hotellings T^2 control chart
MEMWA.R - the simulation study with multivariate performances using the MEWMA control chart
