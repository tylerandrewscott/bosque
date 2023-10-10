# Organizational Fragmentation and Service Performance of Municipal Water Districts

This subdirectory contains all of the data and code materials necessary to replicate the project. For questions, please raise an issue and tag us here on Github.

## Paper Abstract
Existing research identifies numerous organizational and contextual covariates that influence how public sector service organizations perform. This research suggests that service quality is of special importance in the public sector as it affects citizens’ quality of life directly. Concerns for service quality often remain at the top of policy makers’ justifications as organizational forms and approaches continue to be tailored to fit the specifics of a particular public service in increasingly fragmented local governance systems. In the context of drinking water provision, which is the focus of this study, functional fragmentation (via specialized utility districts) and the resulting competition is often touted as a means to achieving better service results. However, fragmentation can potentially result in excessive density or disproportionate resource partitioning between water utilities, perhaps even resulting in serial underperformance. Evidence from municipal water districts shows that, while the argument for competition-service quality link appears to be valid, the concentration-service quality link and the incidences of performance path-dependencies should be of significant concern to local governments. 


## Getting Started

### Dependencies

* The project requires the R statistical programming language. The project has not been tested on Windows or Linux; it was developed in OSX. Some of the R packages required might require loading underlying dependencies (e.g., if you use the rgdal package, the GDAL and PROJ libraries must be installed).


### Subdirectory items
    /code #scripts for running analysis

These files can be run in sequence to replicate the project. The scripts also produce the tables and figures in the paper.

    /code/01_extract_census_data.R
    
grab tract-level measures from U.S. Census American Community Survey (requires you to set up a free API key with Census API)
    /code/02_aggregate_acs_data.R

create a tract-level file for Texas.

    /code/03_make_district_year_dataframe.R
  
aggregate various inputs, including ACS data + state-level resources to file with district-year observations.

    /code/04_fit_inla_model.R: run analysis and produce result tables.

    /input
Data inputs used for the analysis. Some of these are intermediate products, e.g., df_for_model_V2.RDS is the aggregated district-year data.frame.

    /output
Figures and tables, plus a few odds and ends.
## Authors

Contributors names and contact info

Tima T. Moldogaziev, temirlan@iu.edu
Tyler A. Scott, tascott@ucdavis.edu  
Robert A. Greer, rgreer1@tamu.edu

## License

This project is licensed under the MIT License - see the LICENSE.md file for details

