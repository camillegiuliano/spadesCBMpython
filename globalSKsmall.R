projectPath <- "~/spadesCBMpython"
dir.create(projectPath, recursive = TRUE, showWarnings = FALSE)
setwd(projectPath)

repos <- unique(c("predictiveecology.r-universe.dev", getOption("repos")))
install.packages("SpaDES.project",
                 repos = repos)

# start in 1998, and end in 2000
times <- list(start = 1998, end = 2000)

out <- SpaDES.project::setupProject(
  Restart = TRUE,
  useGit = "PredictiveEcology", # a developer sets and keeps this = TRUE
  overwrite = TRUE, # a user who wants to get latest modules sets this to TRUE
  paths = list(projectPath = projectPath),

  options = options(
    repos = c(repos = repos),
    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
    reproducible.destinationPath = "inputs",
    ## These are for speed
    reproducible.useMemoise = TRUE,
    # Require.offlineMode = TRUE,
    spades.moduleCodeChecks = FALSE
  ),
  modules =  c("PredictiveEcology/CBM_defaults@training",
               "PredictiveEcology/CBM_dataPrep_SK@training",
               "PredictiveEcology/CBM_vol2biomass@training",
               "PredictiveEcology/CBM_core@training"),
  times = times,
  require = c("SpaDES.core", "reticulate",
              "PredictiveEcology/libcbmr", "data.table"),

  parameters = list(
    CBM_defaults = list(
      .useCache = TRUE
    ),
    CBM_dataPrep_SK = list(
      .useCache = TRUE
    ),
    CBM_vol2biomass = list(
      .useCache = TRUE
    )
  ),
  functions = "PredictiveEcology/CBM_core@training/R/ReticulateFindPython.R",

  ret = {
    reticulate::virtualenv_create(
      "r-spadesCBM",
      python = if (!reticulate::virtualenv_exists("r-spadesCBM")){
        ReticulateFindPython(
          version        = ">=3.9,<=3.12.7",
          versionInstall = "3.10:latest",
          pyenvRoot      = tools::R_user_dir("r-spadesCBM")
        )
      },
      packages = c(
        "numpy<2",
        "pandas>=1.1.5",
        "scipy",
        "numexpr>=2.8.7",
        "numba",
        "pyyaml",
        "mock",
        "openpyxl",
        "libcbm"
      )
    )
    reticulate::use_virtualenv("r-spadesCBM")
  },

  #### begin manually passed inputs #########################################
  ## define the  study area.
  masterRaster = {
    extent = terra::ext(c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183))
    masterRaster <- terra::rast(extent, res = 30)
    terra::crs(masterRaster) <- "PROJCRS[\"Lambert_Conformal_Conic_2SP\",\n    BASEGEOGCRS[\"GCS_GRS_1980_IUGG_1980\",\n        DATUM[\"D_unknown\",\n            ELLIPSOID[\"GRS80\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Lambert Conic Conformal (2SP)\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-95,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",77,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"
    masterRaster[] <- rep(1, terra::ncell(masterRaster))
    mr <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1zUyFH8k6Ef4c_GiWMInKbwAl6m6gvLJW/view?usp=drive_link",
                                   destinationPath = "inputs",
                                   to = masterRaster,
                                   method = "near")
    mr[mr[] == 0] <- NA
    mr
  },
  ## Give the location of the disturbance rasters.
  disturbanceRasters = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt/view?usp=drive_link",

  # Restart = getOption("SpaDES.project.Restart", FALSE),

  outputs = as.data.frame(expand.grid(objectName = c("cbmPools", "NPP"),
                                      saveTime = sort(c(times$start,
                                                        times$start +
                                                          c(1:(times$end - times$start))
                                      )))),

)

out$loadOrder <- unlist(out$modules)

# Run
simPython <- do.call(SpaDES.core::simInitAndSpades, out)
