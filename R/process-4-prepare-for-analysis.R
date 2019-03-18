#' Prepare sets of values for Analysis purposes.
#'
#' This function puts information about Regions and countries and some other
#' data (like technology lists) into one list of lists which then can be used in
#' analysis
#'
#' @export
#'
#' @param benchregions list of benchmark regions (optional)
#' @param indexregions list of index regions (optional)
#' @param countries data frames with Country names and 2-character ISO codes
#'   (optional)
#' @param techlist list of technologies to consider for analysis
#' @param sectorlist list of sectors to consider in analysis
proc.prepareAllLists4analysis <- function(
  benchregions = pacta::data.benchmark.regions, 
  indexregions = pacta::data.index.regions,
  countries = pacta::data.country.iso.codes,
  techlist = pacta::data.technology,
  sectorlist = pacta::data.sector.classification
) {

  BenchmarkRegionList <- data.frame(
    BenchmarkRegion = benchregions$BenchRegions, 
    BenchmarkRegionColname = benchregions$BenchRegions_ISO_colnames
  )

  CompanyDomicileRegionList <- data.frame(
    CompanyDomicileRegion = indexregions$IndexUniverse,
    CompanyDomicileRegionColname = indexregions$IndexUniverseColname
  )
  
  # Read countryname-conversion file to abbreviation
  CountryISOList <- countries %>% subset(Country != "#N/A")

  
  PowerBenchmarkRegionOECD <- c("OECDEurope", "US", "OECDAsiaOceaniaWoJP", "OECDAmericasWoUS","Japan")
  PowerBenchmarkRegionNonOECD <- c("AfricaWoZA", "SouthAfrica", "Russia", "EEurope_EurasiaWoRU", "Brazil", "LatinAmericaWoBR", "China", "India", "NonOECDAsiaRest", "MiddleEast") 
  PowerBenchmarkRegionGlobal <- c(PowerBenchmarkRegionNonOECD, PowerBenchmarkRegionOECD)
  FossilFuelBenchmarkRegions <- c("OECDAmericas" , "LatinAmerica", "Africa", "EEurope_Eurasia", "NonOECDAsia", "MiddleEast", "OECDAsiaOceania", "OECDEurope")
  FossilFuelBenchmarkRegionsOECD <- c("OECDAmericas" , "OECDAsiaOceania", "OECDEurope")
  FossilFuelBenchmarkRegionsNonOECD <- c("LatinAmerica", "Africa", "EEurope_Eurasia", "NonOECDAsia","MiddleEast")
  MutualExclusiveCompanyDomicileRegions <- c("MSCIEmergingMarkets", "MSCIWorld", "OutsideACWI")
  
  AllLists <- list(
    TechList = techlist,
    BenchmarkRegionList = BenchmarkRegionList,
    CompanyDomicileRegionList = CompanyDomicileRegionList,
    PowerBenchmarkRegionOECD = PowerBenchmarkRegionOECD, 
    PowerBenchmarkRegionNonOECD = PowerBenchmarkRegionNonOECD, 
    PowerBenchmarkRegionGlobal = PowerBenchmarkRegionGlobal,
    FossilFuelBenchmarkRegions = FossilFuelBenchmarkRegions, 
    FossilFuelBenchmarkRegionsOECD = FossilFuelBenchmarkRegionsOECD, 
    FossilFuelBenchmarkRegionsNonOECD = FossilFuelBenchmarkRegionsNonOECD,
    MutualExclusiveCompanyDomicileRegions = MutualExclusiveCompanyDomicileRegions
  )
  
}

#' Prepare International Energy Agency data for analysis
#'
#' @export
#' 
#' @param scenarios data frame with IEA Targets
#' @param startyear reference year for analysis
#' @param years number of consequent years (after startyear) to analyze
proc.prepareIEATargets4analysis <- function(
  scenarios, 
  startyear, 
  years
) {
  ScenarioData <- as_tibble(scenarios) 

  ScenarioData <- ScenarioData %>%
    mutate(
      Sector = ifelse(Units == "PJ", "Demand", Sector),
      # UNIT CONVERSION: Based on kylesconverter.com and callmepower.ca
      # http://www.kylesconverter.com/energy,-work,-and-heat/barrels-of-oil-equivalent-to-gigajoules and https://callmepower.ca/en/faq/gigajoule-cubic-metre-gas
      #tce to GJ: * 29.30760
      #1 Cubic metre (m3) ->	Gigajoules (GJ): * 0.0382599380189004
      #b/a -> GJ/a: * 6.12
      AnnualvalIEAtech = case_when(
        Units == "tce" ~ AnnualvalIEAtech * 29.30760,
        Units == "cm" ~ AnnualvalIEAtech * 0.0382599380189004,
        Units == "b/a" ~ AnnualvalIEAtech * 6.12,
        TRUE ~ AnnualvalIEAtech
      ), 
      refvalIEAtech = case_when(
        Units == "tce" ~ refvalIEAtech * 29.30760,
        Units == "cm" ~ refvalIEAtech * 0.0382599380189004,
        Units == "b/a" ~ refvalIEAtech * 6.12,
        TRUE ~ refvalIEAtech
      ), 
      refvalIEAsec = case_when(
        Units == "tce" ~ refvalIEAsec * 29.30760,
        Units == "cm" ~ refvalIEAsec * 0.0382599380189004,
        Units == "b/a" ~ refvalIEAsec * 6.12,
        TRUE ~ refvalIEAsec
      )
    ) %>% 
    # filter by Year
    dplyr::filter(
      Year >= startyear,
      Year <= startyear + years |
        Year %in% c(2040, 2050)
    ) %>%
    dplyr::select(
      Sector, Technology, Year, BenchmarkRegion, FairSharePerc, Scenario, Direction, AnnualvalIEAtech, refvalIEAtech, refvalIEAsec
    )
  
  ScenarioData
}


#' Prepare SectorsMaster dataset from specific sector datasets
#'
#' function takes datasets for each sector (2 per sector: on Asset and Company
#' levels) thansforms them into universal structure and combines information into one Sectors Master dataset
#'
#' @export
#'
#' @param companymaster.ogc,assetmaster.ogc Oil &  Gas data on company level and
#'   asset level respectively
#' @param companymaster.power,assetmaster.power Electricity generation data on
#'   company level and asset level respectively
#' @param companymaster.auto,assetmaster.auto Automotive data on company level
#'   and asset level
#' @param companymaster.shp,assetmaster.shp Shipping data on company level
#'   and asset level
#' @param companymaster.cmt,assetmaster.cmt Cement data on company level
#'   and asset level
#' @param companymaster.stl,assetmaster.stl Steel data on company level
#'   and asset level
#' @param companymaster.avn,assetmaster.avn Aviation data on company level
#'   and asset level
#' @param startyear reference year for analysis
#' @param years number of consequent years (after startyear) to analyze
#' @param techlist list of technologies to include into result

proc.prepareSectorMaster <- function(
  companymaster.ogc, assetmaster.ogc,
  companymaster.power, assetmaster.power,
  companymaster.auto, assetmaster.auto,
  companymaster.shp, assetmaster.shp,
  companymaster.cmt, assetmaster.cmt,
  companymaster.stl, assetmaster.stl,
  companymaster.avn, assetmaster.avn,
  
  startyear, years, techlist
) {

  # unify input data 
  
  # Process Fossil Fuels data: Company & Asset level views
  cym.ogc <- 
    companymaster.ogc %>% as_tibble() %>%
    dplyr::filter(Year >= startyear & Year <= startyear + years) %>%
    dplyr::mutate(ALD.Level = "Company", ID = as.character(UID), Name = UltimateOwner, Production = as.numeric(CreditProduction)) %>%
    dplyr::select(
      ALD.Level, ID, Name, Country, Year, Technology, Production
    )
  atm.ogc <-
    assetmaster.ogc %>% as_tibble() %>%
    dplyr::filter(Year >= startyear & Year <= startyear + years) %>%
    dplyr::mutate(ALD.Level = "Asset", ID = as.character(Asset.ID), Name = Field.Name, Production = as.numeric(TotalFieldProduction)) %>%
    dplyr::select(
      ALD.Level, ID, Name, Country, Year, Technology, Production
    )
  
  ALDMaster_OGC <- 
    dplyr::union_all(cym.ogc, atm.ogc) %>%
    dplyr::group_by(ALD.Level, ID, Name, Country, Year, Technology) %>%
    dplyr::summarise(Production = sum(Production, na.rm = T)) %>%
    dplyr::mutate(Sector = "Fossil Fuels") %>%
    dplyr::select(ALD.Level, ID, Name, Country, Year, Technology, Production, Sector)
  
  
  #Process Power data: Company & Asset level views
  cym.power <- 
    companymaster.power %>% as_tibble() %>%
    dplyr::filter(Year >= startyear & Year <= startyear + years) %>%
    dplyr::mutate(
      ALD.Level = "Company", ID = UID, Name = UltimateOwner, 
      Country = PlantLocation, Production = CreditProduction
    ) %>%
    dplyr::select(
      ALD.Level, ID, Name, Country, Year, Technology, Production
    )
  
  atm.power <- 
    assetmaster.power %>% as_tibble() %>%
    dplyr::filter(Year >= startyear & Year <= startyear + years) %>%
    dplyr::mutate(
      ALD.Level = "Asset", ID = "", Name = Power.Plant.Name, 
      Country = PlantLocation, Production = DirectProduction
    ) %>%
    dplyr::select(
      ALD.Level, ID, Name, Country, Year, Technology, Production
    )
  
  ALDMaster_Power <-
    dplyr::union_all(cym.power, atm.power) %>%
    dplyr::mutate(
      Technology = dplyr::if_else(
        Technology %in% c("BioPower", "Solar", "Wind", "Ocean", "Geothermal"),
        "Renewables",
        Technology
      )
    ) %>%
    dplyr::group_by(ALD.Level, ID, Name, Country, Year, Technology) %>%
    dplyr::summarise(Production = sum(Production, na.rm = T)) %>%
    dplyr::mutate(Sector = "Power", Technology = paste0(Technology, "Cap")) %>%
    dplyr::select(ALD.Level, ID, Name, Country, Year, Technology, Production, Sector)
  
  
  # Process Automotive data: Company level view
  cym.auto <- 
    companymaster.auto %>% as_tibble() %>%
    dplyr::filter(Year >= startyear & Year <= startyear + years) %>%
    dplyr::mutate(ALD.Level = "Company", ID = UID, Name = WardsVBrandOwner, Country = PlantLocation) %>%
    dplyr::mutate(
      Name = dplyr::if_else(is.na(Name) | Name == "", ID, Name)
    ) %>%
    dplyr::select(
      ALD.Level, ID, Name, Country, Year, Technology, Production
    )
  atm.auto <- if (missing(assetmaster.auto)) {
    empty.SectorMaster()
  } else {
    stop(paste("Conversion for parameter assetmaster.auto not yet implemented"))
  }
  
  ALDMaster_Auto <-
    dplyr::union_all(cym.auto, atm.auto) %>%
    dplyr::filter(Technology != "FuelCell") %>%
    dplyr::mutate(Sector = "Automotive") %>%
    dplyr::select(ALD.Level, ID, Name, Country, Year, Technology, Production, Sector)
  
  # Process Shipping data
  cym.shp <- if (missing(companymaster.shp)) {
    empty.SectorMaster()
  } else {
    stop(paste("Conversion for parameter companymaster.shp not yet implemented"))
  }

  atm.shp <- if (missing(assetmaster.shp)) {
    empty.SectorMaster()
  } else {
    stop(paste("Conversion for parameter assetmaster.shp not yet implemented"))
  }
  ALDMaster_Shipping <- 
    dplyr::union_all(cym.shp, atm.shp)
  
  # Process Cement data
  cym.cmt <- if (missing(companymaster.cmt)) {
    empty.SectorMaster()
  } else {
    stop(paste("Conversion for parameter companymaster.cmt not yet implemented"))
  }
  
  atm.cmt <- if (missing(assetmaster.cmt)) {
    empty.SectorMaster()
  } else {
    stop(paste("Conversion for parameter assetmaster.cmt not yet implemented"))
  }
  ALDMaster_Cement <- 
    dplyr::union_all(cym.cmt, atm.cmt)

  # Process Steel data
  cym.stl <- if (missing(companymaster.stl)) {
    empty.SectorMaster()
  } else {
    stop(paste("Conversion for parameter companymaster.stl not yet implemented"))
  }
  
  atm.stl <- if (missing(assetmaster.stl)) {
    empty.SectorMaster()
  } else {
    stop(paste("Conversion for parameter assetmaster.stl not yet implemented"))
  }
  ALDMaster_Steel <- 
    dplyr::union_all(cym.stl, atm.stl)
  
  # Process Aviation data
  cym.avn <- if (missing(companymaster.avn)) {
    empty.SectorMaster()
  } else {
    stop(paste("Conversion for parameter companymaster.avn not yet implemented"))
  }
  
  atm.avn <- if (missing(assetmaster.avn)) {
    empty.SectorMaster()
  } else {
    stop(paste("Conversion for parameter assetmaster.avn not yet implemented"))
  }
  ALDMaster_Aviation <- 
    dplyr::union_all(cym.avn, atm.avn)
  
  # Aggregated Sector Masters
  SectorsMaster <- 
    list(
      ALDMaster_OGC, ALDMaster_Power, ALDMaster_Auto, ALDMaster_Shipping, 
      ALDMaster_Cement, ALDMaster_Steel, ALDMaster_Aviation
      ) %>% 
    Reduce(f = dplyr::union_all, x = .)
  
  # decide whether result should be filtered by technologies
  result <- if (missing(techlist)) {
    SectorsMaster
  } else {
    SectorsMaster %>%
      # filter by only technologies, specified in analysis parameters
      dplyr::filter(Technology %in% techlist)
  }
  
  result
}


#' Prepare Asset Level Data for analysis
#'
#' function takes datasets for each sector (2 per sector: on Asset and Company
#' levels) and combines information into one dataset suitable for analysis
#' purposes
#'
#' @export
#'
#' @param sectormaster All sectors data on company level and asset level
#'   respectively (prepared by \link{proc.prepareSectorMaster})
#' @param all.lists sets of various values for analysis purposes
#' @param startyear reference year for analysis
#' @param years number of consequent years (after startyear) to analyze
#' @param selregions regions selected for analysis
proc.prepareSectorMaster4analysis <- function(
  sectormaster,
  all.lists,
  startyear, years,
  selregions
) {
  sectormaster <- as_tibble(sectormaster) %>% 
    # filter by Year
    dplyr::filter(
      Year >= startyear,
      Year <= startyear + years
    ) %>%
    mutate(
      Country = if_else(Sector %in% c("Aviation","Shipping"),"Global",Country),
      # TO BE DELETED? DEPENDS ON PAULS RESPONSE!!!
      Sector = if_else(Sector %in% c("Oil&Gas","Coal"),"Fossil Fuels", Sector)
    )
  
  countries <- pacta::data.country.iso.codes
  benchregions <- pacta::data.benchmark.regions

  # create dataframe with all Countries within Benchmark region
  countryRegion <- 
    mapply(
      FUN = function(r, iso) {
        data.frame(
          Country.ISO = benchregions[[iso]], BenchmarkRegion = r, 
          stringsAsFactors = F
        )
      },
      benchregions$BenchRegions,
      benchregions$BenchRegions_ISO_colnames, 
      SIMPLIFY = F
    ) %>% Reduce(f = rbind) %>% unique() %>% as_tibble() %>%
    dplyr::inner_join(
      countries,
      by = "Country.ISO"
    ) %>% unique() %>% as_tibble() %>%
    dplyr::select(
      Country = Country.ISO, BenchmarkRegion
    ) %>%
    # pre-optimisation
    dplyr::filter(
      BenchmarkRegion == "Global" | BenchmarkRegion %in% selregions
    ) %>%
    unique()
  
  # Add Region data to ALD masters 
  # create Global, OECD and Non-OECD company level datasets
  
  SectorMasters_SelRegions <- 
    sectormaster %>%
    dplyr::inner_join(
      countryRegion,
      by = "Country"
    )  %>%
    # regions selected for analysis (Automotive data available only on global level)
    dplyr::filter(
      Production != 0,
      (Sector %in% c("Power","Fossil Fuels","Oil&Gas","Coal") & BenchmarkRegion %in% selregions) |
      (Sector %in% c("Automotive","Cement","Aviation","Steel","Shipping","Demand") & BenchmarkRegion == "Global")
    ) %>%
    dplyr::group_by(
      ALD.Level, ID, Name, Year, Technology, BenchmarkRegion, Sector
    ) %>%
    dplyr::summarise(
      CO2Intensity = weighted.mean(CO2Intensity,Production,na.rm = T),
      Production = sum(Production, na.rm = T)
     ) %>% 
    dplyr::ungroup()
  
  # recognize all selected data for "Global level"
  SectorMasters_Global <-
    SectorMasters_SelRegions %>%
    filter(
      # BenchmarkRegion %in% c("Global", "NonOECD", "OECD")
      BenchmarkRegion %in% c("Global")
    ) %>%
    # mutate(
    #   BenchmarkRegion = "Global"
    # ) %>%
    dplyr::group_by(
      ALD.Level, ID, Name, Year, Technology, BenchmarkRegion, Sector
    ) %>%
    dplyr::summarise(
      CO2Intensity = weighted.mean(CO2Intensity,Production,na.rm = T),
      Production = sum(Production, na.rm = T)
    ) %>%
    dplyr::ungroup()

  # combine data on regions level and global level
  SectorMasters_Regions <-
    SectorMasters_SelRegions %>%
    dplyr::filter(BenchmarkRegion != "Global") %>%
    dplyr::union_all(SectorMasters_Global) %>%
    # add reference GlobalProduction values from Global level
    dplyr::group_by(
      ALD.Level, ID, Name, Year, Technology, Sector
    ) %>%
    mutate(
      GlobalProduction = sum(dplyr::if_else(BenchmarkRegion == "Global", Production, 0), na.rm = T)
    ) %>%
    dplyr::ungroup()
  
  # Add calculation of units across sector
  SectorMasters_Unit <- 
    SectorMasters_Regions %>%
    # normalize Production values
    mutate(
      ProductionSectorUnit = case_when(
        #tce/a to GJ/d
        Technology == "Coal" ~ Production * 29.3076 / 365.25,
        TRUE ~ Production
      ),
      Production = case_when(
        #GJ/d to bd  (thousand barrels per day to barrels per year)
        Technology == "Oil"  ~ ProductionSectorUnit / 6.120,
        #GJ/d to MMCFD
        Technology == "Gas"  ~ ProductionSectorUnit / 1055.05585,
        TRUE ~ Production
      )
    ) %>%
    # calculate production summary for different technology over sector
    dplyr::group_by(ALD.Level, ID, Name, Year, BenchmarkRegion, Sector) %>%
    dplyr::mutate(SectorProduction = sum(ProductionSectorUnit, na.rm = T)) %>%
    dplyr::ungroup()
    
  # Add calculated values for analysis
  SectorMasters_Result <- 
    SectorMasters_Unit %>%
    # add reference values from start year
    dplyr::group_by(
      ALD.Level, ID, Name, Sector, Technology, BenchmarkRegion
    ) %>%
    mutate(
      ProductionRef = 
        sum(dplyr::if_else(Year == startyear, Production, 0)),
      ProductionSectorUnitRef = 
        sum(dplyr::if_else(Year == startyear, ProductionSectorUnit, 0)),
      SectorProductionRef = 
        sum(dplyr::if_else(Year == startyear, SectorProduction, 0)),
      GlobalProductionRef = 
        sum(dplyr::if_else(Year == startyear, GlobalProduction, 0))
    ) %>%
    # add "Global Sector Production" reference values from start year from Global Level
    dplyr::group_by(
      ALD.Level, ID, Name, Sector
    ) %>%
    mutate(
      GlobalSectorProductionRef = 
        sum(dplyr::if_else(BenchmarkRegion == "Global" & Year == startyear, ProductionSectorUnit, 0))
    ) %>%
    dplyr::ungroup() %>%
    # Techshare calculation
    dplyr::mutate(
      TechnologyShare = ProductionSectorUnit / SectorProduction,
      TechnologyShareBasedStartYear = ProductionSectorUnit / SectorProductionRef,
      TechnologyRegionalWeight = Production / GlobalProduction,
      CompanyRegionalWeight = SectorProduction / GlobalSectorProductionRef,
      TechnologyRegionalWeightStartYear = ProductionRef / GlobalProductionRef
    ) %>%
    # Add capex plans data point in % addition of total sector capacity in the start year of the analysis as well as % additions of total technology capacity (high carbon technologies)
    dplyr::mutate(
      TechAdditionsAbs = dplyr::if_else(Production < ProductionRef, 0, Production - ProductionRef)
    ) %>%
    # buildouts for all technologies within sector
    dplyr::group_by(ALD.Level, ID, Name, Sector, Year, BenchmarkRegion) %>%
    dplyr::mutate(SectorBuildOutAbs = sum(TechAdditionsAbs, na.rm = T)) %>%
    dplyr::ungroup() %>%
    mutate(
      SectorAdditionsPerc = SectorBuildOutAbs / SectorProductionRef,
      BuildOutTechShare = TechAdditionsAbs / SectorBuildOutAbs
    )

  SectorMasters_Result
}

#' Take matched loanbook and prepare dataset for analysis purposes
#'
#' @description  Function adds columns necessary for analysis based on input
#'   attribute values
#'
#' @export
#'
#' @param loanbook matched loanbook data - contains pairs of attribues
#'   with ALD Level, ALD ID, ALD Name
#' @param exposure.type defines what value to put as LoanSize (Outstanding,
#'   Exposure)
#' @param customer.level Name of level on which mathcing is done
#' @param ald.levels,ald.ids,ald.names vectors of values that represent
#'   reference to asset-level data. Vectors must be of same length as number of
#'   rows in loanbook.
proc.prepareLoanbook4analysis <- function(
  loanbook, 
  exposure.type,
  customer.level,
  ald.levels,
  ald.ids,
  ald.names
) {
  loanbook %>%
    dplyr::mutate(
      LoanSize = case_when(
        exposure.type == "Outstanding" ~ Outstanding,
        exposure.type == "Credit Limit" ~ Exposure,
        TRUE ~ as.numeric(NA)
      ),
      LoanSize = case_when(
        Sector.Classification %in% pacta::data.sector.classification ~ LoanSize, 
        TRUE ~ 0
      ),
      Exposure.Type = exposure.type,
      Match.Level = customer.level,
      ALD.link.Level = if_else(
        ald.levels %in% c("Direct Owner", "Ultimate Parent"),
        "Company",
        ald.levels
      ),
      ALD.link.ID = ald.ids,
      ALD.link.Name = ald.names
    ) 
}

#------------------------------------------------------------------------------#

#' Empty Sector master data frame
#' 
#' @description Function defines data frame structure for Sector Master data used in 
#' analysis
#' 
#' @export
empty.SectorMaster <- function() {
  data.frame(
    ALD.Level             = character(),
    ID                    = character(),
    Name                  = character(),
    Country               = character(),
    Year                  = integer(),
    Technology            = character(),
    Production            = double(),
    Sector                = character(),
    stringsAsFactors = F
  ) %>% as_tibble()
  
}