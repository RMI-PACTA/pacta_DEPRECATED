#' Make cartesian product of two tables
#'
#' @description function returns combination of all records in \code{l} with all
#'   records from \code{r}. Columns from \code{r} follow columns from \code{l}
#'   in the result.
#'
#' @param l,r - left and right hand side tables
cross_join <- function(l, r) {
  pdt <- 
    dplyr::inner_join(
      l %>% dplyr::mutate(CROSS_JOIN_COL = 1),
      r %>% dplyr::mutate(CROSS_JOIN_COL = 1),
      by = "CROSS_JOIN_COL"
    ) %>%
    dplyr::select(-CROSS_JOIN_COL)
  pdt
}

#' Calculate weight of value in vector: x / sum(x)
#'
#' @description function returns vector where each element represents weight of
#'   original value. If sum of original values equal to 0 then result will
#'   contain NA values. Please be aware that function may return unexpected
#'   result when values in vector are of different sign and sum of them almost
#'   equal to zero (e.g. \code{ratio_to_report(c(1, 2, 3, -6.00000001))} will
#'   return very big numbers).
#'
#' @export
#'
#' @param x vector of numbers
#' @param na.rm remove NA values
#'
#' @examples 
#' pacta::ratio_to_report(c(1, 2, 3, NA, 5), na.rm = TRUE)
ratio_to_report <- function(x, na.rm = FALSE) {
  total <- sum(x, na.rm = na.rm)
  total <- if (is.na(total) || total == 0) NA else total
  
  x / total
}

#-------------------------------------------------------------------------------

#' Analyze corporate economy
#'
#' @export
#'
#' @param ald.master Direct Owner Master dataset prepared by
#'   \link{proc.prepareSectorMaster4analysis} function
#' @param iea.targets IEA Targets dataset prepared by
#'   \link{proc.prepareIEATargets4analysis} function
#' @param startyear reference year for analysis
analyze.CorporateEconomy <- function(ald.master, iea.targets, startyear) {
  ald.master <- as_tibble(ald.master)
  iea.targets <- as_tibble(iea.targets)
  
  scenarios <- 
    iea.targets %>%
    dplyr::filter(
      !Scenario %in% c("BNEF","GPER")
    ) %>%
    dplyr::select(
      Sector, Technology, Year, BenchmarkRegion, FairSharePerc, Scenario, Direction
    )
  
  corporateEconomy <-
    ald.master %>%
    dplyr::filter(ALD.Level == "Asset") %>%
    dplyr::group_by(BenchmarkRegion, Sector, Technology, Year) %>%
    dplyr::summarise(
      Production = sum(Production, na.rm = T),
      ProductionRef = sum(ProductionRef, na.rm = T),
      ProductionSectorUnit = sum(ProductionSectorUnit, na.rm = T),
      TechAdditionsAbs = sum(TechAdditionsAbs, na.rm = T), 
      ProductionSectorUnitRef = sum(ProductionSectorUnitRef, na.rm = T)
    ) %>%
    # calculate Production units across all technologies of Sector
    dplyr::group_by(BenchmarkRegion, Sector, Year) %>%
    dplyr::mutate(
      SectorProduction = sum(ProductionSectorUnit, na.rm = T)
    ) %>%
    # calculate Reference units (for the startyear)
    dplyr::group_by(BenchmarkRegion, Sector) %>%
    dplyr::mutate(
      SectorProductionRef = sum(
        if_else(Year == startyear, ProductionSectorUnit, 0),
        na.rm = T
      )
    ) %>%
    dplyr::ungroup() 
  
    # join sector masters with IEA scenarios (automotive, power, fossil fuels, demand)
  CorporateEconomySub1 <- corporateEconomy %>%
    dplyr::left_join(
      scenarios, 
      by = c("BenchmarkRegion","Sector","Technology", "Year")
    ) %>%
    filter(
      !is.na(Scenario)
    )
  
  # join sector masters with IEA scenarios (automotive, power, fossil fuels, demand)
  CorporateEconomySub2 <- corporateEconomy %>%
    filter(
      Sector %in% c("Aviation","Cement","Steel","Shipping")
    ) %>%
    dplyr::left_join(
      subset(scenarios, select = -Technology), 
      by = c("BenchmarkRegion","Sector", "Year")
    ) %>%
    filter(
      !is.na(Scenario)
    )
  
   corporateEconomyAnalysis <-
    union_all(CorporateEconomySub1,CorporateEconomySub2) %>%
    dplyr::mutate(
      Benchmark_Production = if_else(
        Direction == "declining" | Sector == "Fossil Fuels",
        ProductionSectorUnitRef * (1 + FairSharePerc),
        ProductionSectorUnitRef + (SectorProductionRef * FairSharePerc)
      ),
      Benchmark_BuildOut = if_else(
        Direction == "declining" | Sector == "Fossil Fuels",
        ProductionSectorUnitRef * FairSharePerc,
        SectorProductionRef * FairSharePerc
      ),
      Market_TechShare = ProductionSectorUnit / SectorProductionRef,
      Market_TechShare_Normalized = ProductionSectorUnit / SectorProduction,
      MarketBenchmark_TechShare = Benchmark_Production / SectorProductionRef
    )
  
  # return result with explicitly defined column set
  corporateEconomyAnalysis %>%
    dplyr::select(
      BenchmarkRegion,
      Sector,
      Year,
      Technology,
      ProductionSectorUnit,
      Production,
      ProductionSectorUnitRef,
      ProductionRef,
      TechAdditionsAbs,
      FairSharePerc,
      Scenario,
      Direction,
      SectorProduction,
      SectorProductionRef,
      Benchmark_Production,
      Benchmark_BuildOut,
      Market_TechShare,
      Market_TechShare_Normalized,
      MarketBenchmark_TechShare
    )
  
}

# file: MatchingResults_<matching level>_<loanvalue>
#' Meta-analysis on Loanbook matches
#' 
#' @export
#' 
#' @param loanbook loanbook data with merged match results prepared for analysis
#' @param ald.master Direct Owner Master dataset prepared by \link{proc.prepareSectorMaster4analysis} function
analyze.meta.LoanbookMatches <- function(loanbook, ald.master) {
  loanbook <- as_tibble(loanbook)
  ald.master <- as_tibble(ald.master)

  ald <- 
    ald.master %>% 
    dplyr::select(ALD.Level, Name, Sector) %>%
    unique() %>%
    dplyr::mutate(Analysis.Matched = 1)

  result <-
    loanbook %>%
    dplyr::mutate(
      PortWeight = LoanSize / sum(LoanSize, na.rm = T)
    ) %>%
    dplyr::left_join(
      ald,
      by = c(
        "ALD.link.Level" = "ALD.Level", 
        "ALD.link.Name" = "Name", 
        "Sector.Classification" = "Sector"
      )
    ) %>%
    dplyr::mutate(
      Analysis.Matched = if_else(is.na(Analysis.Matched), 0, 1, 0)
    )

  # return result with explicitly defined column set
  result %>%
    dplyr::select(
      # original loanbook columns
      colnames(loanbook),
      # additional columns
      PortWeight,
      Analysis.Matched
    )
  
}

#' Analyze Portfolio Mix
#' 
#' @export
#' 
#' @param loanbook loanbook data with merged match results prepared for analysis
#' @param ald.master Asset Level Data Master dataset prepared by \link{proc.prepareSectorMaster4analysis} function
#' @param startyear Reference year of analysis
#' @param years number of consequent years (after startyear) to analyze
#' @param all.lists sets of various values for analysis purposes
analyze.PortfolioMix <- function(loanbook, ald.master, startyear, years, all.lists) {
  loanbook <- as_tibble(loanbook)
  ald.master <- as_tibble(ald.master)
  
  # matched.ald.master <- sec.masters %>% filter(Sector == "Fossil Fuels" & ALD.Level == "Company") %>%
  #   mutate(
  #     LoanSize = as.numeric(10000)
  #   ) %>%
  #   dplyr::group_by(Sector, Year, BenchmarkRegion) %>%
  #   dplyr::mutate(Sector.LoanSize = sum(LoanSize, na.rm = T)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(PortWeight = LoanSize / sum(LoanSize, na.rm = T)) %>%
  # dplyr::rename(
  #   CompanyLevelProduction = Production,
  #   SectorSizeMatchedCompany = Sector.LoanSize)
  lbk.TPED <- loanbook %>%
    filter(Sector.Classification %in% c("Fossil Fuels", "Power")) %>% mutate(Sector.Classification = "TPED")
  
  # aggregated loanbook data
  lbk.agg <-
    loanbook %>% rbind(lbk.TPED) %>%
    dplyr::group_by(ALD.link.Level, ALD.link.Name, Sector.Classification) %>%
    dplyr::summarise(LoanSize = sum(LoanSize, na.rm = T)) %>%
    dplyr::group_by(Sector.Classification) %>%
    dplyr::mutate(Sector.LoanSize = sum(LoanSize, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PortWeight = LoanSize / sum(if_else(Sector.Classification != "TPED",LoanSize,0), na.rm = T))
  
  
  # asset level data joined with loanbook data
  matched.ald.master <-
    ald.master %>%
    rbind(
      ald.master %>% 
        filter(Sector %in% c("Fossil Fuels", "Power"), BenchmarkRegion == "Global") %>% 
        mutate(Sector = "TPED")
    ) %>%
    dplyr::inner_join(
      lbk.agg,
      by = c(
        "ALD.Level" = "ALD.link.Level", 
        "Name" = "ALD.link.Name", 
        "Sector" = "Sector.Classification"
      )
    ) %>%
    dplyr::rename(
      CompanyLevelProduction = Production,
      SectorSizeMatchedCompany = Sector.LoanSize
    )
    
  # cartesian product of Regions, Years, Technologies, Sectors
  addLines <-
    list(
      matched.ald.master %>% dplyr::select(BenchmarkRegion) %>% unique,
      data.frame(Technology = all.lists$TechList, stringsAsFactors = F),
      data.frame(Year = startyear:(startyear + years))
    ) %>% 
    Reduce(f = cross_join) %>% 
    dplyr::mutate(
      Sector = case_when(
        Technology %in% c("Coal","Oil","Gas") ~ "Fossil Fuels",
        Technology %in% c("Electric","Hybrid","ICE") ~ "Automotive",
        TRUE ~ "Power"
      )
    ) %>%
    dplyr::select(BenchmarkRegion, Year, Sector, Technology)

  
  # goal of analysis: portfolio mix
  portMix <-
    matched.ald.master %>%
    dplyr::mutate(
      SectorWeightMatchedCompany = LoanSize / SectorSizeMatchedCompany,
      #EnergySectorWeight = LoanSize / EnergyPortfolioSize,
      PortWtCompLvlProduction = CompanyLevelProduction * SectorWeightMatchedCompany, # company level production times the weight of the company in the sector - can be used for assess the volume effects in the demand analysis 
      # PortWtCompBuildOut = 
      WtTechnologyShare_Port_normalized = SectorWeightMatchedCompany * TechnologyShare, # technology share always kept to 100% - used for tech share analysis
      WtTechnologyShare_Port = SectorWeightMatchedCompany * TechnologyShareBasedStartYear,
      WtRegionalWeight = PortWeight * TechnologyRegionalWeight,
      BuildOutTechShare = case_when(
        is.na(BuildOutTechShare) | (Sector == "Power" & SectorAdditionsPerc <= 0.01) ~ 0,
        TRUE ~ BuildOutTechShare
      ),
      WtBuildOutTechShare = BuildOutTechShare * SectorWeightMatchedCompany
    ) %>%
    dplyr::full_join(
      addLines, 
      by = c("BenchmarkRegion", "Year", "Sector", "Technology")
    ) %>%
    dplyr::mutate(
      PortWtCompLvlProduction = if_else(is.na(PortWtCompLvlProduction), 0, PortWtCompLvlProduction),
      WtTechnologyShare_Port = if_else(is.na(WtTechnologyShare_Port), 0, WtTechnologyShare_Port),
      WtTechnologyShare_Port_normalized = if_else(is.na(WtTechnologyShare_Port_normalized), 0, WtTechnologyShare_Port_normalized)
    )

  # return result with explicitly defined column set
  portMix %>%
    dplyr::select(
      ALD.Level, 
      ID, 
      Name, 
      Year, 
      Technology, 
      BenchmarkRegion, 
      Sector, 
      CO2Intensity, 
      CompanyLevelProduction, 
      GlobalProduction, 
      ProductionSectorUnit, 
      SectorProduction, 
      ProductionRef, 
      ProductionSectorUnitRef, 
      SectorProductionRef, 
      GlobalProductionRef, 
      GlobalSectorProductionRef, 
      TechnologyShare, 
      TechnologyShareBasedStartYear, 
      TechnologyRegionalWeight, 
      CompanyRegionalWeight, 
      TechnologyRegionalWeightStartYear, 
      TechAdditionsAbs, 
      SectorBuildOutAbs, 
      SectorAdditionsPerc, 
      BuildOutTechShare, 
      LoanSize, 
      SectorSizeMatchedCompany, 
      PortWeight, 
      SectorWeightMatchedCompany, 
      PortWtCompLvlProduction, 
      WtTechnologyShare_Port_normalized, 
      WtTechnologyShare_Port, 
      WtRegionalWeight, 
      WtBuildOutTechShare
    )
  
}


#' Analyze Matched Clients Mix including Maturity
#' 
#' @export
#' 
#' @param loanbook loanbook data with merged match results prepared for analysis
#' @param ald.master Asset Level Data Master dataset prepared by \link{proc.prepareSectorMaster4analysis} function
#' @param startyear Reference year of analysis
#' @param years number of consequent years (after startyear) to analyze
#' @param all.lists sets of various values for analysis purposes
analyze.MatchedClientsMixInclMaturity <- function(loanbook, ald.master, startyear, years, all.lists) {
  # add time resolution option for maturity inclusion, e.g. quarterly, LT/MT/ST, annually, monthly
  # ald.master <- sec.masters
  # loanbook <- bm.data
  
  loanbook <- as_tibble(loanbook)
  ald.master <- as_tibble(ald.master)
  
  # aggregated loanbook data
  lbk.agg <-
    loanbook %>%
    dplyr::group_by(ALD.link.Level, ALD.link.Name, Sector.Classification, Maturity.Date) %>%
    dplyr::summarise(LoanSize = sum(LoanSize, na.rm = T)) %>%
    dplyr::group_by(Sector.Classification) %>%
    dplyr::mutate(Sector.LoanSize = sum(LoanSize, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PortWeight = LoanSize / sum(LoanSize, na.rm = T))
  # asset level data joined with loanbook data
  matched.ald.master <-
    ald.master %>%
    dplyr::inner_join(
      lbk.agg,
      by = c(
        "ALD.Level" = "ALD.link.Level", 
        "Name" = "ALD.link.Name", 
        "Sector" = "Sector.Classification"
      )
    ) %>%
    dplyr::rename(
      CompanyLevelProduction = Production,
      SectorSizeMatchedCompany = Sector.LoanSize
    )
  # cartesian product of Regions, Years, Technologies, Sectors
  addLines <-
    list(
      matched.ald.master %>% dplyr::select(BenchmarkRegion) %>% unique,
      data.frame(Technology = all.lists$TechList, stringsAsFactors = F),
      data.frame(Year = startyear:(startyear + years))
    ) %>% 
    Reduce(f = cross_join) %>% 
    dplyr::mutate(
      Sector = case_when(
        Technology %in% c("Coal","Oil","Gas") ~ "Fossil Fuels",
        Technology %in% c("Electric","Hybrid","ICE") ~ "Automotive",
        TRUE ~ "Power"
      )
    ) %>%
    dplyr::select(BenchmarkRegion, Year, Sector, Technology)
  
  # goal of analysis: portfolio mix
  portMix <-
    matched.ald.master %>%
    dplyr::mutate(
      SectorWeightMatchedCompany = LoanSize / SectorSizeMatchedCompany,
      #EnergySectorWeight = LoanSize / EnergyPortfolioSize,
      PortWtCompLvlProduction = CompanyLevelProduction * SectorWeightMatchedCompany, # company level production times the weight of the company in the sector - can be used for assess the volume effects in the demand analysis 
      # PortWtCompBuildOut = 
      WtTechnologyShare_Port_normalized = SectorWeightMatchedCompany * TechnologyShare, # technology share always kept to 100% - used for tech share analysis
      WtTechnologyShare_Port = SectorWeightMatchedCompany * TechnologyShareBasedStartYear,
      TechnologyLoanSize = LoanSize * TechnologyShare,
      WtRegionalWeight = PortWeight * TechnologyRegionalWeight,
      BuildOutTechShare = case_when(
        is.na(BuildOutTechShare) | (Sector == "Power" & SectorAdditionsPerc <= 0.01) ~ 0,
        TRUE ~ BuildOutTechShare
      ),
      WtBuildOutTechShare = BuildOutTechShare * SectorWeightMatchedCompany
    ) %>%
    dplyr::full_join(
      addLines, 
      by = c("BenchmarkRegion", "Year", "Sector", "Technology")
    ) %>%
    dplyr::mutate(
      PortWtCompLvlProduction = if_else(is.na(PortWtCompLvlProduction), 0, PortWtCompLvlProduction),
      WtTechnologyShare_Port = if_else(is.na(WtTechnologyShare_Port), 0, WtTechnologyShare_Port),
      WtTechnologyShare_Port_normalized = if_else(is.na(WtTechnologyShare_Port_normalized), 0, WtTechnologyShare_Port_normalized)
    )
  
  # return result with explicitly defined column set
  portMix %>%
    dplyr::select(
      ALD.Level, 
      ID, 
      Name, 
      Year, 
      Technology, 
      BenchmarkRegion, 
      Sector, 
      Maturity.Date,
      CO2Intensity, 
      CompanyLevelProduction, 
      GlobalProduction, 
      ProductionSectorUnit, 
      SectorProduction, 
      ProductionRef, 
      ProductionSectorUnitRef, 
      SectorProductionRef, 
      GlobalProductionRef, 
      GlobalSectorProductionRef, 
      TechnologyShare, 
      TechnologyShareBasedStartYear, 
      TechnologyRegionalWeight, 
      CompanyRegionalWeight, 
      TechnologyRegionalWeightStartYear, 
      TechAdditionsAbs, 
      SectorBuildOutAbs, 
      SectorAdditionsPerc, 
      BuildOutTechShare, 
      LoanSize, 
      SectorSizeMatchedCompany, 
      PortWeight, 
      SectorWeightMatchedCompany, 
      PortWtCompLvlProduction, 
      WtTechnologyShare_Port_normalized, 
      WtTechnologyShare_Port, 
      WtRegionalWeight, 
      WtBuildOutTechShare
    )
  
}


#' Analyze Portfolio Mix
#' 
#' @export
#' 
#' @param portfoliomix Portfolio Mix analysis results prepared by \link{analyze.PortfolioMix} function
#' @param startyear Reference year of analysis
analyze.ClientCompanyData <- function(portfoliomix, startyear) {
  portfoliomix <- as_tibble(portfoliomix)

  result <-
    portfoliomix %>%
    dplyr::filter(
      BenchmarkRegion == "Global" & Year == startyear
    )

  # return result with explicitly defined column set
  result %>%
    dplyr::select(
      CompanyID = ID,
      CompanyName = Name,
      Sector,
      Technology,
      CompanyLevelProduction,
      ProductionSectorUnit,
      SectorProduction,
      TechnologyShare,
      LoanSize,
      SectorSizeMatchedCompany,
      SectorWeightMatchedCompany,
      WtTechnologyShare_Port_normalized,
      ALD.Level
    )
}

#' Analyze Portfolio Mix Trajectory
#' 
#' @export
#' 
#' @param portfoliomix Portfolio Mix analysis results prepared by \link{analyze.PortfolioMix} function
#' @param iea.targets IEA Targets dataset prepared by \link{proc.prepareIEATargets4analysis} function
#' @param startyear Reference year of analysis
#' @param years number of years to analyze
analyze.PortfolioMixTrajectory <- function(
  portfoliomix, iea.targets, startyear, years
) {
  portfoliomix <- as_tibble(portfoliomix) %>%
    mutate(
      WtTechnologyShare_Port = if_else(is.infinite(WtTechnologyShare_Port),1,WtTechnologyShare_Port)
      ) 
    
  iea.targets <- as_tibble(iea.targets)

  targets <- 
    iea.targets %>%
    dplyr::filter(
      Year >= startyear & Year <= startyear + years
      ) %>%
    dplyr::select(
      BenchmarkRegion, Sector, Technology, Year, FairSharePerc, Scenario, Direction
    )
  
  pmx_Region <-
    portfoliomix %>%
    # Technology Level stats
    dplyr::group_by(
      BenchmarkRegion, Sector, Technology, Year
    ) %>%
    dplyr::summarize(
      WtRegionalWeight = sum(WtRegionalWeight, na.rm = T),
      WtTechnologyShare_Port = sum(WtTechnologyShare_Port, na.rm = T),
      WtTechnologyShare_Port_normalized = sum(WtTechnologyShare_Port_normalized, na.rm=T),
      WtBuildOutTechShare = sum(WtBuildOutTechShare, na.rm = T),
      WtCompanyLvlProd = sum(PortWtCompLvlProduction, na.rm = T),
      WtCO2Intensity = weighted.mean(CO2Intensity, LoanSize, na.rm = T),
      ProductionSectorUnit = sum(ProductionSectorUnit, na.rm = T)
    ) %>%
    # Sector Level stats
    dplyr::group_by(
      BenchmarkRegion, Sector, Year
    ) %>%
    dplyr::mutate(
      WtCompanyLvlProdSec = sum(WtCompanyLvlProd, na.rm = T),
      WtSectorShare_Port = sum(WtTechnologyShare_Port, na.rm = T),
      WtSectorShare_Port_normalized = sum(WtTechnologyShare_Port_normalized, na.rm = T),
      WtSectorCO2Intensity = weighted.mean(WtCO2Intensity, ProductionSectorUnit, na.rm = T)
    ) %>%
    # keep only necessary fields
    dplyr::select(
      BenchmarkRegion, Sector, Technology, Year,
      WtRegionalWeight, 
      WtTechnologyShare_Port, WtTechnologyShare_Port_normalized, 
      WtBuildOutTechShare, 
      WtSectorShare_Port, WtSectorShare_Port_normalized,
      WtCompanyLvlProd,WtCompanyLvlProdSec,
      WtCO2Intensity, WtSectorCO2Intensity
      # SectorSizeMatchedCompany
    ) %>%
    # take start year values as reference values
    # !!! in original script IEA data joined to Tech statistics and Sector stats are lost (?)
    dplyr::group_by(
      BenchmarkRegion, Sector, Technology
    ) %>%
    dplyr::mutate(
      WtCompanyLvlProdRef = sum(if_else(Year == startyear, WtCompanyLvlProd, 0), na.rm = T),
      WtTechnologyShare_PortRef = sum(if_else(Year == startyear, WtTechnologyShare_Port, 0), na.rm = T),
      WtTechnologyShare_Port_normalizedRef = sum(if_else(Year == startyear, WtTechnologyShare_Port_normalized, 0), na.rm = T),
      WtCompanyLvlProdSecRef = sum(if_else(Year == startyear, WtCompanyLvlProdSec, 0), na.rm = T)
    ) %>%
    dplyr::ungroup()
  
  
  pmx_Trajectory <-
    pmx_Region %>%
    # join IEA targets data
    dplyr::inner_join(
      targets,
      by = c("BenchmarkRegion", "Sector", "Year", "Technology")
    ) %>%
    dplyr::mutate(
      TrajectoryBenchmark_WtCompanyLvlProd = if_else(
        Direction == "increasing" & Sector != "Fossil Fuels",
        WtCompanyLvlProdRef + WtCompanyLvlProdSecRef * FairSharePerc,
        WtCompanyLvlProdRef * (1 + FairSharePerc)
      ),
      TrajectoryBenchmark_WtTechnologyShare_Port = if_else(
        Direction == "increasing" & Sector != "Fossil Fuels",
        WtTechnologyShare_PortRef + FairSharePerc,
        WtTechnologyShare_PortRef * (1 + FairSharePerc)
      ),
      TrajectoryBenchmark_WtTechnologyShare_Port_normalized = if_else(
        Direction == "increasing" & Sector != "Fossil Fuels",
        WtTechnologyShare_Port_normalizedRef + FairSharePerc,
        WtTechnologyShare_Port_normalizedRef * (1 + FairSharePerc)
      )
    ) %>%
    # # normalize Trajectory Benchmark Values - to be used ones the regional weighting is included
    # dplyr::group_by(
    #   Sector, Scenario, Year
    # ) %>%
    # dplyr::mutate(
    #   TrajectoryBenchmark_WtSectorShare_Port_normalized = 
    #     sum(if_else(BenchmarkRegion == "Global",TrajectoryBenchmark_WtTechnologyShare_Port,0), na.rm = T)
    # ) %>%
    # normalize Trajectory Benchmark Values
    dplyr::group_by(
      BenchmarkRegion, Sector, Scenario, Year
    ) %>%
    dplyr::mutate(
      TrajectoryBenchmark_WtSectorShare_Port_normalized = 
        sum(TrajectoryBenchmark_WtTechnologyShare_Port, na.rm = T)
    ) %>%
    # take start year values as reference values
    dplyr::group_by(
      BenchmarkRegion, Sector, Technology, Scenario
    ) %>%
    dplyr::mutate(
      TrajectoryBenchmark_WtTechnologyShare_Port_normalized = TrajectoryBenchmark_WtTechnologyShare_Port_normalized / TrajectoryBenchmark_WtSectorShare_Port_normalized,
      TrajectoryBenchmark_WtTechnologyShare_PortRef = 
        sum(if_else(Year == startyear, TrajectoryBenchmark_WtTechnologyShare_Port, 0), na.rm = T)
    ) %>%
    dplyr::ungroup()

  # return result with explicitly defined column set
  pmx_Trajectory %>%
    dplyr::select(
      BenchmarkRegion, 
      Sector, 
      Technology, 
      Year, 
      WtRegionalWeight, 
      WtTechnologyShare_Port, 
      WtTechnologyShare_Port_normalized, 
      WtBuildOutTechShare, 
      WtSectorShare_Port, 
      WtSectorShare_Port_normalized, 
      WtTechnologyShare_PortRef, 
      WtTechnologyShare_Port_normalizedRef, 
      WtCompanyLvlProd, # volume analysis proxy
      FairSharePerc, 
      Scenario, 
      Direction, 
      TrajectoryBenchmark_WtTechnologyShare_Port, 
      TrajectoryBenchmark_WtTechnologyShare_Port_normalized, 
      TrajectoryBenchmark_WtTechnologyShare_PortRef,
      TrajectoryBenchmark_WtCompanyLvlProd # Benchmark: volume analysis proxy
    )
    
}

#' Analyze Technology Mix
#' 
#' @export
#' 
#' @param portmixtrj Portfolio Mix Trajectory prepared by \link{analyze.PortfolioMixTrajectory} function
#' @param startyear Reference year of analysis
#' @param years number of years to analyze
analyze.TechnologyMix <- function(portmixtrj, startyear, years) {
  portmixtrj <- as_tibble(portmixtrj)

  techMix <- 
    portmixtrj %>%
    # take every 5th year and last year of analysis
    dplyr::filter(
      (Year - startyear) %% 5 == 0 | Year == startyear + years
    ) %>%
    dplyr::select(
      BenchmarkRegion, Sector, Year, Technology, Scenario, 
      # SectorSizeMatchedCompany,
      WtTechnologyShare_Port_normalized, 
      TrajectoryBenchmark_WtTechnologyShare_Port_normalized
    ) %>%
    dplyr::mutate(
      WtTechnologyShare_Port_normalized = if_else(
        Year == startyear + years,
        as.numeric(NA),
        WtTechnologyShare_Port_normalized
      )
    )
    # ) %>%
    # dplyr::mutate(
    #   LoanProxy_Port = WtTechnologyShare_Port_normalized * SectorSizeMatchedCompany,
    #   LoanProxy_Benchmark = TrajectoryBenchmark_WtTechnologyShare_Port_normalized * SectorSizeMatchedCompany
    # )
    
  # return result with explicitly defined column set
  techMix %>%
    dplyr::select(
      BenchmarkRegion, 
      Sector, 
      Year, 
      Technology, 
      Scenario, 
      WtTechnologyShare_Port_normalized, 
      TrajectoryBenchmark_WtTechnologyShare_Port_normalized
      #LoanProxy_Port,
      #LoanProxy_Benchmark
    )

}


#' Analyze CO2 Intensity
#'
#' @export
#'
#' @param portfoliomix Portfolio Mix analysis results prepared by
#'   \link{analyze.PortfolioMix} function
#' @param corp.econ.data Market Data derived from corporate economy based on
#'   asset level data
#' @param iea.targets IEA Targets dataset prepared by
#'   \link{proc.prepareIEATargets4analysis} function
#' @param startyear Reference year of analysis
#' @param years number of years to analyze
analyze.PortfolioCO2Intensity <- function(portfoliomix, corp.econ.data, iea.targets, startyear, years) {

  portfoliomix <- as_tibble(portfoliomix) %>%
    filter(
      Sector %in% c("Aviation","Cement","Steel") 
    )
  
  marketmix <- corp.econ.data %>%
    filter(
      Sector %in% c("Aviation","Cement","Steel") 
    ) %>%
    mutate(
      Value = "Corp Economy",
      WtSectorCO2Intensity = Plan.EmissionsFactor.Sector,
      TrajectoryBenchmark_CO2Intensity = Scen.EmissionsFactor.Sector,
      BenchmarkRegion = as.character(ScenarioGeography)
    ) %>%
    select(
      Value, Scenario, BenchmarkRegion,  Sector, Year, WtSectorCO2Intensity, TrajectoryBenchmark_CO2Intensity
    )  %>% unique()
    
  
  iea.targets <- as_tibble(iea.targets) %>% ungroup()
  
  targets <- 
    iea.targets %>%
    dplyr::filter(
      Year >= startyear & Year <= startyear + years | Year == 2040
    ) %>%
    dplyr::select(
      BenchmarkRegion, Sector, Technology, Year, FairSharePerc, Scenario, Direction
    )
  
  portCO2Intensity <-
    portfoliomix %>%
    # Technology Level stats
    dplyr::group_by(
      BenchmarkRegion, Sector, Technology, Year
    ) %>%
    dplyr::summarize(
      WtCO2Intensity = weighted.mean(CO2Intensity, LoanSize, na.rm = T),
      ProductionSectorUnit = sum(ProductionSectorUnit, na.rm = T)
    ) %>%
    # Sector Level stats
    dplyr::group_by(
      BenchmarkRegion, Sector, Year
    ) %>%
    dplyr::summarize(
      WtSectorCO2Intensity = weighted.mean(WtCO2Intensity, ProductionSectorUnit, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::complete(
      nesting(BenchmarkRegion, Sector),
      Year = c(seq(startyear, startyear + years), 2040)
    ) %>%
    dplyr::group_by(
      BenchmarkRegion, Sector
    ) %>%
    mutate(
      RefWtSectorCO2Intensity = sum(if_else(Year == startyear, WtSectorCO2Intensity, 0))
    ) %>%
    dplyr::ungroup()
  
  #referenceCO2Intensity <- 
  #  portCO2Intensity %>% 
  #  filter(Year == 2018) %>%
  #  select(BenchmarkRegion, Sector, Technology, WtSectorCO2Intensity)


  portCO2Intensity_Trajectory <-
    portCO2Intensity %>%
    # join IEA targets data
    dplyr::inner_join(
      targets,
      by = c("BenchmarkRegion", "Sector", "Year")
    ) %>%
    dplyr::mutate(
      TrajectoryBenchmark_CO2Intensity = RefWtSectorCO2Intensity * (1 + FairSharePerc)
    ) %>%
    dplyr::ungroup() %>%
    select(
      Scenario,
      BenchmarkRegion, 
      Sector, 
      Year, 
      WtSectorCO2Intensity,
      TrajectoryBenchmark_CO2Intensity
    ) %>%
    mutate(
      Value = "Portfolio"
    )
  
  CO2Intensity <- rbind(portCO2Intensity_Trajectory,marketmix)
  
  return(CO2Intensity)

}

#' Demand Analysis
#'
#' @description Asessment across the Power and Fossil Fuels sector. Scenario
#'   analysis (volume effect proxy) and Technology mix
#'
#' @export
#'
#' @param loanbook loanbook data for analysis
#' @param demand total primary energy demand (TPED) proxy dataset
#' @param startyear Reference year of analysis
#' @param years number of consequent years (after startyear) to analyze
analyze.TPED <- function(loanbook, demand, startyear, years) {
  # demand dataset consist of columns:
  if ({
    required <- c(
      "ALD.Level", "ALD.Name", "Sector", "Technology", "Year", 
      "Plan.CompanyLevelProduction", "Plan.TechnologyShare", 
      "Scen.CompanyLevelProduction", "Scen.TechnologyShare"
    )
    present <- colnames(demand)
    length(setdiff(required, present)) > 0
  }) stop("Incomplete dataset!")

  #filter demand dataset by Year
  dmd <- demand %>% 
    dplyr::filter(Year >= startyear, Year <= startyear + years) %>%
    dplyr::group_by(ALD.Level, ALD.Name, Sector, Technology, Year) %>%
    dplyr::summarise(
      Plan.CompanyLevelProduction = sum(Plan.CompanyLevelProduction, na.rm = T),
      Scen.CompanyLevelProduction = sum(Scen.CompanyLevelProduction, na.rm = T),
      Plan.TechnologyShare = sum(Plan.TechnologyShare, na.rm = T),
      Scen.TechnologyShare = sum(Scen.TechnologyShare, na.rm = T)
    ) %>%
    dplyr::ungroup()

  #filter loanbook by Sector.Classification in [Power, Fossil Fuels]
  lbk <- loanbook %>% 
    dplyr::filter(
      Sector.Classification %in% c("Power", "Fossil Fuels")
    ) %>%
    dplyr::mutate(
      Sector = Sector.Classification,
      ALD.Name = ALD.link.Name,
      ALD.Level = ALD.link.Level
    ) %>%
    # TPED data contains "Demand" as a Sector; Loanbook has: "Power", "Fossil Fuels".
    # Make explicit sector reassignment in loanbook so people can notie
    mutate(
      Sector = "Demand"
    ) %>%
    dplyr::group_by(Sector, ALD.Level, ALD.Name) %>%
    dplyr::summarise(LoanSize = sum(LoanSize, na.rm = T)) %>%
    dplyr::ungroup()

  # then join loanbook with Demand Dataset by Client Names
  analysis <- 
    lbk %>%
    dplyr::inner_join(
      dmd,
      by = c("ALD.Level", "ALD.Name", "Sector")
    ) %>%
    # aggregate by sector, tech etc.
    dplyr::group_by(Sector, Technology, Year) %>%
    # calculate Sector.Weight for the Company
    dplyr::mutate(
      SectorWeightMatchedCompany = ratio_to_report(LoanSize, na.rm = T)
    ) %>%
    # calculate weighted technology share analysis
    dplyr::mutate(
      Plan.WtTechnologyShare = SectorWeightMatchedCompany * Plan.TechnologyShare,
      Plan.WtCompanyLevelProduction = SectorWeightMatchedCompany * Plan.CompanyLevelProduction,
      Scen.WtTechnologyShare = SectorWeightMatchedCompany * Scen.TechnologyShare,
      Scen.WtCompanyLevelProduction = SectorWeightMatchedCompany * Scen.CompanyLevelProduction
    ) %>%
    # aggregate by Sector, Techniology, Year (client do not remain)
    dplyr::summarize(
      Port.WtTechnologyShare = sum(Plan.WtTechnologyShare, na.rm = T),
      Port.WtCompanyLevelProduction = sum(Plan.WtCompanyLevelProduction, na.rm = T),
      Scen.WtTechnologyShare = sum(Scen.WtTechnologyShare, na.rm = T),
      Scen.WtCompanyLevelProduction = sum(Scen.WtCompanyLevelProduction, na.rm = T)
    ) %>%
    group_by(Sector, Technology) %>%
    mutate(
      Port.VolumeProxy = Port.WtCompanyLevelProduction / (sum(if_else(Year == startyear,Port.WtCompanyLevelProduction,0))),
      Scen.VolumeProxy = Scen.WtCompanyLevelProduction / (sum(if_else(Year == startyear,Scen.WtCompanyLevelProduction,0)))
    ) %>%
    # dplyr::group_by(
    #   Year
    # ) %>%
    # dplyr::mutate(
    #   Port.WtTechnologyShare = sum(Port.WtCompanyLevelProduction, na.rm = T),
    #   Scen.WtTechnologyShare = sum(Scen.WtCompanyLevelProduction, na.rm = T)
    # Port.WtTechnologyShare = ratio_to_report(Port.WtCompanyLevelProduction, na.rm = T),
    # Scen.WtTechnologyShare = ratio_to_report(Scen.WtCompanyLevelProduction, na.rm = T)
    # ) %>%
    dplyr::ungroup()

  # return result with explicitly defined column set
  analysis %>%
    dplyr::select(
      Sector, 
      Technology, 
      Year,
      Port.WtTechnologyShare, 
      Port.WtCompanyLevelProduction, 
      Port.VolumeProxy,
      Scen.WtTechnologyShare, 
      Scen.WtCompanyLevelProduction,
      Scen.VolumeProxy
    )
}
