#' NAICS -> Sector Subclassification 
#' 
#' Conversion table of NAICS codes into internal Sector Subclassification.
#' Describes relation between NAICS classification and Sector Subclassification
#' on 3 different levels: Sector, Subsector, Industry. Rules with Industry code 
#' plrevail over rules with Subsector code; Subsector prevails over Sector.
#' 
#' @format A dataframe with following variables \describe{
#'   \item{Level}{Level to which particular code belongs, 
#'     can be "Sector", "Subsector", "Industry"}
#'   \item{Code}{Code of Sector (2-digit), Subsector (3-digit) or Industry (6-digit)}
#'   \item{Description}{Description for related Code}
#'   \item{Sector.Classification}{Assigned Sector subclassification code}
#' }
"data.naics2sc.rules"

#' Benchmark regions 
#' 
#' Different world regions described in terms of regions and countries. 
#' Data contains lists of vectors, vectors represented in pairs of 
#' Long Names (e.g. Country name) and ISO codes (Country ISO 2-char code)
#' 
#' 
#' @format a list of following pairs of vectors \describe{
#'   \item{BenchRegions, BenchRegions_ISO_colnames}{Set of benchmark regions}
#'   \item{Global, Global_ISO}{Global region}
#'   \item{Africa, Afrika_ISO}{Africa}
#'   \item{Brazil, Brazil_ISO}{Brazil}
#'   \item{China, China_ISO}{China}
#'   \item{EEurope_Eurasia, EEurope_Eurasia_ISO}{Eastern Europe and Eurasia}
#'   \item{EU, EU_ISO}{Europe}
#'   \item{India, India_ISO}{India}
#'   \item{Japan, Japan_ISO}{Japan}
#'   \item{LatinAmerica, LatinAmerica_ISO}{Latin America}
#'   \item{MiddleEast, MiddleEast_ISO}{Middle East}
#'   \item{NonOECD, NonOECD_ISO}{non-OECD countries}
#'   \item{NonOECDAsia, NonOECDAsia_ISO}{non-OECD countries of Asia}
#'   \item{OECD, OECD_ISO}{OECD countries}
#'   \item{OECDAmericas, OECDAmericas_ISO}{OECD countries of Americas}
#'   \item{OECDAsiaOceania, OECDAsiaOceania_ISO}{OECD countries of Asia and Oceania}
#'   \item{OECDEurope, OECDEurope_ISO}{OECD countries of Europe}
#'   \item{Russia, Russia_ISO}{Russia}
#'   \item{SouthAfrica, SouthAfrica_ISO}{South Africa}
#'   \item{US, US_ISO}{United States of America}
#'   \item{OECDAsiaOceaniaWoJP, OECDAsiaOceaniaWoJP_ISO}{OECD countries of Asia and Oceania wihout Japan}
#'   \item{OECDAmericasWoUS, OECDAmericasWoUS_ISO}{OECD countries of Americas without US}
#'   \item{LatinAmericaWoBR, LatinAmericaWoBR_ISO}{Latin America without Brazil}
#'   \item{AfricaWoZA, AfricaWoZA_ISO}{Africa without South Africa}
#'   \item{EEurope_EurasiaWoRU, EEurope_EurasiaWoRU_ISO}{Eastern Europe and Eurasia without Russia}
#'   \item{NonOECDAsiaRest, NonOECDAsiaRest_ISO}{non-OECD countries of Asia - the rest}
#'   \item{NonOECDRest, NonOECDRest_ISO}{non-OECD countries - the rest}
#' }
"data.benchmark.regions"


#' Index regions 
#' 
#' Different world regions described in terms of regions and countries. 
#' Data contains lists of vectors, vectors represented in pairs of 
#' Long Names (e.g. Country name) and ISO codes (Country ISO 2-char code)
#' 
#' @format a list of following pairs of vectors \describe{
#'   \item{IndexUniverse, IndexUniverseColname}{Set of Indexes}
#'   \item{Global, Global_ISO}{Countries of global region}
#'   \item{MSCIWorld, MSCIWorld_ISO}{MSCI World}
#'   \item{MSCIEmergingMarkets, MSCIEmergingMarkets_ISO}{MSCI - Emegring Markets}
#'   \item{MSCIACWI, MSCIACWI_ISO}{}
#'   \item{Stoxx600, Stoxx600_ISO}{}
#'   \item{SP500, SP500_ISO}{}
#'   \item{OutsideACWI, OutsideACWI_ISO}{}
#' }
"data.index.regions"


#' Required data files
#'
#' Table includes information about required data for different processing stages.
#' Data should be available before processing may be started.
#' 
#' @format \describe{
#'   \item{Stage}{Processing stage}
#'   \item{Process}{Exact process that requires data}
#'   \item{Dataset}{Required dataset name}
#'   \item{Location}{Dataset location (where it is stored as a file). Some 
#'     parametrization is available to define DataInput, Data, Results folders 
#'     ({data-input}, {data-temp}, {data-result} respectively)}
#'   \item{File Name Pattern}{File name pattern}
#'   \item{Comment}{}
#' }
"data.dataset.req.files"

#' Required Dataset columns
#' 
#' Functions take data as an input and return data as an output. In order to be in control of data completeness attribute sets should be checked before processing starts. Completeness checks described in this data frame.
#'
#' @format \describe{
#'   \item{Dataset}{Dataset name (as it is called in \link{data.dataset.req.files})}
#'   \item{Attribute}{Exact process that requires data}
#'   \item{Purpose}{Required dataset name}
#' }
"data.dataset.req.attrs"

#' Technology list
#' 
#' Vector of Technologies used in analysis
"data.technology"

#' Sector Classification list
#' 
#' Vector of Sector Classification Values
"data.sector.classification"

#' Country names and ISO codes
#'
#' Dataframe with country names and 3-character ISO codes.
"data.country.iso.codes"

#' Name reduction rules 
#' 
#' Name reduction (abbreviation) fules are used for name simplification. Data 
#' is represented in form of data.frame with two columns.
#' @format \describe{
#'   \item{From}{Original value to be replaced}
#'   \item{To}{Resulting value}
#' }
#' Values can be provided in any case, they are explicitely converted into lower
#' case (as a name) before use.
"data.name.reductions"

#' Company ownership types
#' 
#' Vector of company ownership types (like PLC, LLC, etc) is used to distinguish 
#' ownership type in company name during simplification.
"data.ownership.types"

#' Test loanbook
#'
#' Test loanbook data provides single source of data for development and
#' testing. It is not allowed to share commercial data this is why good dataset
#' becomes extremely necessary for cross-company communication.
#'
#' @format dataset represents basic structure neccessary for generic workflow 
#' \describe{ 
#'   \item{Source}{Source of information}
#'   \item{Reporting.Date}{Reporting Date}
#'   \item{Facility.ID}{Credit Facility Identification}
#'   \item{Outstanding}{Outstanding Amount in Reporting Currency}
#'   \item{Exposure}{Credit Limit Exposure}
#'   \item{Loan.Currency}{Original Currency}
#'   \item{Maturity.Date}{Maturity Date}
#'   \item{Asset.ID}{Asset Identification}
#'   \item{Asset.Name}{Asset Name}
#'   \item{Borrower.ID}{Borrower Identification}
#'   \item{Borrower.Name}{Borrower Name}
#'   \item{Borrower.Country}{Borrower Country}
#'   \item{Borrower.NAICS}{Borrower NAICS Code}
#'   \item{Borrower.NAICS.Description}{Borrower NAICS Inductry Description}
#'   \item{Sector.Classification}{Loan Sector Classification according to 2dii}
#'   \item{Ultimate.Parent.ID}{Ultimate Parent Identification}
#'   \item{Ultimate.Parent.Name}{Ultimate Parent Name}
#'   \item{Ultimate.Parent.Country}{Ultimate Parent Country}
#'   \item{Ultimate.Parent.NAICS}{Ultimate Parent NAICS Code}
#'   \item{Ultimate.Parent.NAICS.Description}{Ultimate Parent NAICS Inductry Description}
#'   }
"data.test.loanbook"
