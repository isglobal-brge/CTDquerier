get_ctd_url <- function( index, term = "", category = "" ) {

  url <- list()
  url$basic                     <- "http://ctdbase.org/basicQuery.go?bq=TERM&bqCat=CATEGORY&6578706f7274=1&d-4029212-e=5"
  url$gene_basic                <- "http://ctdbase.org/basicQuery.go?bq=TERM&bqCat=CATEGORY&6578706f7274=1&d-4029212-e=5"
  url$chemical_gene_interaction <- "http://ctdbase.org/detail.go?6578706f7274=1&d-1339283-e=5&view=ixn&type=CATEGORY&acc=TERM"

  url$chemical_gene             <- "http://ctdbase.org/detail.go?d-3572529-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=gene"
  url$chemical_gene_interaction <- "http://ctdbase.org/detail.go?d-1339283-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=ixn"
  url$chemical_disease          <- "http://ctdbase.org/detail.go?d-1332398-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=disease"
  url$chemical_go               <- "http://ctdbase.org/detail.go?d-445124-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=go"
  url$chemical_kegg             <- "http://ctdbase.org/detail.go?d-7820438-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=pathway"

  url$gene_chemical             <- "http://ctdbase.org/detail.go?d-3572529-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=chem"
  url$gene_chemical_interaction <- "http://ctdbase.org/detail.go?d-1339283-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=ixn"
  url$gene_disease              <- "http://ctdbase.org/detail.go?d-1332398-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=disease"
  url$gene_gene_interaction     <- "http://ctdbase.org/detail.go?d-5152978-e=5&d-3572529-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=gg"
  url$gene_go                   <- "http://ctdbase.org/detail.go?d-7449458-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=go"
  url$gene_kegg                 <- "http://ctdbase.org/detail.go?d-148988-e=5&6578706f7274=1&type=CATEGORY&acc=TERM&view=pathway"




  url$disease_chemical          <- "http://ctdbase.org/detail.go?acc=TERM&view=chem&6578706f7274=1&type=CATEGORY&d-1332398-e=5"
  url$disease_gene              <- "http://ctdbase.org/detail.go?acc=TERM&view=gene&6578706f7274=1&type=CATEGORY&d-1332398-e=5"
  url$disease_kegg              <- "http://ctdbase.org/detail.go?acc=TERM&view=pathway&6578706f7274=1&type=CATEGORY&d-148988-e=5"



  ## TODO - IMPLEMENT
  url$gene_exposures <- "http://ctdbase.org/detail.go?acc=TERM&type=CATEGORY&checkAllReceptorFields=on&includePhenotype=on&includeNumberStressorSamples=on&includeReceptorNotes=on&includeAssociatedStudyTitle=on&includeDetectionFrequency=on&includeMedium=on&includeExposureEventNotes=on&includeStressorSourceDetails=on&view=expConsol&includeReceptorDescription=on&includeOutcomeIxnType=on&checkAllOutcomeFields=on&includeAssayNotes=on&includeMeasurementStatistic=on&includeDisease=on&includeStressorSourceCategory=on&6578706f7274=1&includeStateOrProvince=on&d-1393217-e=5&includeSmokerStatus=on&includeNumberOfReceptors=on&includeAge=on&includeMethods=on&includeLimitsOfDetection=on&includeCityTownRegionOrArea=on&includeCountry=on&checkAllReferenceFields=on&checkAllFields=on&checkAllStressorFields=on&includeBiomarkerLevel=on&checkAllEventFields=on&includeStressorNotes=on&includeExposureOutcomeNotes=on&includeGender=on&includeReference=on&includeStudyFactorNms=on&includeEnrollmentYears=on&includeStressorAgent=on&includeRace=on&includeBiomarker=on&includeAnatomy=on"

  sel <- url[ index ][[ 1 ]]

  sel <- stringr::str_replace(
      string      = sel,
      pattern     = "CATEGORY",
      replacement = category
  )

  sel <- stringr::str_replace(
      string      = sel,
      pattern     = "TERM",
      replacement = term
  )

  sel <- stringr::str_replace(
      string      = sel,
      pattern     = " ",
      replacement = "+"
  )

  return( sel )
}
