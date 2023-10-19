#### Load Packages ####
# loads the packages used for the script to load packages
packages_to_install <- c("tidyverse", "ggrepel", "shiny", "readxl","DBI",'RMySQL',"RSQLite")
for (package_name in packages_to_install) {
  if(!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}
ui <- fluidPage(
  #App title
  headerPanel("Thermo Scientific MS Input"),
  #File Inputs
  sidebarLayout(
    sidebarPanel(
      h4("Browse for Files"),
      fileInput("file1", "Choose Databse File (e.g. DabaseFile.db)", multiple = FALSE),
      fileInput("file2", "Upload a Input Parameter Template Excel File"),
      h4("Selecting the Checkbox Only Outputs Masses with a Negative Mass Defect (e.g 18.9984)"),
      checkboxInput("checkbox1", "Turn On Mass Defect Filtering"),
    ),
    mainPanel(
      #Parameter input
      h4("Write in Input Parameters"),
      textInput("FilePath", label = "File Path to Folder with R Project (e.g. E:/Documents/R Scripts/R Project):"),
      textInput("DBcon", label = "File Path to Database File (e.g. E:/MS Files for PFAS Library/PFAS Library 2023.db)):"),
      textInput("Author", label = "Enter Author Name:"),
      textInput("LicenseType", label = "License (e.g. CC0):"),
      textInput("CollisionGas", label = "Collision Gas (e.g. Nitrogen):"),
      textInput("SourceTemp", label = "Source Temperature (e.g. 50 C):"),
      textInput("ResSetting", label = "Resolution Setting (e.g. 30000 R):"),
      textInput("CapTemp", label = "Capillary Temperature (e.g. 250 C):"),
      textInput("IsoWidth", label = "Isolation Width (ex: 1.4 Daltons):"),
      textInput("InstrumentName", label = "Instrument Name (e.g. Orbitrap Fusion):"),
      textInput("InstrumentType", label = "Instrument Type (e.g. LC-ESI-ITFT):"),
      # action button that submits the user inputs into the Shiny App function
      actionButton("Submit_Button", "Submit"),
      # text output of the user input
      textOutput("dbcon_output"),
      textOutput("filepath_output"),
      textOutput("author_output"),
      textOutput("license_output"),
      textOutput("colgas_output"),
      textOutput("sourcetemp_output"),
      textOutput("ressetting_output"),
      textOutput("captemp_output"),
      textOutput("isowidth_output"),
      textOutput("instname_output"),
      textOutput("insttype_output")
    ) 
  )
)
# Shiny function that executes the script after the action button (the Submit Button in this case)
shinyServer <- function(input, output){
  observeEvent(input$Submit_Button, {
    if (!is.null(input$file2)) {
      parameters_template <- read_excel(input$file2$datapath)
    }
    # loads in written user inputs
    user_inputs<- reactiveValuesToList(input)
    #creates a list of the user inputs submitted by the action button
    saveRDS(user_inputs, file = "user_inputs.rds")
    # reads in the list as a variable
    rds_list <- readRDS("user_inputs.rds")
    #### FUNCTION USING FILES AS INPUT ####
    Connect_DB2DF <- function(file_path) {
        DB_connection <- file_path 
        ### Establish Connection to Database ####
        con <- dbConnect(RSQLite::SQLite(), dbname = DB_connection)    
        compound_query <- "SELECT c.CompoundId, c.Name, s.blobMass, s.blobIntensity, s.PrecursorMass, s.CollisionEnergy, s.ScanFilter, s.CreationDate, s.Polarity FROM CompoundTable AS c LEFT JOIN SpectrumTable AS s ON c.CompoundID = s.CompoundID"
        compounds <- dbGetQuery(con, compound_query)
        #### Assign Blob Columns ####
        massVector <- compounds[["blobMass"]]
        intVector <- compounds[["blobIntensity"]]
        #### Convert Blob Data ####
        masses <- sapply(massVector, FUN =  readBin, what = "double", n = 1000)
        intensities <- sapply(intVector, FUN =  readBin, what = "double", n = 1000)
        #### Create DF for Output ####
        spectrum <- tibble(Name = compounds[["Name"]], mass = masses, abun = intensities, 
                           collision = compounds[["CollisionEnergy"]], 
                           precursor = compounds[["PrecursorMass"]],
                           ID = compounds[["CompoundId"]],
                           Polarity = compounds[["Polarity"]],
                           Mass_Range = compounds[["ScanFilter"]],
                           AcquisitionDate = compounds[["CreationDate"]])
        spectrum <- spectrum %>% 
          mutate(Polarity = case_when( Polarity == "-" ~ "Negative", 
                                       Polarity == "+" ~ "Positive",
                                       TRUE ~ "Polarity")) %>% 
          mutate(IonType = case_when( Polarity == "Negative" ~ "[M-H]",
                                      Polarity == "Positive" ~ "[M+H]",
                                      TRUE ~ ""))
        pattern<- "\\[([^\\[\\]]+)\\]"
        extract_Mass_Range <- function(Mass_Range) {
          matches <- gregexpr(pattern, Mass_Range, perl = TRUE)
          if (any(length(matches) == -1)) {
            return(NA_character_)
          }
          return(regmatches(Mass_Range, matches))
        }
        spectrum$Mass_Range <- sapply(spectrum$Mass_Range, extract_Mass_Range)
        spectrum$Mass_Range <- gsub("\\[|\\]", "", spectrum$Mass_Range)
        ### Date Column ###
        # edit date column to proper format
        spectrum$AcquisitionDate <- str_extract(spectrum$AcquisitionDate, "\\d{1,2}/\\d{1,2}/\\d{4}")
        spectrum$AcquisitionDate <- as.Date(spectrum$AcquisitionDate, format = "%m/%d/%Y") %>% as.character() %>% lapply(FUN = gsub, pattern = "-", replacement = "\\.") %>% unlist()
        return(spectrum)
      }
    GetNameStringsFromFile <- function() {
      auth <-  as.character(parameters_template[1,2])
      Author <- paste("Author:", auth)
      LISC <- as.character(parameters_template[2,2])
      License <- paste("License:", LISC)
      colgas <- as.character(parameters_template[3,2])
      Col_gas <- paste("Collision Gas:", colgas)
      sourceTEMP <- as.character(parameters_template[4,2])
      source_temp_string <- paste("Source Temperature:", sourceTEMP)
      resolution <- as.character(parameters_template[5,2])
      Res_setting <- paste("Resolution Setting:", resolution)
      capTEMP <- as.character(parameters_template[6,2])
      Cap_temp_string <- paste("Capillary Temperature:", capTEMP)
      isolation <- as.character(parameters_template[7,2])
      Iso_Width <- paste("Isolation Width:", isolation)
      nameofINST <- as.character(parameters_template[8,2])
      Ins_name <- paste("Instrument Name:", nameofINST)
      InstType <- as.character(parameters_template[9,2])
      Inst_Type <- paste("Instrument Type:", InstType)
      filestring_list <- list(Author, License, Col_gas, source_temp_string, Res_setting, Cap_temp_string, Iso_Width, Ins_name, Inst_Type)
      return(filestring_list)
    }
    #### FILES USING WRITTEN AS INPUT ####
    Connect_DB2DF_FROMLIST <- function(DB_connection) {
      ### Establish Connection to Database ####
      con <- dbConnect(RSQLite::SQLite(), dbname = DB_connection)
      compound_query <- "SELECT c.CompoundId, c.Name, s.blobMass, s.blobIntensity, s.PrecursorMass, s.CollisionEnergy, s.ScanFilter, s.CreationDate, s.Polarity FROM CompoundTable AS c LEFT JOIN SpectrumTable AS s ON c.CompoundID = s.CompoundID"
      compounds <- dbGetQuery(con, compound_query)
      #### Assign Blob Columns ####
      massVector <- compounds[["blobMass"]]
      intVector <- compounds[["blobIntensity"]]
      #### Convert Blob Data ####
      masses <- sapply(massVector, FUN =  readBin, what = "double", n = 1000)
      intensities <- sapply(intVector, FUN =  readBin, what = "double", n = 1000)
      #### Create DF for Output ####
      spectrum <- tibble(Name = compounds[["Name"]], mass = masses, abun = intensities, 
                         collision = compounds[["CollisionEnergy"]], 
                         precursor = compounds[["PrecursorMass"]],
                         ID = compounds[["CompoundId"]],
                         Polarity = compounds[["Polarity"]],
                         Mass_Range = compounds[["ScanFilter"]],
                         AcquisitionDate = compounds[["CreationDate"]])
      spectrum <- spectrum %>% 
        mutate(Polarity = case_when( Polarity == "-" ~ "Negative", 
                                     Polarity == "+" ~ "Positive",
                                     TRUE ~ "Polarity")) %>% 
        mutate(IonType = case_when( Polarity == "Negative" ~ "[M-H]",
                                    Polarity == "Positive" ~ "[M+H]",
                                    TRUE ~ ""))
      #### Edit Mass Range Column ####
      pattern<- "\\[([^\\[\\]]+)\\]"
      extract_Mass_Range <- function(Mass_Range) {
        matches <- gregexpr(pattern, Mass_Range, perl = TRUE)
        if (any(length(matches) == -1)) {
          return(NA_character_)
        }
        return(regmatches(Mass_Range, matches))
      }
      spectrum$Mass_Range <- sapply(spectrum$Mass_Range, extract_Mass_Range)
      spectrum$Mass_Range <- gsub("\\[|\\]", "", spectrum$Mass_Range)
      ### Date Column ###
      # edit date column to proper format
      spectrum$AcquisitionDate <- str_extract(spectrum$AcquisitionDate, "\\d{1,2}/\\d{1,2}/\\d{4}")
      spectrum$AcquisitionDate <- as.Date(spectrum$AcquisitionDate, format = "%m/%d/%Y") %>% as.character() %>% lapply(FUN = gsub, pattern = "-", replacement = "\\.") %>% unlist()
      return(spectrum)
    }
    GetNameStringsFromWrittenInputs <- function(rds_list) {
      Inst_Type <- paste("Instrument Type:", rds_list$InstrumentType)
      Col_gas <- paste("Collision Gas:", rds_list$CollisionGas)
      Res_setting <- paste("Resolution Setting:", rds_list$ResSetting)
      Cap_temp_string <- paste("Capillary Temperature:", rds_list$CapTemp)
      Iso_Width <- paste("Isolation Width:", rds_list$IsoWidth)
      source_temp_string <- paste("Source Temperature:", rds_list$SourceTemp)
      Ins_name <- paste("Instrument Name:",  rds_list$InstrumentName)
      Author <- paste("Author:", rds_list$Author)
      License <- paste("License:", input$LicenseType)
      filestring_list <- list(Author, License, Col_gas, source_temp_string, Res_setting, Cap_temp_string, Iso_Width, Ins_name, Inst_Type)
      return(filestring_list)
    }
    #### GENERAL FUNCTIONS ####
    GetName_Mass_CE_Date_IonMode_IonType <- function() {
      name_string <- paste("Name:", output_row[["Name"]])
      precursor_string <- paste("PrecursorMZ:", sprintf("%.4f", round(output_row[["precursor"]],4)))
      energy_string <- paste("Collision Energy:", output_row["collision"])
      Date <- paste("Acquisition Date:", output_row[["AcquisitionDate"]])
      Ion_Mode <- paste("Ion Mode:", output_row[["Polarity"]])
      Ion_Type <- paste("Ion Type:", output_row[["IonType"]])
      stringlist <- list(
        name_string, energy_string, Date, Ion_Mode, Ion_Type
      )
      return(stringlist)
    }
    NoFilteringWriteTextFiles <- function(spectrum) {
      for (i in 1:length(spectrum$Name)) {
        # isolates the mass and abundance
        output_row <- spectrum[i,]
        out_spectrum <- unnest_legacy(output_row[c("mass","abun")]) %>% mutate(mass = sprintf("%.4f", round(mass,4)),
                                                                               abun = sprintf("%.4f", round(abun,4)))
        # converts the isolated spectral data to numeric and then finds the max abundance
        out_spectrum$mass <- as.numeric(out_spectrum$mass)
        out_spectrum$abun <- as.numeric(out_spectrum$abun)
        #filters out the artifact peak seen in the Thermo Orbitrap
        out_spectrum <- out_spectrum %>% filter(!(mass >=173.45 & mass <= 173.48))
        # creates the strings of spectra parameters with information from the database and from the user inputs
        name_string <- paste("Name:", output_row[["Name"]])
        precursor_string <- paste("PrecursorMZ:", sprintf("%.4f", round(output_row[["precursor"]],4)))
        energy_string <- paste("Collision Energy:", output_row["collision"])
        Date <- paste("Acquisition Date:", output_row[["AcquisitionDate"]])
        Ion_Mode <- paste("Ion Mode:", output_row[["Polarity"]])
        Ion_Type <- paste("Ion Type:", output_row[["IonType"]])
        if (!is.null(input$file2)) {
          auth <-  as.character(parameters_template[1,2])
          Author <- paste("Author:", auth)
          LISC <- as.character(parameters_template[2,2])
          License <- paste("License:", LISC)
          colgas <- as.character(parameters_template[3,2])
          Col_gas <- paste("Collision Gas:", colgas)
          sourceTEMP <- as.character(parameters_template[4,2])
          source_temp_string <- paste("Source Temperature:", sourceTEMP)
          resolution <- as.character(parameters_template[5,2])
          Res_setting <- paste("Resolution Setting:", resolution)
          capTEMP <- as.character(parameters_template[6,2])
          Cap_temp_string <- paste("Capillary Temperature:", capTEMP)
          isolation <- as.character(parameters_template[7,2])
          Iso_Width <- paste("Isolation Width:", isolation)
          nameofINST <- as.character(parameters_template[8,2])
          Ins_name <- paste("Instrument Name:", nameofINST)
          InstType <- as.character(parameters_template[9,2])
          Inst_Type <- paste("Instrument Type:", InstType)
        } else {
          Inst_Type <- paste("Instrument Type:", rds_list$InstrumentType)
          Col_gas <- paste("Collision Gas:", rds_list$CollisionGas)
          Res_setting <- paste("Resolution Setting:", rds_list$ResSetting)
          Cap_temp_string <- paste("Capillary Temperature:", rds_list$CapTemp)
          Iso_Width <- paste("Isolation Width:", rds_list$IsoWidth)
          source_temp_string <- paste("Source Temperature:", rds_list$SourceTemp)
          Ins_name <- paste("Instrument Name:",  rds_list$InstrumentName)
          Author <- paste("Author:", rds_list$Author)
          License <- paste("License:", input$LicenseType)
        }
        # Get number of peaks from out_spectrum rows 
        npeaks_string <- paste("Num Peaks:", length(out_spectrum$mass))
        #names the text files outputted with eacd respective compounds and collision energies
        out_filename = paste0(output_row[["Name"]],"_CE",round(as.numeric(output_row["collision"]),0),".txt")
        # loads in the strings from before into the text file
        write_lines(c(name_string, Author, License, Date, energy_string, precursor_string, Ins_name, Inst_Type, Ion_Mode, Col_gas, Res_setting, Cap_temp_string, Iso_Width, source_temp_string, Ion_Type, npeaks_string), 
                    file = out_filename, sep = "\r\n")
        # loads in the spectral data
        write.table(out_spectrum,
                    out_filename,
                    eol = "\n",
                    sep = " ",
                    quote = FALSE,
                    append = TRUE,
                    col.names = FALSE,
                    row.names = FALSE)
      }
    }
    # if checkbox 1 is checked, turn on mass defect filtering, else no mass defect filtering
    if (input$checkbox1) {
      if (!is.null(input$file1)) {
        file_path <- input$file1$datapath
        spectrum <- Connect_DB2DF(file_path)
        #### Function to convert each row into an Output text file ####
        for (i in 1:length(spectrum$Name)) {
          # isolates the mass and abundance
          output_row <- spectrum[i,]
          out_spectrum <- unnest_legacy(output_row[c("mass","abun")]) %>% mutate(mass = sprintf("%.4f", round(mass,4)),
                                                                                 abun = sprintf("%.4f", round(abun,4)))
          # converts the isolated spectral data to numeric and then finds the max abundance
          out_spectrum$mass <- as.numeric(out_spectrum$mass)
          out_spectrum$abun <- as.numeric(out_spectrum$abun)
          #filters out the artifact peak seen in the Thermo Orbitrap
          out_spectrum <- out_spectrum %>% filter(!(mass >=173.45 & mass <= 173.48))
          #filters for masses with only a mass defect ****
          out_spectrum <- out_spectrum %>% mutate(masskeep = if_else(mass - round(mass) < 0, mass, NA), mass) %>% 
            na.omit(out_spectrum) %>% 
            select(mass, abun)
          # creates the strings of spectra parameters with information from the database and from the user inputs
          Name_Mass_CE_Date_IonMode_IonType <- GetName_Mass_CE_Date_IonMode_IonType()
          if (!is.null(input$file2)) {
            filestring_list <- GetNameStringsFromFile()
          } else {
            filestring_list <- GetNameStringsFromWrittenInputs()
          }
          # Get number of peaks from out_spectrum rows after filtering
          npeaks_string <- paste("Num Peaks:", length(out_spectrum$mass))
          #names the text files outputted with each respective compounds and collision energies
          out_filename = paste0(output_row[["Name"]],"_CE",round(as.numeric(output_row["collision"]),0),".txt")
          # loads in the strings from before into the text file
          write_lines(c(Name_Mass_CE_Date_IonMode_IonType, filestring_list, npeaks_string), 
                      file = out_filename, sep = "\r\n")
          # loads in the spectral data
          write.table(out_spectrum,
                      out_filename,
                      eol = "\n",
                      sep = " ",
                      quote = FALSE,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE)
        }
      } else {
        # selects the file path and the path to the database file for depositing the output text files and connectiong to the database
        connection_result <- rds_list$FilePath
        DB_connection <- rds_list$DBcon
        ### Establish Connection to Database ####
        spectrum <- Connect_DB2DF_FROMLIST(DB_connection)
        #### Function to convert each row into an Output text file ####
        for (i in 1:length(spectrum$Name)) {
          # isolates the mass and abundance
          output_row <- spectrum[i,]
          out_spectrum <- unnest_legacy(output_row[c("mass","abun")]) %>% mutate(mass = sprintf("%.4f", round(mass,4)),
                                                                                 abun = sprintf("%.4f", round(abun,4)))
          # converts the isolated spectral data to numeric and then finds the max abundance
          out_spectrum$mass <- as.numeric(out_spectrum$mass)
          out_spectrum$abun <- as.numeric(out_spectrum$abun)
          #filters out the artifact peak seen in the Thermo Orbitrap
          out_spectrum <- out_spectrum %>% filter(!(mass >=173.45 & mass <= 173.48))
          #filters for masses with only a mass defect ****
          out_spectrum <- out_spectrum %>% mutate(masskeep = if_else(mass - round(mass) < 0, mass, NA), mass) %>% 
            na.omit(out_spectrum) %>% 
            select(mass, abun)
          # creates the strings of spectra parameters with information from the database and from the user inputs
          Name_Mass_CE_Date_IonMode_IonType <- GetName_Mass_CE_Date_IonMode_IonType()
          if (!is.null(input$file2)) {
            filestring_list <- GetNameStringsFromFile()
          } else {
            filestring_list <- GetNameStringsFromWrittenInputs()
          }
          # Get number of peaks from out_spectrum rows after filtering
          npeaks_string <- paste("Num Peaks:", length(out_spectrum$mass))
          #names the text files outputted with eacd respective compounds and collision energies
          out_filename = paste0(output_row[["Name"]],"_CE",round(as.numeric(output_row["collision"]),0),".txt")
          # loads in the strings from before into the text file
          write_lines(c(Name_Mass_CE_Date_IonMode_IonType, filestring_list, npeaks_string), 
                      file = out_filename, sep = "\r\n")
          # loads in the spectral data
          write.table(out_spectrum,
                      out_filename,
                      eol = "\n",
                      sep = " ",
                      quote = FALSE,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE)
        }
    } 
    }  else {
      if (!is.null(input$file1)) {
        file_path <- input$file1$datapath
        spectrum <- Connect_DB2DF(file_path)
        #### Function to convert each row into an Output text file ####
        NoFilteringWriteTextFiles(spectrum)
      } else {
        # selects the file path and the path to the database file for depositing the output text files and connecting to the database
        connection_result <- rds_list$FilePath
        DB_connection <- rds_list$DBcon
        ### Establish Connection to Database ####
        spectrum <- Connect_DB2DF_FROMLIST(DB_connection)
        #### Function to convert each row into an Output text file ####
        NoFilteringWriteTextFiles(spectrum)
      }
    }
  })
  # creates the text output seen in the shiny ui
  output$filepath_output <- 
    renderText(input$FilePath)
  output$dbcon_output <- 
    renderText(input$DBcon)
  output$author_output <- 
    renderText(input$Author)
  output$license_output <-
    renderText(input$LicenseType)
  output$colgas_output <-
    renderText(input$CollisionGas)
  output$sourcetemp_output <-
    renderText(input$SourceTemp)
  output$ressetting_output <-
    renderText(input$ResSetting)
  output$captemp_output <-
    renderText(input$CapTemp)
  output$isowidth_output <-
    renderText(input$IsoWidth)
  output$instname_output <-
    renderText(input$InstrumentName)
  output$insttype_output <-
    renderText(input$InstrumentType)
}
# executes function using the ui and the server components generating text files outputted
shinyApp(ui,shinyServer)