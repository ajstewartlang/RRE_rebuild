library (readxl)
library (dplyr)
library (readr)

rebuild <- function (x,u,y) {
  master <- read_excel(x, 
                       sheet = u)
  allgroups <- read_excel((y[1]))

    for (i in 2:length (y)) {
      data <- read_excel(y[i])
      allgroups <- rbind (allgroups,data)
    }

  #select key columns from each reviewer pair spreadsheet
  allgroups <- dplyr::select (allgroups, "Pure Output ID" ,"AGREED PREDICTED GRADE", "Agreed Reviewer name",
                              "Agreed boundary grade", "Agreed reviewer comment", "Reviewer 1 Name", 
                              "Reviewer 1 Score", "Reviewer 1 Boundary grade", "Reviewer 1 comment", 
                              "Reviewer 2 Name", "Reviewer 2 Score", "Reviewer 2 Boundary Grade", 
                              "Reviewer 2 comment")

  #create new data frame made up of papers which currently do not have scores
  new_papers <- is.na(master$`AGREED PREDICTED GRADE`)
  new_papers_data <- master[new_papers,]
  
  #create new data frame with papers that have legacy scores
  old_papers_data <- master[!new_papers,]
  old_papers_data <- old_papers_data[1:42]
  
  #remove columns greater than 30 from the data frame with unscored papers
  stripped_data <- dplyr::select (new_papers_data, 1:30)

  #use full join function to bring together the stipped_data frame and the 2018 RRE scores
  #joined by the Pure Output ID number
  temp <- full_join(stripped_data, allgroups, by="Pure Output ID")
  
  #rbind the 2018 paper scores with legacy paper scores
  temp <- rbind (temp, old_papers_data)
  
  #generate list of multiples
  mult <- is.na(temp$Multiples)
  multiples <- temp[!mult,]
  
  #save data frame with new scores and legacy scores to a new .csv file
  #also save separate file containing outputs submitted more than once
  write_excel_csv(temp, "Combined RRE.csv")
  write_excel_csv(multiples, "Multiples.csv")
}

#call the function with UoA4 RRE parameters
rebuild ("Fict_UOA04 master.xlsx", "UOA04", c("fict_group1.xlsx", "fict_group2.xlsx", "fict_group3.xlsx", "fict_group4.xlsx", "fict_group5.xlsx","fict_group6.xlsx", "fict_group7.xlsx", "fict_group8.xlsx","fict_group9.xlsx", "fict_group10.xlsx", "fict_group11.xlsx", "fict_group12.xlsx"))
