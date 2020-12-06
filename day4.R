inp  = readLines("day4_inp.txt", )

require(stringr)
require(dplyr)

#### Validators per field --------------------

byr <- function(data){
  if( str_detect(data, "^[0-9]{4}$")) {
    value = as.numeric(data)
    valid = between(value, 1920, 2002) 
  } else {
    valid = FALSE
    value = NA
  }
  return(list(data=data, value=value, valid=valid, unit='year'))
}


iyr <- function(data){
  if( str_detect(data, "^[0-9]{4}$")) {
    value = as.numeric(data)
    valid = between(value, 2010, 2020) 
  } else {
    valid = FALSE
    value = NA
  }
  return(list(data=data, value=value, valid=valid, unit='year'))
}


eyr <- function(data){
  if( str_detect(data, "^[0-9]{4}$")) {
    value = as.numeric(data)
    valid = between(value, 2020, 2030) 
  } else {
    valid = FALSE
    value = NA
  }
  return(list(data=data, value=value, valid=valid, unit='year'))
}


hgt <- function(data){
  valid = FALSE
  value = NA
  unit = NA
  if(str_detect(data, "[0-9]{2}in")) {
    value = as.numeric(str_remove(data, "in"))
    valid = between(value, 59, 76)
    unit = 'in'
  } else if (str_detect(data, "[0-9]{2}cm")) {
    value = as.numeric(str_remove(data, "cm"))
    valid = between(value, 150, 193)
    unit = 'cm'
  }
  return(list(data=data, value=value, valid=valid, unit=unit))
}


hcl <- function(data){
  if (str_detect(data, "^#[0-9a-f]{6}$")) {
    value = data
    valid = TRUE
  } else {
    value = NA
    valid = FALSE
  }
  return(list(data=data, value=value, valid=valid, unit='color'))
}


ecl <- function(data){
  if (str_detect(data, "^(amb|blu|brn|gry|grn|hzl|oth)$")) {
    value = data
    valid = TRUE
  } else {
    value = NA
    valid = FALSE
  }
  return(list(data=data, value=value, valid=valid, unit='color'))
}


pid <- function(data){
  if( str_detect(data, "^[0-9]{9}$")) {
    value = as.numeric(data)
    valid = TRUE
  } else {
    valid = FALSE
    value = NA
  }
  
  return(list(data=data, value=value, valid=valid, unit='serial'))
}


cid <- function(data){
  return(list(data=data, value=data, valid=TRUE, unit='serial'))
}


validator <- function(passport, seperator=" "){
  #' validator (Validate passport) 
  #' 
  #' Takes a passport input and parses the entries
  #' @param passport character. The passport text
  #' @param seperator character. The character that seperates entries within each passport
  #' @return Information for all parsed fields:
  #'  - data: the original data (character)
  #'  - value: the sanitized value (numeric/character)
  #'  - valid: if the data was a valid field (boolean)
  #'  - unit: the measurement unit of the value (character)
  entries = strsplit(passport, split = seperator)[[1]]
  parsed_passport = list()
  for(entry in entries){
    parsed_entry = parse_entry(entry)
    parsed_passport[[parsed_entry$field]] = parsed_entry$data
  }
  return(parsed_passport)
}


parse_entry <- function(entry){
  #' parse_entry (Parse entries)
  #' 
  #' Takes a passport entry and extracts the field and the data.
  #' The data is validated using the function for the respective field 
  #' @param passport character. The passport text
  #' @param seperator character. The character that seperates entries within each passport
  #' @return A list with the field and the following information for all the data:
  #'  - data: the original data (character)
  #'  - value: the sanitized value (numeric/character)
  #'  - valid: if the data was a valid field (boolean)
  #'  - unit: the measurement unit of the value (character)
  entry_data = strsplit(entry, split = ":")[[1]]
  field = entry_data[1]
  data = entry_data[2]
  parsed_data = switch(field, 
                       "byr"=byr,
                       "iyr"=iyr,
                       "eyr"=eyr,
                       "hgt"=hgt,
                       "hcl"=hcl,
                       "ecl"=ecl,
                       "pid"=pid,
                       "cid"=cid)(data)
  return(
    list(field=field,
         data=parsed_data)
  )
}

#### Read and parse data to individual passports ----------
# Read input
inp  = readLines("inp/day4_inp.txt", )

# init passports list
passports = list("")

# iterate over all input lines
for (inp_line in inp) {
  # If empty line start new (empty) passport entry
  if (inp_line == "") {
    passports[[length(passports) + 1]] = ""
  # If not empty concat to current (active) passport
  } else {
    passports[[length(passports)]] = paste(passports[[length(passports)]], inp_line, sep=" ")
  }
}

# trim whitespace within all passports at the beginning and end
passports = lapply(passports, function(x) str_trim(x))

check_Var = c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid')

sum(unlist(lapply(passports, function(x) {
  sum(
    unlist(
      lapply(validator(x), function(y) y$valid)[check_Var]
    )
  )
})) == 7)

#### First Star (test) ------------------
print(
  sum(unlist(lapply(passports, function(x) {
    length(unlist(lapply(validator(x), function(y) y$data)[check_Var])) 
    }
  )) == 7) == 250
)

sum(unlist(lapply(passports, function(x) length(unlist(lapply(validator(x), function(y) y$data)[check_Var])))) == 7)

#### Second Star (test) -----------------
print(
  sum(unlist(lapply(passports, function(x) {
    sum(unlist(lapply(validator(x), function(y) y$valid)[check_Var]))
    }
  )) == 7) == 158
)
