# NOTES ------------------------------------------------------------------------
# https://s3.documentcloud.org/documents/21083819/facebook-dangerous-individuals-and-organizations-list-reproduced-snapshot.pdf
# https://theintercept.com/document/2021/10/12/facebook-praise-support-and-representation-moderation-guidelines-reproduced-snapshot/

# DEEPENDECIES -----------------------------------------------------------------
library(data.table)
library(httr)
library(stringr)
library(tabulizer)

# SCRIPT SETTINGS --------------------------------------------------------------

download_url <- 'https://s3.documentcloud.org/documents/21083819/facebook-dangerous-individuals-and-organizations-list-reproduced-snapshot.pdf'

parse_pdfs <- TRUE

rebuild_db <- TRUE

data_dir <- 'data'

data_file <- file.path(data_dir, 
                       'facebook-dangerous-individuals-and-organizations-list-reproduced-snapshot.pdf')

cache_file <- file.path(data_dir, 
                        'dio_list_raw.rdata')

dump_file <- str_replace(data_file, '\\.pdf', '\\.csv')

toc <- list(
  orgs_terror = 1:24,
  orgs_crime = 25:27,
  orgs_hate = 28:39,
  orgs_militarized_social_movement = 40:62,
  orgs_violent_non_state_actor = 63:64,
  individuals_hate = 65:69,
  individuals_crime = 70:72,
  individuals_terror = 73:100
)

col_counts <- list(
  orgs_terror = 6,
  orgs_crime = 3,
  orgs_hate = 4,
  orgs_militarized_social_movement = 3,
  orgs_violent_non_state_actor = 3,
  individuals_hate = 3,
  individuals_crime = 3,
  individuals_terror = 4
)

col_names_list <- list(
  orgs_terror = c('name', 'category', 'region', 'type', 'affiliated_with', 'designation_sources'),
  orgs_crime = c('name', 'category', 'region'),
  orgs_hate = c('name', 'category', 'region', 'type'),
  orgs_militarized_social_movement = c('name', 'category', 'description'),
  orgs_violent_non_state_actor = c('name', 'category', 'region'),
  individuals_hate = c('name', 'category', 'affiliated_with'),
  individuals_crime = c('name', 'category', 'affiliated_with'),
  individuals_terror = c('name', 'category', 'affiliated_with', 'designation_sources')
)

# INITIALIZATION ROUTINES ------------------------------------------------------

## 1. does data directory exist? -----------------------------------------------
if (!dir.exists(data_dir)) { 
  dir.create(data_dir)
}

## 2. is data file present in the data_dir? ------------------------------------
if (!file.exists(data_file)) {
  GET(download_url, write_disk(data_file), verbose())
}

# MAIN -------------------------------------------------------------------------

## 1. extract tables from pdf --------------------------------------------------
# for some reason this doc is pretty hard to parse correctly
# here is the result of a bit of experimentation
# this manages to get all the tables out without resorting to manual extraction, 
# but most of them are malformed

section_headers <- lapply(toc, min)

if (parse_pdfs) {
  dio_list <- lapply(1:100, function(x) {
    message('extracting tables from page ', x)
    if (x %in% section_headers) {
      tbl <- extract_tables(data_file,
                            pages = x,
                            area = list(c(250, 50, 790, 2500)),
                            guess = FALSE)
    } else {
      tbl <- extract_tables(data_file,
                            pages = x,
                            area = list(c(0, 50, 790, 2500)),
                            guess = FALSE)
    }
    tbl <- tbl[[ 1 ]]
    if (x %in% section_headers) {
      if (nrow(tbl) != 32) {
        # browser()
      }
    } else {
      if (nrow(tbl) != 45) {
        # browser()
      }
    }
    tbl
  })
  # save to cache
  save('dio_list', file = cache_file)
} else {
  if (!file.exists(cache_file)) {
    stop('cache file does not exist yet, set parse_pdfs = TRUE and rebuild_db = TRUE to rebuild the cache')
  }
  # load from cache
  load(cache_file, envir = .GlobalEnv)  
}

## 2. clean up and combine extracted tables ------------------------------------

if (rebuild_db) {
  
  ### 1. clean extracted tables ------------------------------------------------
  dio_list_cleaned <- lapply(1:100, function(x) {
    message('cleaning page ', x)
    df <- dio_list[[ x ]]
    has_cols <- ncol(df)
    section_name <- names(toc)[ which(sapply(toc, function(y) x %in% y)) ]
    needs_cols <- col_counts[[ section_name ]]
    if (needs_cols == has_cols) {
      # even some extracted tables with correct number of columns need additional cleaning
      if (x %in% c(9, 16, 18, 21, 22, 73)) {
        # fix malformed orgs_terror
        df[ , 2 ] <- 'Terror'
        df[ , 1 ] <- str_replace_all(df[ , 1 ], 'Terror$', '')
      } else if (x %in% c(31, 36, 37, 38)) {
        # fix malformed orgs_hate
        df[ , 3 ] <- df[ , 2 ]
        df[ , 2 ] <- 'Hate'
        df[ , 1 ] <- str_replace_all(df[ , 1 ], 'Hate$', '')
      } else if (x %in% c(8, 15, 17)) {
        # fix malformed orgs_hate
        df[ , 5 ] <- df[ , 4 ]
        df[ , 4 ] <- 'Media Wing'
        df[ , 3 ] <- str_replace_all(df[ , 3 ], 'Media Wing$', '')
      }
      df <- as.data.frame(df)
      df$page <- x
      return(df)
    } else {
      if (x %in% c(2, 10, 13, 19)) {
        # fix malformed orgs_terror
        df[ , 2 ] <- 'Terror'
        df[ , 1 ] <- str_replace_all(df[ , 1 ], 'Terror$', '')
        idx <- which(str_detect(df[ , 5 ], 'SDGT$'))
        df <- cbind(df, '')
        df[ idx, 6 ] <- 'SDGT'
        df[ idx, 5 ] <- str_replace_all(df[ idx, 5 ], 'SDGT$', '')
      } else if (x %in% c(3:6)) {
        # fix malformed orgs_terror
        df <- cbind(df, '')
      } else if (x %in% c(28)) {
        # fix malformed orgs_hate
        df <- df[ , -4 ]
      } else if (x %in% c(32, 35)) {
        # fix malformed orgs_hate
        df[ , 2 ] <- 'Hate'
        df[ , 1 ] <- str_replace_all(df[ , 1 ], 'Hate$', '')
        df <- cbind(df, df[ , 3])
        df[ , 3 ] <- str_replace_all(df[ , 3 ], 'Music Band$', '')
        df[ , 4 ] <- str_extract_all(df[ , 4 ], 'Music Band$', simplify = TRUE)
      } else if (x %in% c(74, 84, 86, 87, 91, 93, 96, 97, 98)) {
        # fix malformed individuals_terror
        idx <- which(str_detect(df[ , 3 ], 'SDGT$'))
        df <- cbind(df, '')
        df[ idx, 4 ] <- 'SDGT'
        df[ idx, 3 ] <- str_replace_all(df[ idx, 3 ], 'SDGT$', '')
      } 
    }
    df <- as.data.frame(df)
    df$page <- x
    return(df)
  })
  
  ### 2. combine cleaned tables ------------------------------------------------
  
  dio <- lapply(names(toc), function(x) {
    message('combining ', x)
    idx <- toc[[ x ]]
    df <- dio_list_cleaned[ idx ] |> 
      rbindlist() |> 
      as.data.frame()
    types <- str_split(x, '_') |> unlist()
    cn <- col_names_list[[ x ]]
    cn <- c(cn, 'page')
    colnames(df) <- cn
    df$type_1 <- types[ 1 ]
    df$type_2 <- types[ 2 ]
    df <- df[ -1, ]
    df
  }) 
  names(dio) <- names(toc)
  
  # debugging
  # lapply(names(dio), function(x) View(dio[[ x ]], title = x))
  
  dio_df <- rbindlist(dio, use.names = TRUE, fill = TRUE) |> 
    as.data.frame()
  idx <- order(dio_df$page)
  dio_df <- dio_df[ idx, ]
  rownames(dio_df) <- NULL
  
  # clean up very wide names that extracted incorrectly by tabulizer
  # browser()
  # View(dio_df[ which(sapply(dio_df$name, nchar) > 55), ])
  idx <- which(unlist(lapply(dio_df$name, nchar) > 55))
  dio_df[ idx, 'name' ] <- c('Asyaf International Holding Group for Trading and Investment',
                             'Democratic Front for the Liberation of Palestine - Hawatmeh',
                             'Islamic State of Iraq and the Levant - Caucasus Province',
                             'Joint Partnership of Mohammadreza Khedmati and Associates',
                             'Joint Operation Room for Palestinian Resistance Factions',
                             'Mirage for Waste Management and Environmental Services',
                             'Popular Front for the Liberation of Palestine-General Command',
                             'RA Havacilik Lojistik Ve Tasimacilik Ticaret Limited Sirketi',
                             'The Central Military Media Bureau of the Islamic Resistance',
                             'The Riyadus-Salikhun Reconnaissance and Sabotage Battalion of Chechen Martyrs',
                             'Welfare and Development Organization of Jamaat-ud-Dawah for Qur\'an and Sunnah',
                             'Westside / Eastside - Not inherently a criminal org (do not mark without other signals)',
                             'Aktionsfront Nationaler Sozialisten-Nationale Aktivisten',
                             'Alternative Nationale Strausberger Dart-Piercing und Tattoo-Offensive',
                             'Sozialistische Reichspartei Reichsfront Reichsjugend SRP-Faruenbund',
                             'UK Krusaders of the Traditionalist American Knights of the Ku Klux Klan',
                             'Verein zur Rehabiliteirung der wegen Bestreitens des Holocaust Verfolgten',
                             'Am. Vets Constitutionalists Militia in Serving Country and Christ',
                             'Boogaloo movement (Outside of the RTO designated subset)',
                             'Mississippi SouthEast Militia & Doomsday Prepping Organization',
                             'Texas 2A Support/Response Team - Riot/Looting Prevention',
                             'Higher Military Majisul Shura of the United Mujahideen of the Caucuses',
                             'Abd al-Rahman Ould Muhammad al-Husayn Ould Muhammad Salim')
  dio_df[ idx, 'category' ] <- c('Terror',
                                 'Terror',
                                 'Terror',
                                 'Terror',
                                 'Terror',
                                 'Terror',
                                 'Terror',
                                 'Terror',
                                 'Terror',
                                 'Terror',
                                 'Terror',
                                 'Crime',
                                 'Hate',
                                 'Hate',
                                 'Hate',
                                 'Hate',
                                 'Hate',
                                 'Militarized Social Movement',
                                 'Militarized Social Movement',
                                 'Militarized Social Movement',
                                 'Militarized Social Movement',
                                 'Violent Non-State Actor',
                                 'Terror')
  
  # clean up malformed regions
  idx <- which(str_detect(dio_df$region, 'ArabMicedia Wing'))
  dio_df[ idx, 'region' ] <- 'Middle East, North Africa, Arabic'
  
  idx <- which(dio_df$name == 'Black Panther Militia')
  dio_df[ idx, 'description' ] <- 'Please note this only covers the militia outfit calling itself "black panther militia" vs the larger Black Panther group'
  idx <- which(dio_df$name == '')
  dio_df <- dio_df[ -idx, ]
  
  dio_df$name <- str_squish(dio_df$name)
  
  # save to cache
  save(list = c('dio_df', 'dio_list', 'dio'), file = cache_file)
} else {
  # load from cache
  if (!file.exists(cache_file)) {
    stop('cache file does not exist yet, set parse_pdfs = TRUE and rebuild_db = TRUE to rebuild the cache')
  }
  load(cache_file, envir = .GlobalEnv)  
}

## 3. dump dio list to csv -----------------------------------------------------

write.csv(dio_df, file = dump_file, quote = TRUE, row.names = FALSE, col.names = TRUE)

## 4. exploratory analysis -----------------------------------------------------

# here are the variable names
colnames(dio_df)

# make sure our counts of category reconcile
tapply(dio_df$category, paste0(dio_df$type_1, '_', dio_df$type_2), length)
sapply(dio, nrow)

# what are all possible regions?
dio_df$region |> str_split(',') |> lapply(str_squish) |> unlist() |> setdiff('') |> sort()

# what are possible types?
table(dio_df$type, useNA = 'ifany')

# what are possible affiliations?
dio_df$affiliated_with |> str_split(',') |> unlist() |> str_squish() |> setdiff('') |> sort()

# are all affiliates listed in the dio?
affiliates <- dio_df$affiliated_with |> str_split(',') |> unlist() |> str_squish() |> setdiff('') |> sort()
dio_names <- unique(dio_df$name) |> sort()
all(affiliates %in% dio_names)
# affiliates that are unlisted (or mispelled/malformed) in the dio
affiliates[ which(toupper(affiliates) %in% toupper(dio_names) == FALSE) ]

# who are most connected?
dio_df$affiliated_with |> str_split(',') |> unlist() |> str_squish() |> table() |> sort()

# designation sources
dio_df$designation_sources |> unique() |> sort()

# descriptions seem only to be associated with militarized social movements
dio_df$description |> unique()

