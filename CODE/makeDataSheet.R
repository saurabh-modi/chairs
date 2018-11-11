rm(list = ls())
library(readxl)
library(reshape2)
library(Hmisc)
library(plyr)

LEGEND <- read_excel("metadata.xlsx", sheet = 1)

all.days <- do.call(rbind,lapply(unique(c(2:8)), function(x){
    DAYS <- read_excel("metadata.xlsx", sheet = x)
    if(ncol(DAYS) == 6){
        DAYS <-  cbind(DAYS,  X__6 = "")
    } else {
        DAYS
    }
    DAYS$DATE <- rep(names(DAYS[1])) 
    names(DAYS)[1] <- "MULTIPLEXES"
    DAYS
}))

# write.csv(all.days, file = "daySchedule.csv", row.names = FALSE)

daily <- read.csv("daySchedule.csv", header = TRUE)

daily$MULTIPLEXES[grepl("^OCTOBER|NOVEMBER", daily$MULTIPLEXES)] <- NA
names(daily) <- as.character(unlist(daily[1,]))
colnames(daily) <- c("MULTIPLEX",
                     "10AM",
                     "12PM",
                     "2PM",
                     "5PM",
                     "8PM",
                     "8PM",
                     "DATE")

org.data <- melt(daily, id.vars = c("MULTIPLEX", "DATE"))
colnames(org.data) <- c("MULTIPLEX", "DATE", "TIMESLOT", "MOVIE")

cats <- LEGEND$ABBREVIATION
cats <- strsplit(cats, "\n")
cats <- unlist(cats)
add <- c("conversation", "OPENING FILM", "Mom")
cats <- c(cats, add)
cats <- paste0(cats, collapse="|")
   
org.data$tag <- grepl(pattern = cats, org.data$MOVIE)
#write.csv(org.data, file = "org.data.csv", row.names = FALSE)

not.want <- c("10AM", "12PM", "2PM", "5PM", "8PM")
org.data <- org.data[!org.data$MOVIE %in% not.want,]

org.data$tag1 <- Lag(org.data$MOVIE, 1)
head(org.data)

movies <- subset(org.data, tag == TRUE)
# not.want <- c("10AM", "12PM", "2PM", "5PM", "8PM")
details <- org.data[!org.data$MOVIE %in% movies,]
## details <- details[!with(details,
##                          is.na(MULTIPLEX) &
##                          is.na(MOVIE)),]

details$MOVIE1 <- paste(details$MOVIE, sep = "_",  details$tag1)
details <- subset(details, tag == FALSE)

#details$MOVIE <- as.character(details$MOVIE)
#details <- details[!is.na(details$MOVIE),]
# details$tag <- TRUE

names(details)[names(details) == 'MOVIE'] <- 'TIMINGS'
details$tag1 <- Lag(details$tag1, -1)

# details$MOVIE <- NULL
# details$tag <- NULL
# movies$MULTIPLEX <- NULL
# movies$tag <- NULL
# details$MOVIE1 <- NULL

names(movies)[names(movies) == 'tag1'] <- 'TIMINGS'
names(details)[names(details) == 'tag1'] <- 'ADDITIONAL'

all.films <- merge(details, movies, by = c("DATE",
                                           "TIMESLOT",
                                           "TIMINGS"),
                   all = TRUE)

all.films <- all.films[!with(all.films,is.na(TIMINGS) &
                                       is.na(MULTIPLEX) &
                                       is.na(ADDITIONAL)),]

all.films$MOVIE <- ifelse(is.na(all.films$MOVIE),
                          "No screening",
                          all.films$MOVIE)

all.films$ADDITIONAL <- NULL
# write.csv(all.films, file = "all.films.csv", row.names = FALSE)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

all.films$categories <- substrRight(all.films$MOVIE, 2)
#write.csv(all, file = "all.csv", row.names = FALSE)


all.films$categories <- revalue(all.films$categories,
                                c("I\n" = "DI",
                                  "DV" = "RDV",
                                  "ma" = "TNM",
                                  "D)" = "TR (IND)",
                                  "AO" = "S",
                                  "C\n" = "WC",
                                  "ng" = "No screening",
                                  "AY" = "PL",
                                  "LM" = "OF",
                                  "RA" = "S",
                                  "IM" = "S",
                                  "ra" = "S",
                                  "a\n" = "TNM",
                                  "on" = "SF",
                                  "R)" = "TR (INTER)",
                                  "TR" = "TR (INTER)",
                                  "0+" = "HT",
                                  "1+" = "HT",
                                  "3+" = "HT",
                                  "5+" = "HT",
                                  "8+" = "HT"))


LEGEND <- subset(LEGEND, select = c("PROGRAMME", "ABBREVIATION"))
LEGEND <- as.data.frame(LEGEND)
prog <- strsplit(LEGEND$PROGRAMME, "\n")
prog <- unlist(prog)
categories <- strsplit(LEGEND$ABBREVIATION, "\n")
categories <- unlist(categories)
key <- as.data.frame(cbind(prog, categories))

key$categories <- as.character(key$categories)

final.data <- merge(all.films, key, by = "categories", all = TRUE)
head(final.data)
colnames(final.data) <- c("TAG",
                          "DATE",
                          "TIMESLOT",
                          "TIMINGS",
                          "MULTIPLEX",
                          "MOVIE",
                          "CATEGORY")


final.data$DATE <- as.character(final.data$DATE)

dates <- strsplit(final.data$DATE, "|", fixed = TRUE)
dates <- unlist(dates)

dates <- revalue(dates,
                 c("OCTOBER 26 " = "26-10-2018",
                   "OCTOBER 27 " = "27-10-2018",
                   "OCTOBER 28 " = "28-10-2018",
                   "OCTOBER 29 " = "29-10-2018",
                   "OCTOBER 30 " = "30-10-2018",
                   "OCTOBER 31 " = "31-10-2018",
                   "NOVEMBER 1 " = "01-11-2018"))

dts <- c("26-10-2018",
         "27-10-2018",
         "28-10-2018",
         "29-10-2018",
         "30-10-2018",
         "31-10-2018",
         "01-11-2018")

dates <- dates[dates %in% dts]
final.data$DATE <- dates
final.data$DATE <- as.Date(final.data$DATE, "%d-%m-%Y")
final.data <- final.data[order(final.data$DATE, final.data$TIMESLOT),]
final.data$DAY <- weekdays(as.POSIXct(final.data$DATE))
final.data$DATE <- format(final.data$DATE, "%d-%B-%Y")

final.data <- subset(final.data, select = c("DATE",
                                            "DAY",
                                            "TIMESLOT",
                                            "TIMINGS",
                                            "MULTIPLEX",
                                            "MOVIE",
                                            "CATEGORY",
                                            "TAG"))

# write.csv(final.data, file = "final.data.csv", row.names = FALSE)
##################################################################

 # Manual intervention for 8 entries, in sub-row in pink and blue for days 29 and 30 Oct.

final.data <- read.csv("final.data.csv", header = TRUE)

 # Adding Multiplex geog location

final.data$LOCATION <- ifelse(grepl("KURLA",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "KURLA", 
                       ifelse(grepl("MULUND",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "MULUND",
                       ifelse(grepl("MULUND",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "MULUND",
                       ifelse(grepl("ECX",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "ANDHERI (W)",
                       ifelse(grepl("ICON",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "VERSOVA",
                       ifelse(grepl("SPI",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "BANDRA",
                       ifelse(grepl("JUHU",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "JUHU",
                       ifelse(grepl("REGAL",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "COLABA",
                       ifelse(grepl("MATTERDAN",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "LOWER PAREL",
                       ifelse(grepl("LIBERTY",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "MARINE LINES",
                       ifelse(grepl("MATTERDEN",
                                    final.data$MULTIPLEX,
                                    ignore.case = T), "LOWER PAREL",
                              "REMOVE")))))))))))

final.data <- subset(final.data, LOCATION != "REMOVE")

mami.data <- final.data
# write.csv(mami.data, "mami.data.csv", row.names = FALSE)
#####################################################################
