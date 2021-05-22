
library(dplyr)
library(sf)
library(spikemap)

# Get covid-19 district data
dat <- read.csv("districts.csv")

# India shapefile
com <- st_read("../maps-master/Districts/Census_2011/2011_Dist.shp")
colnames(com)[1] <- "District"

###################
## Big cleanups! ##
###################
Telangana_dist <- c("Adilabad",
                    "Hyderabad",
                    "Karimnagar",
                    "Khammam",
                    "Mahbubnagar",
                    "Medak",
                    "Nalgonda",
                    "Nizamabad",
                    "Rangareddy",
                    "Warangal")

Manipur_dist <- c("Bishnupur",
                  "Chandel",
                  "Churachandpur",
                  "Imphal East",
                  "Imphal West",
                  "Senapati",
                  "Tamenglong",
                  "Thoubal",
                  "Ukhrul")

Assam_dist <- c("Baksa",
                "Barpeta",
                "Bongaigaon",
                "Cachar",
                "Chirang",
                "Darrang",
                "Dhemaji",
                "Dhubri",
                "Dibrugarh",
                "Dima Hasao",
                "Goalpara",
                "Golaghat",
                "Hailakandi",
                "Jorhat",
                "Kamrup",
                "Kamrup Metropolitan",
                "Karbi Anglong",
                "Karimganj",
                "Kokrajhar",
                "Lakhimpur",
                "Marigaon",
                "Nagaon",
                "Nalbari",
                "Sivasagar",
                "Sonitpur",
                "Tinsukia",
                "Udalguri")

# Palette
com$cols <- ifelse(com$District %in%
                   c(Telangana_dist,
                     Manipur_dist,
                     Assam_dist),
                   "gray99",
                   "cornsilk2")


com$District <- ifelse(com$ST_NM == "Sikkim" & com$District == "North",
                       "North Sikkim", as.character(com$District))

dat$District <- ifelse(dat$State == "Sikkim" & dat$District == "Unknown",
       "North Sikkim", as.character(dat$District))

dat$District <- ifelse(dat$State == "Goa" & dat$District == "Unknown",
       "North Goa", as.character(dat$District))

dat$District <- ifelse(dat$State == "Puducherry" &
                       dat$District == "Pondicherry",
       "Puducherry", as.character(dat$District))

dat$District <- ifelse(dat$State == "Andaman and Nicobar Islands" &
                       dat$District == "Unknown",
       "North & Middle Andaman", as.character(dat$District))

dat$District <- recode(dat$District,
                       "Gurugram" = "Gurgaon",
                       "Dadara & Nagar Havelli" = "Dadra & Nagar Haveli",
                       "Budgam" = "Badgam",
                       "Haridwar" = "Hardwar",
                       "Pauri Garhwal" = "Garhwal",
                       "Bandipora" = "Bandipore",
                       "Shopiyan" = "Shupiyan",
                       "Khargone" = "West Nimar",
                       "Khandwa" = "East Nimar",
                       "Lawngtlai" = "Lawangtlai",
                       "East Singhbhum" = "Purbi Singhbhum",
                       "West Singhbhum" = "Pashchimi Singhbhum",
                       "Koderma" = "Kodarma",
                       "Saraikela-Kharsawan" = "Saraikela-kharsawan",
                       "Mewat" = "Nuh",
                       "Baramulla" = "Baramula",
                       "Ribhoi" = "Ri Bhoi",
                       "East Jaintia Hills" = "Jaintia Hills",
                       "Ferozepur" = "Firozpur",
                       "Narsinghpur" = "Narsimhapur",
                       "Sri Muktsar Sahib" = "Muktsar",
                       "S.A.S. Nagar" = "Sahibzada Ajit Singh Nagar",
                       "Lahaul and Spiti" = "Lahul & Spiti",                  
                       "Buldhana" = "Buldana",
                       "Janjgir Champa" = "Janjgir-champa",
                       "Upper Dibang Valley" = "Dibang Valley",
                       "Mumbai" = "Greater Bombay",
                       "Beed" = "Bid",
                       "Raigad" = "Raigarh",
                       "Ahmednagar" = "Ahmadnagar",
                       "Kanyakumari" = "Kanniyakumari",
                       "Nilgiris" = "Nilgiri",
                       "Darjeeling" = "Darjiling", 
                       "Bengaluru Urban" = "Bangalore",
                       "Bengaluru Rural" = "Bangalore Rural",
                       "Puducherry" = "Pondicherry",
                       "Leh"="Leh (ladakh)",
                       "Prayagraj" = "Allahabad",
                       "Amroha" = "Jyotiba Phule Nagar",
                       "Lakhimpur Kheri" = "Kheri",
                       "Barabanki" = "Bara Banki",
                       "Siddharthnagar" = "Siddharth Nagar",
                       "Bhadohi" = "Sant Ravi Das Nagar(bhadohi)",
                       "Ayodhya" = "Faizabad",
                       "Kasganj" = "Kansiram Nagar",
                       "Hathras" = "Mahamaya Nagar",
                       "Delhi" = "New Delhi",
                       "Cooch Behar" = "Koch Bihar",
                       "Howrah" = "Haora",
                       "Hooghly" = "Hugli",
                       "Paschim Medinipur" = "Pashchim Medinipur",
                       "Purulia" = "Puruliya",
                       "Malda" = "Maldah",
                       "Paschim Bardhaman" = "Barddhaman",
                       "Bagalkote" = "Bagalkot",
                       "Ballari" = "Bellary",
                       "Shivamogga" = "Shimoga",
                       "Tumakuru" = "Tumkur",
                       "Chikkamagaluru" = "Chikmagalur",
                       "Belagavi" = "Belgaum",
                       "Chamarajanagara" = "Chamrajnagar",
                       "Mysuru" = "Mysore",
                       "Kalaburagi" = "Gulbarga",
                       "Vijayapura" = "Bijapur",
                       "S.P.S. Nellore" = "Sri Potti Sriramulu Nellore",
                       "Y.S.R. Kadapa" = "Y.s.r.",
                       "Kanyakumari" = "Kanniyakumari",
                       "Nagapattinam" = "Nagappattinam",
                       "Nilgiri" = "The Nilgiris",
                       "Virudhunagar" = "Virudunagar",
                       "Chittorgarh" = "Chittaurgarh",
                       "Dholpur" = "Dhaulpur",
                       "Jalore" = "Jalor",
                       "Jhunjhunu" = "Jhunjhunun",
                       "Ahmedabad" = "Ahmadabad",
                       "Kutch" = "Kachchh",
                       "Dahod" = "Dohad",
                       "Mehsana" = "Mahesana",
                       "Panchmahal" = "Panch Mahals",
                       "Dang" = "The Dangs",
                       "Banaskantha" = "Banas Kantha",
                       "Panchmahal" = "Panch Mahals",
                       "Sabarkantha" = "Sabar Kantha",
                       "Kaimur" = "Kaimur (bhabua)",
                       "East Champaran" = "Purba Champaran",
                       "West Champaran" = "Pashchim Champaran",
                       "Saran" = "Saran (chhapra)",
                       "Angul" = "Anugul",
                       "Boudh" = "Bauda",
                       "Jagatsinghpur" = "Jagatsinghapur",
                       "Jajpur" = "Jajapur",
                       "Deogarh" = "Debagarh",
                       "Balasore" = "Baleshwar")


#  Fortnight dates
Dates <- as.character(seq(as.Date("2020-04-10"),
                          as.Date("2021-05-20"), by = 15))

# Subset
dat$Date <- as.character(dat$Date)
dat <- subset(dat, Date %in% Dates)           

# Create and save a series of plots
allplot <- lapply(unique(dat$Date), function(t){
       
    dat_1 <- subset(dat, Date == t)   
    sr <- merge(com, dat_1, by = "District", all = TRUE)
    
    png(file = paste0("./output/","map_",  t, '.png'),
        width = 965, height = 925, res = 150)

    par(mar=c(1,0,2,0))

    plot(st_geometry(com),
         col = com$cols,
         border = "grey65",
         lwd = 0.2,
         main = "Got so bad, so fast",
         cex.main = 1.5,
         family = "Garamond",
         adj = 0.1)
    
    t <- format(as.Date(t), '%B %d, %Y')

    text(88, 32,
         labels = t,
         cex = 1.4,
         family = "Garamond")

    text(88, 30,
         labels = format(sum(dat_1$Deceased), big.mark = ","),
         cex = 1.6,
         col = "darkred",
         family = "Garamond")

    text(88, 34,
         labels = "Lives lost to covid-19",
         cex = 1.2,
         col = "Darkred",
         family = "Garamond")
    
    text(81, 6,
         labels = "#30DayMapChallenge | Day 20 | Surbhi Bhatia | Data: covid19india.org",
         cex = 0.6,
         col = "black",
         family = "Garamond")
    
     text(89, 7.6,
         labels = "District-wise breakup not available for\nTelangana, Assam and Manipur.",
         cex = 0.7,
         col = "black",
         family = "Garamond")
    
    spikemap(x = sr,
             var = "Deceased",
             inches = 0.4,
             width = 0.02,
             fixmax = 5000,
             col = "darkred",
             border = "darkred",
             lwd = 1.1,
             legend.pos = c(0,0))

    dev.off()
})


# Post process into GIf
############## FIN ######################
