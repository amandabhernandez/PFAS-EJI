naics_1 <- data.frame(naics = c( "313320 Fabric Coating Mills bcdefghik 380",
                               "325510 Paint and Coating Manufacturing abcdefhik 2100",
                               "322220 Paper Bag and Coated and Treated Paper Manufacturing bcdefghi 0",
                               "313210 Broadwoven Fabric Mills bcdefhk 484",
                               "322121 Paper (except Newsprint) Mills bcdefhk 610",
                               "332813 Electroplating, Plating, Polishing, Anodizing, and Coloring bcdefhi 5642",
                               "324110 Petroleum Refineries abcdehk 594",
                               "325612 Polish and Other Sanitation Good Manufacturing abdefhk 673",
                               "334413 Semiconductor and Related Device Manufacturing bcdefi 1552",
                               "326113 Unlaminated Plastics Film and Sheet (except Packaging) Manufacturing bdefhi 597",
                               "332812 Metal Coating, Engraving (except Jewelry and Silverware), and Allied Services to Manufacturers bcefgi 3587",
                               "333318 Other Commercial and Service Industry Machinery Manufacturing bdefgh 0",
                               "334419 Other Electronic Component Manufacturing bcdefg 1663",
                               "562212 Solid Waste Landfill cdefjk 4765",
                               "325199 All Other Basic Organic Chemical Manufacturing abdefi 1847",
                               "323111 Commercial Printing (except Screen and Books) bcdhk 4226",
                               "313110 Fiber, Yarn, and Thread Mills bcefh 0",
                               "314110 Carpet and Rug Mills bdhik 189",
                               "316110 Leather and Hide Tanning and Finishing bdefg 276",
                               "325211 Plastics Material and Resin Manufacturing bdgik 2141",
                               "324191 Petroleum Lubricating Oil and Grease Manufacturing abcdh 919",
                               "325998 All Other Miscellaneous Chemical Product and Preparation Manufacturing aefgk 2598",
                               "562211 Hazardous Waste Treatment and Disposal acefj 1357",
                               "562213 Solid Waste Combustors and Incinerators acefj 407",
                               "313310 Textile and Fabric Finishing Mills bcdh 0",
                               "322219 Other Paperboard Container Manufacturing bcdh 0",
                               "323120 Support Activities for Printing bcdh 0",
                               "313220 Narrow Fabric Mills and Schiffli Machine Embroidery cefk 0",
                               "313230 Nonwoven Fabric Mills bchk 239",
                               "322130 Paperboard Mills cefk 258")
)


naics_2 <- data.frame(naics = c("332999 All Other Miscellaneous Fabricated Metal Product Manufacturing bdgh 4799 ",
                        "424690 Other Chemical and Allied Products Merchant Wholesalers bdhj 1698",
                        "314910 Textile Bag and Canvas Mills befi 0",
                        "326112 Plastics Packaging Film and Sheet (including Laminated) Manufacturing defi 350",
                        "335999 All Other Miscellaneous Electrical Equipment and Component Manufacturing befg 877",
                        "562112 Hazardous Waste Collection cefj 2561",
                        "562219 Other Nonhazardous Waste Treatment and Disposal cefj 744",
                        "325611 Soap and Other Detergent Manufacturing abeh 1012")
)



frs_facilities <- read_csv("PFAS Point Source Data/NATIONAL_SINGLE.CSV")

frs_facilities_test <- head(frs_facilities, 150)

frs_facilities_accurateGEO <- frs_facilities %>% 
  filter(ACCURACY_VALUE < 1000) %>% 
  filter(!is.na(LATITUDE83) & !is.na(LONGITUDE83))

frs_facilities_pfas_naics <- frs_facilities_subset %>% 
  separate_rows(NAICS_CODES, sep = ", ") %>% 
  filter(NAICS_CODES %in% c(naics_salvatore$naics_code)) %>% 
  separate_rows(PGM_SYS_ACRNMS, sep = ", ") %>% 
  mutate(PGM_SYS_ACRNMS = str_extract(PGM_SYS_ACRNMS, "^.*(?=:)"))


write_csv(frs_facilities_pfas_naics, "PFAS Point Source Data/FRS_facilities_pfasNAICSonly.csv")


length(unique(frs_facilities_pfas_naics$REGISTRY_ID))

table(frs_facilities_pfas_naics$PGM_SYS_ACRNMS)

frs_facilities_pfas_naics %>% 
  group_by(NAICS_CODES) %>% 
  summarize(n_frsid = length(unique(REGISTRY_ID))) %>% 
  filter(NAICS_CODES %in% c(naics_salvatore$naics_code)) %>% 
  full_join(naics_salvatore, by = c("NAICS_CODES" = "naics_code")) %>% 
  mutate(diff = (as.numeric(n_facilities)-n_frsid)) %>%
  View()


table(frs_facilities_pfas_naics$PGM_SYS_ACRNMS, frs_facilities_pfas_naics$REGISTRY_ID %in% c(tri_facilities$`3. FRS ID`))


write_csv(frs_facilities_subset, "PFAS Point Source Data/FRS_facilities_accurateGEO.csv")

frs_facilities_test <- head(frs_facilities_subset, 1500)


tri_facilities <- read_csv("PFAS Point Source Data/2021_us.csv")

frs_notTRI <- frs_facilities_pfas_naics %>% 
  filter(!REGISTRY_ID %in% c(tri_facilities$`3. FRS ID`)) %>% 
  select(-PGM_SYS_ACRNMS, -NAICS_CODES) %>% 
  distinct()

write_csv(frs_notTRI, "PFAS Point Source Data/FRS_facilities_notin_TRI.csv")
