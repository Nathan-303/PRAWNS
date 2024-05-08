
refine_prawns <- function(){
filename <- paste0("Data/Historic_PRAWNS/",
                   year,
                   "_",
                   pollutant,
                   "_PRAWN.csv")

# Read the additional data ----------------------------------------------------------------
set_allocator <- read.csv("Data/Historic_stats/YearIMDCensus.csv")

chosen_set <- set_allocator %>% dplyr::filter(Year==year) %>% dplyr::select(Set) %>% as.character()

#read in the set data, this is where any extra data should be added
setdata <- read.csv(paste0("Data/Historic_stats/set",
                           chosen_set,
                           ".csv"))
#Rename ethnic group columns for standardisation, there are three censuses used and each has differnt categories and colnames
#year=2005
if (chosen_set=="A"){
  #trim out excess to save space
 setdata <- setdata %>% dplyr::select(-c(LA.CODE,LA.NAME,GOR.CODE,GOR.NAME,IMD.SCORE,RANK.OF.IMD..where.1.is.most.deprived.))
 longset <- setdata %>% pivot_longer(
   cols=c(Ethnic.Group..All.categories..Ethnic.group..measures..Value,
          Ethnic.Group..White..measures..Value,
          Ethnic.Group..White..British..measures..Value,
          Ethnic.Group..White..Irish..measures..Value,
          Ethnic.Group..White..Other..measures..Value,
          Ethnic.Group..Mixed..measures..Value,
          Ethnic.Group..Mixed..White.and.Black.Caribbean..measures..Value,
          Ethnic.Group..Mixed..White.and.Black.African..measures..Value,
          Ethnic.Group..Mixed..White.and.Asian..measures..Value,
          Ethnic.Group..Mixed..Other..measures..Value,
          Ethnic.Group..Asian.Asian.British..measures..Value,
          Ethnic.Group..Asian.Asian.British..Indian..measures..Value,
          Ethnic.Group..Asian.Asian.British..Pakistani..measures..Value,
          Ethnic.Group..Asian.Asian.British..Bangladeshi..measures..Value,
          Ethnic.Group..Asian.Asian.British..Other..measures..Value,
          Ethnic.Group..Black.Black.British..measures..Value,
          Ethnic.Group..Black.Black.British..Black.Caribbean..measures..Value,
          Ethnic.Group..Black.Black.British..Black.African..measures..Value,
          Ethnic.Group..Black.Black.British..Other..measures..Value,
          Ethnic.Group..Chinese.Other..measures..Value,
          Ethnic.Group..Chinese.Other..Chinese..measures..Value,
          Ethnic.Group..Chinese.Other..Other..measures..Value
),
   names_to = "Ethnic group",
   values_to = "flat_population"
 ) %>%
   #rename the ethnicity categories to tidy them up
   #trim the leading space
   mutate(`Ethnic group`=str_sub(`Ethnic group`,start=15L)) %>%
   #trim the gubbins at the end
   mutate(`Ethnic group`=str_remove(`Ethnic group`,
                                    "..measures..Value")) %>%
   #categorise into the broad groups
   mutate(broad_group=case_when(
     grepl(pattern="White.and.Black.Caribbean|White.and.Black.African|White.and.Asian|Mixed",`Ethnic group`)==1~"Mixed or Multiple",
     grepl(pattern="Asian.Asian.British|Indian|Pakistani|Bangladeshi",`Ethnic group`)==1~"Asian",
     grepl(pattern="Black.Black.British",`Ethnic group`)==1~"Black",
     grepl(pattern="White",`Ethnic group`)==1~"White",
     grepl(pattern="Chinese.Other..Chinese",`Ethnic group`)==1~"Asian",
     grepl(pattern="Chinese.Other..Other",`Ethnic group`)==1~"Other"
   )) %>%

   #trim the redundant prefixes
   mutate(`Ethnic group`=`Ethnic group` %>% str_replace_all(
     c("Chinese.Other..Other"="Other ethnic group",
       "Asian.Asian.British..Other"="Other Asian",
       "Asian.Asian.British.."="",
       "Black.Black.British..Other"="Other Black",
       "Black.Black.British.."="",
       "Chinese.Other.."="",
       "Mixed..Other"="Other mixed or multiple ethnic groups",
       "Mixed.."=""))) %>%
   #remover the defunct other category
   dplyr::filter(`Ethnic group`!="Chinese.Other") %>%
   #Remove the asian summary category as it misses chinese
   dplyr::filter(`Ethnic group`!="Asian.Asian.British")

 recalc_asian <- longset %>% dplyr::filter(broad_group=="Asian") %>%
   pivot_wider(names_from = `Ethnic group`,
               values_from = flat_population) %>%
   mutate(Asian=Indian+Pakistani+Bangladeshi+`Other Asian`+Chinese) %>%
   dplyr::select(-c(Indian,Pakistani,Bangladeshi,`Other Asian`,Chinese)) %>%
   pivot_longer(cols=Asian,
                names_to = "Ethnic group",
                values_to = "flat_population")

 calc_minoritised_white <- longset %>% dplyr::filter(broad_group=="White") %>%
   pivot_wider(names_from = `Ethnic group`,
               values_from = flat_population) %>%
   mutate(`Minoritised white`=White..Irish+White..Other) %>%
   dplyr::select(-c(`White..Irish`,`White..Other`,White,White..British)) %>%
   pivot_longer(cols=`Minoritised white`,
                names_to = "Ethnic group",
                values_to = "flat_population")

 longset <- rbind(longset,recalc_asian) %>%

   rbind(calc_minoritised_white) %>%
   #rename the total column to a standard name
   mutate(`Ethnic group`= case_when(
     grepl(pattern="All.categories..Ethnic.group",`Ethnic group`)==1~"All residents",
     grepl(pattern="Mixed",`Ethnic group`)==1~"Mixed or Multiple Ethnic Groups",
     .default=`Ethnic group`
   )) %>%

            rename("All residents"="All.categories..Ethnic.group",
          "Mixed or Multiple Ethnic Groups"="Mixed")
 #remove setdata as it's served its purpose
 rm(setdata)
}
#year= 2006-2015
if (chosen_set%in%c("B","C","D")){ #trim out excess to save space
  #filter out the unique gubbins thats not standard for each year
  #2006-2008
  if(chosen_set=="B"){
    setdata <- setdata %>% dplyr::select(-c(X.1,X,DateCode,Measurement,Units,IMDrank))
  }
  #2009-2012
  if(chosen_set=="C"){
    setdata <- setdata %>% dplyr::select(-c(X.1,X,LA.CODE,LA.NAME,GOR.CODE,GOR.NAME,IMD.SCORE,RANK.OF.IMD.SCORE..where.1.is.most.deprived.))
  }
  #2013-2015
  if(chosen_set=="D"){
  setdata <- setdata %>% dplyr::select(-c(X.1,X,LSOA.name..2011.,Local.Authority.District.code..2013.,Local.Authority.District.name..2013.,)) %>%
    rename("IMD"="Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.")
  }
  #mutate in the summary categories
  summary_set <- setdata %>% mutate(`Minoritised white`=White..Irish+White..Gypsy.or.Irish.Traveller+White..Other.White,
                                    #regex is being weird, so used this token
                     `Mix_token`=White.and.Black.Caribbean+White.and.Black.African+White.and.Asian+Other.Mixed,
                     Asian=Indian+Pakistani+Bangladeshi+Chinese+Other.Asian,
                     Black=African+Carribean+Other.Black,
                     Other.Ethnic.Group=Arab+Any.other.ethnic.group)

  longset <- summary_set %>% pivot_longer(
    cols=c(`Minoritised white`,
           Mix_token,
           Asian,
           Black,
           Other.Ethnic.Group,
           White..English..Welsh..Scottish.Northern.Irish.or.British,
           White..Irish,
           White..Gypsy.or.Irish.Traveller,
           White..Other.White,
           White.and.Black.Caribbean,
           White.and.Black.African,
           White.and.Asian,
           Other.Mixed,
           Indian,
           Pakistani,
           Bangladeshi,
           Chinese,
           Other.Asian,
           African,
           Carribean,
           Other.Black,
           Arab,
           Any.other.ethnic.group
           ),
    names_to = "Ethnic group",
    values_to = "flat_population"
  ) %>%
    #categorise into the broad groups
    mutate(broad_group=case_when(
      grepl(pattern="Other.Asian|Indian|Pakistani|Bangladeshi|Chinese",`Ethnic group`)==1~"Asian",
      grepl(pattern="White.and.Black.Caribbean|White.and.Black.African|White.and.Asian|Other.Mixed",`Ethnic group`)==1~"Mixed or Multiple",
      grepl(pattern="African",`Ethnic group`)==1~"Black",
      grepl(pattern="Carribean",`Ethnic group`)==1~"Black",
      grepl(pattern="Other.Black",`Ethnic group`)==1~"Black",
      grepl(pattern="Arab|Any.other.ethnic.group",`Ethnic group`)==1~"Other",
      grepl(pattern="White",`Ethnic group`)==1~"White",
      grepl(pattern="White..Irish|White..Gypsy.or.Irish.Traveller|White..Other.White",`Ethnic group`)==1~"Minoritised white"
    )) %>%
    #Contract the broad group names into a more standard form
    #mutate()
    #trim the redundant prefixes
    mutate("Ethnic group"=`Ethnic group` %>% str_replace_all(
      c("Asian.Asian.British.."="",
        "Black.Black.British.."="",
        "Chinese.Other.."="",
        "Mixed.."="",
        "Mix_token"="Mixed or Multiple Ethnic Groups")))
  #remove setdata as it's served its purpose
  rm(setdata)}
if (chosen_set%in%c("E","F")){
  #trim out excess to save space
  #2016
  if(chosen_set=="E"){
    setdata <- setdata %>% dplyr::select(-c(X.1,X,date,geography,LSOA.name..2011.,Local.Authority.District.code..2013.,Local.Authority.District.name..2013.,Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.)) %>%
      rename("IMD"="Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.")
  }
  longset <- setdata %>% pivot_longer(
    # cols=c("Ethnic.group..Total..All.usual.residents",
    #        "Ethnic.group..Asian..Asian.British.or.Asian.Welsh",
    #        "Ethnic.group..Asian..Asian.British.or.Asian.Welsh..Bangladeshi",
    #        "Ethnic.group..Asian..Asian.British.or.Asian.Welsh..Chinese",
    #        "Ethnic.group..Asian..Asian.British.or.Asian.Welsh..Indian",
    #        "Ethnic.group..Asian..Asian.British.or.Asian.Welsh..Pakistani",
    #        "Ethnic.group..Asian..Asian.British.or.Asian.Welsh..Other.Asian",
    #        "Ethnic.group..Black..Black.British..Black.Welsh..Caribbean.or.African",
    #        "Ethnic.group..Black..Black.British..Black.Welsh..Caribbean.or.African..African",
    #        "Ethnic.group..Black..Black.British..Black.Welsh..Caribbean.or.African..Caribbean",
    #        "Ethnic.group..Black..Black.British..Black.Welsh..Caribbean.or.African..Other.Black",
    #        "Ethnic.group..Mixed.or.Multiple.ethnic.groups",
    #        "Ethnic.group..Mixed.or.Multiple.ethnic.groups..White.and.Asian",
    #        "Ethnic.group..Mixed.or.Multiple.ethnic.groups..White.and.Black.African",
    #        "Ethnic.group..Mixed.or.Multiple.ethnic.groups..White.and.Black.Caribbean",
    #        "Ethnic.group..Mixed.or.Multiple.ethnic.groups..Other.Mixed.or.Multiple.ethnic.groups",
    #        "Ethnic.group..White",
    #        "Ethnic.group..White..English..Welsh..Scottish..Northern.Irish.or.British",
    #        "Ethnic.group..White..Irish",
    #        "Ethnic.group..White..Gypsy.or.Irish.Traveller"
    #        [22] "Ethnic.group..White..Roma"
    #        [23] "Ethnic.group..White..Other.White"
    #        [24] "Ethnic.group..Other.ethnic.group"
    #        [25] "Ethnic.group..Other.ethnic.group..Arab"
    #        [26] "Ethnic.group..Other.ethnic.group..Any.other.ethnic.group"   ),
    # names_to = "Ethnic group",
    # values_to = "flat_population"
  ) %>%
    #rename the ethnicity categories to tidy them up
    #trim the leading space
    mutate(`Ethnic group`=str_sub(`Ethnic group`,start=15L)) %>%
    #trim the gubbins at the end
    mutate(`Ethnic group`=str_remove(`Ethnic group`,
                                     "..measures..Value")) %>%
    #categorise into the broad groups
    mutate(broad_group=case_when(
      grepl(pattern="White.and.Black.Caribbean|White.and.Black.African|White.and.Asian|Mixed",`Ethnic group`)==1~"Mixed or Multiple",
      grepl(pattern="Asian.Asian.British|Indian|Pakistani|Bangladeshi",`Ethnic group`)==1~"Asian",
      grepl(pattern="Black.Black.British",`Ethnic group`)==1~"Black",
      grepl(pattern="White",`Ethnic group`)==1~"White",
      grepl(pattern="Chinese.Other..Chinese",`Ethnic group`)==1~"Asian",
      grepl(pattern="Chinese.Other..Other",`Ethnic group`)==1~"Other"
    )) %>%

    #trim the redundant prefixes
    mutate(`Ethnic group`=`Ethnic group` %>% str_replace_all(
      c("Chinese.Other..Other"="Other ethnic group",
        "Asian.Asian.British..Other"="Other Asian",
        "Asian.Asian.British.."="",
        "Black.Black.British..Other"="Other Black",
        "Black.Black.British.."="",
        "Chinese.Other.."="",
        "Mixed..Other"="Other mixed or multiple ethnic groups",
        "Mixed.."=""))) %>%
    #remover the defunct other category
    dplyr::filter(`Ethnic group`!="Chinese.Other") %>%
    #Remove the asian summary category as it misses chinese
    dplyr::filter(`Ethnic group`!="Asian.Asian.British")

  recalc_asian <- longset %>% dplyr::filter(broad_group=="Asian") %>%
    pivot_wider(names_from = `Ethnic group`,
                values_from = flat_population) %>%
    mutate(Asian=Indian+Pakistani+Bangladeshi+`Other Asian`+Chinese) %>%
    dplyr::select(-c(Indian,Pakistani,Bangladeshi,`Other Asian`,Chinese)) %>%
    pivot_longer(cols=Asian,
                 names_to = "Ethnic group",
                 values_to = "flat_population")

  longset <- rbind(longset,recalc_asian)
  #remove setdata as it's served its purpose
  rm(setdata)}
# Combine the pollution means with the additional data --------------------

prawns <- inner_join(read.csv(filename,
                              check.names=FALSE),
                     setdata,by="LSOA")

refined_chunk <- prawns %>% mutate(year=year)
#pivot longer by ethnic group
  #group by ethnic group and IMD

}
