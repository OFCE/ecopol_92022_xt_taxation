library(tidyverse)
library(wid)
library(ggrepel)
library(geomtextpath)
library(ofce)
library(slider)
download.file("https://wid.world/bulk_download/wid_all_data.zip", "data/wid_all_data.zip")
unzip("data/wid_all_data.zip", exdir = "data/wid")
source("codewid.r")
fr.raw <- read_delim("data/wid/WID_data_FR.csv", delim = ";")
de.raw <- read_delim("data/wid/WID_data_DE.csv", delim = ";")
it.raw <- read_delim("data/wid/WID_data_IT.csv", delim = ";")
es.raw <- read_delim("data/wid/WID_data_ES.csv", delim = ";")
nl.raw <- read_delim("data/wid/WID_data_NL.csv", delim = ";")
gb.raw <- read_delim("data/wid/WID_data_GB.csv", delim = ";")
us.raw <- read_delim("data/wid/WID_data_US.csv", delim = ";")

data <- bind_rows(fr.raw, de.raw, it.raw, es.raw, nl.raw, us.raw, gb.raw) |> 
  mutate(
    type = str_sub(variable, 1,1),
    concept = str_sub(variable, 2,6),
    age = str_sub(variable, 7,9),
    pop = str_sub(variable, 10,10)) |> 
  left_join(codetype, by = "type") |> 
  left_join(codeage, by = "age")

deciles <- str_c("p", seq(0,90, 10), "p", seq(10, 100, 10))
picked <-  c("p0p50", "p90p100", "p99p100", "p99.9p100", "p0p90")
top <- c("p99p100")

datar <- data |> 
  filter(percentile %in% picked , concept %in% c("ptinc", "cainc"), type == c("s"), pop == "j", age=="992") |>
  pivot_wider(id_cols = c(country, year, percentile), names_from = concept, values_from = value) |> 
  mutate(dec = floor((year-1)/10)*10,
         polf = case_when(
           year<1970 ~ "autres",
           between(year, 1970, 1974) ~ "gp",
           between(year, 1975, 1981) ~ "vge",
           between(year, 1982, 1986) ~ "fm I",
           between(year, 1987, 1988) ~ "fm cohab I",
           between(year, 1989, 1993) ~ "fm II",
           between(year, 1994, 1995) ~ "fm cohab II",
           between(year, 1996, 1997) ~ "jc I",
           between(year, 1998, 2002) ~ "jc cohab I",
           between(year, 2003, 2007) ~ "jc II",
           between(year, 2008, 2012) ~ "ns",
           between(year, 2013, 2017) ~ "fh",
           year>= 2018 ~ "em"),
         nper = map_dbl(str_split(percentile, "p"), ~as.numeric(.x[[3]]) - as.numeric(.x[[2]]))) |> 
  group_by(country, percentile) |> 
  mutate(
    caincs = slider::slide_dbl(cainc, .f = mean, .before = 1, .after = 1),
    ptincs = slider::slide_dbl(ptinc, .f = mean, .before = 1, .after = 1)) |> 
  group_by(country, dec, percentile) |> 
  mutate(caincm = mean(cainc, na.rm=TRUE),
         ptincm = mean(ptinc, na.rm=TRUE)) |>
  group_by(country, polf, percentile) |> 
  mutate(caincm2 = mean(cainc, na.rm=TRUE),
         ptincm2 = mean(ptinc, na.rm=TRUE)) |>
  group_by(country, year) |> 
  mutate(
    rd50 = cainc/nper/cainc[percentile=="p0p50"]*nper[percentile=="p0p50"],
    rd50s = caincs/caincs[percentile=="p0p50"]/nper*nper[percentile=="p0p50"],
    rd90 = cainc/cainc[percentile=="p0p90"]/nper*nper[percentile=="p0p90"],
    rd90s = caincs/caincs[percentile=="p0p90"]/nper*nper[percentile=="p0p90"],
    rd50m = caincm/caincm[percentile=="p0p50"]/nper*nper[percentile=="p0p50"],
    rd90m = caincm/caincm[percentile=="p0p90"]/nper*nper[percentile=="p0p90"],
    rd50m2 = caincm2/caincm2[percentile=="p0p50"]/nper*nper[percentile=="p0p50"],
    rd90m2 = caincm2/caincm2[percentile=="p0p90"]/nper*nper[percentile=="p0p90"],
    r = cainc-ptinc,
    rr  = (cainc-ptinc)/cainc,
    drtb  = 1-(ptinc/ptinc[percentile=="p0p50"])/(cainc/cainc[percentile=="p0p50"]),
    drtbs  = 1-(ptincs/ptincs[percentile=="p0p50"])/(caincs/caincs[percentile=="p0p50"]),
    drtbs90  = 1-(ptincs/ptincs[percentile=="p0p90"])/(caincs/caincs[percentile=="p0p90"]),
    drtbm  = 1-(ptincm/ptincm[percentile=="p0p50"])/(caincm/caincm[percentile=="p0p50"]),
    drtbm90  = 1-(ptincm/ptincm[percentile=="p0p90"])/(caincm/caincm[percentile=="p0p90"]),
    drtbm2  = 1-(ptincm2/ptincm2[percentile=="p0p50"])/(caincm2/caincm2[percentile=="p0p50"]),
    drtbm290  = 1-(ptincm2/ptincm2[percentile=="p0p90"])/(caincm2/caincm2[percentile=="p0p90"]),
    dtb  = cainc/cainc[percentile=="p0p50"]- ptinc/ptinc[percentile=="p0p50"]) 

datag <- datar |> 
  filter(year %in% c(1980, 1990, 2000, 2010, 2019, 2020, 2021)) |> 
  group_by(country) |> 
  filter(year == max(year[!is.na(r)])|year<2019) |> 
  ungroup()

ggplot(datag |> filter(!percentile%in%c("p0p10", "p10p20")))+geom_point(aes(x=percentile, y=rr*100, col = year))+facet_wrap(vars(country))

ggplot(datar |> filter(year>=1981)) + geom_line(aes(x=year, y=drtb, col=percentile)) + facet_wrap(vars(country))

ggplot(datag |> filter(year>=1980, percentile =="p90p100")) + geom_point(aes(x=rd50, y = drtb, col = country, size=year))

datas <- datag |> 
  group_by(country) |> 
  mutate(
    last = max(year[!is.na(cainc)]), 
    yearc=if_else(year==last, "last", as.character(year))) |> 
  ungroup() |> 
  filter(percentile=="p90p100") |> 
  pivot_wider(id_cols = c(country, last), names_from = yearc, values_from = c(rd50, drtb))

ggplot(datas)+
  geom_segment(aes(x=rd50_1980, y=drtb_1980, xend=rd50_1990, yend=drtb_1990, col = country)) +
  geom_segment(aes(x=rd50_1990, y=drtb_1990, xend=rd50_2000, yend=drtb_2000, col = country, label=country)) +
  geom_segment(aes(x=rd50_2000, y=drtb_2000, xend=rd50_2010, yend=drtb_2010, col = country))+
  geom_segment(aes(x=rd50_2010, y=drtb_2010, xend=rd50_last, yend=drtb_last, col = country),
               arrow = arrow(angle=25,length=unit(9,"pt"),type="closed"))+
  geom_point(aes(x=rd50_1980, y=drtb_1980,col = country), size=4, show.legend=FALSE)+
  geom_text_repel(aes(x=rd50_1980, y=drtb_1980,col = country,label="1980"), size=3, show.legend=FALSE)+
  geom_text_repel(aes(x=rd50_last, y=drtb_last,col = country, label=last), size=3, show.legend=FALSE)+
  scale_x_continuous(label=scales::label_percent(1))+
  scale_y_continuous(label=scales::label_percent(1))+
  xlab("Inégalité de revenu après redistribution s[90-100]/s[0-50]")+
  ylab("Réduction de l'inégalité de revenu par la redistribution") +
  labs(caption = str_c("Source WID, accés en septembre 2022",
                       "Le revenu après redistribution est la variable cainc, horsd redistribution par les dépenses collectives et les transferts en nature,",
                       "Le revenu primaire est la variable ptinc. Ce revenu primaire inclu les revenus de la retraite net des cotisations",
                       "Les catégories sont en part dans le revenu total (s) pour les adultes (992) également partagé entre les couples (j)",
                       "x = scainc992p90p100/scainc992p0p50 ; er = scainc992p90p100/scainc992p0p50-sptinc992p90p100/sptinc992p0p50 ;",
                       "y = er/x", sep="\n")) +
  guides(color = "none")+
  theme_ofce()

datas1 <- datar |> 
  drop_na(rd50) |> 
  filter(year>=1990) |> ungroup()

ggplot(datas1 |> filter(percentile=="p90p100"))+geom_textpath(aes(x=rd50s, y=drtbs, col=country, label=country), size=3, arrow = arrow(angle=25,length=unit(5,"pt"),type="closed"))
ggplot(datas1 |> filter(percentile=="p99p100"))+geom_textpath(aes(x=rd90s, y=drtbs90, col=country, label=country), size=3, arrow = arrow(angle=25,length=unit(5,"pt"),type="closed"))


datas2 <- datar |>
  drop_na(rd50) |>
  filter(year>=1990, percentile == "p90p100") |> 
  group_by(dec,country) |>
  summarize(rapd = first(rd50m), drtb = first(drtbm), .groups = "drop") |> 
  arrange(dec)

ggplot(datas2) +
  geom_textpath(aes(x=rapd, y=drtb, col=country, label=country, hjust = 0.5),
                arrow =arrow(angle=25,length=unit(5,"pt"),type="closed"), size = 3 ) +
  geom_point(data = filter(datas2, dec!="2010"), aes(x=rapd, y=drtb, col=country), size = 2)+
  geom_text_repel(data = ~filter(.x, dec==min(dec)|dec==max(dec)), aes(x=rapd, y=drtb, col=country, label = dec), size=3)

datas3 <- datar |>
  drop_na(rd50) |> 
  filter(percentile == "p90p100") |> 
  group_by(polf,country) |>
  summarize(rapd = first(rd90m2), drtb = first(drtbm2), deb = min(year), fin = max(year), .groups = "drop") |> 
  arrange(deb) |> 
  group_by(country) |> 
  mutate(dlab = case_when(
    polf == "fm I" ~ as.character(deb),
    polf == "em" ~ as.character(fin),
    TRUE ~ as.character(deb))) |> 
  ungroup()

(redist <- ggplot(datas3 |> filter(deb>1980)) +
    geom_textpath(aes(x=rapd, y=drtb, col=country, label=country, hjust = 0.45), size = 3,
                  arrow =arrow(angle=25,length=unit(7,"pt"),type="closed") ) +
    geom_point(data = ~filter(.x, polf!="em"), aes(x=rapd, y=drtb, col=country), size = 1)+
    geom_point(data = ~filter(.x, country=="FR", polf!="em"), aes(x=rapd, y=drtb, col=country), size = 3)+
    geom_text(data = ~filter(.x, country=="FR", polf!="em"), aes(x=rapd, y=drtb, label=str_sub(polf, 1,2)), col="white", size = 2)+
    geom_text_repel(data = ~filter(.x , country =="IT"), 
                    aes(x=rapd, y=drtb, col=country, label = dlab), size = 2, segment.size = 0.2)+
    geom_text_repel(data = ~filter(.x , polf %in% c("em", "fm I"), country != "IT"), 
                    aes(x=rapd, y=drtb, col=country, label = dlab), size = 2, segment.size = 0.2)+
    scale_x_continuous()+
    scale_y_continuous(label=scales::label_percent(1))+
    xlab("Inégalité de revenu après redistribution a[p90p100]/a[p0p90]")+
    ylab("Réduction de l'inégalité de revenu par la redistribution") +
    labs(caption = str_c(
      "Source: World Income Database, accés au site www.wid.org en septembre 2022",
      str_wrap(str_c(
        "Le revenu après redistribution est la variable cainc et n'inclut pas les dépenses collectives et les transferts en nature. ",
        "Le revenu primaire est la variable ptinc. Ce revenu primaire inclu les revenus de la retraite net des cotisations. ",
        "Les catégories sont revenu moyen (a) pour les adultes (992) également partagé entre les couples (j)."), 80),
      str_wrap(str_c(
        "x = acainc992p90p100/acainc992p0p50 ; er = acainc992p90p100/acainc992p0p50-aptinc992p90p100/aptinc992p0p50 ;",
        "y = er/x"), 80), sep="\n")) +
    guides(size = "none") +
    theme_ofce()+
    theme(plot.caption.position = "plot"))

graph2svg(redist, height = 16, width = 16)

datas3 |> filter(deb>1980) |> 
  write_delim(file = "redist.csv", delim=";")

datas32 <- datar |>
  drop_na(rd90) |> 
  filter(percentile == "p99p100") |> 
  group_by(polf,country) |>
  summarize(rapd = first(rd90m2), drtb = first(drtbm290), deb = min(year), fin = max(year), .groups = "drop") |> 
  arrange(deb) |> 
  group_by(country) |> 
  mutate(dlab = case_when(
    polf == "fm I" ~ as.character(deb),
    polf == "em" ~ as.character(fin),
    TRUE ~ as.character(deb))) |> 
  ungroup()

(redist2 <- ggplot(datas32 |> filter(deb>1980)) +
    geom_textpath(aes(x=rapd, y=drtb, col=country, label=country, hjust = 0.45), size = 2,
                  arrow =arrow(angle=25,length=unit(7,"pt"),type="closed") ) +
    geom_point(data = ~filter(.x, polf!="em"), aes(x=rapd, y=drtb, col=country), size = 1)+
    geom_point(data = ~filter(.x, country=="FR", polf!="em"), aes(x=rapd, y=drtb, col=country), size = 3)+
    geom_text(data = ~filter(.x, country=="FR", polf!="em"), aes(x=rapd, y=drtb, label=str_sub(polf, 1,2)), col="white", size = 2)+
    geom_text_repel(data = ~filter(.x , country =="IT"), 
                    aes(x=rapd, y=drtb, col=country, label = dlab), size = 2, segment.size = 0.2)+
    geom_text_repel(data = ~filter(.x , polf %in% c("em", "fm I"), country != "IT"), 
                    aes(x=rapd, y=drtb, col=country, label = dlab), size = 2, segment.size = 0.2)+
    scale_x_continuous()+
    scale_y_continuous(label=scales::label_percent(1))+
    xlab("Inégalité de revenu après redistribution a[p99p100]/a[p0p50]")+
    ylab("Réduction de l'inégalité de revenu par la redistribution") +
    labs(caption = str_wrap(str_c("Source WID, accés au site www.wid.org en septembre 2022",
                                  "Le revenu après redistribution est la variable cainc,",
                                  "hors redistribution par les dépenses collectives et les transferts en nature,",
                                  "Le revenu primaire est la variable ptinc.",
                                  "Ce revenu primaire inclu les revenus de la retraite net des cotisations",
                                  "Les catégories sont en part dans le revenu total (s) pour les adultes (992) également partagé entre les couples (j)",
                                  "x = acainc992p99p100/acainc992p0p50 ; er = acainc992p99p100/acainc992p0p50-aptinc992p99p100/aptinc992p0p50 ;",
                                  "y = er/x", sep="\n"))) +
    guides(size = "none") +
    theme_ofce()+
    theme(plot.caption.position = "plot"))




