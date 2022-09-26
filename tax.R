library(tidyverse)
library(readr)
library(glue)
library(lubridate)
library(conflicted)
library(rsdmx)
library(ofce)
source("oecd_structure.r")

conflict_prefer_all("tidyverse")
conflict_prefer("filter", "dplyr")

eu <- countrycode::countrycode(eurostat::eu_countries$code, "eurostat", "iso3c") |> sort()
eu <- setdiff(eu, "GBR") # brexit means brexit
ea <- countrycode::countrycode(eurostat::ea_countries$code, "eurostat", "iso3c") |> sort()
others <-  c("USA", "CAN", "JPN", "NZL", "GBR", "NOR", "CHE", "KOR")
oecd_a <- OECD::get_dataset(dataset="EO111_INTERNET", filter=".GDPVD.A", pre_formatted = TRUE) |> 
  transmute(Time, gdp = as.numeric(ObsValue)/1e+9, COU = LOCATION)
taxes <- c("1100","1110","1120", "1200", "2000", "3000", "4000", "5000", "5111", "6000")
taxes_rq <- str_c(taxes,collapse="+")
tax.raw <- OECD::get_dataset("RS_GBL", glue(".NES.{taxes_rq}.TAXGDP")) |> 
  mutate(ObsValue = as.numeric(ObsValue))
tax.raw <- oecd_labelize(tax.raw, "RS_GBL")
fratax.raw <- OECD::get_dataset("REVFRA", glue(".NES"))
fratax.raw <- oecd_labelize(fratax.raw, "REVFRA")
csg <- fratax.raw |> 
  filter(TAX == "FRA_1110_L4") |>
  transmute(COU = "FRA", Time, csg = as.numeric(ObsValue)/1000) |> 
  left_join(oecd_a, by=c("COU", "Time")) |> 
  mutate(csg = csg/gdp*100)

taxgdp <- tax.raw |> 
  select(COU, COU_label.fr, Time, GOV, VAR, TAX, TAX_label.fr, ObsValue) |> 
  group_by(COU, Time, GOV, VAR) |> 
  mutate(
    ObsValue = as.numeric(ObsValue),
    ObsValue = ifelse(TAX=="5000", ObsValue-ObsValue[TAX=="5111"], ObsValue),
    TAX_label.fr = ifelse(TAX=="5000", "5000m Impôts sur les biens et services hors TVA", TAX_label.fr)) |> 
  ungroup() |> 
  filter(VAR=="TAXGDP", TAX%in%c("1100", "1200", "2000", "3000", "4000", "5000", "5111", "6000")) |> 
  left_join(oecd_a, by=c("COU", "Time")) |> 
  drop_na(gdp)

pdev <- taxgdp |> 
  filter(COU%in%setdiff(c(eu, others), "FRA")) |> 
  group_by(COU) |> 
  mutate(gdp2020 = first(gdp[Time=="2019"])) |> 
  group_by(VAR, Time, GOV, TAX, TAX_label.fr) |> 
  summarize(ObsValue = weighted.mean(ObsValue, gdp2020, na.rm=TRUE)) |> 
  ungroup() |> 
  mutate(COU = "PDEV", Country = "Pays développés")

peu <- taxgdp |> 
  filter(COU%in%setdiff(eu, "FRA")) |> 
  group_by(COU) |> 
  mutate(gdp2020 = first(gdp[Time=="2019"])) |> 
  group_by(VAR, Time, GOV, TAX, TAX_label.fr) |> 
  summarize(ObsValue = weighted.mean(ObsValue, gdp2020, na.rm=TRUE)) |> 
  ungroup() |> 
  mutate(COU = "EU", Country = "Union Européenne")

taxdata <- taxgdp |> 
  bind_rows(pdev) |> 
  bind_rows(peu)

taxdata.c <- taxdata |> 
  filter(COU == "FRA", TAX=="1100") |> 
  left_join(csg |> select(-gdp), by=c("COU", "Time")) |> 
  mutate(ObsValue = ObsValue - csg) |> 
  bind_rows(csg |> transmute(COU = "FRA",
                             COU_label.fr = "France",
                             Time, 
                             GOV= "NES",
                             VAR = "TAXGDP",
                             TAX = "1110",
                             TAX_label.fr = "1110 CSG",
                             gdp,
                             ObsValue= csg)) |> 
  bind_rows(taxdata |> filter(!(COU=="FRA"&TAX=="1100"))) |> 
  mutate(Time = as.numeric(Time)) |> 
  mutate(TAX = factor(TAX, levels = c("2000", "1110", "1100", "4000", "1200", "3000", "5000", "5111", "6000")),
         TAX_label.fr = fct_reorder(TAX_label.fr, TAX, .fun = first),
         COU = factor(COU, c("FRA", "EU", "PDEV"))) |> 
  group_by(COU, Time, GOV) |> 
  arrange(desc(TAX)) |> 
  mutate(cpo = cumsum(ObsValue)) |> 
  ungroup() |> 
  mutate(text = glue(str_c("{COU} en {Time}", 
                      "{TAX_label.fr}: {signif(ObsValue, 3)}",
                      "cumul: {signif(cpo,3)}", sep="<br>"))) |> 
  arrange(TAX)

# taxdata.c <- taxdata.c |> 
#   mutate(TAX = factor(TAX, labels = ll))

colors <- tribble(
  ~TAX,        ~color,
  "2000", "aquamarine4", 
  "1110", "aquamarine3",
  "1100", "darkolivegreen",
  "4000", "darkolivegreen3",
  "1200", "chocolate3",
  "3000", "darkgoldenrod3",
  "5000", "darkorchid3",
  "5111", "darkorchid1",
  "6000", "ivory4")

ttax <- distinct(taxdata.c, TAX)$TAX
ccol <- set_names((colors |> pull(color, name = TAX))[ttax], ttax)
ll <- set_names((distinct(taxdata.c, TAX, TAX_label.fr) |> pull(TAX_label.fr, TAX))[ttax], ttax)

colors <- colors |> 
  mutate(color = prismatic::color(color)) |> 
  left_join(distinct(taxdata.c, TAX, TAX_label.fr)) |> 
  rename(fr = TAX_label.fr) |> 
  mutate(fr = str_remove(fr, "[:digit:]+m? ?"))

(taxg <- ggplot(taxdata.c |> filter(Time>=1990, COU%in%c("FRA", "PDEV", "EU"), GOV=="NES") ) +
    geom_col(aes(x=Time, y=ObsValue, fill=TAX, text=text), position="stack", alpha = 0.75) + 
    facet_wrap(vars(COU), labeller = labeller(COU = c(EU = "Union européenne", FRA = "France", PDEV = "Pays développés"))) + 
    theme_ofce() +
    scale_fill_manual(
       labels = ~ll[.x], 
       values = ccol) + 
    theme(legend.position = "bottom", 
          legend.key.size = unit(9, "pt"),
          legend.text = element_text(size = rel(0.5)),
          panel.spacing = unit(12, "pt"))+
    guides(fill=guide_legend(title = NULL, ncol=2)) +
    xlab(NULL)+ylab("% du PIB")+
    labs(caption = str_c("Source : OCDE, Global Revenue Statistic Database, accès en septembre 2022",
                         "données de 1990 à 2020, moyennes pondérées par le PIB de 2020 en parité de pouvoir d'achat", 
                         "UE = 26 Etats membres de l'Union Européenne (hors France),",
                         "Pays developpés= UE - FR + USA, UK, Canada, Japon, Suisse, Korée, Norvège", sep = "\n"))
)
graph2svg(taxg, width = 17, height = 18)
plotly::ggplotly(taxg, tooltip = "text")

taxdata.c |> 
  filter(Time>=1990, COU%in%c("FRA", "PDEV", "EU"), GOV=="NES") |> 
  select(pays = COU, pays.fr = COU_label.fr, tax = TAX, tax.fr = TAX_label.fr, in_gdp = ObsValue) |> 
  write_delim(file = "taxdata.csv", delim = ";")

