# ==============================================================================
# MODULE: 12 - DATA CLEANING
# ==============================================================================
#
# This module provides comprehensive functionality to clean all system datasets
# based on predefined exclusion rules. It removes unwanted links from:
# - input.rds
# - final_data.rds
# - parse_error.rds (and domain-specific files)
# - error.rds
# - retry.rds
#
# All removed links are archived in 'discarded.rds' to maintain a record of
# excluded content. The script supports exclusion by:
# 1. Exact URL match
# 2. Domain-specific path prefixes
# 3. Special logic rules (regex patterns)
# 4. HTTP 410 (Gone) status codes from logs
#
# ==============================================================================

# Load required packages
library(data.table)
library(stringr)

# --- 1. CONFIGURATION & SETUP ---

get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    input          = file.path(base_path, "data", "input"),
    output         = file.path(base_path, "data", "output"),
    logs           = file.path(base_path, "data", "logs"),
    parse_error    = file.path(base_path, "data", "input", "parse_error")
  )
}

paths <- get_module_paths()


# --- 2. EXCLUSION LISTS ---

# -- A. Domain-specific URL path exclusions --
# Any URL on that domain containing this path will be removed. 
# (e.g., "shop/" removes "domain.com/shop/anything")

`augsburger-allgemeine.de_excluded_links` <- c(
  "panorama/panorama-bilder-des-tages-" 
)

`berliner-kurier.de_excluded_links` <- c(
  "ticketshop/"
)

`berliner-zeitung.de_excluded_links` <- c(
  "open-source/", 
  "ticketshop/",
  "topics/",
  "berlin/syndication.383984",
  "berlin/kuendigung.282144"
)

`bild.de_excluded_links` <- c(
  "regional/",
  "tv/",
  "unterhaltung/tv-fernsehformate/"
)

`bnn.de_excluded_links` <- c(
  "thema/",
  "person/",
  "ort"
)

`br.de_excluded_links` <- c(
  "br-fernsehen/", 
  "fernsehen/",
  "sogehtmedien/"
)

`businessinsider` <- c(
  "themen/",
  "insider-picks/",
  "abo/"
)

`derwesten.de_excluded_links` <- c(
  "themen/"
  
)

`echo24.de_excluded_links` <- c(
  "geschaeftskunden/"
)

`epochtimes.de_excluded_links` <- c()


`fnp.de_excluded_links` <- c()

`fr.de_excluded_links` <- c()

`frankenpost.de_excluded_links` <- c()

`freiepresse.de_excluded_links` <- c()

`heidelberg24.de_excluded_links` <- c()

`heise.de_excluded_links` <- c(
  "download/",
  "select/", 
  "thema/",
  "bestenlisten/"
)

`karlsruhe-insider.de_excluded_links` <- c(
  "uebernachtung/"
)

`kreiszeitung.de_excluded_links` <- c()

`manager-magazin.de_excluded_links` <- c()

`mdr.de_excluded_links` <- c(
  "geschichte/"
)

`mopo.de_excluded_links` <- c(
  "sportwetten/",
  "hambrug/polizei/",
  "hambrug/gericht/"
)

`n-tv.de_excluded_links` <- c(
  "infografik/",
  "https://www.n-tv.de/politik/Weidel-bedankt-sich-bei-Trump-Make-Germany-great-again--article25515466.html",
  "https://www.n-tv.de/politik/Britischer-Moechtegern-Doppelagent-zu-Haftstrafe-verurteilt-article25535696.html",
  "https://www.n-tv.de/politik/Berufungsgericht-blockiert-Trumps-Ausgabenstopp-article25555752.html",
  "https://www.n-tv.de/politik/Terror-und-Spionageverdacht-Wer-steckt-hinter-dem-Al-Mustafa-Institut-in-Berlin-article25495529.html",
  "https://www.n-tv.de/politik/Attentaeter-von-Muenchen-durchlief-Turboradikalisierung-article25577330.html"
)

`ndr.de_excluded_links` <- c(
  "fernsehen/", 
  "geschichte/",
  "kultur/sendungen/",
  "kochen/",
  "903/",
  "kika/",
  "service/",
  "podcast/",
  "radiomv/",
  "orchester_chor/"
  
  
)

`news.de_excluded_links` <- c()

`newsflash24.de_excluded_links` <- c(
  "blaulicht/",
  "horoskope/",
  "impressum/",
  "kultur-freizeit/",
  "tag/",
  "unterhaltung/podcasts/"
)

`nordkurier.de_excluded_links` <- c()

`noz.de_excluded_links` <- c(
  "audiothek/",
  "kontakt/",
  "service/"
)

`presseportal.de_excluded_links` <- c()

`rollingstone.de_excluded_links` <- c(
  "konzerte/",
  "tour/",
  "themen/",
  "festival/",
  "club",
  "/reviews",
  "/wohnzimmer",
  "/tour",
  "/konzerte",
  "/kontakt"
)

`rp-online.de_excluded_links` <- c()

`rtl.de_excluded_links` <- c(
  "sendungen/",
  "smart-tv/",
  "sport/moderatoren/",
  "sport/nfl/draft/",
  "unterhaltung/", 
  "wetter/",
  "wettermeldungen/",
  "wettertrend/",
  "themen/",
  "sport/"
)

`ruhr24.de_excluded_links` <- c()

`schwaebische.de_excluded_links` <- c()

`shz.de_excluded_links` <- c()

# those are all videos 
`spiegel.de_excluded_links` <- c(
  "https://www.spiegel.de/geschichte/deutsche-kolonien-so-wirkt-das-kaiserreich-bis-heute-in-unseren-alltag-podcast-a-c90f1fa0-5874-47a9-a369-8eba47820ce3", 
  "https://www.spiegel.de/politik/ukraine-krieg-olaf-scholz-aeussert-sich-zu-moeglicher-friedenstruppe-im-livestream-a-8ea58487-7929-4ecf-a35f-4e0445d6df7f", 
  "https://www.spiegel.de/geschichte/kz-auschwitz-politiker-weltweit-gedenken-der-befreiung-vor-80-jahren-a-84667c6a-d159-4bc2-a672-c481201406a2", 
  "https://www.spiegel.de/politik/deutschland/olaf-scholz-bei-buergergespraech-in-erfurt-wahlkampf-wie-der-bundeskanzler-ihn-gerne-haette-a-0c821fd6-190f-4b80-b87e-b0b8652ea487", 
  "https://www.spiegel.de/politik/kita-mangel-die-ueberraschenden-widersprueche-in-der-versorgungslage-a-0dc00269-bc4f-4ae8-b802-a33bf45deb09", 
  "https://www.spiegel.de/panorama/usa-wintersturm-bringt-schnee-und-eis-nach-washington-massen-schneeballschlacht-entfacht-a-c30b0adf-9eed-48c1-8079-33eec418865d", 
  "https://www.spiegel.de/kultur/berlinale-und-das-kleid-von-luisa-neubauer-mode-und-politik-sind-nie-klar-voneinander-getrennt-a-1fe5f40c-7bf2-4e7b-98df-444bde0ca264", 
  "https://www.spiegel.de/ausland/der-moment-der-geiseluebergabe-und-tausende-sind-dabei-a-c7e27947-31c1-49bf-82eb-c3b008cf3fc7", 
  "https://www.spiegel.de/ausland/slowakei-schueler-verweigert-praesident-peter-pellegrini-den-handschlag-a-8b876df2-c141-420e-a288-9185592fc8b7", 
  "https://www.spiegel.de/politik/deutschland/gerhart-baum-2023-im-interview-die-afd-ist-viel-gefaehrlicher-als-die-raf-a-04ba67ba-897a-4116-b5ca-d9f7fb380749", 
  "https://www.spiegel.de/ausland/donald-trump-wie-tickt-pete-hegseth-der-umstrittene-designierte-us-verteidigungsminister-a-fd375794-5cfa-4482-a818-798df175ff40", 
  "https://www.spiegel.de/geschichte/deutsche-kolonien-diese-deutschen-verbrechen-werden-oft-verdraengt-a-c9b09865-c79a-4122-99dd-ff82d9ec591a", 
  "https://www.spiegel.de/ausland/israel-und-gaza-nach-der-geiseluebergabe-einfach-nur-freude-a-8f2a1141-4674-4d76-aa13-d5ce12da80db", 
  "https://www.spiegel.de/wissenschaft/ki-innovationen-was-ist-die-deutsche-antwort-auf-chatgpt-und-deepseek-a-b7b8729d-389b-4f3d-8b86-7aaa6349eaf2", 
  "https://www.spiegel.de/netzwelt/deepfake-sexvideos-das-perfide-geschaeft-mit-verbrechen-gegen-frauen-mithilfe-von-ki-a-bdf9d799-1654-48f5-9e44-1801d5abb676", 
  "https://www.spiegel.de/wirtschaft/nordkorea-schleust-it-kriminelle-in-deutsche-firmen-ein-podcast-a-ae6166dd-723f-4190-8729-750c66b0d3ed", 
  "https://www.spiegel.de/ausland/oesterreich-das-sagt-fpoe-chef-herbert-kickl-zur-regierungskrise-livestream-a-8940c503-5208-4c67-9639-208142700aea", 
  "https://www.spiegel.de/politik/friedrich-merz-und-die-afd-mehrheit-was-der-tabubruch-im-bundestag-ausloest-a-3aa699fa-0d1a-468e-bf13-ab046b27e4ab", 
  "https://www.spiegel.de/wissenschaft/kita-mangel-die-ueberraschenden-widersprueche-in-der-versorgungslage-podcast-a-16a31082-213e-43ab-ae1a-7b1393065503", 
  "https://www.spiegel.de/wissenschaft/fake-news-tipps-zum-erkennen-von-desinformation-a-86ec0a27-2f3e-4251-96da-9012060ba1ac", 
  "https://www.spiegel.de/panorama/gesellschaft/chemnitz-rechter-protest-gegen-kulturhauptstadt-boeller-hass-und-gebruell-a-fc00e92e-7e00-46a3-adf8-a60b45a7116a", 
  "https://www.spiegel.de/wirtschaft/wirtschaft-und-wahlkampf-das-wollen-die-parteien-fuer-deutschlands-industrie-und-den-wohlstand-podcast-a-c15edeb2-825f-496c-886c-6b686b746ff4", 
  "https://www.spiegel.de/wirtschaft/paypal-wie-sich-nutzer-vor-dem-gastkonto-betrug-schuetzen-koennen-a-e21e8b8e-6464-4d2b-b1ec-1efe3f87782f", 
  "https://www.spiegel.de/netzwelt/was-ist-die-deutsche-antwort-auf-chatgpt-und-deepseek-a-a0aa4b96-e32b-453e-bc67-3a7eb415fd6b", 
  "https://www.spiegel.de/politik/deutschland/friedrich-merz-markus-soeder-das-gerangel-bei-cdu-csu-vor-der-bundestagswahl-a-9db89204-76f7-4fa4-89a5-648c762dde0d", 
  "https://www.spiegel.de/ausland/donald-trump-die-plaene-des-47-us-praesidenten-fuer-die-kommenden-vier-jahre-videokommentar-a-afd71f86-5cba-44a3-b80f-5dbe161bf01e", 
  "https://www.spiegel.de/politik/deutschland/christian-lindner-im-spiegel-kandidatencheck-haben-sie-mich-angelogen-herr-lindner-a-d8150785-dc8d-4cf5-85b4-cce26c30655b", 
  "https://www.spiegel.de/ausland/bilder-der-geiseluebergabe-so-inszeniert-sich-die-hamas-a-3ceeb166-fdff-4bc7-9ea2-57ba619bf8b2", 
  "https://www.spiegel.de/ausland/weisses-haus-sperrt-ap-reporter-aus-wegen-namensstreit-um-golf-von-mexiko-a-68b909bb-4478-46a2-9d5e-1c4c3a844aec", 
  "https://www.spiegel.de/panorama/spiegel-tv-vom-10-02-2025-fahrraddiebe-unsaubere-handwerksgeschaefte-illegale-migration-a-8459e16c-cf20-4989-9b1f-9b086e5cab62", 
  "https://www.spiegel.de/auto/fahrberichte/dovra-rig-x-610-im-test-komfortabler-camper-fuer-eisige-temperaturen-a-d483be6a-4fc2-49ad-a914-b9a056b95874", 
  "https://www.spiegel.de/ausland/donald-trump-vs-supreme-court-was-passiert-wenn-er-urteile-einfach-ignoriert-a-cadde2f1-af5b-4351-86e9-51ea10b67eca", 
  "https://www.spiegel.de/panorama/justiz/bremen-polizeieinsatz-am-hauptbahnhof-begleitung-der-taskforce-im-einsatz-spiegel-tv-a-22039d51-dd76-4de9-a99f-d2ca82716e96", 
  "https://www.spiegel.de/sport/joachim-loew-wird-65-wie-gut-kennen-sie-den-ex-bundestrainer-a-1d3c673c-5555-4798-bf02-df7bbf305464", 
  "https://www.spiegel.de/politik/deutschland/sahra-wagenknecht-im-spiegel-kandidatencheck-das-ist-doch-eine-kranke-debatte-a-20131455-f7e2-4d32-abfb-96026028a593", 
  "https://www.spiegel.de/kultur/kino/david-lynch-ist-tot-surreale-und-duestere-aesthetik-rueckblick-auf-die-beruehmtesten-werke-des-regisseurs-a-833f2630-388b-40a7-a669-b0a059c1b60b", 
  "https://www.spiegel.de/ausland/abschiebungen-unter-donald-trump-sanctuary-cities-im-fokus-a-d235c8b8-595d-4615-9a8e-56a203cd44f2", 
  "https://www.spiegel.de/kultur/literatur/cartoons-der-woche-von-thomas-plassmann-klaus-stuttmann-und-chappatte-a-33b42757-5016-46b5-8911-a16d5418ad85", 
  "https://www.spiegel.de/politik/deutschland/spiegel-deep-dive-mit-maja-goepel-im-rueckwaertsgang-in-die-zukunft-a-39199817-9e9c-4bcc-b6fd-9c5d119766b5", 
  "https://www.spiegel.de/politik/tradwives-die-wiege-des-trends-liegt-bei-radikalen-christen-in-den-usa-podcast-a-d05cdd02-8096-49ed-8ccc-bdce5e2426dc", 
  "https://www.spiegel.de/wirtschaft/unternehmen/rene-benko-der-steile-aufstieg-und-der-tiefe-fall-des-immobilienmoguls-a-c3525eb4-b6d1-4434-b47a-b9352e55e155", 
  "https://www.spiegel.de/politik/deutschland/olaf-scholz-gegen-friedrich-merz-ein-unverzeihlicher-fehler-und-groesstes-unbehagen-a-2d56a0bc-52aa-43d6-86f4-57f5a1a65202", 
  "https://www.spiegel.de/wirtschaft/unternehmen/rene-benko-der-steile-aufstieg-und-der-tiefe-fall-des-immobilienmoguls-wo-ist-das-geld-a-8508ba84-f87c-4de3-a236-ef87f972f8aa", 
  "https://www.spiegel.de/politik/robin-mesarosch-erfolgreicher-spd-abgeordneter-auf-tiktok-ueber-seine-strategie-gegen-die-afd-a-b77ab9f0-171b-491e-951d-a31da49d7654", 
  "https://www.spiegel.de/panorama/muenchen-das-wissen-die-ermittler-ueber-die-gewalttat-livestream-der-pressekonferenz-a-84bca89e-c019-4c02-b88d-2c1191bc9aeb", 
  "https://www.spiegel.de/politik/afd-kandidatin-alice-weidel-so-geht-sie-in-die-bundestagswahl-podcast-a-bb406fcb-2762-446e-a536-67d5c7a04954", 
  "https://www.spiegel.de/ausland/new-york-city-wie-die-neue-pkw-maut-anwohner-und-gewerbetreibende-umtreibt-a-d42e0aea-9b21-4288-ab2f-6f22698c29c8", 
  "https://www.spiegel.de/kultur/musik/grammy-awards-in-bildern-beyonce-kendrick-lamar-und-chappell-roan-mit-dem-pinken-pony-a-0d30c7fd-1799-49de-8f9b-35978603c8b7", 
  "https://www.spiegel.de/ausland/ukraine-krieg-deserteure-werden-wieder-aufgenommen-schwierige-truppensituation-und-fehlende-rekruten-a-9a91b9a2-79ee-4372-8bb5-faafdc336664", 
  "https://www.spiegel.de/panorama/silvester-es-werde-licht-die-feiern-von-sydney-bis-new-york-a-4554b659-e2c1-4641-b1f0-41af7b1793c4", 
  "https://www.spiegel.de/kultur/literatur/silvesterknallerei-neujahrsvorsaetze-putin-und-trump-das-sind-die-besten-cartoons-der-woche-a-9007848d-1cf8-4a76-8bff-0c5194773781", 
  "https://www.spiegel.de/panorama/auschwitz-livestream-zum-80-jahrestag-der-befreiung-a-047bf281-2c0b-430d-a25c-364e87d8341c", 
  "https://www.spiegel.de/panorama/justiz/bandenmitglied-vor-gericht-prozess-gegen-angebliche-reinigungsprofis-gestartet-spiegel-tv-a-78ee7c5b-4df2-4ce9-a875-aa4da7d7836b", 
  "https://www.spiegel.de/wirtschaft/steigende-mieten-welche-ideen-haben-die-parteien-zur-begrenzung-der-mietpreise-a-cdf7e74a-11ec-43de-bcd0-edc854460da5", 
  "https://www.spiegel.de/ausland/donald-trump-bischoefin-ermahnt-ihn-zu-mehr-menschlichkeit-im-umgang-mit-migranten-a-2fdda382-1eb3-40ac-b3d5-264a8432ed1a", 
  "https://www.spiegel.de/panorama/auto-attacke-in-muenchen-tatwaffe-kleinwagen-video-a-e7f69cc1-3bce-4abe-ad99-7a6eaee38331", 
  "https://www.spiegel.de/ausland/gaza-krieg-tausende-kehren-in-den-norden-des-landes-zurueck-a-67c6d7f5-84d5-4bbe-ad8b-5554d0fba213", 
  "https://www.spiegel.de/politik/deutschland/friedrich-merz-das-sagt-der-cdu-chef-zur-migrationsdebatte-livestream-a-7fcc662c-0ea6-4d92-ac61-036fbf540f4c", 
  "https://www.spiegel.de/politik/deutschland/bundeswehr-und-verteidigung-so-viele-milliarden-muessen-her-podcast-a-918d7dc0-723f-4f16-b6d7-d43e55f6b165", 
  "https://www.spiegel.de/politik/deutschland/christian-lindner-fdp-chef-bei-wahlkampfauftritt-in-greifswald-mit-schaumtorte-beworfen-a-987d117a-23cf-447e-ae86-c3ff88a2788a", 
  "https://www.spiegel.de/politik/deutschland/markus-soeder-auf-dem-cdu-parteitag-rede-im-livestream-a-3d5e9921-8301-4f69-9551-d78187bba746", 
  "https://www.spiegel.de/ausland/donald-trump-der-47-us-praesident-tanzt-auf-drei-baellen-an-einem-abend-a-ff9dc501-39fa-4735-9691-a43f92a12fcf", 
  "https://www.spiegel.de/politik/deutschland/wifionice-warum-das-bahn-wlan-oft-so-langsam-ist-podcast-a-e08490c3-c01b-44f8-87b9-1026071992d9", 
  "https://www.spiegel.de/ausland/waffenstillstand-in-gaza-warum-der-deal-zwischen-israel-und-hamas-so-fragil-ist-a-4f9c9122-e25b-4e77-adde-78ae408635cf", 
  "https://www.spiegel.de/gesundheit/schlafmangel-unruhige-naechte-was-tun-fuer-erholsamen-schlaf-podcast-a-de244185-49b1-41dc-b7f8-60306be3b30a", 
  "https://www.spiegel.de/panorama/justiz/wuetend-respektlos-gewalttaetig-wenn-patienten-zu-schlaegern-werden-spiegel-tv-a-a1aeee74-1b95-4125-9381-300ea72747a7", 
  "https://www.spiegel.de/politik/deutschland/bundestagwahl-2025-das-waren-die-letzten-reden-vor-der-bundestagswahl-a-48d0ee89-22ad-4ed8-844a-17c19af10a06", 
  "https://www.spiegel.de/politik/afd-spenden-affaere-ein-strohmann-fuer-milliardaer-henning-conle-podcast-a-8ad748aa-1e5b-4d84-9d44-7951b82a4e47", 
  "https://www.spiegel.de/politik/bundestagswahl-2025-was-junge-deutsche-zu-nichtwaehlern-macht-a-6a01ddb5-f375-4214-ad9a-4e702b00dc85", 
  "https://www.spiegel.de/ausland/donald-trump-kehrt-zurueck-juedin-ueber-die-angst-dass-auch-antisemitismus-wieder-zunimmt-a-a30fd2bf-41e5-4e02-9900-97f48129b58d", 
  "https://www.spiegel.de/ausland/suedkorea-neue-proteste-nach-gescheiterter-verhaftung-von-praesident-yoon-die-bilder-a-ebd11af4-99a3-4fd4-8cef-5bed5c69ea49", 
  "https://www.spiegel.de/stil/haute-couture-week-in-paris-designer-praesentieren-sommer-und-fruehjahrskollektionen-a-e935f7a6-9d08-4966-b64f-af368bbbe772", 
  "https://www.spiegel.de/politik/deutschland/duisburg-vor-der-bundestagswahl-fuer-jedes-kreuzchen-ein-bierchen-a-fed16b01-44bc-4ab2-b832-87557c9f163c", 
  "https://www.spiegel.de/ausland/pete-hegseth-anhoerung-im-us-senat-entscheidet-ueber-zukunft-des-trump-kandidaten-a-a25fd844-c30a-4215-9d6b-bc284458d408", 
  "https://www.spiegel.de/ausland/israel-und-hamas-einigen-sich-auf-deal-und-waffenruhe-jubel-erleichterung-und-skepsis-a-dc6a427d-b1b1-4fdb-be88-482cb337b522", 
  "https://www.spiegel.de/wissenschaft/fake-news-tipps-zum-erkennen-von-desinformation-podcast-a-3b168019-47e3-4ff7-a329-d1c9ce72ddba", 
  "https://www.spiegel.de/politik/deutschland/cdu-parteitag-friedrich-merz-spricht-zu-delegierten-livestream-aus-berlin-a-5a1e121e-6f90-41eb-8671-8aeb74ac105f", 
  "https://www.spiegel.de/politik/spiegel-tv-vom-17-02-2025-deutschland-vor-der-wahl-a-416989c8-8057-4959-b6bd-b65143da8ac8", 
  "https://www.spiegel.de/politik/deutschland/syrer-in-deutschland-buergermeister-alshebl-ueber-die-migrationsdebatte-und-zukunftssorgen-a-87bc604a-8587-4fea-9253-db8cbb8233eb", 
  "https://www.spiegel.de/politik/afd-spenden-affaere-ein-strohmann-fuer-milliardaer-henning-conle-a-b0ed43fb-87be-44fd-ab35-0fd80510892b", 
  "https://www.spiegel.de/politik/tradwives-die-wiege-des-trends-liegt-bei-radikalen-christen-in-den-usa-podcast-a-1ac6e1da-919c-47ab-8fdc-e80780d378cf", 
  "https://www.spiegel.de/ausland/usa-eindruecke-von-der-grenze-zu-mexiko-trumps-notstandserklaerung-und-abschottung-a-ed42b4be-3313-4b5f-9f04-4d77a1f710b8", 
  "https://www.spiegel.de/ausland/donald-trump-karoline-leavitt-die-loyale-stimme-im-weissen-haus-a-3a2b49dd-6898-4104-8ef3-721c6a6a977a", 
  "https://www.spiegel.de/politik/deutschland/bundestagswahl-2025-szenen-aus-den-wahllokalen-kreuz-fuer-kreuz-a-20ba1ffb-8eed-454c-aa93-c3c8a7af8ad6", 
  "https://www.spiegel.de/panorama/justiz/promille-poebler-und-polizei-auf-streife-in-kiel-spiegel-tv-a-b60f444f-8d33-4fef-807d-eca13fa97ccc", 
  "https://www.spiegel.de/politik/donald-trump-und-die-ukraine-was-die-grossmachtlogik-fuer-russland-und-europa-bedeutet-shortcut-a-5b47c4bb-6667-4ce6-8cba-7b4fdbfa85d7", 
  "https://www.spiegel.de/wissenschaft/winter-wetter-in-deutschland-warum-wir-schnee-tatsaechlich-riechen-koennen-a-231ab3c3-6bd7-4341-bc4e-1d45b01d3c99", 
  "https://www.spiegel.de/ausland/so-wurde-herbert-kickl-zum-chef-der-fpoe-podcast-inside-austria-a-9bf0df40-61e0-4055-97e4-b1fabab50d06", 
  "https://www.spiegel.de/politik/bundestag-schlagabtausch-zwischen-olaf-scholz-und-friedrich-merz-a-6d3603d1-66ce-406a-b69e-266cdd18d855", 
  "https://www.spiegel.de/kultur/berlinale-eroeffnung-im-video-tilda-swintons-rede-und-luisa-neubauers-kleid-a-932eae69-e08d-4a6e-ae2f-69ef62fc3edf", 
  "https://www.spiegel.de/ausland/robert-f-kennedy-jr-trumps-wunschkandidat-stellt-sich-den-fragen-der-abgeordneten-a-3adbffcc-30fa-48a5-8583-c3dd272a663b", 
  "https://www.spiegel.de/wirtschaft/mieten-welche-ideen-haben-die-parteien-zur-begrenzung-der-mietpreise-a-6fa14bf5-a1c3-4eb1-90a7-194cb5877c9c", 
  "https://www.spiegel.de/ausland/wie-schlaegt-sich-trumps-wunsch-justizministerin-vor-dem-us-kongress-a-e87ca114-ea55-4f3c-ba28-4cb4e968bfda", 
  "https://www.spiegel.de/ausland/abschiebungen-unter-donald-trump-warum-jetzt-die-sanctuary-cities-in-den-fokus-ruecken-a-b347ec94-a2e6-4f29-a507-7c92214522cf", 
  "https://www.spiegel.de/panorama/gesellschaft/spiegel-tv-vom-20-01-2025-nazi-demo-in-chemnitz-angriffe-gegen-retter-auf-streife-in-kiel-a-6de89c58-21ee-43c3-9d5f-cf81e95864f1", 
  "https://www.spiegel.de/ausland/joe-bidens-karriere-vom-juengsten-senator-zum-aeltesten-praesidenten-a-98653216-9682-4868-a6e4-24381f07c1f3", 
  "https://www.spiegel.de/panorama/justiz/clanmitglied-attackiert-reporter-im-gericht-soll-ich-ihn-schlagen-spiegel-tv-a-3efcc850-fd35-4022-8983-f09ec06af32e", 
  "https://www.spiegel.de/kultur/literatur/cartoons-im-monat-von-chappatte-thomas-plassmann-klaus-stuttmann-a-2016667c-6d27-4a53-840d-cbbc6ea8f76e", 
  "https://www.spiegel.de/politik/bundestagswahl-2025-buerger-zwischen-schulchaos-und-populismus-reportage-von-spiegel-tv-a-4998d575-6973-4f11-8b86-5066e49886aa", 
  "https://www.spiegel.de/ausland/israel-hamas-deal-erste-palaestinensische-haeftlinge-nach-freilassung-in-westjordanland-angekommen-a-107d82c3-8fdf-4060-ae4f-878ee80ff76a", 
  "https://www.spiegel.de/panorama/golden-globes-2025-gold-silber-und-viel-knalleffekt-die-bilder-vom-roten-teppich-a-6b67a1f4-f69a-4f28-ae01-b1071eab4c4b", 
  "https://www.spiegel.de/wirtschaft/cyberkriminalitaet-nordkorea-schleust-it-kriminelle-in-deutsche-firmen-ein-a-33fa6969-5d33-47df-8df7-50c87e99ffb8", 
  "https://www.spiegel.de/politik/die-linke-heidi-reichinnek-als-neue-hoffnung-im-wahlkampf-a-29514097-4b48-4a43-b207-dfdcfd2d2233", 
  "https://www.spiegel.de/panorama/gesellschaft/spiegel-tv-vom-27-01-2025-blankes-erfolgsrezept-micaela-schaefer-neonazis-attacken-der-elblandrevolte-a-d64f16c6-8a0e-49d3-b188-4e8cdb66d357", 
  "https://www.spiegel.de/politik/bundestagswahl-cdu-wahlkampf-in-hamburg-herr-merz-ist-der-groesste-a-2b165c70-ccad-4976-9722-5307e960d7ab", 
  "https://www.spiegel.de/panorama/irland-grossbritannien-sturm-eowyn-reisst-baeume-um-und-deckt-daecher-ab-a-0b4da6fa-1809-45bf-ae38-6447b54c5d35", 
  "https://www.spiegel.de/ausland/jimmy-carter-us-praesidenten-versammeln-sich-fuer-staatsbegraebnis-in-washington-a-f77505ec-ffc5-474d-961b-0727ce005ab5", 
  "https://www.spiegel.de/ausland/syrien-fuer-die-kurden-ist-noch-krieg-wie-ein-frieden-mit-der-tuerkei-gelingen-kann-podcast-a-05a73c38-e065-4a52-b0c1-ac99ac19acd1", 
  "https://www.spiegel.de/gesundheit/ernaehrung/bmi-krankhaft-uebergewichtig-was-sich-an-der-adipositas-diagnose-aendern-soll-podcast-a-74a599ec-a502-48fe-92d5-fd447157a638", 
  "https://www.spiegel.de/ausland/joe-biden-der-letzte-transatlantiker-geht-was-nun-a-e3c52e83-c87c-4d53-9f72-fe405e31c999", 
  "https://www.spiegel.de/panorama/leute/blankes-erfolgsrezept-micaela-schaefer-karriere-booster-dschungelcamp-spiegel-tv-a-cd8bc572-0108-4eb7-a368-b6f99d108799", 
  "https://www.spiegel.de/netzwelt/donald-trump-und-der-tiktok-bann-in-den-usa-was-die-neue-schonfrist-bedeutet-a-2c2b7d69-c5c8-43a5-906f-a070955bc147", 
  "https://www.spiegel.de/panorama/silvester-und-neujahr-fotos-der-aufraeumarbeiten-nach-dem-grossen-knallen-a-f8f7887f-d2fe-4463-902d-0830353287f6", 
  "https://www.spiegel.de/netzwelt/das-perfide-geschaeft-mit-deepfake-sexvideos-podcast-a-b9c1c83c-a16b-484f-82b3-c60ae20cfa53", 
  "https://www.spiegel.de/politik/olaf-scholz-und-friedrich-merz-diskutieren-ueber-migration-und-wirtschaft-auszuege-im-video-a-5ea8e917-2681-43ff-bf09-fb7de60060cb", 
  "https://www.spiegel.de/ausland/us-grenze-mexiko-baut-aufnahmelager-fuer-trumps-abschiebeaktion-a-fdba5e6e-c4e5-4509-a8e8-0ac4a93ef9c3", 
  "https://www.spiegel.de/politik/deutschland/wahlhelfer-in-hamburg-ich-haette-angst-dass-die-gewinnen-a-cc795f32-313a-4369-9dc4-aae7e23ea8bb", 
  "https://www.spiegel.de/panorama/justiz/manfred-genditzki-wie-die-bayerische-justiz-einen-unschuldigen-zur-kasse-bittet-spiegel-tv-a-60cc35c8-1b53-45fc-9dc1-7e4ff89571c6", 
  "https://www.spiegel.de/politik/deutschland/bundeswehr-und-verteidigung-so-viele-milliarden-muessen-her-a-82fcb724-b4f1-42e6-967e-0c4dd0afa879", 
  "https://www.spiegel.de/ausland/waldbraende-in-kalifornien-die-leute-haben-ihre-autos-einfach-auf-der-strasse-stehen-lassen-a-4b99458e-5470-4c3b-a67a-48078d42f962", 
  "https://www.spiegel.de/politik/bundestagswahl-2025-markus-feldenkirchen-ueber-seine-erfahrung-mit-scholz-habeck-co-a-2d05c5fa-7953-44e7-bc90-b3cfc11b6a2c", 
  "https://www.spiegel.de/politik/deutschland/tv-quadrell-bei-rtl-ausschnitte-des-kanzlerkandidaten-duells-im-video-a-2f40e733-1097-4c0c-b6cc-39c52237f2be", 
  "https://www.spiegel.de/wissenschaft/natur/grossbritannien-zufallsfund-enthuellt-alte-fussspuren-von-dinosauriern-a-777e7aa5-6c1c-475e-a5e4-912cf8e22ff0", 
  "https://www.spiegel.de/wirtschaft/elektronische-patientenakte-epa-was-karl-lauterbach-verspricht-und-was-sie-wirklich-bedeutet-podcast-a-a0dc94e2-3290-4d99-8b47-e2ccfb81a123", 
  "https://www.spiegel.de/politik/deutschland/der-afd-parteitag-in-riesa-radikale-reden-und-brodelnde-proteste-spiegel-tv-a-22d6c122-e82e-45db-bbd7-88033a8e80d2", 
  "https://www.spiegel.de/wissenschaft/natur/kuriose-tierfunde-diese-arten-wurden-auf-ungewoehnliche-weise-entdeckt-a-913ed2d5-595c-47dd-be70-73a04f5aa864", 
  "https://www.spiegel.de/netzwelt/instagram-facebook-und-threads-wie-mark-zuckerberg-seine-plattformen-umbaut-a-f7be38a2-d819-41f9-8aff-0a394339be19", 
  "https://www.spiegel.de/wirtschaft/soziales/buergergeld-in-schwerin-stadt-beschliesst-arbeitspflicht-fuer-empfaenger-a-19e6a513-3507-4f46-b753-3fe41de1dd40", 
  "https://www.spiegel.de/gesundheit/einsamkeit-was-gegen-das-alleinsein-hilft-podcast-a-dd79c9a9-f2f4-4792-8f4d-b59e0871372a", 
  "https://www.spiegel.de/ausland/israel-gaza-krieg-hamas-uebergibt-vier-tote-geiseln-an-israel-a-a96ac940-a824-4aa7-b5be-73c32818b5ff", 
  "https://www.spiegel.de/ausland/tuerkei-drohnenbilder-zeigen-ausmass-der-zerstoerung-nach-hotelbrand-in-bolu-a-e84912ce-cd89-4d3f-a07f-bfddf6e78edc", 
  "https://www.spiegel.de/politik/horst-koehler-das-wirken-des-frueheren-bundespraesidenten-in-bildern-a-54c1f2a8-1779-45fb-b0c9-68670f9604db", 
  "https://www.spiegel.de/panorama/gesellschaft/afd-blockade-clankriminalitaet-drogen-hotspot-bremen-a-bdd7e81b-9e24-46b4-a104-90f1fb660226", 
  "https://www.spiegel.de/politik/bundestagswahl-2025-markus-feldenkirchen-ueber-seine-erfahrung-mit-scholz-habeck-co-podcast-a-0e074e2e-458b-42eb-8492-95a49fccce48", 
  "https://www.spiegel.de/wissenschaft/natur/underwater-photographer-of-the-year-2025-die-besten-bilder-a-155ee627-6c9c-44d2-977d-1ffd6714e967", 
  "https://www.spiegel.de/ausland/donald-trumps-migrationspolitik-proteste-in-us-staedten-und-angebot-aus-el-salvador-a-36190ce1-c027-44dc-b458-2b90e58f83c1", 
  "https://www.spiegel.de/politik/donald-trump-und-die-ukraine-was-die-grossmachtlogik-fuer-russland-und-europa-bedeutet-a-efe2eab6-9b94-4f92-b9cf-9c1b1e54c8c1", 
  "https://www.spiegel.de/ausland/donald-trump-spricht-auf-dem-weltwirtschaftsforum-in-davos-livestream-a-f5fbcd44-eb40-47f4-9c56-88b632db6e89", 
  "https://www.spiegel.de/ausland/donald-trump-und-sturm-aufs-us-kapitol-hoffnung-auf-begnadigung-durch-den-designierten-praesidenten-a-ee7876d3-bbde-49e7-97b0-3086cdff98c7", 
  "https://www.spiegel.de/ausland/ukraine-krieg-spiegel-reporter-aus-kyjiw-ueber-donald-trumps-einfluss-auf-die-ukraine-a-816f008b-8752-42f7-af6a-15ac45be8989", 
  "https://www.spiegel.de/panorama/kanada-schneestuerme-und-minus-30-grad-wintereinbruch-in-bildern-a-b62e4c00-cc3b-4b19-8f5f-ef19797eb1cf", 
  "https://www.spiegel.de/politik/wifionice-warum-das-bahn-wlan-oft-so-langsam-ist-a-a99cd677-c923-42b3-bfc6-96233cdb74ab", 
  "https://www.spiegel.de/panorama/hundeschau-eindruecke-von-der-westminster-kennel-club-dog-show-in-new-york-a-2f1059e5-d9cc-47e9-b78e-266866e592bd", 
  "https://www.spiegel.de/gesundheit/ernaehrung/bmi-krankhaft-uebergewichtig-was-sich-an-der-adipositas-diagnose-aendern-soll-spiegel-shortcut-a-bb255a1f-b77d-45fd-a3a7-3d0384300d61", 
  "https://www.spiegel.de/panorama/kalifornien-grossbrand-bei-los-angeles-ausnahmezustand-in-pacific-palisades-a-b42c901f-f0c6-4b31-b245-b6903a46fd3f", 
  "https://www.spiegel.de/politik/afd-kandidatin-alice-weidel-so-geht-sie-in-die-bundestagswahl-a-511549b9-6ab4-4873-a445-9ba5883e7481", 
  "https://www.spiegel.de/ausland/syrien-fuer-die-kurden-ist-noch-krieg-wie-ein-frieden-mit-der-tuerkei-gelingen-kann-a-f1d9c5e2-e8f3-4901-baa6-6eff99af1981", 
  "https://www.spiegel.de/ausland/waffenstillstand-in-gaza-man-kann-die-hamas-nicht-mit-krieg-besiegen-a-ab87996f-c3f3-4c91-9d20-ec47569de170", 
  "https://www.spiegel.de/auto/alfa-romeo-junior-im-test-der-italiener-der-nicht-milano-heissen-darf-a-a7fb2fc9-fd42-4601-929b-2b0001006049", 
  "https://www.spiegel.de/auto/fahrberichte/kia-ev3-im-test-kompakt-suv-mit-ungewoehnlichem-design-a-1db0622e-a2d2-48db-a8f9-bff9e9f21c2e", 
  "https://www.spiegel.de/politik/afd-parteitag-in-riesa-aktivisten-vor-protest-sachsen-nicht-der-afd-ueberlassen-a-c1f335ef-7cd9-449b-a2ce-8dc3fd603441", 
  "https://www.spiegel.de/karriere/sinnlose-meetings-und-aufgaben-frustrierende-ablaeufe-das-hilft-gegen-unnoetige-arbeit-a-36ea35db-59e5-409b-a0fd-e12e6dc23e68", 
  "https://www.spiegel.de/ausland/kanada-delta-airlines-flugzeug-landet-kopfueber-in-toronto-a-1081920a-1a76-4b61-85a9-2256c547dc02", 
  "https://www.spiegel.de/ausland/spaniens-flutkatastrophe-not-und-wut-nach-der-jahrhundertflut-in-valencia-spiegel-tv-fuer-arte-re-a-4393324a-3c6b-41fa-8163-dc35aa4d4c4f", 
  "https://www.spiegel.de/politik/deutschland/pete-hegseth-us-verteidigungsminister-besucht-armeeeinrichtungen-in-stuttgart-a-7b9b63a2-e2ac-4071-9d50-c055b4a7451a", 
  "https://www.spiegel.de/politik/friedrich-merz-und-die-afd-mehrheit-was-der-tabubruch-im-bundestag-ausloest-podcast-a-1e15da7b-3994-4723-b110-327b306f69f1", 
  "https://www.spiegel.de/politik/deutschland/bundestagswahl-2025-unterwegs-mit-volt-spitzenkandidatin-maral-koohestanian-a-7e588e90-1a80-40ed-af1c-6b6e09b4075f", 
  "https://www.spiegel.de/politik/friedrich-merz-und-die-afd-die-brandmauer-loest-sich-langsam-in-wohlgefallen-auf-a-1e718309-1517-44e6-a51e-d469ddf6f70d", 
  "https://www.spiegel.de/politik/schlafstoerungen-und-unruhige-naechte-was-tun-fuer-erholsamen-schlaf-a-f6646df5-11ee-4dec-a0dd-f909cc10725a", 
  "https://www.spiegel.de/ausland/donald-trump-razzien-gegen-illegale-einwanderer-in-mehreren-us-staedten-a-4282ed79-fdd1-4299-a67e-4d3c0c66484f", 
  "https://www.spiegel.de/panorama/gesellschaft/neujahrsbaden-nordsee-tiber-genfer-see-so-badet-sich-europa-ins-neue-jahr-a-e67ebbdf-2a9d-417d-a932-aedf0864f38b", 
  "https://www.spiegel.de/panorama/justiz/fahrraddiebe-in-berlin-wie-die-polizei-die-taeter-jagt-spiegel-tv-a-1309e4c1-3b36-4834-81fa-42c6795e6be4", 
  "https://www.spiegel.de/ausland/usa-donald-trump-verstaerkt-grenzschutz-mit-militaer-und-stacheldraht-a-c19a4305-9038-49b1-8cb7-fe8110e1380a", 
  "https://www.spiegel.de/panorama/spiegel-tv-vom-03-02-2025-nach-dem-dschungelcamp-polit-poker-im-bundestag-justiz-skandal-ohne-ende-a-f6fa0e36-031e-4cfe-ba7b-499df89372cf", 
  "https://www.spiegel.de/ausland/ecuador-drogenkrieg-gegen-die-narcos-kartelle-killer-korruption-spiegel-tv-a-e3a1c1d0-c4ac-4000-8cf5-22cbc14dbc26", 
  "https://www.spiegel.de/gesundheit/einsamkeit-was-gegen-das-alleinsein-hilft-a-81fe60ab-2ced-4e72-8c1e-49415fb6243e", 
  "https://www.spiegel.de/politik/deutschland/bundestagswahl-in-bildern-eindruecke-aus-den-wahllokalen-a-02bae12d-9732-4956-904f-cc8f3e8950f5", 
  "https://www.spiegel.de/ausland/new-orleans-und-las-vegas-ermittler-pruefen-zusammenhang-zwischen-beiden-taten-in-den-usa-a-8b6ebcb9-c0f4-4250-b899-ba42c7189527", 
  "https://www.spiegel.de/ausland/washington-d-c-hubschrauber-kollidiert-mit-passagiermaschine-beten-fuer-die-familien-a-8e760e9d-a51b-4688-8f8b-218d61921ae8", 
  "https://www.spiegel.de/ausland/suedkorea-festnahme-von-yoon-suk-yeol-mit-mehr-als-3000-beamten-a-ccbe70d2-a98a-42d0-891e-95a338712876", 
  "https://www.spiegel.de/politik/attacken-durch-rechtsradikale-wie-junge-neonazis-ihre-gegner-ins-visier-nehmen-spiegel-tv-a-8d269655-d02d-4425-9a8a-11dfc448bc2c", 
  "https://www.spiegel.de/ausland/benjamin-netanyahu-bei-donald-trump-gazastreifen-als-riviera-des-nahen-ostens-a-2ecd4525-8e59-4f2c-9eae-e806b0265fb1", 
  "https://www.spiegel.de/wissenschaft/weltall/spacex-und-blue-origin-jeff-bezos-gelingt-der-flug-elon-musk-die-landung-a-8f414fc1-c324-4815-a9cc-de950128742b", 
  "https://www.spiegel.de/panorama/silvester-so-feiert-die-welt-den-jahreswechsel-die-bilder-a-c1ede106-bed8-4059-ab52-83a98c205642", 
  "https://www.spiegel.de/ausland/groenland-daenemark-plant-aufruestung-mit-neuen-kriegsschiffen-a-344d813a-39a8-423f-8df7-596e6db18a7a", 
  "https://www.spiegel.de/ausland/groenland-donald-trump-junior-besucht-die-insel-sein-vater-provoziert-a-824d3c15-a823-4ba3-916b-5bab73ad5cd6", 
  "https://www.spiegel.de/politik/deutschland/spd-parteitag-die-rede-von-olaf-scholz-im-video-a-4ce14f27-663d-46a9-a947-5fe30cc6e869", 
  "https://www.spiegel.de/karriere/sinnlose-meetings-und-aufgaben-frustrierende-ablaeufe-das-hilft-gegen-unnoetige-arbeit-podcast-a-9aecafa8-cf07-486f-b61c-1a89c8d39e86", 
  "https://www.spiegel.de/politik/bundestagswahl-2025-annalena-baerbock-kaempft-gegen-olaf-scholz-um-ein-direktmandat-a-77fd5470-4168-4bfd-aa56-10577488c49d", 
  "https://www.spiegel.de/sport/fussball/australian-open-quiz-wann-wurde-das-turnier-zuletzt-auf-gras-ausgetragen-a-d7db73e5-ddba-4239-a623-1a5270b8aab9", 
  "https://www.spiegel.de/kultur/literatur/cartoons-der-woche-von-thomas-plassmann-klaus-stuttmann-und-chappatte-a-b52afa2b-d990-4ca7-a570-0142af815471", 
  "https://www.spiegel.de/politik/bundestagswahl-2025-wahlpartys-party-bei-der-linken-katerstimmung-bei-den-ampelparteien-a-f3bafb8c-338e-4c93-a4e9-6d0c5971ecaf", 
  "https://www.spiegel.de/politik/christian-lindner-fdp-chef-beim-wahlkampf-in-bremen-beschimpft-a-c79af74e-56b3-42bb-89cc-425a22bc3369", 
  "https://www.spiegel.de/kultur/tv/saturday-night-live-stars-feiern-50-jubilaeum-mit-cameos-und-humor-a-8477c838-a6c9-46d6-be1d-3dbbfc16d02a", 
  "https://www.spiegel.de/ausland/oesterreich-ist-herbert-kickl-fpoe-bald-kanzler-a-91e611da-e9c2-4eaf-9c18-3a384b3e9c1b", 
  "https://www.spiegel.de/reise/staedte/karneval-in-venedig-all-you-need-is-amore-a-6b910b09-4a5f-43dc-aca4-8ce34e301dbb", 
  "https://www.spiegel.de/wirtschaft/elektronische-patientenakte-epa-was-karl-lauterbach-verspricht-und-was-sie-wirklich-bedeutet-a-2c3200ff-5921-4717-a262-d547d5163297", 
  "https://www.spiegel.de/wirtschaft/wirtschaft-und-wahlkampf-das-wollen-die-parteien-fuer-deutschlands-industrie-und-den-wohlstand-a-28c62a97-9c71-4c70-ab6f-1662ef761ad8", 
  "https://www.spiegel.de/panorama/hamburg-ice-unglueck-passagiere-schildern-schreckensszenen-a-4cb09c6d-b95a-45f7-9005-f0f7d27b9080", 
  "https://www.spiegel.de/politik/deutschland/aschaffenburg-markus-soeder-und-joachim-herrmann-zum-ermittlungsstand-a-b9a0d142-da23-4ad0-972c-624508ee3059", 
  "https://www.spiegel.de/ausland/donald-trump-inauguration-mit-familie-und-oligarchen-a-443787d8-daf5-46c3-8550-8fbfc36e1f10", 
  "https://www.spiegel.de/politik/deutschland/jan-van-aken-linke-im-kandidatencheck-was-haben-sie-vor-mit-deutschland-a-8d16341f-2dbb-439c-98b0-eb6aca0a83a8", 
  "https://www.spiegel.de/politik/deutschland/olaf-scholz-im-spiegel-talk-zur-hofnarr-aussage-gegen-cdu-politiker-joe-chialo-a-d6655878-f6f5-44c8-bd5a-e03e6bd1b422", 
  "https://www.spiegel.de/panorama/gesellschaft/demonstrationen-gegen-rechts-zehntausende-protestieren-vor-der-bundestagswahl-a-01da1f9e-c5b5-43db-9572-dad4eb8697fd", 
  "https://www.spiegel.de/wirtschaft/strompreise-und-ausbau-der-erneuerbaren-fehlt-deutschland-die-power-podcast-a-7127698b-8972-4aa2-a435-d608a250630c", 
  "https://www.spiegel.de/ausland/donald-trump-nach-der-vereidigung-saebeltanz-und-dekrete-flut-a-8e15318e-087c-40e5-95f4-76a659e73c1f", 
  "https://www.spiegel.de/politik/deutschland/bundestagswahlkampf-in-bayern-auf-den-fersen-von-markus-soeder-a-71a231a6-7622-4006-b0ef-fc93932baa0a", 
  "https://www.spiegel.de/ausland/oesterreich-ist-herbert-kickl-fpoe-bald-kanzler-podcast-a-de4285a9-6551-43ce-9110-d6914a8577d7", 
  "https://www.spiegel.de/ausland/abschiedsreden-der-us-praesidenten-mic-drops-drueckeberger-und-zweite-chancen-a-79161f58-33f8-4434-ad7c-fdbe23b20984", 
  "https://www.spiegel.de/wirtschaft/strompreise-und-ausbau-der-erneuerbaren-fehlt-deutschland-die-power-a-a3eb6908-0682-457a-9d92-9c3110e0b157", 
  "https://www.spiegel.de/wissenschaft/winter-wetter-in-deutschland-warum-wir-schnee-tatsaechlich-riechen-koennen-podcast-a-94d9c101-f95b-4f3c-a430-69c9c6c3951a", 
  "https://www.spiegel.de/ausland/tschernobyl-video-soll-explosion-an-reaktorhuelle-nach-drohnenangriff-zeigen-a-933ca435-a545-47f1-9524-ddae9fccfc21", 
  "https://www.spiegel.de/panorama/justiz/aschaffenburg-tausende-bei-trauerfeier-fuer-die-opfer-der-messerattacke-a-774eaf9d-9ea8-4ecb-8016-e2ac3742677f", 
  "https://www.spiegel.de/wissenschaft/close-up-photographer-of-the-year-die-besten-nahaufnahmen-des-jahres-a-9efd6124-b902-441d-b6b9-c2a1bd23750f", 
  "https://www.spiegel.de/ausland/donald-trump-vs-supreme-court-was-passiert-wenn-er-urteile-ignoriert-podcast-a-6039c25d-08e7-4d50-89cc-4284cd129c6f", 
  "https://www.spiegel.de/politik/hamburg-demo-gegen-rechts-zehntausende-protestieren-gegen-merz-und-afd-a-c813047d-c847-4cd8-8997-25c447bf0e77", 
  "https://www.spiegel.de/ausland/suedafrika-mehr-als-hundert-tote-in-illegaler-goldmine-ihr-seid-schon-tot-a-2b0e14bd-3f8b-4048-afef-e0a37f71a574", 
  "https://www.spiegel.de/politik/deutschland/robert-habeck-im-kandidatencheck-nichts-zieht-mich-zur-cdu-a-1c738c57-2296-4afe-8be8-34b12c510589", 
  "https://www.spiegel.de/netzwelt/instagram-facebook-und-threads-wie-mark-zuckerberg-meta-umbaut-podcast-a-614bebc7-0983-4b6a-b5fe-ac07a8edac3c", 
  "https://www.spiegel.de/kultur/literatur/cartoons-der-woche-von-thomas-plassmann-klaus-stuttmann-und-chappatte-merz-auf-kurs-a-d7141fb4-c350-4d1b-82af-ce7022683267", 
  "https://www.spiegel.de/politik/bundestagswahl-was-junge-deutsche-zu-nichtwaehlern-macht-podcast-a-c8b09b2e-7cbb-483f-add8-d685fbc7e5b1", 
  "https://www.spiegel.de/ausland/donald-trump-behoerden-in-den-usa-verschaerfen-vorgehen-gegen-illegale-migranten-in-chicago-a-2d607f6e-50e7-49a5-8abe-8a1c07956d5f", 
  "https://www.spiegel.de/kultur/horst-janson-ist-tot-trauer-um-den-fernsehliebling-einer-generation-a-dff6cd1f-64f3-4395-934c-e6b1ce2c441b", 
  "https://www.spiegel.de/ausland/indien-massenpanik-bei-maha-kumbh-mela-dutzende-tote-a-0df1e29b-afb4-4b39-9500-c6f30b97251a", 
  "https://www.spiegel.de/politik/friedrich-merz-und-cdu-zustrombegrenzungsgesetz-debatte-um-migration-und-afd-im-bundestag-a-9cb91ec6-39c6-491f-9b2d-bdf247a4b689", 
  "https://www.spiegel.de/panorama/augenblicke-die-bilder-des-tages-im-januar-a-fe2c5f22-2a2f-4997-b680-de9f40a6aa6c", 
  "https://www.spiegel.de/kultur/literatur/cartoons-der-woche-von-thomas-plassmann-klaus-stuttmann-und-chappatte-a-41c09a90-0cdb-4a20-b67e-23ca14b798f5", 
  "https://www.spiegel.de/politik/bundestagswahl-2025-rambo-zambo-hier-abschiedsstimmung-da-a-8429bdc0-e83b-4d50-9c6a-139b807aa0e3", 
  "https://www.spiegel.de/politik/deutschland/demonstrationen-gegen-rechts-in-berlin-koeln-aschaffenburg-und-halle-a-62c294d7-fa66-4ff1-8cc2-526135ccbc99", 
  "https://www.spiegel.de/panorama/leute/karim-aga-khan-ist-tot-sein-pompoeses-leben-zwischen-religion-und-luxus-a-0bd3baba-e778-4142-8481-701713519a22", 
  "https://www.spiegel.de/ausland/us-demokratin-nach-donald-trumps-wiederwahl-wir-brauchen-ein-riesiges-mikrofon-a-9b0ae372-04ac-46d9-97aa-d5755c5fc3e5", 
  "https://www.spiegel.de/ausland/israel-gaza-krieg-erste-erfolge-beim-gefangenenaustausch-waffenruhe-bleibt-fragil-a-c6314683-7ddc-4d1a-b589-eb4ed5bc34b6", 
  "https://www.spiegel.de/ausland/usa-mexiko-grenze-donald-trumps-migrationspolitik-ist-brutal-und-rassistisch-a-3357ef10-45ff-41ab-bc0b-5666e8171382", 
  "https://www.spiegel.de/panorama/leute/was-das-dschungelcamp-aus-c-promis-macht-spiegel-tv-a-e58ea0a6-5dd8-4fc1-aec9-d7211d442893", 
  "https://www.spiegel.de/politik/deutschland/afd-und-broeckelnde-brandmauer-wie-die-cdu-den-extremen-rechten-auftrieb-gibt-spiegel-tv-a-ba85f571-d7f5-4e70-a92a-cc9e6faba535", 
  "https://www.spiegel.de/netzwelt/donald-trump-und-der-tiktok-bann-was-folgt-auf-die-entsperrung-in-den-usa-a-d893c0f7-ef1b-4d4e-a54c-4f5905da1a56", 
  "https://www.spiegel.de/panorama/santorini-hunderte-erdbeben-versetzen-insel-in-alarmbereitschaft-a-8162b22e-749d-491b-b455-1eacb6068a54", 
  "https://www.spiegel.de/ausland/illegale-migration-in-den-usa-es-gibt-keine-mauer-keine-ueberwachungstechnik-und-nur-sehr-wenige-grenzschuetzer-spiegel-tv-a-339304e8-1f8d-4767-ad49-5810ee052f6a", 
  "https://www.spiegel.de/panorama/indien-baden-und-beten-beim-kumbh-mela-dem-groessten-religioesen-fest-der-welt-a-1f363b28-e8fc-4be6-8253-cd1649b2966a", 
  "https://www.spiegel.de/politik/bundestagswahl-sahra-wagenknecht-wirbt-in-kassel-fuer-das-bsw-a-e0facc71-89e9-4553-96fa-4cb7a0b409fd", 
  "https://www.spiegel.de/ausland/waldbraende-in-kalifornien-spiegel-reporter-berichten-von-der-verheerenden-lage-in-los-angeles-a-9a72f441-cc24-4108-bd02-98a4cdba42a8", 
  "https://www.spiegel.de/politik/deutschland/friedrich-merz-und-markus-soeder-das-gerangel-bei-cdu-csu-vor-der-bundestagswahl-podcast-a-27d569dd-ca71-4e98-9d4b-eb4c9db678f8", 
  "https://www.spiegel.de/ausland/us-praesident-donald-trump-proteste-und-feiern-zur-amtseinfuehrung-a-68fea4d4-eb8a-490a-b4bd-51515785cb13", 
  "https://www.spiegel.de/panorama/gesellschaft/fridays-for-future-bundesweite-demos-zehntausende-fordern-fokus-auf-klimakrise-im-wahlkampf-a-d89d5f1d-c921-4736-8b42-56979088ec7a", 
  "https://www.spiegel.de/ausland/kanada-flugzeug-landet-kopfueber-in-toronto-aufnahmen-zeigen-moment-der-landung-a-2ed40033-c958-4d6a-a3c7-59f31147e231"
    
  
)

`stuttgarter-zeitung.de_excluded_links` <- c()

`swp.de_excluded_links` <- c()

`swr.de_excluded_links` <- c()

`swr3.de_excluded_links` <- c()

`t3n.de_excluded_links` <- c(
  "tag/",
  "https://t3n.de/news/usb-c-ladegeraete-smartphone-tablet-stiftung-warentest-1656878",
  "https://t3n.de/news/apple-samsung-google-beste-smartwatches-stiftung-warentest-1689616",
  "https://t3n.de/news/tuev-ai-lab-ai-act-umsetzung-1645963/",
  "https://t3n.de/news/tuev-ai-lab-ai-act-umsetzung-1645963",
  "https://t3n.de/news/ki-betrug-beim-schach-1676795",
  "https://t3n.de/news/so-will-openai-sicherstellen-dass-sich-chatgpt-gut-benimmt-doch-es-gibt-immer-noch-einen-haken-1659982",
  "https://t3n.de/news/trojaner-playstore-diese-apps-solltest-du-deinstallieren-1648009"
)

`tag24.de_excluded_links` <- c(
  "thema/",
  "unternehmen/",
  "unterhaltung/promis/"
)

`tagesspiegel.de_excluded_links` <- c()

`taz.de_excluded_links` <- c()

`volksstimme.de_excluded_links` <- c()

`wa.de_excluded_links` <- c()

`watson.de_excluded_links` <- c()

`welt.de_excluded_links` <- c(
  "vermischtes/bilder-des-tages/",
  "autor/",
  "themen",
  "https://www.welt.de/satire/gallery252125268/Satire-WELT-Karikaturen.html",
  "https://www.welt.de/satire/gallery157673104/Satire-WELT-Klassenraum-Alternative-gefunden.html",
  "https://www.welt.de/bildergalerien/gallery255092958/Wenn-Woelfe-Hunde-jagen.html"
)

`wiwo.de_excluded_links` <- c(
  "themen/"
)

`wz.de_excluded_links` <- c(
  "liveticker/"
)

`zeit.de_excluded_links` <- c(
  "https://www.zeit.de/x/index",
  "thema/",
  "rezepte/",
  "serie/",
  "wochenende",
  "administratives/",
  "angebote",
  "schwerpunkte/",
  "zeit-magazin/",
  "https://www.zeit.de/2025/01/index",
  
  "https://www.zeit.de/digital/index",
  "https://www.zeit.de/exklusive-zeit-artikel",
  "https://www.zeit.de/hamburg/index",
  "https://www.zeit.de/gesundheit/index",

  "https://www.zeit.de/zona/audio/playlists/btw-playlist"

  
  

)


# -- B. General exclusion list for exact URLs --
excluded_links <- c(
  "https://www.augsburger-allgemeine.de/sport/fc-augsburg/fc-augsburg-unter-thorup-jubilaeum-und-chance-auf-ungeschlagene-serie-gegen-gladbach-106034005", 
  "https://www.augsburger-allgemeine.de/neu-ulm/voehringen-unbekannter-tritt-autofahrer-unvermittelt-gegen-den-kopf-105096858",
  
  "https://www.ndr.de/ndrblue/index.html",
  
  "https://www.badische-zeitung.de/die-mondaensten-museen-der-region",
  "https://www.badische-zeitung.de/romantische-ausflugsziele-fuer-den-valentinstag",
  "https://www.badische-zeitung.de/insolvenzen-bei-biopulver-und-monte-ziego",
  "https://www.badische-zeitung.de/das-sind-die-lieblingsplaetze-der-bz-leserinnen-und-leser",
  "https://www.badische-zeitung.de/die-skurrilsten-museen-in-suedbaden",
  "https://www.badische-zeitung.de/hier-kann-man-kutsche-fahren",
  
  
  "https://www.businessinsider.de/themen/julia-boesch", 

  "https://www.ndr.de/ndr1niedersachsen/epg/Kulturspiegel-,sendung1517892.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Kulturspiegel-,sendung1516624.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Neuer-Jahrgang-Als-Quereinsteiger-zur-Bundeswehr,bundeswehr2924.html",
  "https://www.ndr.de/nachrichten/mecklenburg-vorpommern/haff-mueritz/Telefonbetrueger-geben-sich-als-Mitarbeiter-von-Banken-aus,mvregioneubrandenburg2664.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Kulturspiegel-,sendung1508214.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Kulturspiegel-,sendung1510808.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Kulturspiegel-,sendung1504320.html",
  "https://www.ndr.de/der_ndr/presse/mitteilungen/NDR-Northvolt-war-laut-Regierungsgutachten-bei-Foerderzusage-offenbar-noch-weit-von-Serienreife-entfernt,pressemeldungndr24964.html",
  "https://www.ndr.de/ndr1niedersachsen/Wildes-Wunsch-Wochenende-nur-bei-NDR-1-Wilde-Titel-und-5000-Euro-Traumreise,wunschhits990.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Plattdeutsch,sendung1508184.html",
  "https://www.ndr.de/nachrichten/mecklenburg-vorpommern/Fernwaerme-in-MV-Monopol-Abzocke-oder-Zukunftstechnologie,fernwaerme340.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Plattdeutsch,sendung1504254.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Kulturspiegel-,sendung1512746.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Kulturspiegel-,sendung1506470.html",
  "https://www.ndr.de/nachrichten/info/Northvolt-bei-Foerderzusage-wohl-noch-weit-von-Serienreife-entfernt,northvolt538.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Plattdeutsch,sendung1517838.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Kulturspiegel-,sendung1520770.html",
  "https://www.ndr.de/der_ndr/unternehmen/rundfunkrat/Hauke-Jagau,jagau124.html",
  "https://www.ndr.de/wellenord/sendungen/am_morgen/Werden-Sie-die-Leuchte-des-Morgens,leuchte114.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Plattdeutsch,sendung1512804.html",
  "https://www.ndr.de/der_ndr/unternehmen/organisation/Datenschutz-beimNDR,datenschutz6.html",
  "https://www.ndr.de/ndr1niedersachsen/sendungen/Der-Hit-Jackpot-mitmachen-und-gewinnen,hitjackpot100.html",

  "https://www.badische-zeitung.de/insolvenz-bei-biopulver-und-monte-ziego",
  "https://www.badische-zeitung.de/kuriose-museen",
  "https://www.badische-zeitung.de/mondaene-museen",
  "https://www.badische-zeitung.de/leserfotos-x7x",
  "https://www.badische-zeitung.de/loipen-x1x",
  "https://www.badische-zeitung.de/kutschfahrten-x1x",
  "https://www.badische-zeitung.de/lokale-mit-sonnenterrasse",
  "https://www.badische-zeitung.de/valentinstag-x5x",
  
  "https://www.badische-zeitung.de/die-schoensten-loipen-im-schwarzwald",
  "https://www.badische-zeitung.de/sonne-tanken-lokale-zum-draussensitzen",
  
  "https://www.bild.de/buehne/7AvnA0H1riXTmjP1kiPl",
  "https://www.bild.de/schlank-pass/IWa3EkMo6ZyG2bWIrjCJ",
  "https://www.bild.de/partner/erotik/erotik/dicke-oster-eier-laraiza-blondes-super-bunny-kuemmert-sich-um-dich-87601894.bild.html",
  "https://www.bild.de/partner/erotik/visit-x/erotische-angebote-83370372.bild.html",
  "https://www.bild.de/sport/fussball/bochum-bvb-kovac-erklaert-warum-die-dortmund-stars-nach-der-pleite-schweigen-67b0d986a918eb195a71cb8e#fromWall",
  "https://www.bild.de/buehne/jpFkSCuAE0zCK7wHncJQ",
  "https://www.bild.de/buehne/jhuFd75rS2OheKgh3YsU",
  "https://www.bild.de/partner/unterhaltung/90s-Super-Show",
  "https://www.bild.de/news/5-jahre-corona",
  "https://www.bild.de/partner/ratgeber/advertorial/meta-quest-3-mixed-reality-fuer-alle-86030654.bild.html",
  "https://www.bild.de/ratgeber/job-karriere/job-und-karriere/jobs-fuer-deutschland-81902054.bildMobile.html",
  "https://www.bild.de/ratgeber/gesundheit/vision-zero-j4nbVH2yduHWxxB9YyP7",
  "https://www.bild.de/ratgeber/gesundheit/vision-zero-9iGuTw3YPPY6PeRGXqtj",
  
  
  
  
  "https://www.berliner-kurier.de/",
  "https://www.berliner-kurier.de/topics", 
  "https://www.berliner-kurier.de/kuendigung.297245",
  "https://www.berliner-kurier.de/fuechse/fuechse-berlin-feiern-wichtigen-sieg-gegen-fredericia-li.2295103",
  
  "https://www.berliner-zeitung.de/sponsored/gruene-woche-2025-ein-event-fuer-die-sinne-im-herzen-berlins-li.2288999", 
  "https://www.berliner-zeitung.de/sport-leidenschaft/1-fc-union-berlin/wie-startet-der-1-fc-union-berlin-gegen-den-fsv-mainz-05-in-die-rueckrunde-li.2289983",
  
  "https://bnn.de/format/scheitern",
  
  "http://www.br.de/franken/inhalt/nachrichten/index.html",
  "http://www.br.de/radio/bayern2/sendungen/weltempfaenger/index.html",
  "http://www.br.de/radio/bayern2/sendungen/bayern-2-am-samstagvormittag/index.html",
  "http://www.br.de/frech-und-frei-naerrisches-aus-franken/index.html",
  "http://www.br.de/radio/bayern2/sendungen/gesundheitsgespraech/index.html",
  "http://www.br.de/puls/programm/internet-girl/index.html",
  "http://www.br.de/naerrische-weinprobe/index.html",
  "http://www.br.de/kinder/hoeren/pumuckl/index.html",
  "http://www.br.de/puls/programm/puls-radio/index.html",
  "http://www.br.de/radio/bayern2/podcasts/uwe-timm-ikarien/index.html",
  "http://www.br.de/kinder/hoeren/index.html",
  "http://www.br.de/fastnacht-in-franken/index.html",
  "http://www.br.de/franken/inhalt/kultur/index.html",
  "http://www.br.de/kinder/schauen/pumuckl/index.html",
  
  "https://www.mopo.de/purple_issue/",
  "https://www.mopo.de/hamburg/gericht/",
  "https://www.mopo.de/hamburg/polizei/",
  
  "https://bnn.de/nachrichten/baden-wuerttemberg/mehrere-unfaelle-nach-wintereinbruch-im-baden-wuerttemberg",
  "https://bnn.de/nachrichten/baden-wuerttemberg/urteil-im-hoeri-mordprozess-erwartet",
  "https://bnn.de/nachrichten/deutschland-und-welt/ermittler-gehen-von-islamistischem-motiv-fuer-anschlag-aus",
  "https://bnn.de/nachrichten/wirtschaft/aldi-sued-keine-wurst-aus-unterster-haltungsform-mehr",
  "https://bnn.de/nachrichten/deutschland-und-welt/papst-franziskus-im-krankenhaus",
  "https://bnn.de/nachrichten/wirtschaft/streit-in-der-wohnungseigentuemergemeinschaft-wer-zahlt-was",
  "https://bnn.de/nachrichten/deutschland-und-welt/soldat-gesteht-mordserie-im-landkreis-rotenburg",
  "https://bnn.de/nachrichten/baden-wuerttemberg/verletzte-polizisten-bei-demonstration-in-lahr",
  "https://bnn.de/nachrichten/deutschland-und-welt/steinmeier-und-baerbock-treffen-us-vizepraesident-vance",
  "https://bnn.de/sport/wirtz-gegen-musiala-kompany-spricht-ueber-eine-seltenheit", 
  "https://bnn.de/nachrichten/baden-wuerttemberg/schuesse-in-goeppinger-bar-verdaechtiger-wegen-mordes-in-haft",
  "https://bnn.de/sport/leverkusen-unter-druck-bayern-winkt-titel-vorentscheidung",
  "https://bnn.de/nachrichten/kultur/tilda-swinton-ich-bin-eine-grosse-bewunderin-von-bds",
  
  "https://www.moz.de/nachrichten/panorama/papst-franziskus-aktuell-vatikan-gibt-gesundheits-update-10032025-77909437.html",
  "https://www.swp.de/lokales/balingen/b463-anhaenger-mit-baumstaemmen-stuerzt-um-77851069.html",
  "https://www.swp.de/panorama/papst-franziskus-aktuell-wie-geht-es-ihm-heute-77876119.html",
  "https://www.wz.de/nrw/krefeld/missbrauch-an-grundschulen-in-krefeld-diese-massnahmen-werden-ergriffen_aid-123101741", 
  "https://www.zeit.de/2025/07/playlist",
  "https://www.swp.de/panorama/eurojackpot-zahlen-heute-das-sind-die-gewinnzahlen-am-freitag-21-02-25-77849679.html",
  "https://www.swp.de/panorama/papst-franziskus-aktuell-update-aus-dem-krankenhaus-wie-geht-es-dem-papst-heute-77862262.html",
  "https://www.swp.de/unterhaltung/tv/grill-den-henssler-2025-start-sendetermine-stream-77803418.html",
  "https://www.swp.de/panorama/papst-franziskus-aktuell-neue-diagnose-sorgt-fuer-besorgnis-77865620.html",
  "https://www.swp.de/panorama/papst-franziskus-aktuell-wie-geht-es-ihm-heute-77867018.html", 
  
  
  "https://www.swr3.de/aktuell/nachrichten/t-rex-teen-dino-badlands-100.html",
  "https://www.swr3.de/aktuell/nachrichten/oktopus-wagen-mainz-pfandflaschen-102.html",
  
  "https://www.swr.de/swraktuell/rheinland-pfalz/trier/frau-soll-rolex-und-goldbarren-von-ex-parnter-gestohlen-haben-prozess-amtsgericht-trier-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/suedbaden/polizeischutz-nach-mordrohungen-pole-dance-in-strassburger-kirche-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/bierwoche-in-kirchheimbolanden-im-donnersbergkreis-100.html",
  
  "https://www.derwesten.de/sport/1-bundesliga",
  
  "https://www.berliner-zeitung.de/",
  "https://www.berliner-zeitung.de/topics",
  
  "https://www.echo24.de/leben/verbraucher/neue-sammlermuenzen-ab-2025-tabelle-zeigt-ausgabetermine-selten-wertvoll-liste-93377319.html", 
  "https://www.epochtimes.de/politik/ausland/die-verfahren-gegen-donald-trump-eine-chronik-a5009546.html",
  
  "https://www.epochtimes.de/politik/deutschland/bundesregierung-streicht-charterfluege-von-afghanistan-nach-deutschland-a5041340.html",
  "https://www.epochtimes.de/politik/ausland/buerokratieabbau-was-ist-usaid-und-warum-trump-sie-schliessen-will-a5035464.html",
  "https://www.epochtimes.de/politik/deutschland/bundesregierung-will-gefaehrliche-drohnen-abschiessen-a4999722.html",
  
  "https://www.businessinsider.de/abo/family",
  
  "https://rp-online.de/nrw/staedte/mettmann/mettmann-nach-zahnpasta-dieben-wird-per-foto-gefahndet_aid-124171635",
  
  "https://www.noz.de/service",
  "https://www.noz.de/deutschland-welt/politik/bundestagswahl/alice-weidel",
  "https://www.noz.de/deutschland-welt/politik/bundestagswahl/olaf-scholz",
  "https://www.noz.de/deutschland-welt/politik/bundestagswahl/robert-habeck",
  "https://www.noz.de/lebenswelten/geld-verbraucher/noz-advertorial-vermoegenstag",
  "https://www.noz.de/sport/amateurfussball-os/hallenfussball-os",
  "https://www.noz.de/sport/ergebnisse-tabellen/fussball/kreisliga-os",
  "https://www.noz.de/sport/fussball/champions-league",
  "https://www.noz.de/sport/vfl-osnabrueck/spielplan",
  "https://www.noz.de/sport/vfl-osnabrueck/tabelle",
  
  "https://bnn.de/kraichgau/bruchsal/oberhausen-rheinhausen/einbruch-in-ein-wohnhaus-in-oberhausen-rheinhausen",
  "https://bnn.de/mittelbaden/baden-baden/fruehlingsgefuehle-in-baden-baden-da-schmeckt-auch-das-eis",
  "https://bnn.de/karlsruhe/ettlingen/glosse-was-wir-vom-tapferen-schneemann-in-ettlingen-lernen-koennen",
  "https://bnn.de/karlsruhe/sabine-wittwers-ksc-ole-ole-der-dauerbrenner-im-wildpark",
  "https://bnn.de/nachrichten/deutschland-und-welt/das-waren-die-skurrilsten-auftritte-beim-esc-2024-in-malmoe",
  "https://bnn.de/nachrichten/deutschland-und-welt/bauhaus-dessau-das-weltkulturerbe-mit-der-kamera-eingefangen",
  "https://bnn.de/mittelbaden/buehl/wohnungseinbruch-in-buehl-taeter-verursachen-2000-euro-schaden",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/demonstration-gegen-hass-und-hetze",
  "https://bnn.de/nachrichten/baden-wuerttemberg/unfallflucht-in-wildberg-endet-mit-zweitem-unfall-und-36000-euro-schaden",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/meinung-im-alltag-steckt-viel-hoffnung-auch-in-karlsruhe",
  "https://bnn.de/pforzheim/enzkreis/bildergalerie-24-nachtumzug-in-schellbronn",
  "https://bnn.de/nachrichten/deutschland-und-welt/muenster-in-westfalen-mit-kamera-auf-suche-nach-schoensten-motiven",
  "https://bnn.de/karlsruhe/ettlingen/bnn-leser-an-der-kamera-die-schoensten-wetterfotos-aus-ettlingen-und-umgebung",
  "https://bnn.de/nachrichten/baden-wuerttemberg/neues-am-mittag-raser-rekord-in-karlsruhe-illegale-bauschutt-deponie-nahe-der-unterstmatt",
  "https://bnn.de/nachrichten/baden-wuerttemberg/neues-am-morgen-holocaust-gedenktag-in-karlsruhe-hoffen-auf-geld-fuer-world-games",
  "https://bnn.de/nachrichten/politik/verbot-von-killerrobotern-ein-ganz-dickes-brett",
  "https://bnn.de/karlsruhe/ettlingen/einfach-nur-zum-lachen-warum-die-humor-kuren-der-fastnachter-in-der-kaugummi-kampagne",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/fotogalerie-fastnachtsleckereien-in-badischen-baeckereien",
  "https://bnn.de/kraichgau/bruchsal/das-leben-der-bruchsaler-influencerin-vanessa-schmitt-in-bildern",
  "https://bnn.de/pforzheim/enzkreis/wurmberg",
  "https://bnn.de/karlsruhe/drei-polizisten-bei-einsatz-in-karlsruher-wohnung-verletzt",
  "https://bnn.de/karlsruhe/ettlingen/karlsbad/geschwindigkeitskontrollen-bei-karlsbad-200-verstoesse-festgestellt",
  "https://bnn.de/nachrichten/politik/hitzige-und-spannende-tv-viererunde-bei-rtl-zwischen-kanzlerkandidaten",
  "https://bnn.de/mittelbaden/gaggenau/ein-neuer-anlauf-fuer-alte-fastnachtstraditionen-in-gaggenau",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/meinung-so-trifft-man-in-karlsruhe-eine-sehr-gute-wahl",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/galerie-welche-faschingskostueme-in-karlsruhe-gefragt-sein-koennten",
  "https://bnn.de/mittelbaden/baden-baden/opernhaus-mal-anders-takeover-festival-im-festspielhaus-baden-baden",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/daxlanden/polizei-sucht-zeugen-nach-einbruch-in-einfamilienhaus-in-karlsruhe",
  "https://bnn.de/organisation/baden-badener-philharmonie",
  "https://bnn.de/nachrichten/meinung/meinung-einhorn-mayo-lika-minister-mentrup-quatsch-aus-karlsruhe",
  "https://bnn.de/pforzheim/radfahrer-fluechtet-nach-unfall-mit-fussgaenger-in-pforzheim",
  "https://bnn.de/kraichgau/bretten/mit-schwung-ins-neue-jahr-erfolgreicher-auftakt-im-jazz-club-bretten",
  "https://bnn.de/nachrichten/deutschland-und-welt/esc-die-sieger-der-vergangenen-zehn-jahre",
  "https://bnn.de/nachrichten/baden-wuerttemberg/neues-am-mittag-so-laeuft-der-endspurt-bei-drei-baden-badener-luxushotels",
  "https://bnn.de/organisation/philharmonie-baden-baden",
  "https://bnn.de/mittelbaden/rastatt/muggensturm/meinung-nachhaltigkeit-zahlt-sich-fuer-muggensturmer-syrienhilfe-aus",
  "https://bnn.de/karlsruhe/ettlingen/braende-ueberschatten-jahreswechsel-dennoch-gibt-es-in-der-region-um-ettlingen-grund-fuer-zuversicht",
  "https://bnn.de/pforzheim/enzkreis/neuhausen/einbruch-in-pforzheim-steinegg-unbekannte-entwenden-tresore",
  "https://bnn.de/karlsruhe/polizei-sucht-zeugen-nach-versuchtem-handtaschenraub",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/meinung-die-karlsruher-zeigen-beim-bnn-wahlforum-gespraechskultur",
  "https://bnn.de/karlsruhe/80-jaehriger-aus-karlsruhe-gibt-falschen-polizisten-30000-euro",
  "https://bnn.de/pforzheim/betrunkener-e-scooter-fahrer-kollidiert-in-pforzheim-mit-glastuer",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/kinderkonzert-begeistert-mit-peter-und-der-wolf-am-badischen-staatstheater-in-karlsruhe",
  "https://bnn.de/mittelbaden/baden-baden/gestohlener-mercedes-nach-unfall-in-baden-baden-aufgefunden",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/durlach/demonstration-gegen-afd-veranstaltung-in-karlsruhe-durlach",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/meinung-zu-viel-politik-tauchen-sie-in-karlsruhe-einfach-mal-ab",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/baumbesetzung-am-staatstheater",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/weiherfeld-dammerstock",
  "https://bnn.de/kraichgau/bruchsal/bruchsaler-wahlkampf-ist-der-wahlausgang-wirklich-schon-sicher",
  "https://bnn.de/nachrichten/deutschland-und-welt/energie-sparen-mit-diesen-tricks-sparen-sie-im-haushalt-geld",
  "https://bnn.de/karlsruhe/ettlingen/guenther-oettinger-mahnt-in-ettlingen-deutsche-wirtschaft-ist-sanierungsfall",
  "https://bnn.de/karlsruhe/ettlingen/glosse1",
  "https://bnn.de/mittelbaden/baden-baden/wunderlicht-am-nachthimmel-der-halo-mond-ueber-mittelbaden",
  "https://bnn.de/karlsruhe/karlsruhe-stadt/in-persoenlichen-notlagen-zeigt-sich-der-zusammenhalt-der-karlsruher-gesellschaft",
  "https://bnn.de/kraichgau/bretten/bretten-freundschaft-haelt-auch-nach-dem-gefaengnis",
  
  "https://www.nordkurier.de/regional/nordwestmecklenburg/darum-fuehrte-dieser-mann-mehrere-kriegsschiffe-in-seine-heimatstadt-3276111",
  "https://www.ruhr24.de/promi-tv/nord-bei-nordwest-ard-neue-folgen-staffel-bergdoktor-andrea-gerhard-programm-tv-zdf-93495151.html",
  "https://www.ruhr24.de/service/rueckruf-spielzeug-warnung-eltern-produkt-gefahr-ersticken-kinder-umtausch-zeichentafel-familie-93514231.html",
  
  "https://www.wa.de/kino/beginnt-mit-beziehungskrise-zdf-bergdoktor-18-staffel-hans-sigl-93492366.html",
  
  "https://newsflash24.de/ratgeber/finanzen",
  "https://newsflash24.de/ratgeber/gesundheit",
  
  "https://www.rollingstone.de/locations.kml",
  "https://www.rollingstone.de/artists",
  
  # all of those are navigational / carry no content 
  "https://www.rtl.de/cms/service.html",
  "https://www.rtl.de/freizeit/garten",
  "https://www.rtl.de/trending/videos", 
  "https://www.rtl.de/ratgeber/digitales",
  "https://www.rtl.de/regionale-nachrichten/nordrhein-westfalen",
  "https://www.rtl.de/geld/black-friday-angebote",
  "https://www.rtl.de/rtl-hessen/ueber-uns",
  "https://www.rtl.de/regionale-nachrichten/niedersachsen",
  "https://www.rtl.de/rtl-west/ganze-folgen",
  "https://www.rtl.de/geld/shopping-service",
  "https://www.rtl.de/regionale-nachrichten/schleswig-holstein",
  "https://www.rtl.de/ratgeber/tools",
  "https://www.rtl.de/rtl-nord/niedersachsen-bremen",
  "https://www.rtl.de/ratgeber/energie",
  "https://www.rtl.de/rtl-hessen/news",
  "https://www.rtl.de/corporate/schlagerliebe",
  "https://www.rtl.de/ratgeber/tiere",
  "https://www.rtl.de/leben/beauty",
  "https://www.rtl.de/rtl-west/ueber-uns",
  "https://www.rtl.de/ratgeber/verbrauchertests",
  "https://www.rtl.de/ratgeber/gesundheit/krankheiten",
  "https://www.rtl.de/geld/sparen",
  "https://www.rtl.de/ratgeber/vielfalt",
  "https://www.rtl.de/regionale-nachrichten/brandenburg",
  "https://www.rtl.de/regionale-nachrichten/hamburg",
  "https://www.rtl.de/regionale-nachrichten/mecklenburg-vorpommern",
  "https://www.rtl.de/ratgeber/liebe",
  "https://www.rtl.de/rtl-west/videos",
  "https://www.rtl.de/rtl-hessen/videos",
  "https://www.rtl.de/leben/reisen",
  "https://www.rtl.de/rtl-hd/sendungen-in-uhd",
  "https://www.rtl.de/ratgeber/gesundheit/schwangerschaft",
  "https://www.rtl.de/leben/wohnen-diy",
  "https://www.rtl.de/leben/wissens-quiz",
  "https://www.rtl.de/news/videos",
  "https://www.rtl.de/ratgeber/familie/baby",
  "https://www.rtl.de/regionale-nachrichten/bremen",
  "https://www.rtl.de/ratgeber/gesundheit",
  "https://www.rtl.de/regionale-nachrichten/baden-wuerttemberg",
  "https://www.rtl.de/leben/fitness",
  "https://www.rtl.de/rtl-hessen/ganze-folgen",
  "https://www.rtl.de/rtl-nord/hamburg-schleswig-holstein",
  "https://www.rtl.de/regionale-nachrichten/saarland",
  "https://www.rtl.de/regionale-nachrichten/thueringen",
  "https://www.rtl.de/leben/mode",
  "https://www.rtl.de/ratgeber/gesundheit/docfleck",
  "https://www.rtl.de/ratgeber/familie/erziehung",
  "https://www.rtl.de/leben/essen-trinken",
  "https://www.rtl.de/ratgeber/haushalt",
  "https://www.rtl.de/rtl-west/themen",
  "https://www.rtl.de/ratgeber/nachhaltigkeit",
  "https://www.rtl.de/geld/job",
  "https://www.rtl.de/regionale-nachrichten/bayern",
  "https://www.rtl.de/ratgeber/gesundheit/gesundheitslexikon",
  "https://www.rtl.de/regionale-nachrichten/rheinland-pfalz",
  "https://www.rtl.de/ratgeber/familie",
  "https://www.rtl.de/ratgeber/gesundheit/abnehmen",
  "https://www.rtl.de/regionale-nachrichten/sachsen-anhalt",
  "https://www.rtl.de/regionale-nachrichten/berlin",
  "https://www.rtl.de/ratgeber/psychologie",
  "https://www.rtl.de/news/bundestagswahl-2025",
  "https://www.rtl.de/leben/reisen/urlaubsretter",
  "https://www.rtl.de/regionale-nachrichten/sachsen",
  "https://www.rtl.de/leben/horoskop",
  "https://www.rtl.de/regionale-nachrichten/hessen",
  "https://www.rtl.de/geld/amazon-prime-day-angebote",
  "https://www.rtl.de/ratgeber/gesundheit/vorsorge",
  "https://www.rtl.de/rtl-nord/service",
  "https://www.rtl.de/rtl-nord/ganze-folgen",
  
  "https://www.heidelberg24.de/heidelberg/neuenheimer-feld-wehrsteg-wieblingen-erneuert-neckar-ueberquerung-sanierung-radfahrer-fussgaenger-bergheim-93565332.html",
  
  "https://www.karlsruhe-insider.de/news/gruenes-licht-millionen-buerger-erhalten-bald-500-euro-zuschuss-233172",
  "https://www.karlsruhe-insider.de/uebernachtung",
  
  "https://www.wz.de/nrw/krefeld/missbrauc",
  
  "https://www.tag24.de/unterhaltung/tv/blutige-anfaenger",
  "https://www.tag24.de/unterhaltung/promis/elyas-m-barek",
  "https://www.tag24.de/unterhaltung/promis/angelina-pannek",
  "https://www.tag24.de/unterhaltung/promis/johnny-depp",
  "https://www.tag24.de/unterhaltung/tv/reschke-fernsehen",
  "https://www.tag24.de/unterhaltung/tv/promis-unter-palmen/explosiver-auftakt-bei-promis-unter-palmen-zoff-zwischen-iris-klein-und-yvonne-woelke-3351996",
  "https://www.tag24.de/unterhaltung/tv/promis-unter-palmen/von-lotto-millionaer-bis-dsds-saengerin-promis-unter-palmen-kommt-zurueck-3348868",
  "https://www.tag24.de/unterhaltung/tv/promis-unter-palmen/dritte-staffel-promis-unter-palmen-startet-mit-pikanten-ueberraschungen-3359008",
  
  
  "https://www.volksstimme.de/wahl/kommunalwahl/kommunalwahl-jerichower-land/online-portal-fur-briefwahl-in-gommern-3979624",
  
  
  
  # all those contain video, pictures, podcasts, topics or announcements 
  "https://daserste.ndr.de/annewill/media/Richtig-ihn-im-Amt-zu-belassen,annewill8078.html",
  "https://www.ndr.de/kultur/epg/Becoming-The-Beatles,sendung1511214.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1521210.html",
  "https://www.ndr.de/kultur/epg/Der-suesse-Wahn-12,sendung1508910.html",
  "https://www.ndr.de/der_ndr/presse/mappen/index.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1517934.html",
  "https://www.ndr.de/ratgeber/garten/Farbige-Beeren-und-Blueten-mitten-im-Winter,gartenblog954.html",
  "https://www.ndr.de/ndr2/epg/Alle-Spiele-alle-Tore-und-die-Schlusskonferenz,sendung1515646.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Zerfall-Babylons-mit-Volker-Kutscher-durch-Berlin,sendung1521740.html",
  "https://www.ndr.de/kultur/buehne/haeuser/Hinter-den-Kulissen-der-Hamburger-Stage-School,stageschoolhamburg100.html",
  "https://www.ndr.de/nachrichten/niedersachsen/braunschweig_harz_goettingen/Wer-bekommt-meine-Stimme-Ueber-1000-Schueler-beim-Kandidatencheck,erstwaehler174.html",
  "https://www.ndr.de/kultur/kunst/hamburg/Eindruecke-von-der-Ausstellung-Bodies-of-Ambivalence,bodiesofambivalence102.html",
  "https://www.ndr.de/kultur/epg/Haubolds-Soloalbum,sendung1517418.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1527052.html",
  "https://www.ndr.de/nachrichten/info/epg/Frag-dich-fit-mit-Doc-Esser-und-Anne,sendung1518632.html",
  "https://www.ndr.de/nachrichten/info/epg/Kumbh-Mela-Das-groesste-Fest-des-Planeten,sendung1514730.html",
  "https://www.ndr.de/n-joy/events/konzerte/Sean-Paul-in-Hannover,seanpaul390.html",
  "https://www.ndr.de/nachrichten/info/epg/80-Jahre-Befreiung-KZ-Auschwitz,sendung1510010.html",
  "https://www.ndr.de/ratgeber/garten/zierpflanzen/Mit-Blumen-den-Fruehling-ins-Haus-holen,fruehblueher171.html",
  "https://www.ndr.de/nachrichten/info/epg/Papua-Neuguinea-Hoelle-oder-Paradies,sendung1509900.html",
  "https://www.ndr.de/ndr2/epg/Das-NDR-2-Wochenende,sendung1523846.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Niederdeutsches-Hoerspiel,sendung1516560.html",
  "https://www.ndr.de/media/Pferd-im-Fenster,hansesail1410.html",
  "https://www.ndr.de/nachrichten/info/epg/Bradford-Kulturhauptstadt-des-Vereinigten-Koenigreichs-2025,sendung1513332.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Inka-von-Puttkamers-Karriere-in-der-Marine,marine1504.html",
  "https://www.ndr.de/kultur/epg/eatREADsleep,sendung1517394.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Niedersachsen-Host-Story-5,niedersachsenhost194.html",
  "https://www.ndr.de/n-joy/appmedien/Kaeltebus-Nummern,kaeltebusnummern102.html",
  "https://www.ndr.de/nachrichten/info/Wie-die-Elbphilharmonie-waechst,elbphilbaustelle100.html",
  "https://www.ndr.de/n-joy/appmedien/Schutz-vor-Unfaellen-bei-Glatteis,glatteis494.html",
  "https://www.ndr.de/kultur/Kulturpartner-in-Hamburg,kulturpartnerhh104.html",
  "https://www.ndr.de/kultur/epg/Haubolds-Soloalbum,sendung1504554.html",
  "https://www.ndr.de/kultur/epg/Die-drei-Leben-der-Connie-Converse,sendung1511104.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Bundespraesident-Steinmeier-arbeitet-drei-Tage-von-Eckernfoerde-aus,steinmeier1188.html",
  "https://www.ndr.de/kultur/epg/Wintertraeume-aus-Saarbruecken,sendung1506814.html",
  "https://www.ndr.de/sport/fussball/Ein-Herz-und-eine-Seele-Ilka-und-Uwe-Seeler-im-Interview,seeler526.html",
  "https://www.ndr.de/der_ndr/programmangebote/Entdecken-und-Erleben-der-NDR-fuer-Dich,entdeckenerlebendoku114.html",
  "https://www.ndr.de/nachrichten/hamburg/Hamburg-Die-Bundestagswahl-in-Zahlen,bundestagswahlinzahlen104.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Zerfall-Babylons-mit-Volker-Kutscher-durch-Berlin,sendung1518658.html",
  "https://www.ndr.de/nachrichten/info/podcast4396.html",
  "https://www.ndr.de/kultur/epg/Solidarisch-preppen,sendung1508906.html",
  "https://www.ndr.de/kultur/epg/Musica,sendung1520086.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Brandstiftung-und-Zerstoerung-Unbekannte-randalieren-in-Kappeln,kappeln308.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Podcast-Mein-Einsatz,sendung1517874.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Presseclub,sendung1514718.html",
  "https://www.ndr.de/nachrichten/hamburg/Diese-Dinge-wurden-2024-im-Hamburger-Fundbuero-abgegeben,fundsachen138.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1517896.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1522194.html",
  "https://www.ndr.de/kultur/epg/Hoerspiel,sendung1517396.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1524926.html",
  "https://www.ndr.de/ratgeber/reise/ostseekueste_sh/Hafen-Strand-und-Ostsee-Ausflug-nach-Neustadt-in-Holstein,neustadt226.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Erster-Schneefall-2025-in-Schleswig-Holstein,schnee4344.html",
  "https://www.ndr.de/ratgeber/garten/zierpflanzen/Diese-Pflanzen-haben-eine-schoene-Herbstfaerbung,herbstfaerbung100.html",
  "https://www.ndr.de/nachrichten/info/epg/Singapurs-Rezept-fuer-ein-langes-gesundes-Leben,sendung1514392.html",
  "https://www.ndr.de/sport/fussball/Bundesliga-VfL-Wolfsburg-verliert-bei-Bayern-Muenchen,wolfsburg20494.html",
  "https://www.ndr.de/kultur/Kulturpartner-von-A-Z,kulturpartner510.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1525022.html",
  "https://www.ndr.de/kultur/epg/Urban-Pop,sendung1511130.html",
  "https://www.ndr.de/nachrichten/investigation/peters366.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Die-Bilder-zur-Bundestagswahl-2021-aus-Niedersachsen,bundestagswahl822.html",
  "https://www.ndr.de/der_ndr/empfang_und_technik/satellit/Digitalfernsehen-ueber-DVB-SS2,digitalfernsehen108.html",
  "https://www.ndr.de/nachrichten/info/epg/Island-und-die-Macht-der-Influencer,sendung1509570.html",
  "https://www.ndr.de/ratgeber/Diese-Zeichen-sollten-Wassersportler-kennen,faqpaddeln102.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Zerfall-Babylons-mit-Volker-Kutscher-durch-Berlin,sendung1523606.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1527698.html",
  "https://www.ndr.de/kultur/musik/klassik/festspiele_mv/index.html",
  "https://www.ndr.de/kultur/buch/sachbuecher/Impressionen-aus-dem-Bildband-Deutschland-fuer-Buchverliebte,hampp102.html",
  "https://www.ndr.de/nachrichten/info/epg/Suedafrika-und-der-illegale-Bergbau,sendung1513374.html",
  "https://www.ndr.de/kultur/kunst/mecklenburg-vorpommern/Museen-und-Bildende-Kunst-in-Mecklenburg-Vorpommern,museenmv104.html",
  "https://www.ndr.de/sport/fussball/Der-Augsburg-Schock-oder-Werder-Bremens-Defensiv-Probleme,sportclub14458.html",
  "https://www.ndr.de/sport/fussball/VfL-Fussballerinnen-siegen-klar-gegen-Jena-Borbe-im-Tor,wolfsburg20564.html",
  "https://www.ndr.de/nachrichten/info/epg/Presseclub-Talk-Forward,sendung1520402.html",
  "https://www.ndr.de/kultur/epg/Das-Gespraech,sendung1521202.html",
  "https://www.ndr.de/kultur/epg/Hoerspiel-Anton-und-Pepe-55,sendung1520088.html",
  "https://www.ndr.de/nachrichten/info/epg/Schlussrunde-der-Spitzenkandidaten,sendung1514714.html",
  "https://www.ndr.de/nachrichten/info/sendungen/NDR-Info-Podcasts-Reportagen-und-Recherchen,infopodcastsreportagen100.html",
  "https://www.ndr.de/ratgeber/garten/Slow-Gardening-Zwischen-Haengematte-und-Gemuesegarten,gartenblog942.html",
  "https://www.ndr.de/nachrichten/info/epg/DFB-Pokal-der-Maenner,sendung1526044.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Unser-Thema,sendung1508250.html",
  "https://www.ndr.de/kultur/buch/sachbuecher/Impressionen-aus-dem-Bildband-Warten-auf-Regenbogen,bender206.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Polizei-Kiel-ermittelt-in-den-eigenen-Reihen,regionkielnews1872.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1520774.html",
  "https://www.ndr.de/Seebruecke-Prerow,prerow252.html",
  "https://www.ndr.de/Bilder-vom-Unfall,beverstedt142.html",
  "https://www.ndr.de/sport/fussball/Selkes-Jochbeinbruch-ueberschattet-HSV-Sieg-gegen-Hertha-BSC,sportclub14484.html",
  "https://www.ndr.de/ratgeber/reise/Pilgern-in-Norddeutschland,pilgerwege103.html",
  "https://www.ndr.de/sport/mehr_sport/Achtung-Eisberg-Vendee-Globe-Segler-haben-unerwarteten-Besuch,segeln2242.html",
  "https://www.ndr.de/ndr1niedersachsen/Schorse-liefert-Dabbis-aus,wunschhits1062.html",
  "https://www.ndr.de/ndr1niedersachsen/So-sieht-es-im-Studio-aus,wunschhits1024.html",
  "https://www.ndr.de/nachrichten/niedersachsen/hannover_weser-leinegebiet/Waehlen-vor-dem-Wahltag-Grosses-Interesse-an-Briefwahl-in-Hannover,btw146.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1527658.html",
  "https://www.ndr.de/nachrichten/mecklenburg-vorpommern/westmecklenburg/Kamera-Zwischenloesung-fuer-den-Schweriner-Marienplatz,mvregioschwerin3012.html",
  "https://www.ndr.de/nachrichten/niedersachsen/oldenburg_ostfriesland/Elektro-Katamaran-Frisia-E-I-soll-bald-in-Betrieb-gehen,katamaran268.html",
  "https://www.ndr.de/sport/fussball/Werder-Bremens-Abwehrschwaechen-von-RB-Leipzig-eiskalt-bestraft-,werder16272.html",
  "https://www.ndr.de/nachrichten/info/epg/Plusminus-Mehr-als-nur-Wirtschaft,sendung1515984.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1523880.html",
  "https://www.ndr.de/kultur/epg/Feature-OZ-22,sendung1520002.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Das-sagen-die-schleswig-holsteinischen-Spitzenkandidaten-zum-Thema-Wirtschaft,wahlen722.html",
  "https://www.ndr.de/nachrichten/info/epg/ARD-ZDF-das-TV-Duell,sendung1514398.html",
  "https://www.ndr.de/kultur/epg/Im-Koenigreich-Deutschland,sendung1517326.html",
  "https://www.ndr.de/kultur/epg/Musica,sendung1519914.html",
  "https://www.ndr.de/ndr2/epg/NDR-2-Spezial,sendung1511306.html",
  "https://www.ndr.de/kultur/epg/ARD-Konzert,sendung1511180.html",
  "https://www.ndr.de/nachrichten/info/epg/Frag-dich-fit-mit-Doc-Esser-und-Anne,sendung1516652.html",
  "https://www.ndr.de/n-joy/appmedien/WhatsApp-Funktionen,whatsappfunktion102.html",
  "https://www.ndr.de/leipertz100.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Visite-,sendung1518976.html",
  "https://www.ndr.de/kultur/epg/Der-einaeugige-Karpfen,sendung1511224.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1524982.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1524396.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1790104.html",
  "https://www.ndr.de/nachrichten/info/So-wars-bei-Gruss-an-Bord-in-Hamburg,grussanbord914.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1527170.html",
  "https://www.ndr.de/nachrichten/info/sendungen/zwischen_hamburg_und_haiti/Das-kulturelle-Leben-auf-Jiddisch-in-New-York-,jiddischnewyork100.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1523888.html",
  "https://www.ndr.de/nachrichten/info/epg/Frag-dich-fit-mit-Doc-Esser-und-Anne,sendung1526856.html",
  "https://www.ndr.de/nachrichten/info/infospezial/epg/WeltMachtChina,sendung1516654.html",
  "https://www.ndr.de/n-joy/events/konzerte/Camila-Cabello-in-Hamburg,camilacabello104.html",
  "https://www.ndr.de/kultur/epg/Musica,sendung1519736.html",
  "https://www.ndr.de/sport/fussball/St-Pauli-spielt-gegen-Augsburg-unentschieden,video12232.html",
  "https://www.ndr.de/ratgeber/reise/wattenmeer/Urlaubsorte-am-Wattenmeer,wattenmeer277.html",
  "https://www.ndr.de/sport/mehr_sport/Boris-Herrmann-Es-koennten-ziemlich-heftige-Wellen-werden,segeln2532.html",
  "https://www.ndr.de/nachrichten/info/epg/Die-Entscheidung,sendung1514892.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Zerfall-Babylons-mit-Volker-Kutscher-durch-Berlin,sendung1514886.html",
  "https://www.ndr.de/nachrichten/info/sendungen/zwischen_hamburg_und_haiti/Lofoten-in-Norwegen-Urlaubsort-fuer-Naturliebhaber,lofoten300.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Presseclub,sendung1507810.html",
  "https://www.ndr.de/der_ndr/empfang_und_technik/dab-informationen,dabregionen100.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1790090.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1516012.html",
  "https://www.ndr.de/n-joy/team/Team-N-JOY-Wir-ueber-uns,portraets154.html",
  "https://www.ndr.de/nachrichten/info/epg/Das-Jahr-in-dem-Trump-zurueckkehrt,sendung1507868.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1522228.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1520810.html",
  "https://www.ndr.de/kultur/film/Hamburg-Empfang-bei-der-Berlinale-2025-,moinberlinale128.html",
  "https://www.ndr.de/kultur/epg/Aller-guten-Dinge-sind-23-die-Artemis-Quartett-Edition,sendung1452006.html",
  "https://www.ndr.de/kultur/oelschlegel114.html",
  "https://www.ndr.de/kultur/musik/hurricane_festival/Hurricane-Festival-2023-Polizei-zieht-positive-Bilanz,hurricane4474.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Duet-un-dat-op-Platt,sendung1527706.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Auto-in-Kieler-Wohnhaus-gekracht-Bewohner-hoffen-auf-Versicherung,unfall19212.html",
  "https://www.ndr.de/ndr2/epg/Das-NDR-2-Wochenende,sendung1523544.html",
  "https://www.ndr.de/nachrichten/info/podcast4398.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1524432.html",
  "https://www.ndr.de/ratgeber/reise/tierparks/Loewe-Giraffe-Nashorn-Co-im-Zoo-Osnabrueck,zooosnabrueck129.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Duet-un-dat-op-Platt,sendung1520784.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1510416.html",
  "https://www.ndr.de/ratgeber/reise/vorpommern/Rundgang-durch-die-Altstadt-in-Stralsund,stralsund255.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-KI-Podcast,sendung1517658.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Nachrichten-aus-Pinneberg-Segeberg-und-Stormarn,studionorderstedtnews960.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Niedersachsen-Host-Story-7,niedersachsenhost198.html",
  "https://www.ndr.de/sport/fussball/Kiel-gegen-Bochum-Abstiegskampf-pur,video12252.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1517876.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Presseclub,sendung1510054.html",
  "https://www.ndr.de/ndr1niedersachsen/Behindertensportlerin-des-Jahres-die-Nominierten,behindertensportler610.html",
  "https://www.ndr.de/nachrichten/info/epg/Teurer-Fahren,sendung1519726.html",
  "https://www.ndr.de/ratgeber/garten/gartenkalender/Gartentipps-fuer-Februar,februar102.html",
  "https://www.ndr.de/kultur/epg/Das-Konzert,sendung1507298.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Zerfall-Babylons-mit-Volker-Kutscher-durch-Berlin,sendung1525980.html",
  "https://www.ndr.de/ratgeber/reise/weser_weserbergland/Bremen-Heimat-von-Roland-und-Stadtmusikanten,bremen403.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Datawrapper-Uebergabe-Inzidenzraten-Influenza,datawrappernds1108.html",
  "https://www.ndr.de/nachrichten/niedersachsen/hannover_weser-leinegebiet/Das-sagen-Erstwaehler-zur-Bundestagswahl-2025,erstwaehler140.html",
  "https://www.ndr.de/nachrichten/info/epg/Antonias-Weg-aus-der-Depression,sendung1515796.html",
  "https://www.ndr.de/ndr2/epg/Alle-Spiele-alle-Tore-und-die-Schlusskonferenz,sendung1523408.html",
  "https://www.ndr.de/kultur/epg/Kriminalhoerspiel,sendung1507336.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Die-Bundestagswahl-ist-in-vollem-Gange,bundestagswahl1056.html",
  "https://www.ndr.de/kultur/Es-leuchtet-der-Stern,audio1777478.html",
  "https://www.ndr.de/ratgeber/gesundheit/krankheiten/Knoetchenflechte-Bilder-der-Erkrankung,lichenruber104.html",
  "https://www.ndr.de/sport/fussball/Hannover-96-stolpert-gegen-Muenster-im-Aufstiegsrennen,sportclub14480.html",
  "https://www.ndr.de/nachrichten/info/Ev-Gottesdienst-aus-der-Stadtkirche-Zum-heiligen-Namen-Gottes-in-Radeberg,audio1786028.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Waffenschmuggel-40-Pistolen-in-Bad-Oldesloe-sichergestellt,waffen474.html",
  "https://www.ndr.de/ndr2/epg/Alle-Spiele-alle-Tore-und-die-Schlusskonferenz,sendung1515528.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1527730.html",
  "https://www.ndr.de/kultur/epg/ARD-Konzert,sendung1519728.html",
  "https://www.ndr.de/kultur/epg/Dima-Slobodeniouk-dirigiert-die-Berliner-Philharmoniker,sendung1518280.html",
  "https://www.ndr.de/nachrichten/info/epg/Frag-dich-fit-mit-Doc-Esser-und-Anne,sendung1523602.html",
  "https://www.ndr.de/sport/fussball/Eintracht-Coach-Scherning-kein-Fan-vom-VAR-Geht-auch-anders,sportclub14538.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Duet-un-dat-op-Platt,sendung1524496.html",
  "https://www.ndr.de/nachrichten/info/epg/Online-Shopping-bei-Temu,sendung1509526.html",
  "https://www.ndr.de/kultur/film/videos/Trailer-Armand-mit-Renate-Reinsve-Norwegischer-Oscarkandidat,trailerarmand100.html",
  "https://www.ndr.de/kultur/epg/NDR-Kultur-Foyerkonzert-on-tour-in-Celle,sendung1436318.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1520826.html",
  "https://www.ndr.de/sport/fussball/Erfolgsgarant-Hauke-Wahl-St-Paulis-Mister-Zuverlaessig-,wahl2640.html",
  "https://www.ndr.de/kultur/epg/Hoerspiel-Anton-und-Pepe-35,sendung1519786.html",
  "https://www.ndr.de/kultur/epg/Das-Gespraech,sendung1522380.html",
  "https://www.ndr.de/kultur/epg/Kronberg-Festival-2024-Sturm-und-Drang,sendung1507156.html",
  "https://www.ndr.de/nachrichten/niedersachsen/hannover_weser-leinegebiet/Zaehlen-messen-wiegen-Inventur-im-Zoo-Hannover,zooinventur162.html",
  "https://www.ndr.de/kultur/epg/Becoming-The-Beatles,sendung1518320.html",
  "https://www.ndr.de/kultur/Festivals,kpfestival100.html",
  "https://www.ndr.de/nachrichten/niedersachsen/braunschweig_harz_goettingen/Freundsch,brockenbahn302.html",
  "https://daserste.ndr.de/annewill/ANNE-WILL-Die-Sendung-in-Gebaerdensprache,annewill8162.html",
  "https://www.ndr.de/n-joy/appmedien/Serien-Highlights-2025,serienhighlights112.html",
  "https://www.ndr.de/Bilder-der-Faehre-nach-dem-Zusammenstoss,unfall19266.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Visite-,sendung1518970.html",
  "https://www.ndr.de/nachrichten/info/epg/Mangel-an-Menschen-In-der-Ukraine-fehlen-Millionen-Arbeitskraefte,sendung1505960.html",
  "https://www.ndr.de/sport/fussball/Addo-kaempft-gegen-Rassismus-Bessere-Zukunft-fuer-Kinder-schaffen,addo134.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1527068.html",
  "https://www.ndr.de/kultur/epg/Feature-Leeres-Orchester,sendung1507060.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Walkadaver-Abschleppen-zerlegen-praeparieren,pottwal308.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischentoene-Frohe-Weihnachten,audio1777370.html",
  "https://www.ndr.de/sport/fussball/Wolfsburg-Coach-Tommy-Stroot-Der-VfL-ist-sehr-attraktiv,sportclub14526.html",
  "https://www.ndr.de/ndr2/sendungen/morgen/index.html",
  "https://www.ndr.de/nachrichten/info/epg/Buergerschaftswahl-Hamburg-2025-Wahlsondersendung,sendung1516978.html",
  "https://www.ndr.de/nachrichten/info/epg/Buergerschaftswahl-Hamburg-2025-Wahlsondersendung,sendung1516990.html",
  "https://www.ndr.de/nachrichten/mecklenburg-vorpommern/westmecklenburg/Eine-Verbindung-zwischen-Ost-und-West,elbbruecke133.html",
  "https://www.ndr.de/kultur/epg/Hoerspiel-Anton-und-Pepe-45,sendung1520166.html",
  "https://www.ndr.de/kultur/epg/Schwimmen-gegen-den-Strom,sendung1509010.html",
  "https://www.ndr.de/ndr1niedersachsen/Dat-kannst-mi-gloeven-De-Koenige-un-ehr-Boeskup-vann-Freeden-in-de-Welt,audio1777432.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1790100.html",
  "https://www.ndr.de/nachrichten/mecklenburg-vorpommern/rostock/Carport-mit-zwei-Autos-in-Nienhagen-bei-Rostock-abgebrannt,mvregiorostock2504.html",
  "https://www.ndr.de/ratgeber/NDRde-wuenscht-alles-Gute-fuer-Jahr-2025,silvesterindex112.html",
  "https://www.ndr.de/kultur/epg/Urban-Pop,sendung1519790.html",
  "https://www.ndr.de/nachrichten/niedersachsen/lueneburg_heide_unterelbe/Wildpark-Mueden-verfuettert-Weihnachtsbaeume-an-Tiere,wildpark340.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1524488.html",
  "https://www.ndr.de/kultur/Geistliche-Musik-zum-1-Sonntag-nach-Epiphanias,audio1791786.html",
  "https://www.ndr.de/kultur/epg/ARD-Konzert,sendung1508866.html",
  "https://www.ndr.de/kultur/film/-Lieber-Thomas-Drama-mit-Albrecht-Schuch-und-Jella-Haase,lieberthomas108.html",
  "https://www.ndr.de/sport/fussball/Remis-in-Rostock-Die-Tore-von-Hansas-11-gegen-Viktoria-Koeln,hansa12834.html",
  "https://www.ndr.de/kultur/epg/ARD-Konzert,sendung1517352.html",
  "https://www.ndr.de/kultur/musik/hurricane_festival/Hurricane-2016-Zum-Abschluss-kommt-der-Trecker,hurricane2634.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Bundestagswahl-2025,sendung1516636.html",
  "https://www.ndr.de/nachrichten/info/podcast4384.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Zerfall-Babylons-mit-Volker-Kutscher-durch-Berlin,sendung1514884.html",
  "https://www.ndr.de/sport/fussball/St-Pauli-im-Aufwind-Guilavogui-trifft-doppelt-gegen-Union-Berlin,sportclub14490.html",
  "https://www.ndr.de/ndr2/epg/Das-NDR-2-Wochenende,sendung1515934.html",
  "https://www.ndr.de/kultur/epg/Kinderhoerspiel-Der-Geraeuschehaendler,sendung1518418.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Niedersachsen-Host-Story-4,niedersachsenhost184.html",
  "https://www.ndr.de/nachrichten/investigation/vonderheide126.html",
  "https://www.ndr.de/nachrichten/info/epg/DFB-Pokal-der-Maenner,sendung1526042.html",
  "https://www.ndr.de/kultur/epg/Urban-Pop,sendung1526612.html",
  "https://www.ndr.de/ndr1niedersachsen/Dat-kannst-mi-gloeven,audio1787694.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Messerattacke-Elmshorn-Kontrollbereich-um-Bahnhof,elmshorn522.html",
  "https://www.ndr.de/kultur/epg/Hoerspiel-Anton-und-Pepe-25,sendung1518322.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Niederdeutsches-Hoerspiel,sendung1524460.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Martin-Kindl-wird-neuer-Buergermeister-von-Husum,wahlhusum100.html",
  "https://www.ndr.de/ndr2/epg/Das-NDR-2-Wochenende,sendung1515824.html",
  "https://www.ndr.de/sport/fussball/Bundesliga-VfL-Wolfsburg-gewinnt-bei-Hoffenheim,wolfsburg20466.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Duet-un-dat-op-Platt,sendung1527120.html",
  "https://www.ndr.de/nachrichten/niedersachsen/oldenburg_ostfriesland/Vechta-23-Jaehrige-getoetet-Verdaechtiger-in-Untersuchungshaft,aktuelloldenburg12848.html",
  "https://www.ndr.de/ratgeber/reise/mecklenburgische_ostseekueste/Phantechnikum-Technik-zum-Staunen-und-Ausprobieren,phantechnikum131.html",
  "https://www.ndr.de/ndr2/epg/Alle-Spiele-alle-Tore-und-die-Schlusskonferenz,sendung1515964.html",
  "https://www.ndr.de/der_ndr/programmangebote/Entdecken-und-Erleben-der-NDR-fuer-Dich,entdeckenerlebeninformation104.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Niederdeutsches-Hoerspiel,sendung1527186.html",
  "https://www.ndr.de/kultur/epg/Haubolds-Soloalbum,sendung1507254.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Wie-stehen-die-Parteien-zum-Buergergeld,wahlen742.html",
  "https://www.ndr.de/sport/mehr_sport/Fertig-machen-fuer-die-Einfahrt-Malizia-Crew-kommt-zu-Herrmann-an-Bord,segeln2578.html",
  "https://www.ndr.de/ratgeber/reise/flensburg_schlei/Jeder-kann-hier-Forscher-sein-,phaenomenta162.html",
  "https://www.ndr.de/ratgeber/reise/lueneburger_heide/Die-Sehenswuerdigkeiten-der-Fachwerkstadt-Celle,celle386.html",
  "https://www.ndr.de/nachrichten/info/epg/Das-Imperium-Heidi-Klum-Catwalk-zur-Macht,sendung1528920.html",
  "https://www.ndr.de/kultur/buch/sachbuecher/Impressionen-aus-dem-Bildband-Wie-Banksy-die-Kunst-rettete,banksy246.html",
  "https://www.ndr.de/nachrichten/ndrdata/annabehrend100.html",
  "https://www.ndr.de/ndr2/epg/Das-NDR-2-Wochenende,sendung1511374.html",
  "https://www.ndr.de/nachrichten/mecklenburg-vorpommern/Zum-Saisonausklang-Kleine-Eisshow-in-Sellin,mvregiogreifswald2360.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1525000.html",
  "https://www.ndr.de/sport/fussball/Der-FC-St-Pauli-oder-Problem-mit-dem-Toreschiessen,sportclub14436.html",
  "https://www.ndr.de/nachrichten/hamburg/Der-erste-Schnee-in-diesem-Winter-in-Hamburg,hamburgwinter136.html",
  "https://www.ndr.de/der_ndr/programmangebote/Entdecken-und-Erleben-der-NDR-fuer-Dich,entdeckenerlebenkultur110.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1516534.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1786098.html",
  "https://www.ndr.de/nachrichten/info/epg/Wer-hat-Angst-vor-Nordkorea,sendung1513248.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1508698.html",
  "https://www.ndr.de/sport/fussball/Alle-HSV-Trainer-seit-Gruendung-der-Bundesliga,hsv4493.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1517426.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Abenteuer-Diagnose,sendung1514256.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Drogen-und-Diebstahl-Polizei-stellt-Autofahrer-in-Itzehoe-,regionheidenews1776.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1527210.html",
  "https://www.ndr.de/kultur/epg/Das-Gespraech,sendung1517428.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1785232.html",
  "https://daserste.ndr.de/Anja-Reschke-Knicken-wir-jetzt-ein,panorama6018.html",
  "https://www.ndr.de/ratgeber/reise/radtouren/Nordseekuestenradweg-Unterwegs-an-Deichen-und-Duenen,nordseekuestenradweg6.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1517842.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Luebecks-Weltkulturerbe-umrahmt-von-Industriebrachen,industriebrache108.html",
  "https://www.ndr.de/nachrichten/info/epg/Wahlarena-2025-zur-Bundestagswahl,sendung1525986.html",
  "https://www.ndr.de/hand_in_hand_fuer_norddeutschland/DAS-Spezial-Bilder-vom-Roten-Sofa-mit-Bettina-Tietjen,dasspezial126.html",
  "https://www.ndr.de/nachrichten/niedersachsen/hannover_weser-leinegebiet/Mehr-Spass-im-Einsatzwagen-So-gruesst-sich-die-Feuerwehr-Aerzen,feuerwehr4832.html",
  "https://www.ndr.de/kultur/buch/sachbuecher/Impressionen-aus-dem-Bildband-Feed-the-Planet,steinmetz124.html",
  "https://www.ndr.de/kultur/epg/Ins-Gras-beissen-die-anderen,sendung1509014.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Polarlichter-Bunte-Himmelsbilder-aus-der-Nacht-zum-1110-aus-SH,polarlichter564.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Reaktionen-auf-die-Angriffe-auf-Einsatzkraefte-an-Silvester,rettungskraefte176.html",
  "https://www.ndr.de/ndrblue/index.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken,audio1782396.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischentoene,audio1777390.html",
  "https://www.ndr.de/kultur/film/videos/Trailer-Das-Licht-Berlinale-Eroeffnungsfilm-von-Tom-Tykwer,daslichttrailer100.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1785354.html",
  "https://www.ndr.de/nachrichten/mecklenburg-vorpommern/rostock/Sternsinger-bringen-Segen-in-das-Rostocker-Rathaus,mvregiorostock2254.html",
  "https://www.ndr.de/nachrichten/info/Gott-und-die-Welt-mit-Astrid-Kleist,audio1768306.html",
  "https://www.ndr.de/ndr1niedersachsen/sendungen/unser_thema/index.html",
  "https://www.ndr.de/ndr2/epg/Alle-Spiele-alle-Tore-und-die-Schlusskonferenz,sendung1523512.html",
  "https://www.ndr.de/sport/fussball/Hannover-96-muss-sich-mit-11-gegen-Duesseldorf-begnuegen,hannover19000.html",
  "https://www.ndr.de/ratgeber/reise/Wandern-Schoene-Touren-durch-Naturparks-und-an-den-Kuesten,wandertouren100.html",
  "https://www.ndr.de/sport/fussball/VfL-Frauen-scheiden-bei-der-TSG-Hoffenheim-im-DFB-Pokal-aus-Das-Tor,wolfsburg20600.html",
  "https://www.ndr.de/nachrichten/info/sendungen/zwischen_hamburg_und_haiti/Vancouver-Baeren-Berge-und-eine-multikulturelle-Metropole,vancouver216.html",
  "https://www.ndr.de/sport/mehr_sport/Tennis-alexander-zverev-karriere-bilder,zverev177.html",
  "https://www.ndr.de/kultur/epg/Nils-Holgerssons-wunderbare-Reise,sendung1519916.html",
  "https://www.ndr.de/kultur/film/Avatar-2-Bilder-der-Unterwasserwelten-auf-Pandora,avatarzwei102.html",
  "https://www.ndr.de/sport/fussball/Magath-zum-HSV-Team-und-Trainer-gut-zusammengewachsen,sportclub14502.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1520756.html",
  "https://www.ndr.de/nachrichten/info/infospezial/epg/Feierstunde-zum-Gedenken-an-die-Opfer-des-Nationalsozialismus,sendung1522448.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Planet-Alsen-in-Itzehoe,planetalsen116.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1527680.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Warnstreiks-in-Niedersachsen-Tausende-Menschen-legen-Arbeit-nieder,warnstreik2966.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Chiara-trifft-Frauen-bei-der-Feuerwehr,niedersachsenhost196.html",
  "https://www.ndr.de/ndrblue/sendungen/nachtclub/Nachtclub-und-Nightlounge,nachtclubindex120.html",
  "https://www.ndr.de/kultur/buch/Der-Norden-liest-2024,bildergaleriedernordenliest110.html",
  "https://www.ndr.de/nachrichten/mecklenburg-vorpommern/Norddeutscher-Tag-in-Doemitz-Markt-und-Unterhaltung-up-Platt,norddeutschertag100.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Duet-un-dat-op-Platt,sendung1522164.html",
  "https://www.ndr.de/sport/fussball/Kultclub-aus-dem-Norden-Die-Geschichte-von-Hansa-Rostock-in-Bildern,hansarostock24.html",
  "https://www.ndr.de/kultur/radiokunst/Hoerspiel-Anton-und-Pepe-Beind-the-Scenes,antonundpepe176.html",
  "https://www.ndr.de/ndr2/epg/Alle-Spiele-alle-Tore-und-die-Schlusskonferenz,sendung1511334.html",
  "https://www.ndr.de/kultur/epg/Hoerspiel,sendung1508798.html",
  "https://www.ndr.de/n-joy/appmedien/Hier-im-Norden,hiermemes100.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Wie-findet-man-die-grosse-Liebe,niedersachsenhost202.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1520866.html",
  "https://www.ndr.de/kultur/epg/Jazz,sendung1519822.html",
  "https://www.ndr.de/sport/fussball/Zwischen-Frust-und-Freude-Werder-Bremens-Remis-beim-BVB,sportclub14488.html",
  "https://www.ndr.de/n-joy/musik/index.html",
  "https://www.ndr.de/nachrichten/info/epg/Klima-Zukunft-Deutschland-in-15-Jahren,sendung1507732.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischentoene-Spiel-Gott,audio1777372.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1792508.html",
  "https://www.ndr.de/kultur/epg/Nils-Holgerssons-wunderbare-Reise,sendung1519966.html",
  "https://www.ndr.de/nachrichten/info/Gott-und-die-Welt-mit-Klemens-Buescher,audio1749932.html",
  "https://www.ndr.de/sport/fussball/Bundesliga-FC-St-Pauli-schlaegt-Heidenheim,stpauli8448.html",
  "https://www.ndr.de/n-joy/team/index.html",
  "https://www.ndr.de/der_ndr/programmangebote/Entdecken-und-Erleben-der-NDR-fuer-Dich,entdeckenerlebenkultur108.html",
  "https://www.ndr.de/der_ndr/programmangebote/Entdecken-und-Erleben-der-NDR-fuer-Dich,entdeckenerlebenudreissig112.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Zerfall-Babylons-mit-Volker-Kutscher-durch-Berlin,sendung1516660.html",
  "https://www.ndr.de/ndr1niedersachsen/Dat-kannst-mi-gloeven-Dat-groettste-Kark-up-Eer-Petersdom,audio1789936.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Geesthacht-Laengste-mobile-Carrerabahn-Europas-aufgebaut,carrerabahn100.html",
  "https://www.ndr.de/ratgeber/garten/nutzpflanzen/Sprossen-auf-der-Fensterbank-ziehen,sprossen186.html",
  "https://www.ndr.de/kultur/Alles-ist-moeglich,audio1777474.html",
  "https://www.ndr.de/kultur/epg/Kriminalhoerspiel-Heilige-Moerderin-12,sendung1518432.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken,audio1782394.html",
  "https://www.ndr.de/n-joy/appmedien/Trump-Memes,trumpmemes110.html",
  "https://www.ndr.de/kultur/epg/ARD-Konzert,sendung1518278.html",
  "https://www.ndr.de/kultur/epg/ARD-Konzert,sendung1520092.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Zerfall-Babylons-mit-Volker-Kutscher-durch-Berlin,sendung1526870.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Unser-Thema,sendung1510840.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Duet-un-dat-op-Platt,sendung1517904.html",
  "https://www.ndr.de/ndr1niedersachsen/Dat-kannst-mi-gloeven,audio1787690.html",
  "https://www.ndr.de/sport/fussball/Waldhof-Mannheim-FC-Hansa-Rostock-Die-Tore-des-Spiels,hansa12854.html",
  "https://www.ndr.de/nachrichten/info/epg/Wahl-im-Kosovo-Albin-Kurti-auf-dem-Pruefstand,sendung1514480.html",
  "https://www.ndr.de/sport/fussball/HSV-ziehen-durch-20-gegen-Gladbach-ins-Pokal-Halbfinale-ein-Die-Tore,hsv29562.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischentoene,audio1795502.html",
  "https://www.ndr.de/nachrichten/niedersachsen/braunschweig_harz_goettingen/Der-Norden-faehrt-Ski-beste-Bedingungen-im-Harz,ski206.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1516588.html",
  "https://www.ndr.de/kultur/Museen-und-Ausstellungshaeuser,kpmuseum100.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1522376.html",
  "https://www.ndr.de/ndr2/sendungen/ndr2spezial/NDR-2-Spezial,sendung1498372.html",
  "https://www.ndr.de/nachrichten/info/Oha-Zwei-Welten-an-einem-Tisch-So-debattiert-der-Norden,oha106.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1790130.html",
  "https://www.ndr.de/sport/Spassvogel-Fischer-will-mit-DHB-Team-bei-der-WM-ernst-machen,sportclub14442.html",
  "https://www.ndr.de/kultur/epg/Urban-Pop,sendung1518344.html",
  "https://www.ndr.de/ndr1niedersachsen/Dat-kannst-mi-gloeven-Du-buest-mien-Schutzengel,audio1777430.html",
  "https://www.ndr.de/ratgeber/gesundheit/Krebsvorsorge-fuer-Frauen-HPV-Test-ist-Kassenleistung,hpvtest102.html",
  "https://www.ndr.de/sport/fussball/War-mehr-drin-FC-St-Pauli-verliert-bei-RB-Leipzig,sportclub14532.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1527104.html",
  "https://www.ndr.de/ratgeber/medienkompetenz/Medienkompetenz-Unterrichtsmaterial-fuer-die-Grundschule,unterrichtsmaterialindex108.html",
  "https://www.ndr.de/kultur/Anfangen-braucht-einen-Stern,audio1777504.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/AKW-Brokdorf-Rueckbau-gestartet,akwbrokdorf128.html",
  "https://www.ndr.de/Bilder-nach-dem-Unfall-am-Bahnuebergang,unfall19306.html",
  "https://www.ndr.de/nachrichten/niedersachsen/hannover_weser-leinegebiet/Fotoausstellung-von-Studenten-zu-Krieg-Frieden-und-Hoffnung,fotoausstellung154.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1516610.html",
  "https://www.ndr.de/kultur/epg/Das-Gespraech,sendung1516004.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischendrin-Junger-Pfarrer,audio1777466.html",
  "https://www.ndr.de/kultur/film/Nosferatu-spukt-wieder,nosferatu115.html",
  "https://www.ndr.de/kultur/Jesus-Christus,audio1780698.html",
  "https://www.ndr.de/ratgeber/reise/hannover/Bernwardtuer-und-weitere-Kunstschaetze-im-Hildesheimer-Dom,domhildesheim115.html",
  "https://www.ndr.de/n-joy/appmedien/Die-Gastkonto-Masche-bei-Paypal,gastkontomasche100.html",
  "https://www.ndr.de/kultur/epg/Das-Konzert,sendung1507338.html",
  "https://www.ndr.de/nachrichten/niedersachsen/braunschweig_harz_goettingen/Endlich-Schnee-Ski-Fans-zieht-es-in-Harz-und-Deister,schnee4426.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischendrin,audio1777500.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1522264.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1790094.html",
  "https://www.ndr.de/nachrichten/mecklenburg-vorpommern/vorpommern/Volkswerft-Stralsund-Sanierung-des-Segelschulschiffs-Greif-geht-voran-,mvregiogreifswald2086.html",
  "https://www.ndr.de/kultur/buch/sachbuecher/Impressionen-aus-dem-Bildband-Die-engen-Wasser,esser238.html",
  "https://www.ndr.de/kultur/buch/sachbuecher/Impressionen-aus-dem-Bildband-Nordlichter,nordlichter366.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Elmshorn-Baby-allein-im-Zug-Richtung-Sylt-unterwegs,baby864.html",
  "https://www.ndr.de/ratgeber/gesundheit/Brustkrebs-bei-Maennern-erkennen-und-behandeln,brustkrebs129.html",
  "https://www.ndr.de/ndr2/sendungen/ndr2spezial/index.html",
  "https://www.ndr.de/kultur/epg/ARD-Konzert,sendung1520112.html",
  "https://www.ndr.de/ndr2/Moment-Mal,audio1803172.html",
  "https://www.ndr.de/sport/fussball/Holstein-Kiel-verliert-in-Frankfurt-und-ist-Schlusslicht,sportclub14556.html",
  "https://www.ndr.de/sport/fussball/bundesliga/Werder-Bremen-Bayern-Muenchen-Bilder-einer-grossen-Rivalitaet,werder6207.html",
  "https://www.ndr.de/nachrichten/info/epg/DFB-Pokal-der-Maenner,sendung1522442.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischentoene,audio1777388.html",
  "https://www.ndr.de/nachrichten/info/epg/Der-Zerfall-Babylons-mit-Volker-Kutscher-durch-Berlin,sendung1514590.html",
  "https://www.ndr.de/nachrichten/info/Gott-und-die-Welt-mit-Ulrike-Purrer,audio1769058.html",
  "https://www.ndr.de/nachrichten/info/Katholischer-Gottesdienst-aus-der-Wallfahrtskirche-in-Lage-Rieste,audio1790688.html",
  "https://www.ndr.de/kultur/musik/jazz/All-That-Queer-Jazz-Kleine-Reise-durch-eine-Subkultur,queerjazz100.html",
  "https://www.ndr.de/Bilder-von-Borkum,bilderborkum100.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Work-Life-Balance-auf-dem-Bauernhof,niedersachsenhost204.html",
  "https://www.ndr.de/hand_in_hand_fuer_norddeutschland/Partner-der-NDR-Benefizaktion-Hand-in-Hand-fuer-Norddeutschland,partner140.html",
  "https://www.ndr.de/sport/fussball/Waldhof-Mannheim-Hansa-Rostock-Das-Spiel-in-voller-Laenge,sportclub14542.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Radschnellweg-Elmshorn-Hamburg-Buendnis-soll-Tempo-machen-,radschnellweg192.html",
  "https://www.ndr.de/nachrichten/info/epg/Frag-dich-fit-mit-Doc-Esser-und-Anne,sendung1520386.html",
  "https://www.ndr.de/kultur/kunst/provenienzforschung/Raubkunst-Verbrechen-gegen-die-Menschlichkeit,raubkunst274.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1516552.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken,audio1777490.html",
  "https://www.ndr.de/kultur/Neu-Anfang-Anfangen-braucht-Rituale,audio1777508.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1788394.html",
  "https://www.ndr.de/ndr2/epg/Das-NDR-2-Wochenende,sendung1523444.html",
  "https://www.ndr.de/kultur/epg/Kriminalhoerspiel-Heilige-Moerderin-22,sendung1520008.html",
  "https://www.ndr.de/n-joy/appmedien/Urlaubsplanung-Hier-umgeht-ihr-die-Ferien,urlaubsplanung118.html",
  "https://www.ndr.de/nachrichten/info/epg/Frag-dich-fit-mit-Doc-Esser-und-Anne,sendung1526176.html",
  "https://www.ndr.de/sport/fussball/Werder-Bremen-verliert-ueberraschend-gegen-Hoffenheim,sportclub14558.html",
  "https://www.ndr.de/kultur/Geistliche-Musik-zum-2-Sonntag-nach-Epiphanias,audio1796548.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken,audio1795536.html",
  "https://www.ndr.de/ndr1niedersachsen/Dat-kannst-mi-gloeven-Pantheon-Erst-Tempel-dann-Kark,audio1789934.html",
  "https://www.ndr.de/nachrichten/info/Katholischer-Gottesdienst-aus-dem-Koelner-Dom,audio1786102.html",
  "https://www.ndr.de/kultur/epg/angst-wut-hoffnung,sendung1517324.html",
  "https://www.ndr.de/kultur/Auferstehung,audio1780700.html",
  "https://www.ndr.de/sport/fussball/Alle-Trainer-des-FC-Hansa-Rostock,hansatrainer101.html",
  "https://www.ndr.de/ratgeber/garten/zierpflanzen/Diese-Pflanzen-bluehen-auch-im-Winter,winterblueher107.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1800504.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1524416.html",
  "https://www.ndr.de/nachrichten/niedersachsen/braunschweig_harz_goettingen/Gifhorn-Feuerwehr-befreit-eingeschlossenes-Kleinkind-aus-Auto,aktuellbraunschweig14842.html",
  "https://daserste.ndr.de/annewill/videos/Ich-persoenlich-wuerde-eine-Lanze-fuer-Nancy-Faeser-brechen,annewill8126.html",
  "https://www.ndr.de/ratgeber/reise/nordseekueste_sh/Auf-Spuren-des-Dichters-Friedrich-Hebbel,hebbelwanderweg100.html",
  "https://www.ndr.de/sport/fussball/Holstein-Kiel-mit-Remis-gegen-VfL-Bochum-Aufregung-um-VAR,kiel8062.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1516628.html",
  "https://www.ndr.de/nachrichten/info/epg/Presseclub-Talk-Forward,sendung1516662.html",
  "https://www.ndr.de/nachrichten/info/epg/Teurer-Fahren,sendung1515982.html",
  "https://www.ndr.de/sport/fussball/VfL-Wolfsburg-sichert-sich-bei-Eintracht-Frankfurt-einen-Punkt,sportclub14514.html",
  "https://www.ndr.de/ratgeber/reise/inseln/Sand-Sonne-und-Meer-Langeoog-in-Bildern,langeoog213.html",
  "https://www.ndr.de/nachrichten/info/epg/DFB-Pokal-der-Maenner,sendung1522440.html",
  "https://www.ndr.de/ratgeber/reise/Kanu-fahren-im-Norden-Unterwegs-auf-Oetze-Oker-und-Co,kanufahren100.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken,audio1777464.html",
  "https://www.ndr.de/sport/fussball/Selke-verschiesst-und-trifft-HSV-mit-einem-Punkt-in-Regensburg,sportclub14552.html",
  "https://www.ndr.de/ndr2/epg/Alle-Spiele-alle-Tore-und-die-Schlusskonferenz,sendung1523838.html",
  "https://www.ndr.de/ratgeber/reise/nordseekueste_sh/Amrum-ein-Inselspaziergang,amrum322.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Landesweite-Proteste-Zehntausende-demonstrieren-gegen-Rechts,omasgegenrechts150.html",
  "https://www.ndr.de/ratgeber/reise/ostseekueste_sh/Luebecker-Impressionen,luebeck665.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1523886.html",
  "https://www.ndr.de/nachrichten/niedersachsen/oldenburg_ostfriesland/Altes-Militaerkino-in-Oldenburg-zu-wieder-Ort-fuer-Kultur-werden,globe266.html",
  "https://www.ndr.de/ratgeber/reise/Rundgang-durch-Hitzacker,hitzacker161.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1517954.html",
  "https://www.ndr.de/kultur/Die-Himmelsleiter,audio1780714.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischentoene-Blockfloete,audio1777380.html",
  "https://www.ndr.de/nachrichten/info/sendungen/mikado/Beim-Boxtraining,boxen962.html",
  "https://www.ndr.de/kultur/Geistliche-Musik-zum-2-Sonntag-nach-Weihnachten,audio1786988.html",
  "https://www.ndr.de/nachrichten/hamburg/Silvester-in-Hamburg-Die-Bilder-vom-Jahreswechsel-2024-2025,silvester1616.html",
  "https://www.ndr.de/nachrichten/info/epg/Alles-anders-Was-mein-Leben-veraendert-hat,sendung1523612.html",
  "https://www.ndr.de/sport/fussball/Lutz-Wagner-zu-den-Schiedsrichter-Durchsagen-Bin-sicher-es-klappt,sportclub14508.html",
  "https://www.ndr.de/ndr1niedersachsen/Dat-kannst-mi-gloeven,audio1792462.html",
  "https://www.ndr.de/ndr1niedersachsen/Dat-kannst-mi-gloeven-Rom-de-ewige-Stadt,audio1789930.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1523884.html",
  "https://www.ndr.de/ndr2/Moment-Mal,audio1791028.html",
  "https://www.ndr.de/nachrichten/info/Evangelischer-Gottesdienst-aus-der-St-Jacobi-Kirche-in-Neuenkirchen,audio1794034.html",
  "https://www.ndr.de/Lastwagen-rollt-in-Hafenbecken,hafenbecken134.html",
  "https://www.ndr.de/nachrichten/niedersachsen/oldenburg_ostfriesland/Altpapier-Kunst-was-Schuhkartons-und-Korallen-gemeinsam-haben,papierkunst104.html",
  "https://www.ndr.de/Neuer-Forschungseisbrecher-Polarstern-2,polarstern498.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Solo-Mama-als-bewusste-Entscheidung,niedersachsenhost200.html",
  "https://www.ndr.de/kultur/epg/Roman-eines-Schicksallosen,sendung1508904.html",
  "https://www.ndr.de/nachrichten/info/sendungen/NDR-Info-Podcasts-Ausland,infopodcastsausland100.html",
  "https://www.ndr.de/ratgeber/reise/wattenmeer/Foehr-Eine-Bilderreise-auf-die-Nordseeinsel,foehrinbildern101.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Ich-werde-als-Fahrradfahrer-ganz-oft-uebersehen,kreuzung156.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Itzehoe-Kirchturmkreuz-drohte-abzustuerzen,kirchenkreuz102.html",
  "https://www.ndr.de/kultur/epg/Das-Jerusalem-Quartet-spielt-Schostakowitsch,sendung1517378.html",
  "https://www.ndr.de/nachrichten/info/sendungen/das_feature/index.html",
  "https://www.ndr.de/nachrichten/info/E-Rechnung-kommt-mehr-Transparenz-weniger-Kosten,audio1783592.html",
  "https://www.ndr.de/der_ndr/zahlen_und_daten/Rechtsgrundlagen-und-gesetzliche-Vorgaben-fuer-den-NDR,rechtsgrundlagen102.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Demenz-bei-Hunden-Schleichender-Prozess-,dementerhund118.html",
  "https://www.ndr.de/ratgeber/reise/Strand-Watt-Meer-St-Peter-Ording-in-Bildern,stpeterording270.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Schlechte-Noten-Zeugnis-Telefon-bietet-Hilfe-mit-Nummer-gegen-Kummer,zeugnis240.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1522156.html",
  "https://www.ndr.de/ndr1niedersachsen/Dat-kannst-mi-gloeven-Dat-Kolloseum,audio1789932.html",
  "https://www.ndr.de/nachrichten/info/epg/Talk-Forward-Presseclub,sendung1518480.html",
  "https://www.ndr.de/nachrichten/info/epg/Plusminus-Mehr-als-nur-Wirtschaft,sendung1518656.html",
  "https://www.ndr.de/ratgeber/reise/ostseekueste_sh/Strand-Seebruecke-Hafen-Bilder-aus-Timmendorfer-Strand-,timmendorf139.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Digitalisierung-bei-Rattenbekaempfung,rattenbekaempfung114.html",
  "https://www.ndr.de/kultur/In-der-Dunkelheit-Kraefte-fuer-Neuanfang-sammeln,audio1777518.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Ein-Filmstar-besucht-seine-alte-Heimat,braeden120.html",
  "https://www.ndr.de/ndr2/epg/Das-NDR-2-Wochenende,sendung1515548.html",
  "https://www.ndr.de/ndr2/Moment-Mal,audio1792134.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Wenn-der-Bundespraesident-ueber-Eckernfoerder-Wochenmarkt-spaziert,steinmeier1202.html",
  "https://www.ndr.de/ratgeber/garten/Primeln-Die-ersten-Blumen-des-Fruehlings,primel106.html",
  "https://www.ndr.de/ratgeber/reise/hamburg/Das-Gruftgewoelbe-in-der-Hamburger-Hauptkirche-St-Michaelis,krypta150.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken,audio1777496.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Ratgeber-,sendung1518972.html",
  "https://www.ndr.de/ratgeber/reise/inseln/Usedom-Urlaubsorte-und-Sehenswertes-auf-der-Ostseeinsel,usedom30.html",
  "https://www.ndr.de/kultur/Zweifeln-gehoert-dazu,audio1780710.html",
  "https://www.ndr.de/nachrichten/niedersachsen/hannover_weser-leinegebiet/Dauerparker-am-BER-Inzwischen-sind-200000-Euro-Gebuehr-faellig,auto1388.html",
  "https://www.ndr.de/nachrichten/mecklenburg-vorpommern/kurzerklaertMV-danach-bist-du-immer-schlauer,kurzerklaertmv100.html",
  "https://www.ndr.de/nachrichten/niedersachsen/hannover_weser-leinegebiet/Ein-Fighting-Falcon-im-Einsatz-bei-der-Luftwaffe,falke224.html",
  "https://www.ndr.de/nachrichten/info/Sturm-Staerken-Von-Windstille-bis-zum-Orkan,windstaerken101.html",
  "https://www.ndr.de/sport/mehr_sport/Herrmanns-Vendee-Globe-Finish-Letzte-Meilen-im-Regen,segeln2574.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Hintergrund-Die-Feuerwehr-im-Einsatz,feuerwehr905.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischentoene,audio1794060.html",
  "https://www.ndr.de/nachrichten/info/Evangelischer-Gottesdienst-aus-der-Erloeserkirche-in-Essen-Holsterhausen,audio1780286.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/11KM-der-Tagesschau-Podcast,sendung1516586.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Niederdeutsches-Hoerspiel,sendung1520836.html",
  "https://www.ndr.de/sport/fussball/regionalliga/Hansas-Helden-Meisterteam-von-1991-,hansalegenden101.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Der-Winter-kommt-zurueck-nach-Niedersachsen,schnee4398.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1800490.html",
  "https://www.ndr.de/kultur/podcast5682.html",
  "https://www.ndr.de/sport/fussball/Nordduell-zwischen-Hamburger-SV-und-Hannover-96-endet-remis,nordduell1252.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1786094.html",
  "https://www.ndr.de/nachrichten/hamburg/Feuerwehreinsatz-im-Tierpark-Hagenbeck-Elefantendame-braucht-Hilfe,elefant600.html",
  "https://www.ndr.de/ratgeber/reise/ostseekueste_sh/Die-Ostseebaeder-an-der-Luebecker-Bucht,ostseebaeder100.html",
  "https://www.ndr.de/nachrichten/niedersachsen/Klimastreik-in-Niedersachsen-Demonstrationen-in-19-Staedten,fff358.html",
  "https://www.ndr.de/kultur/film/videos/Trailer-Nosferatu-Neuer-Vampirfilm-von-Robert-Eggers-,nosferatutrailer100.html",
  "https://www.ndr.de/ratgeber/garten/zierpflanzen/Heidekraut-Mit-Besenheide-und-Erika-Akzente-setzen,heide339.html",
  "https://www.ndr.de/kultur/Herzensreise,audio1777470.html",
  "https://www.ndr.de/n-joy/leben/Welches-Spiel-ist-,brettspiele100.html",
  "https://www.ndr.de/nachrichten/niedersachsen/oldenburg_ostfriesland/Neben-der-Pflege-an-die-Uni,pflege1772.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/FSG-Nobiskrug-bekommt-neue-Struktur,fsg442.html",
  "https://www.ndr.de/nachrichten/info/NDR-Info-Schwerpunkt-Wie-sicher-ist-die-Rente-in-Deutschland,rente808.html",
  "https://www.ndr.de/nachrichten/investigation/bewarder102.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken-Krankenschwester,audio1777460.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken,audio1777498.html",
  "https://www.ndr.de/nachrichten/info/epg/Plusminus-Mehr-als-nur-Wirtschaft,sendung1521742.html",
  "https://www.ndr.de/nachrichten/info/Wie-KI-uns-voranbringt-Neue-Ideen-aus-dem-Norden,kiserieindex100.html",
  "https://www.ndr.de/kultur/epg/eatREADsleep,sendung1508794.html",
  "https://www.ndr.de/nachrichten/niedersachsen/hannover_weser-leinegebiet/Bergsteiger-ohne-Beine-Mann-erklimmt-Berge-mit-Prothesen,prothese240.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1524450.html",
  "https://www.ndr.de/kultur/Anfangen-mit-einem-Segen,audio1777512.html",
  "https://www.ndr.de/ratgeber/reise/radtouren/Weserradweg-Vom-Weserbergland-an-die-Nordsee,weserradweg104.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1522246.html",
  "https://www.ndr.de/nachrichten/info/sendungen/zwischen_hamburg_und_haiti/Dingle-raue-Schoenheit-in-Irland,dingleirland100.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischentoene,audio1777404.html",
  "https://www.ndr.de/kultur/epg/Haubolds-Soloalbum,sendung1508960.html",
  "https://www.ndr.de/ndr2/epg/Alle-Spiele-alle-Tore-und-die-Schlusskonferenz,sendung1515878.html",
  "https://www.ndr.de/der_ndr/programmangebote/Entdecken-und-Erleben-der-NDR-fuer-Dich,entdeckenerlebendoku112.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Beton-Brutalismus-unter-Denkmalschutz,beton166.html",
  "https://www.ndr.de/ratgeber/reise/hamburg/Wildes-Hamburg-Naturschutzgebiete-der-Hansestadt,naturhotspots100.html",
  "https://www.ndr.de/nachrichten/niedersachsen/osnabrueck_emsland/Blindgaenger-in-Osnabrueck-Knapp-12000-Menschen-evakuiert,bombe4304.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken-Epiphanias,audio1777452.html",
  "https://www.ndr.de/ratgeber/gesundheit/krankheiten/Vorsicht-ansteckender-Kopfpilz-So-sehen-die-Infektionen-aus,kopfpilz126.html",
  "https://www.ndr.de/kultur/musik/Gravitations-Neue-Konzertserie-der-Hamburger-Symphoniker,gravitations100.html",
  "https://www.ndr.de/sport/legenden/Michael-Westphal-Geheimnisvoller-Tod-eines-Tennisstars,westphal130.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/So-sieht-der-neue-Pfahlbau-in-St-Peter-Ording-aus,pfahlbauten140.html",
  "https://www.ndr.de/kultur/Die-Traeume-der-Alten,audio1780724.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1510410.html",
  "https://www.ndr.de/kultur/epg/Das-Konzert,sendung1511132.html",
  "https://www.ndr.de/ndr1niedersachsen/Zwischentoene,audio1795496.html",
  "https://www.ndr.de/nachrichten/niedersachsen/hannover_weser-leinegebiet/Streetlife-und-die-Welt-der-Insekten,ausstellung2322.html",
  "https://www.ndr.de/nachrichten/niedersachsen/hannover_weser-leinegebiet/Fast-50-jaehrige-Fototradition-Familie-Riese-und-die-Wahlplakate-,wahlplakate698.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken,audio1799452.html",
  "https://www.ndr.de/kultur/epg/Kriminalhoerspiel,sendung1511102.html",
  "https://www.ndr.de/ndr2/Moment-mal,audio1792618.html",
  "https://www.ndr.de/nachrichten/info/Gott-und-die-Welt-mit-Annika-Woydack,audio1768388.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/Superfood-Trend-Microgreens-Winzige-Pflanzen-grosse-Wirkung,microgreens134.html",
  "https://www.ndr.de/kultur/epg/Feature-Bin-ich-ueberfluessig,sendung1504668.html",
  "https://www.ndr.de/nachrichten/info/Gott-und-die-Welt-mit-Klemens-Buescher,audio1749930.html",
  "https://www.ndr.de/nachrichten/info/epg/Kalter-Krieg-20-Schatten-ueber-der-Ostseeregion,sendung1516152.html",
  "https://www.ndr.de/ratgeber/reise/hannover/Engesohde-Rundgang-ueber-Hannovers-Stadtfriedhof,friedhofengesohde110.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Unser-Thema,sendung1504304.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Abenteuer-Diagnose,sendung1525322.html",
  "https://www.ndr.de/sport/fussball/Felix-Magaths-Karriere-in-Bildern,magath188.html",
  "https://www.ndr.de/ndr1niedersachsen/epg/Die-Blaue-Stunde,sendung1527620.html",
  "https://www.ndr.de/kultur/Kulturpartner-in-Niedersachsen-Bremen,kulturpartnernds104.html",
  "https://www.ndr.de/kultur/epg/Feature-OZ-12,sendung1518430.html",
  "https://www.ndr.de/sport/fussball/Den-Blutkrebs-besiegt-Tom-Weilandts-emotionales-Interview,sportclub14548.html",
  "https://www.ndr.de/kultur/epg/Giuseppe-Verdi-Aida,sendung1508900.html",
  "https://www.ndr.de/ndr1niedersachsen/Nachtgedanken,audio1782388.html",
  "https://www.ndr.de/nachrichten/niedersachsen/braunschweig_harz_goettingen/Eine-Winterlandschaft-dank-Schneeflocken-und-Schneekanonen,schnee4388.html",
  "https://www.ndr.de/nachrichten/schleswig-holstein/FSG-Werften-600-Beschaeftigte-in-Flensburg-und-Rendsburg-streiken,fsg446.html",
  "https://www.ndr.de/n-joy/appmedien/Endlich-wieder-mehr-Tageslicht,tageslicht102.html",
  "https://www.ndr.de/nachrichten/info/epg/Presseclub-Talk-Forward,sendung1523608.html",
  "https://www.ndr.de/kultur/epg/vertikal-horizontal,sendung1508704.html",
  "https://www.ndr.de/sport/fussball/Koenigsdoerffer-trifft-und-laesst-HSV-gegen-Koeln-jubeln,sportclub14448.html",
  "https://www.ndr.de/ndr1niedersachsen/wir_ueber_uns/Korrespondentenbuero-Emsland,korrespondentenbuero2.html",
  
  "https://www.derwesten.de/der-westen-klaert-auf-newsletter-abmeldung-erfolgreich",
  
  "https://www.derwesten.de/der-westen-klaert-auf-fast-geschafft", 
  
  "https://www.derwesten.de/der-westen-klaert-auf-newsletter-geschafft-deine-anmeldung-war-erfolgreich",
  
  "https://www.derwesten.de/royals-report-newsletter",
  
  "https://www.derwesten.de/unwetter-nrw-glaette-deutsche-bahn",
  
  "https://www.n-tv.de/sport/der_sport_tag/Die-Schlagzeilen-aus-der-Nacht-article25542717.html",
  "https://www.n-tv.de/incoming/Flourish-Anination-minuetliche-Tabellenstaende-in-der-Champions-League-am-8-Gruppenspieltag-2024-25-article25525477.html",
  "https://www.n-tv.de/ticker/Polioviren-in-Abwasser-entdeckt-article25513521.html",
  "https://www.n-tv.de/sport/der_sport_tag/15-Breaks-Siegemund-kaempft-sich-in-kuriosem-Tennis-Marathon-weiter-article25483780.html",
  "https://www.n-tv.de/der_tag/TV-Moderatorin-missbraucht-Marius-Borg-Høiby-schweigt-article25562586.html",
  
  # All redirects 
  "https://www.schwaebische.de/panorama/wenn-die-eltern-sich-bekriegen-leiden-die-kinder-das-kann-drastische-folgen-haben-3302068", 
  "https://www.schwaebische.de/regional/baden-wuerttemberg/sie-arbeitet-seit-jahren-am-flughafen-stuttgart-doch-diesen-anblick-vergisst-sie-nie-3278011",
  "https://www.schwaebische.de/regional/ulm-alb-donau/ehingen/atomkraft-und-zweifel-am-klimawandel-was-dieser-afd-kandidat-fuer-die-region-fordert-3290185", 
  "https://www.schwaebische.de/regional/ulm-alb-donau/ulm/10000-menschen-demonstrieren-gegen-rechts-in-ulm-3294117", 
  "https://www.schwaebische.de/regional/ulm-alb-donau/ulm/polizeichef-veser-so-etwas-wie-in-magdeburg-darf-sich-nicht-wiederholen-3173427",
  
  # All videos 
  
  "https://www.swr.de/swraktuell/baden-wuerttemberg/friedrichshafen/indoor-drachenbootrennen-in-friedrichshafen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/karlsruhe/axel-kromer-zu-jugendarbeit-bei-hbw-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-241205-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/heilbronn/familienberaterin-linda-reisdies-zu-kinderaerztemangel-in-der-region-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-gesa-walch-berichtet-ueber-zerstoerte-wahlplakate-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/suedbaden/ob-martin-horn-bei-der-gedenkfeier-von-80-jahren-befreiung-von-auschwitz-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-rabea-amri-berichtet-ueber-mehr-pflegekraefte-aus-dem-ausland-100.html",
  "https://www.swr.de/swraktuell/snacks/holocaust-wie-sich-das-gedenken-aendert-und-welche-gefahren-dadurch-drohen-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-rabea-amri-berichtet-ueber-eine-krankenpflegerin-aus-brasilien-an-mainzer-unimedizin-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/kulturbuergermeisterin-martina-pfister-zum-karlstorbahnhof-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/tuebingen/nachtschicht-in-der-geburtshilfe-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/so-viele-anrufe-wie-noch-nie-beim-elterntelefon-des-kinderschutzbundes-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/hauke-wiemer-zu-fortnite-abwandlung-reload-democracy-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/autofahrer-betrunken-mit-sechsjaehirgem-sohn-in-friesenheim-unterwegs-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/markus-zwick-zur-zuzugssperre-fuer-pirmasens-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/ulm/oliver-bernsau-zur-uebung-der-dlrg-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-1312025-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/ob-markus-zwick-cdu-hofft-auf-zuschlag-zur-landesgartenschau-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-lucretia-gather-zum-prozess-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/kind-wird-in-zweibruecken-von-auto-erfasst-und-kommt-ins-krankenhaus-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/karlsruhe/ein-tag-in-der-ard-rechtsredaktion-108.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/heilbronn/aenderung-des-kauf-und-essverhaltens-wegen-pest-und-seuche-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/tuebingen/broetchenklau-in-der-cafeteria-beim-sitzungssaal-im-tuebinger-rathaus-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/aufsager-lars-landgericht-zw-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/fck-spielt-am-abend-bei-greuther-fuerth-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/suedbaden/dreiland-aktuell-unterstuetzung-fuer-geisel-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/oberbuergermeister-christian-specht-zum-miteinander-in-mannheim-100.html",
  "https://www.swr.de/swraktuell/snacks/trump-20-was-kommt-auf-die-deutsche-wirtschaft-zu-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/friedel-durben-zum-risiko-des-polizeiberufs-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/moe-lage-der-baubranche-in-bw-weiter-schlecht-sperrfrist-12-uhr-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporter-juergen-wolff-zum-streit-um-werbung-an-alzeyer-innenstadt-geschaeften-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/groesserer-polizeieinsatz-in-wiesloch-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/batterien-gefahr-fuer-recyclingfirmen-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/rainer-saurwein-von-der-kassenaerztlichen-vereinigung-zur-mobilen-arztpraxis-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/kristina-keuper-aus-kaiserslautern-lebt-fuer-zwei-jahre-in-kopenhagen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/heilbronn/neue-kinderarztpraxis-in-neckarsulm-104.html",
  "https://www.swr.de/swraktuell/snacks/wie-teuer-wird-mein-leben-wenn-die-co2-abgabe-steigt-100.html",
  "https://www.swr.de/swraktuell/snacks/info-date-am-mittag-biodeutsch-ist-unwort-des-jahres-rostige-atommuell-faesser-bei-karlsruhe-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/schuss-in-woerrstadt-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/karlsruhe/abgang-von-zivzivadze-ein-schock-100.html",
  "https://www.swr.de/swraktuell/snacks/nutella-nutoka-oder-was-wie-geht-gesunder-brotaufstrich-104.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/trier/kleinere-doerfer-lehnen-vorschlag-ab-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/suedbaden/bildergalerie-upcycling-waldshut-tiengen-kunst-auf-solarmodulen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/gestaendnis-im-ukraine-prozess-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/swr-reporterin-irmgard-reissinger-zu-widerstand-gegen-produktions-verlagerung-eberspaecher-catem-herxheim-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/karlsruhe/axel-kromer-zu-entscheidung-fuer-hbw-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/schuhfabrik-aus-pirmasens-geht-ins-insolvenzverfahren-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/friedrichshafen/backen-und-fahren-der-pizza-hans-aus-oberschwaben-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-1712025-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/fortnite-abwandlung-als-demokratie-lehrgame-100.html",
  "https://www.swr.de/swraktuell/snacks/mks-schweinepest-vogelgrippe-wie-ist-die-tierseuchen-lage-in-deutschland-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-corinna-lutz-zu-den-taxikontrollen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/ulm/ssv-ulm-sorgt-fuer-effekte-im-wert-von-66-mio-euro-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/polizei-tasert-mann-nach-verkehrskontrolle-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/umfrage-jugendliche-bundestagswahl-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/lars-theobald-der-sportliche-leiter-des-sv-zu-den-probleme-die-das-loch-bringt-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/groesste-rollstuhl-schiebergruppe-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/sternsinger-in-berlin-beim-kanzler-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-vanessa-siemers-zum-haushalt-bad-kreuznach-104.html",
  "https://www.swr.de/swraktuell/snacks/info-date-am-mittag-fussball-profi-vereine-muessen-fuer-polizeieinsaetze-zahlen-schleuse-mueden-neues-tor-ist-unterwegs-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-mailin-engels-zum-elterntelefon-des-kinderschutzbundes-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/46-jaehriger-beim-abladen-von-ladung-ums-leben-gekommen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/karlsruhe/lagerhalle-in-waghaeusel-in-brand-geraten-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/handballer-david-spaeth-zur-weltmeisterschaft-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/heilbronn/umfrage-zum-start-des-buergerspitals-wertheim-100.html",
  "https://www.swr.de/swraktuell/snacks/giftchemikalie-pfas-torpediert-der-lobby-ansturm-bei-der-eu-den-umweltschutz-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/teilnehmerrekord-bei-dreikoenigsschwimmen-im-gelterswoog-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/tuebingen/trotz-verbots-stehen-noch-windkraftprotestplakate-in-starzach-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/swr-reporterin-anna-lena-fuerst-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/wintersport-para-ski-weltcup-2025-feldberg-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/da-albert-schweitzer-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/heilbronn/bme-wilhelma-rettet-gefaehrdete-seerose-und-andere-pflanzen-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/koblenz/caritassprecher-markus-goepfert-zu-psychisch-kranken-fluechtlingen-100.html",
  "https://www.swr.de/swraktuell/snacks/info-date-am-morgen-merz-will-migrationsreform-notfalls-auch-mit-afd-stimmen-bw-und-rp-erinnern-an-ns-opfer-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/investition-in-kryptowaehrung-war-betrug-100.html",
  "https://www.swr.de/swraktuell/stoerche-in-theisbergstegen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/karlsruhe/ein-sieg-fuer-palmer-aber-nicht-ueber-den-muell-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/best-of-wintersport-104.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/razzia-in-illegalem-bordell-in-wiesbaden-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/sarah-dowidat-von-der-landesschuelervertretung-100.html",
  "https://www.swr.de/swraktuell/wahl/bw/landtagswahl-2021/koalitionsrechner-landtagswahl-baden-wuerttemberg-2021-102.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/sirch-im-trainingslager-100.html",
  "https://www.swr.de/swraktuell/snacks/info-date-am-abend-iran-laesst-deutsch-iranerin-taghavi-frei-pfaelzerwald-in-schlechtem-zustand-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/sternsinger-freuen-sich-auf-besuch-beim-kanzler-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-1512025-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/karlsruhe/prozessauftakt-gegen-ex-mdb-axel-e-fischer-104.html",
  "https://www.swr.de/swraktuell/snacks/patientenschuetzer-kritik-an-elektronischer-patientenakte-100.html",
  "https://www.swr.de/swraktuell/snacks/tiktok-verkaufs-ultimatum-in-den-usa-endet-wie-geht-es-jetzt-weiter-102.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-karin-pezold-ueber-das-projekt-vertrauliche-hilfe-nach-gewalt-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/grosse-sorgen-bei-den-78-pfaelzischen-weinbautagen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/tuebingen/brand-in-einer-tiefgarage-in-altensteig-100.html",
  "https://www.swr.de/swraktuell/snacks/wie-wird-trumps-comeback-die-wissenschaft-in-den-usa-veraendern-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/aggressionen-im-wahlkampf-neu-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/mutmasslicher-reichsbuerger-vor-gericht-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/prozess-wegen-mordes-in-althornbach-beginnt-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-2812025-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/tuebingen/student-kleidet-sich-wie-zu-kaiserzeiten-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-3012025-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/tuebingen/100-jahre-narrenzunft-rottenburg-nif-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/karlsruhe/sturmproblematik-beim-karlsuher-sc-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/friedrichshafen/die-vsan-hat-einen-neuen-praesidenten-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-2912025-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/blitzer-bilanz-nach-zwei-jahren-blitzen-in-eigenregie-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/pontifikalamt-im-freiburger-muenster-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/westpfalz-wartet-auf-urteil-des-bundesverfassungsgerichts-zu-hochrisikospilen-beim-fussball-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/peter-weissler-chef-der-arbeitsagentur-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-vanessa-siemers-zum-urteil-am-landgericht-bad-kreuznach-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-2712025-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/steffen-hauthdrei-einsatzgebiete-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/spd-landtagsabgeordneter-und-arzt-oliver-kusch-aus-kusel-100.html",
  "https://www.swr.de/swraktuell/snacks/wirtschaftsnews-spaet-wenig-wohneigentum-in-deutschland-102.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/innenminister-ebling-spd-zu-drohnenueberfluegen-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-612025-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/meinung-zum-denkmalschutz-in-mannheim-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/swr-reporterin-karin-pezold-zu-dem-vandalismus-in-alzey-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/karlsruhe/muessen-den-siegeshunger-beibehalten-100.html",
  "https://www.swr.de/swraktuell/snacks/warum-ausgerechnet-biodeutsch-das-unwort-des-jahres-ist-104.html",
  "https://www.swr.de/swraktuell/snacks/meinungen-zu-sb-kassen-und-die-probleme-des-handels-100.html",
  "https://www.swr.de/swraktuell/snacks/hauseigentuemer-verband-zu-grundsteuer-drama-oder-alles-halb-so-wild-102.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/bildergalerie-6402.html",
  "https://www.swr.de/swraktuell/snacks/wirtschaftsnews-spaet-absatzeinbruch-bei-daimler-truck-102.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/streik-bei-der-post-106.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-912025-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-ilona-hartmann-zum-wolf-verdacht-bei-bad-sobernheim-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/neue-ideen-fuer-die-westpfalz-100.html",
  "https://www.swr.de/swraktuell/snacks/trump-und-musk-wollen-den-mars-besiedeln-und-was-macht-die-eu-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/karlsruhe/axel-kromer-zum-hbw-image-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-vanessa-siemers-zum-wolf-in-bad-sobernheim-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-712025-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/ein-mini-wald-fuer-lautern-100.html",
  "https://www.swr.de/swraktuell/snacks/info-date-am-abend-bundesliga-muss-polizeikosten-zahlen-neues-antiterrorzentrum-in-baden-wuerttemberg-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/rheinland-pfalz-wetter-vom-16012025-100.html",
  "https://www.swr.de/swraktuell/snacks/symbolischer-cent-preis-fuer-einweg-plastiktuetchen-umweltschutz-oder-abzocke-100.html",
  "https://www.swr.de/swraktuell/snacks/info-date-am-morgen-netanjahu-einigung-ueber-israels-abkommen-mit-hamas-bezahlkarten-fuer-gefluechtete-in-trier-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-ilona-hartmann-zur-rueckkehr-der-kraniche-102.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/21-knutfest-in-weidenthal-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/cdu-rlp-mitglieder-sollen-sich-von-kritik-nicht-verunsichern-lassen-100.html",
  "https://www.swr.de/swraktuell/snacks/polizeikosten-bei-fussball-hochrisikospielen-rheinland-pfalz-will-bundesweite-regelung-104.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/kb-mann-muss-nach-brandstiftung-in-krankenhaus-ps-evtl-in-klinik-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporter-andreas-neubrechueber-die-aktion-der-malteser-fuer-fluechtlingskinder-in-mainz-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/fruehschicht-im-club-toniq-in-heidelberg-102.html",
  "https://www.swr.de/swraktuell/snacks/bundestagswahl-im-februar-hat-der-termin-einfluss-auf-die-ergebnisse-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/ulm/swr-wetterexperte-bernd-madlener-ueber-industrieschnee-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/koehler-tot-bundespraesident-anteilnahme-schweitzer-schnieder-rlp-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/beginn-mannheimer-vesperkirche-2025-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/ulm/o-ton-kucher-forderungen-an-die-politik-bei-der-bauernkundgebung-100.html",
  "https://www.swr.de/swraktuell/snacks/hopplahopp-bundestagswahl-wie-das-organisieren-in-rp-so-schnell-klappen-kann-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/sprache-und-spass-in-mundart-so-beantragen-pfaelzer-ihren-urlaub-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/dreikoenigstag-einkaufen-in-der-pfalz-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/koblenz/reality-check-hochwasser-wie-sicher-ist-das-ahrtal-108.html",
  "https://www.swr.de/swraktuell/snacks/info-date-am-mittag-gesundheitsdaten-to-go-die-elektronische-patientenakte-epa-glaette-behindert-den-verkehr-in-vielen-regionen-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/sirch-aus-fck-trainingslager-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/heilbronn/der-gaildorfer-buergermeister-frank-zimmermann-blickt-auf-2025-und-2024-optimismus-stadt-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/franziska-brantner-fordert-eine-staatsreform-100.html",
  "https://www.swr.de/swraktuell/snacks/nahost-warum-das-waffenruhe-abkommen-halten-koennte-100.html",
  "https://www.swr.de/swraktuell/snacks/info-date-am-morgen-einigung-auf-waffenruhe-in-gaza-diskussion-um-grundschul-leistungstest-in-bw-100.html",
  "https://www.swr.de/swraktuell/snacks/fruehstuecks-quarch-sollte-man-wegen-elon-musks-aeusserungen-x-verlassen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/friedrichshafen/so-wird-die-brandruine-abgetragen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/ulm/chef-des-bwk-ulm-benedikt-friemert-100.html",
  "https://www.swr.de/swraktuell/snacks/info-date-am-abend-lauterbach-lobt-elektronische-patientenakte-glaette-und-blitzeis-im-suedwesten-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/dorothee-wuest-kirchenpraesidentin-evangelische-kirche-pfalz-106.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/heilbronn/brand-obersontheim-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/polizei-hilft-durchgefrorenem-mann-an-bushaltestelle-in-thalfang-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/friedrichshafen/dreikoenigstauchen-vor-ueberlingen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/suedbaden/thomas-stoeckle-gedenkstaettenleiter-von-grafeneck-ueber-die-nachfahren-von-zeitzeugen-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-corinna-lutz-ueber-die-aktion-geschkenkter-baum-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/swr-reporter-marius-mueller-fck-beendet-trainingslager-auf-malta-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/koblenz/hochwassersituation-in-altenahr-scheitel-am-nachmittag-erwartet-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/spendenaktion-volker-express-alzey-100.html",
  "https://www.swr.de/swraktuell/snacks/tierseuche-mks-bw-landwirtschaftsminister-hauk-fordert-verhandlungen-102.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/nochmalnochmal-unfall-100.html",
  "https://www.swr.de/swraktuell/snacks/info-date-am-morgen-union-erweitert-plaene-fuer-asyl-aenderungen-bw-landesweite-busfahrer-streiks-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/schon-wieder-hunde-im-gnadenhof-eifel-beschlagnahmt-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/stuttgart-pruegelei-in-bus-waehrend-fahrt-durch-hausen-weilimdorf-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/suedbaden/umfrage-bei-der-gedenkveranstaltung-zu-80-jahre-befreiung-von-auschwitz-in-freiburg-100.html",
  "https://www.swr.de/swraktuell/snacks/pornos-im-internet-wie-kann-man-kinder-und-jugendliche-schuetzen-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-ilona-hartmann-zum-tod-des-kletterers-am-rotenfels-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/bernhard-erfort-zum-polizeiberuf-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/juniorwahlen-zur-bundestagswahl-in-der-westpfalz-100.html",
  "https://www.swr.de/swraktuell/snacks/schattenbericht-zu-armut-in-deutschland-warum-arm-nicht-gleich-hilflos-ist-100.html",
  "https://www.swr.de/swraktuell/snacks/terrorismus-experte-magdeburg-anschlag-war-kaum-vorhersehbar-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/sebastian-muenzenmaier-afd-zum-wahlergebnis-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporterin-corinna-lutz-zu-dem-schweren-unfall-bei-waldlaubersheim-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/moeglicherweise-wolf-bei-bad-sobernheim-unterwegs-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/suedbaden/bildergalerie-80-jahre-auschwitz-befreiung-gedenken-freiburg-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/suedbaden/spuren-preview-im-swr-studio-freiburg-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/schalte-grosseinsatz-polizei-bad-friedrichshall-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/das-sagt-der-fck-zum-urteil-pressesprecher-stefan-rosskopf-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/stadtbahn-unfall-muehlhausen-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/amokalarm-ausgeloest-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/ot-ortsbuergermeister-bischheim-zu-solarparkprojekt-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/wilde-verfolgungsfahrt-in-homburg-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/ludwigshafen/retroboerse-in-speyer-am-wochenende-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/mannheim/mannheimer-studie-niedriges-einkommen-steigende-mieten-mehr-afd-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/kaiserslautern/schuhfabrik-carl-semler-in-pirmasens-beantragt-insolvenz-in-eigenverwaltung-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/trier/wie-wittlicher-schueler-den-besuch-den-kz-auschwitz-erlebt-haben-100.html",
  "https://www.swr.de/swraktuell/baden-wuerttemberg/tuebingen/fdp-wahlkampf-pascal-kober-100.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/mainz/swr-reporter-juergen-wolff-zu-dem-fall-100.html",
  "https://www.swr.de/swraktuell/snacks/buerger-fragen-der-staat-muss-antworten-wie-steht-es-mit-der-informationsfreiheit-in-rheinland-pfalz-102.html",
  "https://www.swr.de/swraktuell/rheinland-pfalz/trump-und-pfaelzer-unternehmen-die-schon-in-den-usa-produzieren-100.html",
  
  # All Videos 
  "https://www.tagesspiegel.de/im-video-amtseinfuhrung-von-donald-trump-13041838.html",
  "https://www.tagesspiegel.de/gesellschaft/queerspiegel/berlinale-chefin-tricia-tuttle-im-interview-queeres-kino-ist-toll-13149967.html",
  "https://www.tagesspiegel.de/gesellschaft/queerspiegel/erster-winter-csd-winter-csd-13222327.html",
  "https://www.tagesspiegel.de/kultur/regisseurinnen-auf-der-berlinale-der-weibliche-blick-13222351.html",
  "https://www.tagesspiegel.de/videos/autorin-dr-windmuller-video-rucktritt-von-ttt-moderator-gefordert-12952141.html",
  "https://www.tagesspiegel.de/gesellschaft/queerspiegel/flinta-march-tausende-demonstrieren-fur-gleichberechtigung--und-gegen-rechts-13049818.html",
  "https://www.tagesspiegel.de/gesellschaft/queerspiegel/queere-filme-auf-der-berlinale-wo-sind-die-deutschen-beitrage-13238803.html",
  
  # All Phots / media 
  "https://taz.de/Katharina-Guenther-Wuensch/!t6021363",
  "https://taz.de/Liebvondir/!6065595",
  "https://taz.de/Molkenmarkt-Berlin-Mitte/!t6023042",
  "https://taz.de/Dr-TAZ/!6066261",
  "https://taz.de/boulevard-der-besten/!5715442",
  "https://taz.de/Tim-Walz/!t6029002",
  "https://taz.de/Seine/!t6027441", 
  
  # All navigational
  
  "https://www.volksstimme.de/sport/fussball",
  "https://www.volksstimme.de/live",
  "https://www.volksstimme.de/wahl/bundestagswahl-sachsen-anhalt-wahlkreis-67-boerde-salzlandkreis",
  "https://www.volksstimme.de/wahl/bundestagswahl",
  "https://www.volksstimme.de/wahl/bundestagswahl-sachsen-anhalt",
  
  # No Content 
  "https://www.wz.de/nrw/wuppertal/liveblog-so-unterliegt-wuppertaler-sv-beim-vfl-bochum_aid-122694295",
  "https://www.wz.de/nrw/wuppertal/liveblog-so-unterliegt-der-wuppertaler-sv-bei-fortuna-koeln-mit-stimmen_aid-124432749",
  "https://www.wz.de/nrw/wuppertal/liveblog-so-gewinnt-union-die-wuppertaler-stadtmeisterschaften-2025-mit-bildern-und-videos_aid-122693489",
  "https://www.wz.de/sport/fussball/wuppertaler-sv/liveblog-so-unterliegt-der-wuppertaler-sv-in-oberhausen_aid-123409627",
  "https://www.wz.de/sport/fussball/wuppertaler-sv/liveblog-mit-dem-wuppertaler-sv-im-trainingslager-in-der-tuerkei-abreisetag_aid-122984417",
  "https://www.wz.de/sport/fussball/wuppertaler-sv/liveblog-so-gewinnt-der-wuppertaler-sv-gegen-luckenwalde_aid-123083899",
  "https://www.wz.de/sport/fussball/wuppertaler-sv/liveblog-so-verliert-der-wuppertaler-sv-in-paderborn-mit-stimmen_aid-123873265",
  "https://www.wz.de/sport/fussball/wuppertaler-sv/liveblog-so-ueberzeugend-siegt-der-wuppertaler-sv-im-test-gegen-schermbeck-mit-videos_aid-123177525",
  "https://www.wz.de/nrw/wuppertal/liveblog-so-spielt-der-wuppertaler-sv-gegen-duesseldorfs-u-23-mit-stimmen_aid-124170677",
  "https://www.wz.de/nrw/wuppertal/liveblog-so-spielt-der-wuppertaler-sv-gegen-hohkeppel-mit-videos_aid-123663901",
  
  # empty 
  "https://www.wz.de/nrw/krefeld/bundestagswahl-cdu-sichert-sich-den-wahlsieg-in-krefeld_aid-124437233", 
  "https://www.wz.de/nrw/wuppertal/kultur/kunst-im-autohaus-wuppertal-video_iid-124166801", 
  "https://www.wz.de/nrw/wuppertal/kultur/lance-david-arnold-enthuellt-audi-a6-e-tron-in-wuppertal_bid-124003681",
  "https://www.wz.de/nrw/krefeld/krefeld-sek-einsatz-sorgt-fuer-aufsehen-in-der-innenstadt_bid-122874553",
  # poll 
  "https://www.wz.de/nrw/wuppertal/bundestagswahl-2025-lindh-spd-gewinnt-in-wuppertal_aid-124417467",
  
  
  # All those are topics, no content 
  "https://www.tag24.de/unterhaltung/tv/kripo-live",
  "https://www.tag24.de/ratgeber/haushalt/reinigen",
  "https://www.tag24.de/sport/fussball/verein/fc-st-pauli",
  "https://www.tag24.de/sport/sportler/lukas-podolski",
  "https://www.tag24.de/nachrichten/wirtschaft/unternehmen",
  "https://www.tag24.de/sport/american-football/nfl",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartengestaltung/naturgarten/tierschutz-im-garten",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartengestaltung",
  "https://www.tag24.de/unterhaltung/tv/tv-tipps",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/gerhard-schroeder",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien/buendnis-sahra-wagenknecht",
  "https://www.tag24.de/sport/fussball/verein/vfl-wolfsburg",
  "https://www.tag24.de/ratgeber/haushalt/gerueche-neutralisieren",
  "https://www.tag24.de/ratgeber/essen-und-trinken/ofengerichte/auflauf-rezepte",
  "https://www.tag24.de/ratgeber/essen-und-trinken/mittagessen",
  "https://www.tag24.de/unterhaltung/tv/rtl",
  "https://www.tag24.de/sport/basketball/dresden-titans",
  "https://www.tag24.de/unterhaltung/tv/promis-unter-palmen",
  "https://www.tag24.de/ratgeber/haustierratgeber/kleintiere-ratgeber",
  "https://www.tag24.de/nachrichten/politik/international/politiker-international/donald-trump",
  "https://www.tag24.de/ratgeber/essen-und-trinken/kartoffel-rezepte",
  "https://www.tag24.de/sport/fussball/verein/fc-erzgebirge-aue",
  "https://www.tag24.de/ratgeber/essen-und-trinken/vegane-rezepte",
  "https://www.tag24.de/unterhaltung/tv/prominent-getrennt",
  "https://www.tag24.de/ratgeber/essen-und-trinken/haltbar-machen",
  "https://www.tag24.de/ratgeber/essen-und-trinken/fleisch-rezepte",
  "https://www.tag24.de/sport/fussball/verein/lok-leipzig",
  "https://www.tag24.de/nachrichten/regionales/nordsee",
  "https://www.tag24.de/sport/fussball/verein/sg-dynamo-dresden",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/annalena-baerbock",
  "https://www.tag24.de/unterhaltung/tv/hochzeit-auf-den-ersten-blick",
  "https://www.tag24.de/ratgeber/leben/sachsen/chemnitz",
  "https://www.tag24.de/sport/wintersport/eishockey/eispiraten-crimmitschau",
  "https://www.tag24.de/unterhaltung/musik/rammstein",
  "https://www.tag24.de/ratgeber/essen-und-trinken/weihnachtsrezepte",
  "https://www.tag24.de/unterhaltung/tv/gzsz",
  "https://www.tag24.de/sport/wintersport/ski-alpin",
  "https://www.tag24.de/unterhaltung/tv/prince-charming",
  "https://www.tag24.de/ratgeber/essen-und-trinken/fruehling-rezepte",
  "https://www.tag24.de/ratgeber/leben/magdeburg",
  "https://www.tag24.de/ratgeber/essen-und-trinken/backen",
  "https://www.tag24.de/unterhaltung/tv/ndr-talk-show",
  "https://www.tag24.de/unterhaltung/tv/rote-rosen",
  "https://www.tag24.de/ratgeber/essen-und-trinken/ofengerichte",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/reiner-haseloff",
  "https://www.tag24.de/unterhaltung/tv/gzsz/schauspieler",
  "https://www.tag24.de/unterhaltung/tv/geissens",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/kevin-kuehnert",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/wirtschaftspolitik",
  "https://www.tag24.de/ratgeber/essen-und-trinken/dessert",
  "https://www.tag24.de/sport/fussball/verein/vfb-stuttgart",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartenkalender/gartenarbeit-sommer",
  "https://www.tag24.de/ratgeber/essen-und-trinken/kochen",
  "https://www.tag24.de/unterhaltung/musik/rap-news",
  "https://www.tag24.de/sport/fussball/verein/karlsruher-sc",
  "https://www.tag24.de/unterhaltung/tv/first-dates",
  "https://www.tag24.de/nachrichten/politik/international/politiker-international/erdogan",
  "https://www.tag24.de/unterhaltung/tv/the-voice-of-germany",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/hubert-aiwanger",
  "https://www.tag24.de/nachrichten/politik/international/politiker-international/kim-jong-un",
  "https://www.tag24.de/unterhaltung/tv/tv-krimis",
  "https://www.tag24.de/sport/fussball/verein/vfl-osnabrueck",
  "https://www.tag24.de/sport/sportler/max-kruse",
  "https://www.tag24.de/unterhaltung/tv/wer-stiehlt-mir-die-show",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/energiepolitik",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien/cdu",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/cem-oezdemir",
  "https://www.tag24.de/sport/sportler/uli-hoeness",
  "https://www.tag24.de/sport/fussball/verein/fsv-zwickau",
  "https://www.tag24.de/sport/fussball/sachsenpokal",
  "https://www.tag24.de/sport/fussball/frauenfussball",
  "https://www.tag24.de/sport/fussball/dfb/dfb-pokal",
  "https://www.tag24.de/sport/sportler/andreas-wellinger",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartengestaltung/naturgarten",
  "https://www.tag24.de/sport/volleyball/dresden-dsc-damen",
  "https://www.tag24.de/sport/fussball/verein/fc-hansa-rostock",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/ricarda-lang",
  "https://www.tag24.de/sport/handball/handball-em",
  "https://www.tag24.de/unterhaltung/tv/bauer-sucht-frau",
  "https://www.tag24.de/nachrichten/politik/deutschland/wahlen",
  "https://www.tag24.de/sport/fussball/verein/sc-freiburg",
  "https://www.tag24.de/unterhaltung/tv/princess-charming",
  "https://www.tag24.de/ratgeber/essen-und-trinken/omas-rezepte",
  "https://www.tag24.de/nachrichten/politik/deutschland",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/michael-kretschmer",
  "https://www.tag24.de/sport/wintersport/eishockey/koelner-haie",
  "https://www.tag24.de/ratgeber/haustierratgeber/hunde-ratgeber/hundeverhalten",
  "https://www.tag24.de/ratgeber/haustierratgeber/hunde-ratgeber/hunde-erziehung",
  "https://www.tag24.de/unterhaltung/kino-news/filmkritik",
  "https://www.tag24.de/sport/wintersport/eishockey/dresdner-eisloewen",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/bjoern-hoecke",
  "https://www.tag24.de/sport/fussball/bundesliga/zweite-bundesliga",
  "https://www.tag24.de/ratgeber/haus-und-garten/balkon-terrasse-gestalten",
  "https://www.tag24.de/unterhaltung/tv/die-wollnys",
  "https://www.tag24.de/sport/wintersport/skispringen/vierschanzentournee",
  "https://www.tag24.de/unterhaltung/tv/armes-deutschland",
  "https://www.tag24.de/unterhaltung/tv/rosenheim-cops",
  "https://www.tag24.de/ratgeber/haushalt/flecken-entfernen",
  "https://www.tag24.de/sport/american-football/nfl/super-bowl",
  "https://www.tag24.de/unterhaltung/tv/von-hecke-zu-hecke",
  "https://www.tag24.de/unterhaltung/tv/bachelor-in-paradise",
  "https://www.tag24.de/sport/sportler/hansi-flick",
  "https://www.tag24.de/sport/fussball/verein/fc-carl-zeiss-jena",
  "https://www.tag24.de/ratgeber/essen-und-trinken/kuerbis-rezepte",
  "https://www.tag24.de/ratgeber/essen-und-trinken/suppen",
  "https://www.tag24.de/unterhaltung/tv/inas-nacht",
  "https://www.tag24.de/unterhaltung/tv/grossstadtrevier",
  "https://www.tag24.de/ratgeber/essen-und-trinken/fruehstueck-rezepte",
  "https://www.tag24.de/unterhaltung/tv/das-grosse-promibuessen",
  "https://www.tag24.de/ratgeber/haustierratgeber/katzen-ratgeber",
  "https://www.tag24.de/unterhaltung/tv/riverboat",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/boris-pistorius",
  "https://www.tag24.de/sport/fussball/europa-league",
  "https://www.tag24.de/ratgeber/essen-und-trinken/abendessen",
  "https://www.tag24.de/unterhaltung/royales/kate-middleton",
  "https://www.tag24.de/unterhaltung/tv/shopping-queen",
  "https://www.tag24.de/unterhaltung/tv/unter-uns",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/markus-soeder",
  "https://www.tag24.de/sport/fussball/verein/arminia-bielefeld",
  "https://www.tag24.de/sport/fussball/verein/bsg-chemie-leipzig",
  "https://www.tag24.de/sport/sportler/alica-schmidt",
  "https://www.tag24.de/sport/fussball/champions-league",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/familienpolitik",
  "https://www.tag24.de/sport/fussball/verein/hallescher-fc",
  "https://www.tag24.de/nachrichten/politik/international/politiker-international/wladimir-putin",
  "https://www.tag24.de/unterhaltung/kino-news/james-bond",
  "https://www.tag24.de/unterhaltung/royales/prinz-harry",
  "https://www.tag24.de/ratgeber/haushalt/ungeziefer",
  "https://www.tag24.de/justiz/vermisste-personen/rebecca-reusch",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/haushaltspolitik",
  "https://www.tag24.de/unterhaltung/tv/mein-lokal-dein-lokal",
  "https://www.tag24.de/unterhaltung/tv/supertalent",
  "https://www.tag24.de/ratgeber/wohnen-und-deko/einrichtungsideen",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/volker-wissing",
  "https://www.tag24.de/unterhaltung/tv/big-brother",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/nancy-faeser",
  "https://www.tag24.de/unterhaltung/tv/gzsz/vorschau",
  "https://www.tag24.de/unterhaltung/tv/hartes-deutschland",
  "https://www.tag24.de/sport/fussball/bundesliga/relegation",
  "https://www.tag24.de/unterhaltung/royales/william-mountbatten-windsor",
  "https://www.tag24.de/unterhaltung/tv/die-bachelorette",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartenkalender",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartenpflege",
  "https://www.tag24.de/unterhaltung/tv/lol-last-one-laughing",
  "https://www.tag24.de/ratgeber/haustierratgeber/katzen-ratgeber/katzenernaehrung",
  "https://www.tag24.de/sport/fussball/frauenfussball/frauen-fussball-wm",
  "https://www.tag24.de/unterhaltung/royales/koenig-charles",
  "https://www.tag24.de/unterhaltung/tv/tagesschau",
  "https://www.tag24.de/ratgeber/haushalt/entkalken",
  "https://www.tag24.de/sport/fussball/dfb",
  "https://www.tag24.de/sport/fussball/europameisterschaft",
  "https://www.tag24.de/unterhaltung/tv/dokus",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/hendrik-wuest",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien/die-linke",
  "https://www.tag24.de/sport/fussball/verein/fc-augsburg",
  "https://www.tag24.de/nachrichten/politik/deutschland/militaer-und-verteidigung",
  "https://www.tag24.de/sport/sportler/harry-kane",
  "https://www.tag24.de/ratgeber/leben/sachsen",
  "https://www.tag24.de/unterhaltung/streaming/dazn",
  "https://www.tag24.de/ratgeber/essen-und-trinken/nudel-rezepte",
  "https://www.tag24.de/sport/fussball/verein/rb-leipzig",
  "https://www.tag24.de/ratgeber/leben/hamburg",
  "https://www.tag24.de/unterhaltung/streaming/disney-plus",
  "https://www.tag24.de/unterhaltung/tv/mdr-umschau",
  "https://www.tag24.de/unterhaltung/tv/dschungelcamp",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/ursula-von-der-leyen",
  "https://www.tag24.de/sport/fussball/bundesliga/dritte-liga",
  "https://www.tag24.de/unterhaltung/tv/germanysnexttopmodel",
  "https://www.tag24.de/sport/handball/hc-elbflorenz",
  "https://www.tag24.de/unterhaltung/tv/good-luck-guys",
  "https://www.tag24.de/ratgeber/haustierratgeber/katzen-ratgeber/katzenrassen",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/sahra-wagenknecht",
  "https://www.tag24.de/justiz/polizei/hubschraubereinsatz",
  "https://www.tag24.de/ratgeber/rekorde/naturrekorde",
  "https://www.tag24.de/ratgeber/essen-und-trinken/backen/gebaeck",
  "https://www.tag24.de/unterhaltung/tv/erzgebirgskrimi",
  "https://www.tag24.de/nachrichten/regionales/wetter",
  "https://www.tag24.de/sport/olympia/olympische-winterspiele",
  "https://www.tag24.de/ratgeber/essen-und-trinken/rezepte-mit-eiern",
  "https://www.tag24.de/ratgeber/essen-und-trinken/vegetarische-rezepte",
  "https://www.tag24.de/sport/handball/sc-dhfk-leipzig",
  "https://www.tag24.de/sport/sportler/pep-guardiola",
  "https://www.tag24.de/ratgeber/essen-und-trinken/haltbar-machen/gemuese-einkochen",
  "https://www.tag24.de/sport/wintersport/eishockey",
  "https://www.tag24.de/unterhaltung/tv/in-aller-freundschaft",
  "https://www.tag24.de/ratgeber/essen-und-trinken/salate",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/jens-spahn",
  "https://www.tag24.de/sport/sportler/julian-nagelsmann",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien/freie-waehler",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/gesundheitspolitik",
  "https://www.tag24.de/unterhaltung/streaming/amazon-prime",
  "https://www.tag24.de/unterhaltung/streaming/netflix",
  "https://www.tag24.de/unterhaltung/tv/wer-wird-millionaer",
  "https://www.tag24.de/sport/sportler/toni-kroos",
  "https://www.tag24.de/sport/fussball/fifa-weltmeisterschaft",
  "https://www.tag24.de/sport/sportler/mick-schumacher",
  "https://www.tag24.de/ratgeber/leben/sachsen/leipzig",
  "https://www.tag24.de/unterhaltung/tv/tatort",
  "https://www.tag24.de/ratgeber/haushalt/entsorgen",
  "https://www.tag24.de/unterhaltung/royales/meghan-markle",
  "https://www.tag24.de/ratgeber/essen-und-trinken/getraenke",
  "https://www.tag24.de/unterhaltung/tv/elefant-tiger-co",
  "https://www.tag24.de/sport/handball/handball-wm",
  "https://www.tag24.de/sport/fussball/verein/fc-rot-weiss-erfurt",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/alice-weidel",
  "https://www.tag24.de/unterhaltung/tv/sturm-der-liebe",
  "https://www.tag24.de/unterhaltung/tv/das-sommerhaus-der-stars",
  "https://www.tag24.de/sport/fussball/bundesliga",
  "https://www.tag24.de/sport/sportler/juergen-klopp",
  "https://www.tag24.de/unterhaltung/tv/mdr-exakt",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartenkalender/gartenarbeit-winter",
  "https://www.tag24.de/unterhaltung/tv/love-island-show",
  "https://www.tag24.de/sport/sportler/steffen-baumgart",
  "https://www.tag24.de/sport/fussball/transfermarkt",
  "https://www.tag24.de/sport/handball/sc-magdeburg",
  "https://www.tag24.de/ratgeber/wohnen-und-deko/zimmerpflanzen",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartengestaltung/naturgarten/pflanzenschutz-im-garten",
  "https://www.tag24.de/unterhaltung/tv/hartz-rot-gold",
  "https://www.tag24.de/nachrichten/politik/international/politiker-international/joe-biden",
  "https://www.tag24.de/unterhaltung/tv/wolfsland",
  "https://www.tag24.de/ratgeber/essen-und-trinken/ddr-rezepte",
  "https://www.tag24.de/ratgeber/essen-und-trinken/zucchini-rezepte",
  "https://www.tag24.de/unterhaltung/tv/notruf-hafenkante",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/olaf-scholz",
  "https://www.tag24.de/unterhaltung/tv/joko-klaas-gegen-prosieben",
  "https://www.tag24.de/unterhaltung/tv/tieraerztin-dr-mertens",
  "https://www.tag24.de/ratgeber/leben/sachsen/dresden",
  "https://www.tag24.de/unterhaltung/tv/bergdoktor",
  "https://www.tag24.de/sport/fussball/verein/hsv",
  "https://www.tag24.de/ratgeber/essen-und-trinken/snacks-rezepte",
  "https://www.tag24.de/sport/fussball/verein/werder-bremen",
  "https://www.tag24.de/sport/fussball/verein/spvgg-greuther-fuerth",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien/csu",
  "https://www.tag24.de/nachrichten/politik/europaeische-union",
  "https://www.tag24.de/unterhaltung/tv/bachelor",
  "https://www.tag24.de/sport/fussball/verein/fc-bayern-muenchen",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartenpflege/rasenpflege",
  "https://www.tag24.de/ratgeber/essen-und-trinken/backen/broetchen",
  "https://www.tag24.de/sport/fussball/verein/chemnitzer-fc",
  "https://www.tag24.de/ratgeber/essen-und-trinken/ostern-rezepte",
  "https://www.tag24.de/nachrichten/regionales/wetter/unwetter-deutschland",
  "https://www.tag24.de/unterhaltung/tv/schwiegertochter-gesucht",
  "https://www.tag24.de/sport/american-football/dresden-monarchs",
  "https://www.tag24.de/unterhaltung/tv/make-love-fake-love",
  "https://www.tag24.de/sport/fussball/fussball-international",
  "https://www.tag24.de/sport/fussball/verein/hertha-bsc",
  "https://www.tag24.de/sport/wintersport/bobsport",
  "https://www.tag24.de/sport/fussball/bundesliga/erste-bundesliga",
  "https://www.tag24.de/unterhaltung/tv/reality-backpackers",
  "https://www.tag24.de/unterhaltung/tv/kampf-der-realitystars",
  "https://www.tag24.de/ratgeber/essen-und-trinken/herbst-rezepte",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/marie-agnes-strack-zimmermann",
  "https://www.tag24.de/nachrichten/regionales/ostsee",
  "https://www.tag24.de/ratgeber/essen-und-trinken/backen/brot-broetchen",
  "https://www.tag24.de/unterhaltung/tv/love-fool",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/verkehrspolitik",
  "https://www.tag24.de/sport/fussball/deutsche-nationalmannschaft",
  "https://www.tag24.de/unterhaltung/tv/aktenzeichen-xy",
  "https://www.tag24.de/sport/sportler/fabian-huerzeler",
  "https://www.tag24.de/nachrichten/politik/international/politiker-international/kamala-harris",
  "https://www.tag24.de/ratgeber/rekorde/menschliche-rekorde",
  "https://www.tag24.de/ratgeber/essen-und-trinken/backen/torten-rezepte",
  "https://www.tag24.de/sport/fussball/dfb-pokal",
  "https://www.tag24.de/unterhaltung/tv/das-perfekte-dinner",
  "https://www.tag24.de/unterhaltung/musik/schlager-news",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien/fdp",
  "https://www.tag24.de/sport/fussball/nations-league",
  "https://www.tag24.de/nachrichten/unfall/staumeldungen",
  "https://www.tag24.de/ratgeber/essen-und-trinken/haltbar-machen/marmelade-einkochen",
  "https://www.tag24.de/sport/sportler/francesco-friedrich",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/winfried-kretschmann",
  "https://www.tag24.de/unterhaltung/tv/die-hoehle-der-loewen",
  "https://www.tag24.de/unterhaltung/tv/are-you-the-one",
  "https://www.tag24.de/unterhaltung/tv/forsthaus-rampensau",
  "https://www.tag24.de/ratgeber/essen-und-trinken/sommer-rezepte",
  "https://www.tag24.de/unterhaltung/musik/kraftklub",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/robert-habeck",
  "https://www.tag24.de/sport/basketball/basketball-bundesliga",
  "https://www.tag24.de/unterhaltung/tv/the-voice-kids",
  "https://www.tag24.de/ratgeber/essen-und-trinken/kuechenwissen",
  "https://www.tag24.de/unterhaltung/tv/temptation-island",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/christian-lindner",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartenkalender/gartenarbeit-fruehling",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/sozialpolitik",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/fluechtlingspolitik",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/bodo-ramelow",
  "https://www.tag24.de/ratgeber/haustierratgeber/katzen-ratgeber/katzengesundheit",
  "https://www.tag24.de/ratgeber/haus-und-garten/gartenkalender/gartenarbeit-herbst",
  "https://www.tag24.de/unterhaltung/tv/ex-on-the-beach",
  "https://www.tag24.de/nachrichten/politik/international/politiker-international/emmanuel-macron",
  "https://www.tag24.de/unterhaltung/tv/lets-dance",
  "https://www.tag24.de/sport/olympia/olympische-sommerspiele",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/tino-chrupalla",
  "https://www.tag24.de/nachrichten/politik/deutschland/gesellschaft",
  "https://www.tag24.de/nachrichten/politik/international/politiker-international",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/dirk-hilbert",
  "https://www.tag24.de/ratgeber/essen-und-trinken/winter-rezepte",
  "https://www.tag24.de/ratgeber/haustierratgeber/katzen-ratgeber/katzenverhalten",
  "https://www.tag24.de/unterhaltung/tv/talkshows",
  "https://www.tag24.de/sport/fussball/verein/borussia-moenchengladbach",
  "https://www.tag24.de/unterhaltung/tv/promi-big-brother",
  "https://www.tag24.de/nachrichten/politik/deutschland/wahlen/europawahl",
  "https://www.tag24.de/sport/fussball/verein/borussia-dortmund",
  "https://www.tag24.de/ratgeber/rekorde/tierrekorde",
  "https://www.tag24.de/unterhaltung/tv/alles-was-zaehlt",
  "https://www.tag24.de/sport/fussball/verein/fc-magdeburg",
  "https://www.tag24.de/ratgeber/essen-und-trinken/rezepte",
  "https://www.tag24.de/sport/wintersport/biathlon",
  "https://www.tag24.de/unterhaltung/musik/kmn-gang",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/friedrich-merz",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/hans-georg-maassen",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/energiewende",
  "https://www.tag24.de/ratgeber/wohnen-und-deko/deko-ideen",
  "https://www.tag24.de/ratgeber/haustierratgeber/hunde-ratgeber",
  "https://www.tag24.de/nachrichten/politik/international",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/integration",
  "https://www.tag24.de/sport/fussball/verein/vfl-bochum",
  "https://www.tag24.de/nachrichten/politik/deutschland/aussenpolitik",
  "https://www.tag24.de/sport/fussball/verein/fortuna-duesseldorf",
  "https://www.tag24.de/unterhaltung/tv/masked-singer",
  "https://www.tag24.de/sport/wintersport/skispringen",
  "https://www.tag24.de/sport/basketball/niners-chemnitz",
  "https://www.tag24.de/sport/fussball/verein/holstein-kiel",
  "https://www.tag24.de/unterhaltung/tv/soko-leipzig",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/klimapolitik",
  "https://www.tag24.de/ratgeber/leben/berlin",
  "https://www.tag24.de/ratgeber/haustierratgeber/hunde-ratgeber/hundegesundheit",
  "https://www.tag24.de/unterhaltung/tv/dsds",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/angela-merkel",
  "https://www.tag24.de/nachrichten/politik/deutschland/wahlen/bundestagswahl",
  "https://www.tag24.de/sport/fussball/regionalliga",
  "https://www.tag24.de/unterhaltung/tv/zdf-fernsehgarten",
  "https://www.tag24.de/ratgeber/essen-und-trinken/backen/kuchen",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/umweltpolitik",
  "https://www.tag24.de/sport/sportler/thomas-tuchel",
  "https://www.tag24.de/sport/basketball/nba",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien/afd",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/karl-lauterbach",
  "https://www.tag24.de/unterhaltung/tv/hart-aber-fair",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik",
  "https://www.tag24.de/sport/sportler/mats-hummels",
  "https://www.tag24.de/ratgeber/essen-und-trinken/aufstrich-rezepte",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/sicherheitspolitik",
  "https://www.tag24.de/justiz/ungeklaerte-kriminalfaelle/maddie-mccann",
  "https://www.tag24.de/unterhaltung/royales/queen-elizabeth",
  "https://www.tag24.de/sport/fussball/verein/sv-sandhausen",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien/werteunion",
  "https://www.tag24.de/sport/fussball/verein",
  "https://www.tag24.de/sport/sportler/xabi-alonso",
  "https://www.tag24.de/nachrichten/politik/deutschland/innenpolitik/bildungspolitik",
  "https://www.tag24.de/sport/fussball/verein/energie-cottbus",
  "https://www.tag24.de/sport/fussball/verein/eintracht-frankfurt",
  "https://www.tag24.de/sport/fussball/dfl",
  "https://www.tag24.de/unterhaltung/tv/goodbye-deutschland",
  "https://www.tag24.de/unterhaltung/tv/bares-fuer-rares",
  "https://www.tag24.de/unterhaltung/tv/hartz-und-herzlich",
  "https://www.tag24.de/nachrichten/politik/deutschland/politiker/boris-palmer",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien/spd",
  "https://www.tag24.de/ratgeber/essen-und-trinken/reis-gerichte",
  "https://www.tag24.de/ratgeber/haustierratgeber/hunde-ratgeber/hunderassen",
  "https://www.tag24.de/nachrichten/unfall/unfall-a59",
  "https://www.tag24.de/nachrichten/unfall/unfall-a20",
  "https://www.tag24.de/nachrichten/unfall/unfall-a23",
  "https://www.tag24.de/nachrichten/unfall/unfall-a44",
  "https://www.tag24.de/nachrichten/unfall/unfall-a5",
  "https://www.tag24.de/unterhaltung/tv/polizeiruf-110",
  "https://www.tag24.de/nachrichten/unfall/unfall-a4",
  "https://www.tag24.de/sport/fussball/verein/1-fc-koeln",
  "https://www.tag24.de/nachrichten/unfall/unfall-a73",
  "https://www.tag24.de/nachrichten/unfall/unfall-a66",
  "https://www.tag24.de/nachrichten/unfall/unfall-a17",
  "https://www.tag24.de/nachrichten/unfall/unfall-a30",
  "https://www.tag24.de/nachrichten/unfall/unfall-a95",
  "https://www.tag24.de/nachrichten/unfall/unfall-a71",
  "https://www.tag24.de/nachrichten/unfall/unfall-a94",
  "https://www.tag24.de/nachrichten/unfall/unfall-a61",
  "https://www.tag24.de/sport/fussball/verein/fc-schalke-04",
  "https://www.tag24.de/nachrichten/politik/deutschland/parteien/bnd90-die-gruenen",
  "https://www.tag24.de/nachrichten/unfall/unfall-a19",
  "https://www.tag24.de/nachrichten/unfall/unfall-a81",
  "https://www.tag24.de/nachrichten/unfall/unfall-a2",
  "https://www.tag24.de/nachrichten/unfall/unfall-a3",
  "https://www.tag24.de/sport/fussball/verein/tsg-1899-hoffenheim",
  "https://www.tag24.de/nachrichten/unfall/unfall-a9",
  "https://www.tag24.de/nachrichten/unfall/unfall-a25",
  "https://www.tag24.de/nachrichten/unfall/unfall-a72",
  "https://www.tag24.de/nachrichten/unfall/unfall-a8",
  "https://www.tag24.de/nachrichten/unfall/unfall-a6",
  "https://www.tag24.de/nachrichten/unfall/unfall-a13",
  "https://www.tag24.de/nachrichten/unfall/unfall-a38",
  "https://www.tag24.de/sport/fussball/verein/sc-paderborn-07",
  "https://www.tag24.de/sport/fussball/verein/1-fc-union-berlin",
  "https://www.tag24.de/nachrichten/unfall/unfall-a10",
  "https://www.tag24.de/nachrichten/unfall/unfall-a24",
  "https://www.tag24.de/sport/fussball/verein/1-fc-heidenheim",
  "https://www.tag24.de/nachrichten/unfall/unfall-a96",
  "https://www.tag24.de/unterhaltung/musik/187-strassenbande",
  "https://www.tag24.de/sport/fussball/verein/1-fsv-mainz-05",
  "https://www.tag24.de/nachrichten/unfall/unfall-a100",
  "https://www.tag24.de/nachrichten/unfall/unfall-a93",
  "https://www.tag24.de/unterhaltung/tv/3nach9",
  "https://www.tag24.de/sport/motorsport/formel-1",
  "https://www.tag24.de/nachrichten/unfall/unfall-a57",
  "https://www.tag24.de/sport/fussball/verein/hannover-96",
  "https://www.tag24.de/nachrichten/unfall/unfall-a14",
  "https://www.tag24.de/nachrichten/unfall/unfall-a111",
  "https://www.tag24.de/sport/fussball/verein/tsv-1860-muenchen",
  "https://www.tag24.de/nachrichten/unfall/unfall-a39",
  "https://www.tag24.de/nachrichten/unfall/unfall-a12",
  "https://www.tag24.de/sport/fussball/verein/sv-darmstadt-98",
  "https://www.tag24.de/nachrichten/unfall/unfall-a7",
  "https://www.tag24.de/unterhaltung/tv/the-50",
  "https://www.tag24.de/nachrichten/unfall/unfall-a92",
  "https://www.tag24.de/nachrichten/unfall/unfall-a46",
  "https://www.tag24.de/sport/fussball/verein/bayer-04-leverkusen",
  "https://www.tag24.de/sport/fussball/verein/1-fc-nuernberg",
  "https://www.tag24.de/nachrichten/unfall/unfall-a21",
  "https://www.tag24.de/nachrichten/unfall/unfall-a1",
  "https://www.tag24.de/nachrichten/unfall/unfall-a99",
  "https://www.tag24.de/unterhaltung/tv/7-vs-wild",
  "https://www.tag24.de/nachrichten/unfall/unfall-a33",
  
  
  # Live blogs, no content 
  "https://www.zeit.de/politik/deutschland/2025-01/asylpolitik-union-afd-abstimmung-bundestag-liveblog",
  "https://www.zeit.de/politik/deutschland/2025-02/tv-duell-bundestagswahl-olaf-scholz-friedrich-merz-live",
  "https://www.zeit.de/gesellschaft/zeitgeschehen/2025-02/livestream-muenchen-polizei-pressekonferenz",
  "https://www.zeit.de/politik/ausland/2024-12/syrien-assad-sturz-liveblog",
  "https://www.zeit.de/politik/2025-01/wolodymyr-selenskyj-weltwirtschaftsforum-davos-2025-live",
  "https://www.zeit.de/politik/deutschland/2025-02/tv-quadrell-bundestagswahl-friedrich-merz-alice-weidel-robert-habeck-olaf-scholz-live",
  "https://www.zeit.de/politik/deutschland/2025-02/tv-debatte-bundestagswahl-olaf-scholz-friedrich-merz-robert-habeck-alice-weidel-live",
  "https://www.zeit.de/politik/deutschland/2025-01/migrationsplaene-union-abstimmung-bundestag-liveblog",
  "https://www.zeit.de/politik/deutschland/2025-01/innen-asylpolitik-regierungserklaerung-olaf-scholz-migration-cdu-live",
  "https://www.zeit.de/politik/ausland/2023-12/news-israel-gaza-krieg-live",
  "https://www.zeit.de/politik/deutschland/2025-01/migrationsgesetz-bundestag-abstimmung-zuwanderungsbegrenzungsgesetz-live",
  "https://www.zeit.de/politik/deutschland/2025-02/olaf-scholz-rede-muenchner-sicherheitskonferenz-muenchen-live",
  "https://www.zeit.de/politik/2025-01/olaf-scholz-weltwirtschaftsforum-davos-2025-live",
  "https://www.zeit.de/politik/ausland/2025-01/amtseinfuehrung-donald-trump-usa-praesidentschaft-live",
  "https://www.zeit.de/politik/deutschland/2025-02/bundestagsdebatte-situation-deutschland-letzte-sitzung-bundestagswahl-live",
  "https://www.zeit.de/politik/2025-02/olaf-scholz-paris-ukraine-gipfel-europa-live",
  "https://www.zeit.de/politik/2025-01/fdp-dreikoenigstreffen-christian-lindner-stuttgart-live",
  "https://www.zeit.de/politik/ausland/2025-02/wolodymyr-selenskyj-rede-muenchner-sicherheitskonferenz-live",
  "https://www.zeit.de/sport/2025-02/super-bowl-lix-2025-nfl-live",
  
  # Videos:
  
  "https://www.zeit.de/politik/ausland/2025-02/vance-usa-russland-ukraine-putin-trump",
  "https://www.zeit.de/politik/ausland/2025-01/trump-davos-weltwirtschaftsforum-usa-russland-china",
  "https://www.zeit.de/politik/ausland/2025-01/waldbraende-kalifornien-los-angeles-feuer-evakuierung-betroffene",
  "https://www.zeit.de/politik/2025-01/80-jahre-auschwitz-befreiung-ueberlebende-gedenktag-nationalsozialismus",
  "https://www.zeit.de/kultur/film/2025-01/horst-janson-schauspieler-sesamstrasse-fs",
  "https://www.zeit.de/politik/deutschland/2025-01/migrationsdebatte-bundestag-abstimmung-friedrich-merz-cdu-momente",
  "https://www.zeit.de/wirtschaft/2025-02/superreiche-ungleichheit-vermoegen-demokratie-gefahr",
  "https://www.zeit.de/politik/2025-01/trump-inauguration-saebel-filzstift",
  "https://www.zeit.de/wissen/2025-01/waldbraende-kalifornien-feuerfeste-haeuser-architektur-los-angeles",
  "https://www.zeit.de/politik/deutschland/2025-02/angela-merkel-interview-eine-stunde-zeit-zusammenschnitt",
  "https://www.zeit.de/politik/ausland/2025-01/braende-los-angeles-video-flammen",
  "https://www.zeit.de/gesellschaft/2025-01/margot-friedlaender-holocaust-ueberlebende-erinnerung-gedenktag",
  "https://www.zeit.de/gesellschaft/2025-02/mutmasslicher-anschlag-muenchen-demonstration-auto-verletzte",
  "https://www.zeit.de/politik/deutschland/2025-02/muenchen-auto-menschenmenge",
  
  # All navigational
  "https://www.shz.de/deutschland-welt/panorama/tatort-reiterhof",
  "https://www.shz.de/sport/ergebnisse-tabellen/frauen-handball/kreisliga-hh",
  "https://www.shz.de/sport/ergebnisse-tabellen/frauen-handball/landesliga-hh",
  "https://www.shz.de/deutschland-welt/schleswig-holstein/robert-habeck",
  "https://www.shz.de/audiothek/true-crime",
  "https://www.shz.de/sport/ergebnisse-tabellen/frauen-handball/bezirksoberliga-hh",
  "https://www.shz.de/deutschland-welt/kindernachrichten/kiwi-reporter",
  "https://www.shz.de/deutschland-welt/politik/bundestagswahl/olaf-scholz",
  "https://www.shz.de/deutschland-welt/politik/bundestagswahl/alice-weidel",
  "https://www.shz.de/sport/ergebnisse-tabellen/handball/oberliga-hh",
  "https://www.shz.de/sport/ergebnisse-tabellen/handball/landesliga-hh",
  "https://www.shz.de/lebenswelten/reisen-freizeit/caravan-und-co",
  "https://www.shz.de/service/nutzerumfragen-shz/nutzerumfragen-profil",
  "https://www.shz.de/sport/handball/handball-em",
  "https://www.shz.de/suche",
  "https://www.shz.de/sport/ergebnisse-tabellen/handball/kreisliga-hh",
  "https://www.shz.de/deutschland-welt/kindernachrichten/umwelt",
  "https://www.shz.de/deutschland-welt/schleswig-holstein/auskenner-quiz",
  "https://www.shz.de/sport/ergebnisse-tabellen/frauen-handball/oberliga-hh",
  "https://www.shz.de/sport/ergebnisse-tabellen/handball/bezirksoberliga-hh",
  "https://www.shz.de/newsletter/durchblick-abend/artikel/wie-ist-der-erste-tatort-der-stelzenmann-mit-ulrike-folkerts-48154839",
  
  # empty 
  "https://www.shz.de/lokales/neumuenster/artikel/teure-ampelschaltung-tempo-30-und-radler-auf-die-strasse-48130081",
  
  # empty 
  "https://www.fr.de/wissen/gefluegelter-liebesreigen-der-tanz-der-kraniche-beginnt-zr-93569121.html",
  "https://www.fr.de/rhein-main/austausch-93584084.html",
  "https://www.fr.de/rhein-main/infos-93568717.html",
  "https://www.fr.de/rhein-main/anlaufstelle-fuer-betroffene-und-begleiter-93558069.html",
  "https://www.fr.de/rhein-main/oeffnungszeiten-93536736.html",
  "https://www.fr.de/rhein-main/vhs-hochtaunus-in-zahlen-93527833.html",
  "https://www.fr.de/rhein-main/drei-fragen-an-dr-andreas-hain-93502636.html",
  "https://www.fr.de/rhein-main/lukrative-altpapier-verwertung-93497510.html"
)


# --- 3. HELPER FUNCTIONS ---

# Function to get 410 links from log
get_410_links <- function(log_path) {
  if (!file.exists(log_path)) return(character(0))
  response_log <- readRDS(log_path)
  setDT(response_log)
  if ("status_code" %in% names(response_log)) {
    return(unique(response_log[status_code == 410, url]))
  }
  return(character(0))
}

# Function to construct regex pattern for path exclusions
build_url_pattern <- function(domain_with_tld, excluded_paths) {
  host_regex  <- gsub("\\.", "\\\\.", domain_with_tld)
  paths_regex <- paste(excluded_paths, collapse = "|")
  # Matches: http(s)://(www.)domain.tld/anything(path_regex)
  sprintf("^https?://(www\\.)?%s/.*?(%s)", host_regex, paths_regex)
}

# Function to identify indices to remove based on path rules
get_path_exclusion_indices <- function(dt) {
  exclusion_lists <- ls(pattern = "_excluded_links$", envir = .GlobalEnv)
  to_drop_idx <- integer(0)
  
  for (list_name in exclusion_lists) {
    domain_with_tld <- str_replace(list_name, "_excluded_links$", "")
    excluded_paths  <- get(list_name, envir = .GlobalEnv)
    
    if (length(excluded_paths) == 0) next
    
    url_pattern <- build_url_pattern(domain_with_tld, excluded_paths)
    
    # Check if 'domain' column exists, otherwise extract from URL or just match URL pattern
    if ("domain" %in% names(dt)) {
      hits <- which(str_detect(dt$url, url_pattern) & dt$domain == domain_with_tld)
    } else {
      hits <- which(str_detect(dt$url, url_pattern))
    }
    
    if (length(hits)) to_drop_idx <- c(to_drop_idx, hits)
  }
  return(unique(to_drop_idx))
}

# Function to identify indices to remove based on special rules
get_special_rule_indices <- function(dt) {
  if (nrow(dt) == 0) return(integer(0))
  
  to_drop_idx <- integer(0)
  
  # Ensure domain column exists for accurate matching
  if (!"domain" %in% names(dt)) {
    # If no domain column, temporary extraction could be implemented, 
    # but relying on URL patterns is safer for generic datasets
    return(integer(0)) 
  }
  
  # mdr.de: ends with index.html
  hits_mdr <- which(dt$domain == 'mdr.de' & str_detect(dt$url, "index\\.html$"))
  to_drop_idx <- c(to_drop_idx, hits_mdr)
  
  # rp-online.de: contains video strings
  hits_rp <- which(dt$domain == 'rp-online.de' & str_detect(dt$url, "_bid-|_vid-|_iid-"))
  to_drop_idx <- c(to_drop_idx, hits_rp)
  
  # taz.de: contains video strings or is a column
  hits_taz <- which(dt$domain == 'taz.de' & (str_detect(dt$url, "/!t5") | str_detect(dt$url, "taz\\.de/Kolumne-")))
  to_drop_idx <- c(to_drop_idx, hits_taz)
  
  # br.de: contains "kontakt"
  hits_br <- which(dt$domain == 'br.de' & str_detect(dt$url, "kontakt"))
  to_drop_idx <- c(to_drop_idx, hits_br)
  
  # Structure-based rules (path segments)
  # Pre-calculate segment counts to avoid repetitive regex
  # Only for domains that need it
  target_domains <- c('newsflash24.de', 'rtl.de', 'tag24.de')
  relevant_rows <- which(dt$domain %in% target_domains)
  
  if (length(relevant_rows) > 0) {
    sub_dt <- dt[relevant_rows]
    # Count slashes after the protocol and domain part
    # A simplified approach: Remove protocol://domain/ and count remaining slashes + 1
    # Or strict regex logic as before
    
    # newsflash24.de & rtl.de: exactly one segment (e.g. domain.de/segment/)
    hits_nf_rtl <- relevant_rows[grepl("^https?://(?:www\\.)?(newsflash24\\.de|rtl\\.de)/[^/]+/?$", sub_dt$url)]
    to_drop_idx <- c(to_drop_idx, hits_nf_rtl)
    
    # tag24.de: exactly two segments
    # Extract path and count slashes
    paths <- str_remove(sub_dt$url, "^https?://[^/]+")
    # Count segments (approximate by slashes, assuming cleaned trailing slash)
    seg_counts <- str_count(paths, "/")
    hits_tag24 <- relevant_rows[sub_dt$domain == 'tag24.de' & seg_counts == 2]
    to_drop_idx <- c(to_drop_idx, hits_tag24)
  }
  
  return(unique(to_drop_idx))
}


# --- 4. MAIN CLEANING FUNCTION ---

perform_global_cleanup <- function() {
  
  message(paste(rep("=", 80), collapse = ""))
  message("STARTING GLOBAL DATA CLEANUP")
  message(paste(rep("=", 80), collapse = ""))
  
  paths <- get_module_paths()
  
  # 1. Define files to clean
  # Core system files that should never be deleted, only emptied
  core_files <- c("input.rds", "final_data.rds", "parse_error.rds", "error.rds", "retry.rds")
  
  files_to_process <- list(
    "input.rds"       = file.path(paths$input, "input.rds"),
    "final_data.rds"  = file.path(paths$output, "final_data.rds"),
    "parse_error.rds" = file.path(paths$input, "parse_error.rds"),
    "error.rds"       = file.path(paths$output, "error.rds"),
    "retry.rds"       = file.path(paths$input, "retry.rds")
  )
  
  # Add domain-specific parse error files dynamically
  # These are located in: data/input/parse_error/
  if (dir.exists(paths$parse_error)) {
    domain_files <- list.files(paths$parse_error, pattern = "\\.rds$", full.names = TRUE)
    for (f in domain_files) {
      files_to_process[[basename(f)]] <- f
    }
  }
  
  # 2. Gather removal criteria
  manual_exact_urls <- unique(excluded_links)
  links_410 <- get_410_links(file.path(paths$logs, "response_log.rds"))
  all_exact_excludes <- unique(c(manual_exact_urls, links_410))
  
  message(sprintf("Global Exclusion Criteria:"))
  message(sprintf(" - Manual exact URLs: %d", length(manual_exact_urls)))
  message(sprintf(" - HTTP 410 URLs:     %d", length(links_410)))
  message(sprintf(" - Total exact URLs:  %d", length(all_exact_excludes)))
  message(" - Plus Path Rules and Special Rules defined in script.\n")
  
  # 3. Initialize statistics container
  stats <- list(
    exact   = list(total = 0),
    path    = list(total = 0),
    special = list(total = 0),
    files   = list() # To store breakdown per file
  )
  
  # Container for all discarded rows across all files
  all_discarded_rows <- list()
  
  # --- HELPER: Path Exclusion Logic (Updated) ---
  # We define this inside or ensure the global one is updated to handle full URLs
  # By applying the fix here directly to ensure it works for this run:
  
  # Function to identify indices to remove based on path rules
  # (Redefined locally to ensure it has the fix for full URLs)
  get_path_exclusion_indices_safe <- function(dt) {
    exclusion_lists <- ls(pattern = "_excluded_links$", envir = .GlobalEnv)
    to_drop_idx <- integer(0)
    
    for (list_name in exclusion_lists) {
      domain_with_tld <- str_replace(list_name, "_excluded_links$", "")
      excluded_paths  <- get(list_name, envir = .GlobalEnv)
      
      if (length(excluded_paths) == 0) next
      
      # FIX: Clean excluded_paths to ensure they are relative paths, not full URLs
      # This allows users to copy-paste full URLs into the list without breaking the regex
      # 1. Remove Protocol and Domain (https://www.spiegel.de/)
      excluded_paths_cleaned <- str_remove(excluded_paths, "^https?://[^/]+/")
      # 2. Remove leading slashes
      excluded_paths_cleaned <- str_remove(excluded_paths_cleaned, "^/")
      
      # Re-build pattern with cleaned paths
      url_pattern <- build_url_pattern(domain_with_tld, excluded_paths_cleaned)
      
      # Check if 'domain' column exists, otherwise match URL pattern only
      if ("domain" %in% names(dt)) {
        hits <- which(str_detect(dt$url, url_pattern) & dt$domain == domain_with_tld)
      } else {
        hits <- which(str_detect(dt$url, url_pattern))
      }
      
      if (length(hits)) to_drop_idx <- c(to_drop_idx, hits)
    }
    return(unique(to_drop_idx))
  }
  
  # 4. Process each file
  for (fname in names(files_to_process)) {
    fpath <- files_to_process[[fname]]
    
    if (!file.exists(fpath)) {
      message(sprintf("Skipping %s (not found)", fname))
      next
    }
    
    # Explicitly state we are processing/checking the file
    message(sprintf("Checking %s...", fname))
    
    dt <- readRDS(fpath)
    setDT(dt)
    
    # If file is already empty, record stats and skip (or delete if domain specific)
    if (nrow(dt) == 0) {
      stats$files[[fname]] <- list(exact=0, path=0, special=0)
      
      # Logic: If it is NOT a core file (meaning it is a domain specific file), delete it
      if (!fname %in% core_files) {
        file.remove(fpath)
        message(sprintf(" -> File was empty. Deleted."))
      } else {
        message(sprintf(" -> File is empty. No action needed."))
      }
      next
    }
    
    # Safety Check: Infer domain from filename if column is missing (crucial for domain-specific files)
    if (!"domain" %in% names(dt) || all(is.na(dt$domain))) {
      if (grepl("_parse_error\\.rds$", fname)) {
        inferred_domain <- sub("_parse_error\\.rds$", "", fname)
        dt[, domain := inferred_domain]
      }
    }
    
    initial_rows <- nrow(dt)
    rows_to_discard <- integer(0) # Indices to remove
    
    # -- A. Exact URL Match --
    idx_exact <- which(dt$url %in% all_exact_excludes)
    count_exact <- length(idx_exact)
    if (count_exact > 0) {
      # Store discarded data
      discarded_chunk <- dt[idx_exact]
      discarded_chunk[, reason := "exact_match"]
      discarded_chunk[, source_file := fname]
      all_discarded_rows[[length(all_discarded_rows) + 1]] <- discarded_chunk
      
      rows_to_discard <- c(rows_to_discard, idx_exact)
    }
    
    # -- B. Path Exclusion Rules (Using the SAFE version) --
    # Only check rows not already marked for deletion
    remaining_indices <- setdiff(seq_len(nrow(dt)), rows_to_discard)
    if (length(remaining_indices) > 0) {
      dt_sub <- dt[remaining_indices]
      # USE THE UPDATED FUNCTION HERE
      sub_idx_path <- get_path_exclusion_indices_safe(dt_sub)
      
      if (length(sub_idx_path) > 0) {
        orig_idx_path <- remaining_indices[sub_idx_path]
        count_path <- length(orig_idx_path)
        
        discarded_chunk <- dt[orig_idx_path]
        discarded_chunk[, reason := "path_rule"]
        discarded_chunk[, source_file := fname]
        all_discarded_rows[[length(all_discarded_rows) + 1]] <- discarded_chunk
        
        rows_to_discard <- c(rows_to_discard, orig_idx_path)
      } else {
        count_path <- 0
      }
    } else {
      count_path <- 0
    }
    
    # -- C. Special Rules --
    remaining_indices <- setdiff(seq_len(nrow(dt)), rows_to_discard)
    if (length(remaining_indices) > 0) {
      dt_sub <- dt[remaining_indices]
      sub_idx_special <- get_special_rule_indices(dt_sub)
      
      if (length(sub_idx_special) > 0) {
        orig_idx_special <- remaining_indices[sub_idx_special]
        count_special <- length(orig_idx_special)
        
        discarded_chunk <- dt[orig_idx_special]
        discarded_chunk[, reason := "special_rule"]
        discarded_chunk[, source_file := fname]
        all_discarded_rows[[length(all_discarded_rows) + 1]] <- discarded_chunk
        
        rows_to_discard <- c(rows_to_discard, orig_idx_special)
      } else {
        count_special <- 0
      }
    } else {
      count_special <- 0
    }
    
    # -- Perform Deletion & Save --
    if (length(rows_to_discard) > 0) {
      dt_clean <- dt[-rows_to_discard]
      
      # LOGIC CHANGE: Check if this is a domain-specific file (not in core_files)
      # If it is domain-specific AND the result is empty (0 rows), DELETE the file.
      if (!fname %in% core_files && nrow(dt_clean) == 0) {
        file.remove(fpath)
        message(sprintf(" -> Removed %d rows. Result is empty -> File Deleted.", length(rows_to_discard)))
      } else {
        # Standard behavior: Save the cleaned file
        saveRDS(dt_clean, fpath)
        message(sprintf(" -> Removed %d rows. Remaining: %d.", length(rows_to_discard), nrow(dt_clean)))
      }
    } else {
      message(sprintf(" -> No links removed. File kept."))
    }
    
    # Update Stats
    stats$exact$total   <- stats$exact$total + count_exact
    stats$path$total    <- stats$path$total + count_path
    stats$special$total <- stats$special$total + count_special
    
    stats$files[[fname]] <- list(
      exact = count_exact,
      path = count_path,
      special = count_special
    )
  }
  
  # --- 5. UPDATE DISCARDED.RDS ---
  message("\n--- Updating Discarded Archive ---")
  discarded_path <- file.path(paths$output, "discarded.rds")
  
  if (length(all_discarded_rows) > 0) {
    # Combine all new discarded rows
    new_discarded_dt <- rbindlist(all_discarded_rows, fill = TRUE, use.names = TRUE)
    
    # Keep minimal columns for the archive to avoid schema conflicts
    cols_to_keep <- c("url", "domain", "reason", "source_file", "timestamp_scraped")
    cols_to_keep <- intersect(names(new_discarded_dt), cols_to_keep)
    new_discarded_dt <- new_discarded_dt[, ..cols_to_keep]
    
    # Add timestamp of deletion
    new_discarded_dt[, deleted_at := Sys.time()]
    
    # Load existing archive
    if (file.exists(discarded_path)) {
      existing_discarded <- readRDS(discarded_path)
      setDT(existing_discarded)
      combined_discarded <- rbindlist(list(existing_discarded, new_discarded_dt), fill = TRUE, use.names = TRUE)
    } else {
      combined_discarded <- new_discarded_dt
    }
    
    # Deduplicate Discarded Archive (Unique URLs only)
    initial_archive_rows <- nrow(combined_discarded)
    combined_discarded <- unique(combined_discarded, by = "url")
    final_archive_rows <- nrow(combined_discarded)
    
    saveRDS(combined_discarded, discarded_path)
    
    message(sprintf("Appended %d new rows to discarded.rds.", nrow(new_discarded_dt)))
    message(sprintf("Archive Deduplication: %d -> %d rows (Removed %d duplicates)", 
                    initial_archive_rows, final_archive_rows, initial_archive_rows - final_archive_rows))
    
  } else {
    message("No rows were discarded in this run.")
  }
  
  # --- 6. PRINT SUMMARY REPORT ---
  
  print_category_report <- function(title, category_key, stats_list) {
    total <- stats_list[[category_key]]$total
    message(paste(rep("-", 40), collapse = ""))
    message(sprintf("%s: Total %d", title, total))
    
    if (total > 0) {
      for (fname in names(stats_list$files)) {
        count <- stats_list$files[[fname]][[category_key]]
        if (count > 0) {
          message(sprintf("  %s : %d", fname, count))
        }
      }
    }
  }
  
  message("\n", paste(rep("=", 60), collapse = ""))
  message("CLEANING SUMMARY REPORT")
  message(paste(rep("=", 60), collapse = ""))
  
  print_category_report("Specific Links Deleted (Exact Match + 410)", "exact", stats)
  print_category_report("Links Excluded by Path Rules", "path", stats)
  print_category_report("Links Excluded by Special Rules", "special", stats)
  
  message(paste(rep("=", 60), collapse = ""))
  message("Cleanup Complete.")
}



perform_global_cleanup()