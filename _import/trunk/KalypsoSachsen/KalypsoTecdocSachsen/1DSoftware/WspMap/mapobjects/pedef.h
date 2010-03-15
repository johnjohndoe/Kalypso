/* $Id: pedef.h,v 1.1 2002-02-15 09:36:59 belger Exp $ */
/* -------------------------------------------------------------------------- */
/* Projection Engine                                                          */
/* Copyright (C) 1996-1999 Environmental Systems Research Institute, Inc.     */
/* All rights reserved.                                                       */
/* -------------------------------------------------------------------------- */
#ifndef PEDEF_INCLUDED
#define PEDEF_INCLUDED

/* from pef.h    															  */
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*                      U N I T S   O F   M E A S U R E                       */
/*----------------------------------------------------------------------------*/
#define PE_U_METER          9001 /* International meter                       */
#define PE_U_METER_GERMAN   9031 /* German legal meter                        */
#define PE_U_FOOT           9002 /* International foot                        */
#define PE_U_FOOT_US        9003 /* US survey foot                            */
#define PE_U_FOOT_MA        9004 /* Modified American foot                    */
#define PE_U_FOOT_CLARKE    9005 /* Clarke's foot                             */
#define PE_U_FOOT_INDIAN    9006 /* Indian geodetic foot                      */
#define PE_U_FOOT_SEARS     9032 /* Sears' foot                               */
#define PE_U_LINK           9007 /* Link (Clarke's ratio)                     */
#define PE_U_LINK_BENOIT    9008 /* Link (Benoit)                             */
#define PE_U_LINK_SEARS     9009 /* Link (Sears)                              */
#define PE_U_CHAIN_BENOIT   9010 /* Chain (Benoit)                            */
#define PE_U_CHAIN_SEARS    9011 /* Chain (Sears)                             */
#define PE_U_YARD_SEARS     9012 /* Yard (Sears)                              */
#define PE_U_YARD_INDIAN    9013 /* Indian yard                               */
#define PE_U_FATHOM         9014 /* Fathom                                    */
#define PE_U_NAUTICAL_MILE  9030 /* International nautical mile               */
#define PE_U_RADIAN         9101 /* Radian                                    */
#define PE_U_DEGREE         9102 /* Degree                                    */
#define PE_U_MINUTE         9103 /* Arc-minute                                */
#define PE_U_SECOND         9104 /* Arc-second                                */
#define PE_U_GRAD           9105 /* Grad                                      */
#define PE_U_GON            9106 /* Gon                                       */
#define PE_U_MICRORADIAN    9109 /* Microradian                               */

/*----------------------------------------------------------------------------*/
/*                    G E O D E T I C   S P H E R O I D S                     */
/*----------------------------------------------------------------------------*/
#define PE_S_AIRY_1830           7001 /* Airy 1830                            */
#define PE_S_AIRY_MOD            7002 /* Airy modified                        */
#define PE_S_ATS_1977            7041 /* Average Terrestrial System 1977      */
#define PE_S_AUSTRALIAN          7003 /* Australian National                  */
#define PE_S_BESSEL_1841         7004 /* Bessel 1841                          */
#define PE_S_BESSEL_MOD          7005 /* Bessel modified                      */
#define PE_S_BESSEL_NAMIBIA      7006 /* Bessel Namibia                       */
#define PE_S_CLARKE_1858         7007 /* Clarke 1858                          */
#define PE_S_CLARKE_1866         7008 /* Clarke 1866                          */
#define PE_S_CLARKE_1866_MICH    7009 /* Clarke 1866 Michigan                 */
#define PE_S_CLARKE_1880         7034 /* Clarke 1880                          */
#define PE_S_CLARKE_1880_ARC     7013 /* Clarke 1880 (Arc)                    */
#define PE_S_CLARKE_1880_BENOIT  7010 /* Clarke 1880 (Benoit)                 */
#define PE_S_CLARKE_1880_IGN     7011 /* Clarke 1880 (IGN)                    */
#define PE_S_CLARKE_1880_RGS     7012 /* Clarke 1880 (RGS)                    */
#define PE_S_CLARKE_1880_SGA     7014 /* Clarke 1880 (SGA)                    */
#define PE_S_EVEREST_1830        7015 /* Everest 1830                         */
#define PE_S_EVEREST_DEF_1967    7016 /* Everest (definition 1967)            */
#define PE_S_EVEREST_DEF_1975    7017 /* Everest (definition 1975)            */
#define PE_S_EVEREST_MOD         7018 /* Everest modified                     */
#define PE_S_GEM_10C             7031 /* GEM gravity potential model          */
#define PE_S_GRS_1967            7036 /* GRS 1967 = International 1967        */
#define PE_S_GRS_1980            7019 /* GRS 1980                             */
#define PE_S_HELMERT_1906        7020 /* Helmert 1906                         */
#define PE_S_INDONESIAN          7021 /* Indonesian National                  */
#define PE_S_INTERNATIONAL_1924  7022 /* International 1924                   */
#define PE_S_INTERNATIONAL_1967  7023 /* International 1967                   */
#define PE_S_KRASOVSKY_1940      7024 /* Krasovsky 1940                       */
#define PE_S_NWL_9D              7025 /* Transit precise ephemeris            */
#define PE_S_OSU_86F             7032 /* OSU 1986 geoidal model               */
#define PE_S_OSU_91A             7033 /* OSU 1991 geoidal model               */
#define PE_S_PLESSIS_1817        7027 /* Plessis 1817                         */
#define PE_S_SPHERE              7035 /* Authalic sphere                      */
#define PE_S_STRUVE_1860         7028 /* Struve 1860                          */
#define PE_S_WAR_OFFICE          7029 /* War Office                           */
#define PE_S_NWL_10D             7026 /* NWL-10D == WGS 1972                  */
#define PE_S_WGS_1984            7030 /* WGS 1984                             */
#define PE_S_WGS_1966            (7001+33000) /* WGS 1966                     */
#define PE_S_FISCHER_1960        (7002+33000) /* Fischer 1960                 */
#define PE_S_FISCHER_1968        (7003+33000) /* Fischer 1968                 */
#define PE_S_FISCHER_MOD         (7004+33000) /* Fischer modified             */
#define PE_S_HOUGH_1960          (7005+33000) /* Hough 1960                   */
#define PE_S_EVEREST_MOD_1969    (7006+33000) /* Everest modified 1969        */
#define PE_S_WALBECK             (7007+33000) /* Walbeck                      */
#define PE_S_SPHERE_AI           (7008+33000) /* Authalic sphere (ARC/INFO)   */
#define PE_S_WGS_1972            107026 /* WGS 1972                           */

/*----------------------------------------------------------------------------*/
/*                     H O R I Z O N T A L   D A T U M S                      */
/*                        (S P H E R O I D   O N L Y)                         */
/*----------------------------------------------------------------------------*/
#define PE_D_AIRY_1830           6001 /* Airy 1830                            */
#define PE_D_AIRY_MOD            6002 /* Airy modified                        */
#define PE_D_AUSTRALIAN          6003 /* Australian National                  */
#define PE_D_BESSEL_1841         6004 /* Bessel 1841                          */
#define PE_D_BESSEL_MOD          6005 /* Bessel modified                      */
#define PE_D_BESSEL_NAMIBIA      6006 /* Bessel Namibia                       */
#define PE_D_CLARKE_1858         6007 /* Clarke 1858                          */
#define PE_D_CLARKE_1866         6008 /* Clarke 1866                          */
#define PE_D_CLARKE_1866_MICH    6009 /* Clarke 1866 Michigan                 */
#define PE_D_CLARKE_1880         6034 /* Clarke 1880                          */
#define PE_D_CLARKE_1880_ARC     6013 /* Clarke 1880 (Arc)                    */
#define PE_D_CLARKE_1880_BENOIT  6010 /* Clarke 1880 (Benoit)                 */
#define PE_D_CLARKE_1880_IGN     6011 /* Clarke 1880 (IGN)                    */
#define PE_D_CLARKE_1880_RGS     6012 /* Clarke 1880 (RGS)                    */
#define PE_D_CLARKE_1880_SGA     6014 /* Clarke 1880 (SGA)                    */
#define PE_D_EVEREST_1830        6015 /* Everest 1830                         */
#define PE_D_EVEREST_DEF_1967    6016 /* Everest (definition 1967)            */
#define PE_D_EVEREST_DEF_1975    6017 /* Everest (definition 1975)            */
#define PE_D_EVEREST_MOD         6018 /* Everest modified                     */
#define PE_D_GEM_10C             6031 /* GEM gravity potential model          */
#define PE_D_GRS_1967            6036 /* GRS 1967                             */
#define PE_D_GRS_1980            6019 /* GRS 1980                             */
#define PE_D_HELMERT_1906        6020 /* Helmert 1906                         */
#define PE_D_INDONESIAN          6021 /* Indonesian National                  */
#define PE_D_INTERNATIONAL_1924  6022 /* International 1927                   */
#define PE_D_INTERNATIONAL_1967  6023 /* International 1967                   */
#define PE_D_KRASOVSKY_1940      6024 /* Krasovsky 1940                       */
#define PE_D_NWL_9D              6025 /* Transit precise ephemeris            */
#define PE_D_OSU_86F             6032 /* OSU 1986 geoidal model               */
#define PE_D_OSU_91A             6033 /* OSU 1991 geoidal model               */
#define PE_D_PLESSIS_1817        6027 /* Plessis 1817                         */
#define PE_D_SPHERE              6035 /* Authalic sphere                      */
#define PE_D_STRUVE_1860         6028 /* Struve 1860                          */
#define PE_D_WAR_OFFICE          6029 /* War Office                           */
#define PE_D_WGS_1966            (6001+33000) /* WGS 1966                     */
#define PE_D_FISCHER_1960        (6002+33000) /* Fischer 1960                 */
#define PE_D_FISCHER_1968        (6003+33000) /* Fischer 1968                 */
#define PE_D_FISCHER_MOD         (6004+33000) /* Fischer modified             */
#define PE_D_HOUGH_1960          (6005+33000) /* Hough 1960                   */
#define PE_D_EVEREST_MOD_1969    (6006+33000) /* Everest modified 1969        */
#define PE_D_WALBECK             (6007+33000) /* Walbeck                      */
#define PE_D_SPHERE_AI           (6008+33000) /* Authalic sphere (ARC/INFO)   */

/*----------------------------------------------------------------------------*/
/*                     H O R I Z O N T A L   D A T U M S                      */
/*----------------------------------------------------------------------------*/
#define PE_D_ADINDAN             6201 /* Adindan                              */
#define PE_D_AFGOOYE             6205 /* Afgooye                              */
#define PE_D_AGADEZ              6206 /* Agadez                               */
#define PE_D_AGD_1966            6202 /* Australian Geodetic Datum 1966       */
#define PE_D_AGD_1984            6203 /* Australian Geodetic Datum 1984       */
#define PE_D_AIN_EL_ABD_1970     6204 /* Ain el Abd 1970                      */
#define PE_D_AMERSFOORT          6289 /* Amersfoort                           */
#define PE_D_ARATU               6208 /* Aratu                                */
#define PE_D_ARC_1950            6209 /* Arc 1950                             */
#define PE_D_ARC_1960            6210 /* Arc 1960                             */
#define PE_D_ATF                 6901 /* Ancienne Triangulation Francaise     */
#define PE_D_ATS_1977            6122 /* Average Terrestrial System 1977      */
#define PE_D_BARBADOS            6212 /* Barbados                             */
#define PE_D_BATAVIA             6211 /* Batavia                              */
#define PE_D_BEDUARAM            6213 /* Beduaram                             */
#define PE_D_BEIJING_1954        6214 /* Beijing 1954                         */
#define PE_D_BELGE_1950          6215 /* Reseau National Belge 1950           */
#define PE_D_BELGE_1972          6313 /* Reseau National Belge 1972           */
#define PE_D_BERMUDA_1957        6216 /* Bermuda 1957                         */
#define PE_D_BERN_1898           6217 /* Bern 1898                            */
#define PE_D_BERN_1938           6306 /* Bern 1938                            */
#define PE_D_BOGOTA              6218 /* Bogota                               */
#define PE_D_BUKIT_RIMPAH        6219 /* Bukit Rimpah                         */
#define PE_D_CAMACUPA            6220 /* Camacupa                             */
#define PE_D_CAMPO_INCHAUSPE     6221 /* Campo Inchauspe                      */
#define PE_D_CAPE                6222 /* Cape                                 */
#define PE_D_CARTHAGE            6223 /* Carthage                             */
#define PE_D_CHUA                6224 /* Chua                                 */
#define PE_D_CONAKRY_1905        6315 /* Conakry 1905                         */
#define PE_D_CORREGO_ALEGRE      6225 /* Corrego Alegre                       */
#define PE_D_COTE_D_IVOIRE       6226 /* Cote d'Ivoire                        */
#define PE_D_DATUM_73            6274 /* Datum 73                             */
#define PE_D_DEIR_EZ_ZOR         6227 /* Deir ez Zor                          */
#define PE_D_DEALUL_PISCULUI_1933  6316 /* Dealul Piscului 1933               */
#define PE_D_DEALUL_PISCULUI_1970  6317 /* Dealul Piscului 1970               */
#define PE_D_DHDN                6314 /* Deutsche Hauptdreiecksnetz           */
#define PE_D_DOUALA              6228 /* Douala                               */
#define PE_D_ED_1950             6230 /* European Datum 1950                  */
#define PE_D_ED_1987             6231 /* European Datum 1987                  */
#define PE_D_EGYPT_1907          6229 /* Egypt 1907                           */
#define PE_D_ETRS_1989           6258 /* European Terrestrial Ref. Sys. 1989  */
#define PE_D_FAHUD               6232 /* Fahud                                */
#define PE_D_GANDAJIKA_1970      6233 /* Gandajika 1970                       */
#define PE_D_GAROUA              6234 /* Garoua                               */
#define PE_D_GDA_1994            6283 /* Geocentric Datum of Australia 1994   */
#define PE_D_GGRS_1987           6121 /* Greek Geodetic Reference System 1987 */
#define PE_D_GREEK               6120 /* Greek                                */
#define PE_D_GUYANE_FRANCAISE    6235 /* Guyane Francaise                     */
#define PE_D_HERAT_NORTH         6255 /* Herat North                          */
#define PE_D_HITO_XVIII_1963     6254 /* Hito XVIII 1963                      */
#define PE_D_HU_TZU_SHAN         6236 /* Hu Tzu Shan                          */
#define PE_D_HUNGARIAN_1972      6237 /* Hungarian Datum 1972                 */
#define PE_D_INDIAN_1954         6239 /* Indian 1954                          */
#define PE_D_INDIAN_1975         6240 /* Indian 1975                          */
#define PE_D_INDONESIAN_1974     6238 /* Indonesian Datum 1974                */
#define PE_D_JAMAICA_1875        6241 /* Jamaica 1875                         */
#define PE_D_JAMAICA_1969        6242 /* Jamaica 1969                         */
#define PE_D_KALIANPUR           6243 /* Kalianpur                            */
#define PE_D_KANDAWALA           6244 /* Kandawala                            */
#define PE_D_KERTAU              6245 /* Kertau                               */
#define PE_D_KKJ                 6123 /* Kartastokoordinaattijarjestelma      */
#define PE_D_KOC                 6246 /* Kuwait Oil Company                   */
#define PE_D_KUDAMS              6319 /* Kuwait Utility                       */
#define PE_D_LA_CANOA            6247 /* La Canoa                             */
#define PE_D_LAKE                6249 /* Lake                                 */
#define PE_D_LEIGON              6250 /* Leigon                               */
#define PE_D_LIBERIA_1964        6251 /* Liberia 1964                         */
#define PE_D_LISBON              6207 /* Lisbon                               */
#define PE_D_LOMA_QUINTANA       6288 /* Loma Quintana                        */
#define PE_D_LOME                6252 /* Lome                                 */
#define PE_D_LUZON_1911          6253 /* Luzon 1911                           */
#define PE_D_MAHE_1971           6256 /* Mahe 1971                            */
#define PE_D_MAKASSAR            6257 /* Makassar                             */
#define PE_D_MALONGO_1987        6259 /* Malongo 1987                         */
#define PE_D_MANOCA              6260 /* Manoca                               */
#define PE_D_MASSAWA             6262 /* Massawa                              */
#define PE_D_MERCHICH            6261 /* Merchich                             */
#define PE_D_MGI                 6312 /* Militar-Geographische Institut       */
#define PE_D_MHAST               6264 /* Mhast                                */
#define PE_D_MINNA               6263 /* Minna                                */
#define PE_D_MONTE_MARIO         6265 /* Monte Mario                          */
#define PE_D_MPORALOKO           6266 /* M'poraloko                           */
#define PE_D_NAD_MICH            6268 /* NAD Michigan                         */
#define PE_D_NAD_1927            6267 /* North American Datum 1927            */
#define PE_D_NAD_1983            6269 /* North American Datum 1983            */
#define PE_D_NAHRWAN_1967        6270 /* Nahrwan 1967                         */
#define PE_D_NAPARIMA_1972       6271 /* Naparima 1972                        */
#define PE_D_NDG                 6902 /* Nord de Guerre                       */
#define PE_D_NGN                 6318 /* National Geodetic Network (Kuwait)   */
#define PE_D_NGO_1948            6273 /* NGO 1948                             */
#define PE_D_NORD_SAHARA_1959    6307 /* Nord Sahara 1959                     */
#define PE_D_NSWC_9Z_2           6276 /* NSWC 9Z-2                            */
#define PE_D_NTF                 6275 /* Nouvelle Triangulation Francaise     */
#define PE_D_NZGD_1949           6272 /* New Zealand Geodetic Datum 1949      */
#define PE_D_OS_SN_1980          6279 /* OS (SN) 1980                         */
#define PE_D_OSGB_1936           6277 /* OSGB 1936                            */
#define PE_D_OSGB_1970_SN        6278 /* OSGB 1970 (SN)                       */
#define PE_D_PADANG_1884         6280 /* Padang 1884                          */
#define PE_D_PALESTINE_1923      6281 /* Palestine 1923                       */
#define PE_D_POINTE_NOIRE        6282 /* Pointe Noire                         */
#define PE_D_PSAD_1956           6248 /* Provisional South Amer. Datum 1956   */
#define PE_D_PULKOVO_1942        6284 /* Pulkovo 1942                         */
#define PE_D_PULKOVO_1995        6200 /* Pulkovo 1995                         */
#define PE_D_QATAR               6285 /* Qatar                                */
#define PE_D_QATAR_1948          6286 /* Qatar 1948                           */
#define PE_D_QORNOQ              6287 /* Qornoq                               */
#define PE_D_SAD_1969            6291 /* South American Datum 1969            */
#define PE_D_SAPPER_HILL_1943    6292 /* Sapper Hill 1943                     */
#define PE_D_SCHWARZECK          6293 /* Schwarzeck                           */
#define PE_D_SEGORA              6294 /* Segora                               */
#define PE_D_SERINDUNG           6295 /* Serindung                            */
#define PE_D_STOCKHOLM_1938      6308 /* Stockholm 1938                       */
#define PE_D_SUDAN               6296 /* Sudan                                */
#define PE_D_TANANARIVE_1925     6297 /* Tananarive 1925                      */
#define PE_D_TIMBALAI_1948       6298 /* Timbalai 1948                        */
#define PE_D_TM65                6299 /* TM65                                 */
#define PE_D_TM75                6300 /* TM75                                 */
#define PE_D_TOKYO               6301 /* Tokyo                                */
#define PE_D_TRINIDAD_1903       6302 /* Trinidad 1903                        */
#define PE_D_TRUCIAL_COAST_1948  6303 /* Trucial Coast 1948                   */
#define PE_D_VOIROL_1875         6304 /* Voirol 1875                          */
#define PE_D_VOIROL_UNIFIE_1960  6305 /* Voirol Unifie 1960                   */
#define PE_D_WGS_1972            6322 /* WGS 1972                             */
#define PE_D_WGS_1972_BE         6324 /* WGS 1972 Transit Broadcast Ephemeris */
#define PE_D_WGS_1984            6326 /* WGS 1984                             */
#define PE_D_YACARE              6309 /* Yacare                               */
#define PE_D_YOFF                6310 /* Yoff                                 */
#define PE_D_ZANDERIJ            6311 /* Zanderij                             */
#define PE_D_EUROPEAN_1979       (6201+33000) /* European 1979                */
#define PE_D_EVEREST_BANGLADESH  (6202+33000) /* Everest - Bangladesh         */
#define PE_D_EVEREST_INDIA_NEPAL (6203+33000) /* Everest - India and Nepal    */
#define PE_D_HJORSEY_1955        (6204+33000) /* Hjorsey 1955                 */
#define PE_D_HONG_KONG_1963      (6205+33000) /* Hong Kong 1963               */
#define PE_D_OMAN                (6206+33000) /* Oman                         */
#define PE_D_S_ASIA_SINGAPORE    (6207+33000) /* South Asia Singapore         */
#define PE_D_AYABELLE            (6208+33000) /* Ayabelle Lighthouse          */
#define PE_D_BISSAU              (6209+33000) /* Bissau                       */
#define PE_D_DABOLA              (6210+33000) /* Dabola                       */
#define PE_D_POINT58             (6211+33000) /* Point 58                     */
#define PE_D_BEACON_E_1945       (6212+33000) /* Astro Beacon E 1945          */
#define PE_D_TERN_ISLAND_1961    (6213+33000) /* Tern Island Astro 1961       */
#define PE_D_ASTRO_1952          (6214+33000) /* Astronomical Station 1952    */
#define PE_D_BELLEVUE            (6215+33000) /* Bellevue IGN                 */
#define PE_D_CANTON_1966         (6216+33000) /* Canton Astro 1966            */
#define PE_D_CHATHAM_ISLAND_1971 (6217+33000) /* Chatham Island Astro 1971    */
#define PE_D_DOS_1968            (6218+33000) /* DOS 1968                     */
#define PE_D_EASTER_ISLAND_1967  (6219+33000) /* Easter Island 1967           */
#define PE_D_GUAM_1963           (6220+33000) /* Guam 1963                    */
#define PE_D_GUX_1               (6221+33000) /* GUX 1 Astro                  */
#define PE_D_JOHNSTON_ISLAND_1961 (6222+33000) /* Johnston Island 1961        */
#define PE_D_KUSAIE_1951         (6259+33000) /* Kusaie Astro 1951            */
#define PE_D_MIDWAY_1961         (6224+33000) /* Midway Astro 1961            */
#define PE_D_OLD_HAWAIIAN        (6225+33000) /* Old Hawaiian                 */
#define PE_D_PITCAIRN_1967       (6226+33000) /* Pitcairn Astro 1967          */
#define PE_D_SANTO_DOS_1965      (6227+33000) /* Santo DOS 1965               */
#define PE_D_VITI_LEVU_1916      (6228+33000) /* Viti Levu 1916               */
#define PE_D_WAKE_ENIWETOK_1960  (6229+33000) /* Wake-Eniwetok 1960           */
#define PE_D_WAKE_ISLAND_1952    (6230+33000) /* Wake Island Astro 1952       */
#define PE_D_ANNA_1_1965         (6231+33000) /* Anna 1 Astro 1965            */
#define PE_D_GAN_1970            (6232+33000) /* Gan 1970                     */
#define PE_D_ISTS_073_1969       (6233+33000) /* ISTS 073 Astro 1969          */
#define PE_D_KERGUELEN_ISLAND_1949 (6234+33000) /* Kerguelen Island 1949      */
#define PE_D_REUNION             (6235+33000) /* Reunion                      */
#define PE_D_ANTIGUA_ISLAND_1943 (6236+33000) /* Antigua Island Astro 1943    */
#define PE_D_ASCENSION_ISLAND_1958 (6237+33000) /* Ascension Island 1958      */
#define PE_D_DOS_71_4           (6238+33000) /* Astro DOS 71/4                */
#define PE_D_CAPE_CANAVERAL     (6239+33000) /* Cape Canaveral                */
#define PE_D_FORT_THOMAS_1955   (6240+33000) /* Fort Thomas 1955              */
#define PE_D_GRACIOSA_1948      (6241+33000) /* Graciosa Base SW 1948         */
#define PE_D_ISTS_061_1968      (6242+33000) /* ISTS 061 Astro 1968           */
#define PE_D_LC5_1961           (6243+33000) /* L.C. 5 Astro 1961             */
#define PE_D_MONTSERRAT_ISLAND_1958 (6244+33000) /* Montserrat Isl Astro 1958 */
#define PE_D_OBSERV_METEOR_1939 (6245+33000) /* Observ. Meteorologico 1939    */
#define PE_D_PICO_DE_LAS_NIEVES (6246+33000) /* Pico de Las Nieves            */
#define PE_D_PORTO_SANTO_1936   (6247+33000) /* Porto Santo 1936              */
#define PE_D_PUERTO_RICO        (6248+33000) /* Puerto Rico                   */
#define PE_D_SAO_BRAZ           (6249+33000) /* Sao Braz                      */
#define PE_D_SELVAGEM_GRANDE_1938 (6250+33000) /* Selvagem Grande 1938        */
#define PE_D_TRISTAN_1968       (6251+33000) /* Tristan Astro 1968            */
#define PE_D_SAMOA_1962         (6252+33000) /* American Samoa 1962           */
#define PE_D_CAMP_AREA          (6253+33000) /* Camp Area Astro               */
#define PE_D_DECEPTION_ISLAND   (6254+33000) /* Deception Island              */
#define PE_D_GUNUNG_SEGARA      (6255+33000) /* Gunung Segara                 */
#define PE_D_INDIAN_1960        (6256+33000) /* Indian 1960                   */
#define PE_D_S42_HUNGARY        (6257+33000) /* S-42 Hungary                  */
#define PE_D_S_JTSK             (6258+33000) /* S-JTSK                        */
#define PE_D_ALASKAN_ISLANDS    (6260+33000) /* Alaskan Islands               */

/*----------------------------------------------------------------------------*/
/*                       P R I M E   M E R I D I A N S                        */
/*----------------------------------------------------------------------------*/
#define PE_PM_GREENWICH  8901 /*   0~00~00"     E                             */
#define PE_PM_ATHENS     8912 /*  23~42'58".815 E                             */
#define PE_PM_BERN       8907 /*   7~26'22".5   E                             */
#define PE_PM_BOGOTA     8904 /*  74~04'51".3   W                             */
#define PE_PM_BRUSSELS   8910 /*   4~22'04".71  E                             */
#define PE_PM_FERRO      8909 /*  17~40'00"     W                             */
#define PE_PM_JAKARTA    8908 /* 106~48'27".79  E                             */
#define PE_PM_LISBON     8902 /*   9~07'54".862 W                             */
#define PE_PM_MADRID     8905 /*   3~41'16".58  W                             */
#define PE_PM_PARIS      8903 /*   2~20'14".025 E                             */
#define PE_PM_ROME       8906 /*  12~27'08".4   E                             */
#define PE_PM_STOCKHOLM  8911 /*  18~03'29".8   E                             */

/*----------------------------------------------------------------------------*/
/*         G E O G R A P H I C   C O O R D I N A T E   S Y S T E M S          */
/*                        (S P H E R O I D   O N L Y)                         */
/*----------------------------------------------------------------------------*/
#define PE_GCS_AIRY_1830           4001 /* Airy 1830                          */
#define PE_GCS_AIRY_MOD            4002 /* Airy modified                      */
#define PE_GCS_ATS_1977            4122 /* Average Terrestrial System 1977    */
#define PE_GCS_AUSTRALIAN          4003 /* Australian National                */
#define PE_GCS_BESSEL_1841         4004 /* Bessel 1841                        */
#define PE_GCS_BESSEL_MOD          4005 /* Bessel modified                    */
#define PE_GCS_BESSEL_NAMIBIA      4006 /* Bessel Namibia                     */
#define PE_GCS_CLARKE_1858         4007 /* Clarke 1858                        */
#define PE_GCS_CLARKE_1866         4008 /* Clarke 1866                        */
#define PE_GCS_CLARKE_1866_MICH    4009 /* Clarke 1866 Michigan               */
#define PE_GCS_CLARKE_1880         4034 /* Clarke 1880                        */
#define PE_GCS_CLARKE_1880_ARC     4013 /* Clarke 1880 (Arc)                  */
#define PE_GCS_CLARKE_1880_BENOIT  4010 /* Clarke 1880 (Benoit)               */
#define PE_GCS_CLARKE_1880_IGN     4011 /* Clarke 1880 (IGN)                  */
#define PE_GCS_CLARKE_1880_RGS     4012 /* Clarke 1880 (RGS)                  */
#define PE_GCS_CLARKE_1880_SGA     4014 /* Clarke 1880 (SGA)                  */
#define PE_GCS_EVEREST_1830        4015 /* Everest 1830                       */
#define PE_GCS_EVEREST_DEF_1967    4016 /* Everest (definition 1967)          */
#define PE_GCS_EVEREST_DEF_1975    4017 /* Everest (definition 1975)          */
#define PE_GCS_EVEREST_MOD         4018 /* Everest modified                   */
#define PE_GCS_GEM_10C             4031 /* GEM gravity potential model        */
#define PE_GCS_GRS_1967            4036 /* GRS 1967                           */
#define PE_GCS_GRS_1980            4019 /* GRS 1980                           */
#define PE_GCS_HELMERT_1906        4020 /* Helmert 1906                       */
#define PE_GCS_INDONESIAN          4021 /* Indonesian National                */
#define PE_GCS_INTERNATIONAL_1924  4022 /* International 1927                 */
#define PE_GCS_INTERNATIONAL_1967  4023 /* International 1967                 */
#define PE_GCS_KRASOVSKY_1940      4024 /* Krasovsky 1940                     */
#define PE_GCS_NWL_9D              4025 /* Transit precise ephemeris          */
#define PE_GCS_OSU_86F             4032 /* OSU 1986 geoidal model             */
#define PE_GCS_OSU_91A             4033 /* OSU 1991 geoidal model             */
#define PE_GCS_PLESSIS_1817        4027 /* Plessis 1817                       */
#define PE_GCS_SPHERE              4035 /* Authalic sphere                    */
#define PE_GCS_STRUVE_1860         4028 /* Struve 1860                        */
#define PE_GCS_WAR_OFFICE          4029 /* War Office                         */
#define PE_GCS_WGS_1966            (4001+33000) /* WGS 1966                   */
#define PE_GCS_FISCHER_1960        (4002+33000) /* Fischer 1960               */
#define PE_GCS_FISCHER_1968        (4003+33000) /* Fischer 1968               */
#define PE_GCS_FISCHER_MOD         (4004+33000) /* Fischer modified           */
#define PE_GCS_HOUGH_1960          (4005+33000) /* Hough 1960                 */
#define PE_GCS_EVEREST_MOD_1969    (4006+33000) /* Everest modified 1969      */
#define PE_GCS_WALBECK             (4007+33000) /* Walbeck                    */
#define PE_GCS_SPHERE_AI           (4008+33000) /* Authalic sphere (ARC/INFO) */

/*----------------------------------------------------------------------------*/
/*         G E O G R A P H I C   C O O R D I N A T E   S Y S T E M S          */
/*----------------------------------------------------------------------------*/
#define PE_GCS_ADINDAN             4201 /* Adindan                            */
#define PE_GCS_AFGOOYE             4205 /* Afgooye                            */
#define PE_GCS_AGADEZ              4206 /* Agadez                             */
#define PE_GCS_AGD_1966            4202 /* Australian Geodetic Datum 1966     */
#define PE_GCS_AGD_1984            4203 /* Australian Geodetic Datum 1984     */
#define PE_GCS_AIN_EL_ABD_1970     4204 /* Ain el Abd 1970                    */
#define PE_GCS_AMERSFOORT          4289 /* Amersfoort                         */
#define PE_GCS_ARATU               4208 /* Aratu                              */
#define PE_GCS_ARC_1950            4209 /* Arc 1950                           */
#define PE_GCS_ARC_1960            4210 /* Arc 1960                           */
#define PE_GCS_ATF_PARIS           4901 /* ATF (Paris)                        */
#define PE_GCS_BARBADOS            4212 /* Barbados                           */
#define PE_GCS_BATAVIA             4211 /* Batavia                            */
#define PE_GCS_BATAVIA_JAKARTA     4813 /* Batavia (Jakarta)                  */
#define PE_GCS_BEDUARAM            4213 /* Beduaram                           */
#define PE_GCS_BEIJING_1954        4214 /* Beijing 1954                       */
#define PE_GCS_BELGE_1950          4215 /* Reseau National Belge 1950         */
#define PE_GCS_BELGE_1950_BRUSSELS 4809 /* Belge 1950 (Brussels)              */
#define PE_GCS_BELGE_1972          4313 /* Reseau National Belge 1972         */
#define PE_GCS_BERMUDA_1957        4216 /* Bermuda 1957                       */
#define PE_GCS_BERN_1898           4217 /* Bern 1898                          */
#define PE_GCS_BERN_1898_BERN      4801 /* Bern 1898 (Bern)                   */
#define PE_GCS_BERN_1938           4306 /* Bern 1938                          */
#define PE_GCS_BOGOTA              4218 /* Bogota                             */
#define PE_GCS_BOGOTA_BOGOTA       4802 /* Bogota (Bogota)                    */
#define PE_GCS_BUKIT_RIMPAH        4219 /* Bukit Rimpah                       */
#define PE_GCS_CAMACUPA            4220 /* Camacupa                           */
#define PE_GCS_CAMPO_INCHAUSPE     4221 /* Campo Inchauspe                    */
#define PE_GCS_CAPE                4222 /* Cape                               */
#define PE_GCS_CARTHAGE            4223 /* Carthage                           */
#define PE_GCS_CARTHAGE_DEGREE     (4223+33000) /* Carthage (degrees)         */
#define PE_GCS_CHUA                4224 /* Chua                               */
#define PE_GCS_CONAKRY_1905        4315 /* Conakry 1905                       */
#define PE_GCS_CORREGO_ALEGRE      4225 /* Corrego Alegre                     */
#define PE_GCS_COTE_D_IVOIRE       4226 /* Cote d'Ivoire                      */
#define PE_GCS_DATUM_73            4274 /* Datum 73                           */
#define PE_GCS_DEALUL_PISCULUI_1933  4316 /* Dealul Piscului 1933 (Romania)   */
#define PE_GCS_DEALUL_PISCULUI_1970  4317 /* Dealul Piscului 1970 (Romania)   */
#define PE_GCS_DEIR_EZ_ZOR         4227 /* Deir ez Zor                        */
#define PE_GCS_DHDN                4314 /* Deutsche Hauptdreiecksnetz         */
#define PE_GCS_DOUALA              4228 /* Douala                             */
#define PE_GCS_ED_1950             4230 /* European Datum 1950                */
#define PE_GCS_ED_1987             4231 /* European Datum 1987                */
#define PE_GCS_EGYPT_1907          4229 /* Egypt 1907                         */
#define PE_GCS_ETRS_1989           4258 /* European Terrestrial Ref. Sys. 1989*/
#define PE_GCS_FAHUD               4232 /* Fahud                              */
#define PE_GCS_GANDAJIKA_1970      4233 /* Gandajika 1970                     */
#define PE_GCS_GAROUA              4234 /* Garoua                             */
#define PE_GCS_GDA_1994            4283 /* Geocentric Datum of Australia 1994 */
#define PE_GCS_GGRS_1987           4121 /* Greek Geodetic Ref. System 1987    */
#define PE_GCS_GREEK               4120 /* Greek                              */
#define PE_GCS_GREEK_ATHENS        4815 /* Greek (Athens)                     */
#define PE_GCS_GUYANE_FRANCAISE    4235 /* Guyane Francaise                   */
#define PE_GCS_HERAT_NORTH         4255 /* Herat North                        */
#define PE_GCS_HITO_XVIII_1963     4254 /* Hito XVIII 1963                    */
#define PE_GCS_HU_TZU_SHAN         4236 /* Hu Tzu Shan                        */
#define PE_GCS_HUNGARIAN_1972      4237 /* Hungarian Datum 1972               */
#define PE_GCS_INDIAN_1954         4239 /* Indian 1954                        */
#define PE_GCS_INDIAN_1975         4240 /* Indian 1975                        */
#define PE_GCS_INDONESIAN_1974     4238 /* Indonesian Datum 1974              */
#define PE_GCS_JAMAICA_1875        4241 /* Jamaica 1875                       */
#define PE_GCS_JAMAICA_1969        4242 /* Jamaica 1969                       */
#define PE_GCS_KALIANPUR           4243 /* Kalianpur                          */
#define PE_GCS_KANDAWALA           4244 /* Kandawala                          */
#define PE_GCS_KERTAU              4245 /* Kertau                             */
#define PE_GCS_KKJ                 4123 /* Kartastokoordinaattijarjestelma    */
#define PE_GCS_KOC                 4246 /* Kuwait Oil Company                 */
#define PE_GCS_KUDAMS              4319 /* Kuwait Utility                     */
#define PE_GCS_LA_CANOA            4247 /* La Canoa                           */
#define PE_GCS_LAKE                4249 /* Lake                               */
#define PE_GCS_LEIGON              4250 /* Leigon                             */
#define PE_GCS_LIBERIA_1964        4251 /* Liberia 1964                       */
#define PE_GCS_LISBON              4207 /* Lisbon                             */
#define PE_GCS_LISBON_LISBON       4803 /* Lisbon (Lisbon)                    */
#define PE_GCS_LOMA_QUINTANA       4288 /* Loma Quintana                      */
#define PE_GCS_LOME                4252 /* Lome                               */
#define PE_GCS_LUZON_1911          4253 /* Luzon 1911                         */
#define PE_GCS_MAHE_1971           4256 /* Mahe 1971                          */
#define PE_GCS_MAKASSAR            4257 /* Makassar                           */
#define PE_GCS_MAKASSAR_JAKARTA    4804 /* Makassar (Jakarta)                 */
#define PE_GCS_MALONGO_1987        4259 /* Malongo 1987                       */
#define PE_GCS_MANOCA              4260 /* Manoca                             */
#define PE_GCS_MASSAWA             4262 /* Massawa                            */
#define PE_GCS_MERCHICH            4261 /* Merchich                           */
#define PE_GCS_MGI                 4312 /* Militar-Geographische Institut     */
#define PE_GCS_MGI_FERRO           4805 /* MGI (Ferro)                        */
#define PE_GCS_MHAST               4264 /* Mhast                              */
#define PE_GCS_MINNA               4263 /* Minna                              */
#define PE_GCS_MONTE_MARIO         4265 /* Monte Mario                        */
#define PE_GCS_MONTE_MARIO_ROME    4806 /* Monte Mario (Rome)                 */
#define PE_GCS_MPORALOKO           4266 /* M'poraloko                         */
#define PE_GCS_NAD_MICH            4268 /* NAD Michigan                       */
#define PE_GCS_NAD_1927            4267 /* North American Datum 1927          */
#define PE_GCS_NAD_1983            4269 /* North American Datum 1983          */
#define PE_GCS_NAD_1983_HARN     104269 /* North American Datum 1983 (HARN)   */
#define PE_GCS_NAHRWAN_1967        4270 /* Nahrwan 1967                       */
#define PE_GCS_NAPARIMA_1972       4271 /* Naparima 1972                      */
#define PE_GCS_NDG_PARIS           4902 /* Nord de Guerre (Paris)             */
#define PE_GCS_NGN                 4318 /* National Geodetic Network (Kuwait) */
#define PE_GCS_NGO_1948            4273 /* NGO 1948                           */
#define PE_GCS_NORD_SAHARA_1959    4307 /* Nord Sahara 1959                   */
#define PE_GCS_NSWC_9Z_2           4276 /* NSWC 9Z-2                          */
#define PE_GCS_NTF                 4275 /* Nouvelle Triangulation Francaise   */
#define PE_GCS_NTF_PARIS           4807 /* NTF (Paris)                        */
#define PE_GCS_NZGD_1949           4272 /* New Zealand Geodetic Datum 1949    */
#define PE_GCS_OS_SN_1980          4279 /* OS (SN) 1980                       */
#define PE_GCS_OSGB_1936           4277 /* OSGB 1936                          */
#define PE_GCS_OSGB_1970_SN        4278 /* OSGB 1970 (SN)                     */
#define PE_GCS_PADANG_1884         4280 /* Padang 1884                        */
#define PE_GCS_PADANG_1884_JAKARTA 4808 /* Padang 1884 (Jakarta)              */
#define PE_GCS_PALESTINE_1923      4281 /* Palestine 1923                     */
#define PE_GCS_POINTE_NOIRE        4282 /* Pointe Noire                       */
#define PE_GCS_PSAD_1956           4248 /* Provisional South Amer. Datum 1956 */
#define PE_GCS_PULKOVO_1942        4284 /* Pulkovo 1942                       */
#define PE_GCS_PULKOVO_1995        4200 /* Pulkovo 1995                       */
#define PE_GCS_QATAR               4285 /* Qatar                              */
#define PE_GCS_QATAR_1948          4286 /* Qatar 1948                         */
#define PE_GCS_QORNOQ              4287 /* Qornoq                             */
#define PE_GCS_RT38                4308 /* RT38                               */
#define PE_GCS_RT38_STOCKHOLM      4814 /* RT38 (Stockholm)                   */
#define PE_GCS_SAD_1969            4291 /* South American Datum 1969          */
#define PE_GCS_SAPPER_HILL_1943    4292 /* Sapper Hill 1943                   */
#define PE_GCS_SCHWARZECK          4293 /* Schwarzeck                         */
#define PE_GCS_SEGORA              4294 /* Segora                             */
#define PE_GCS_SERINDUNG           4295 /* Serindung                          */
#define PE_GCS_SUDAN               4296 /* Sudan                              */
#define PE_GCS_TANANARIVE_1925     4297 /* Tananarive 1925                    */
#define PE_GCS_TANANARIVE_1925_PARIS 4810 /* Tananarive 1925 (Paris)          */
#define PE_GCS_TIMBALAI_1948       4298 /* Timbalai 1948                      */
#define PE_GCS_TM65                4299 /* TM65                               */
#define PE_GCS_TM75                4300 /* TM75                               */
#define PE_GCS_TOKYO               4301 /* Tokyo                              */
#define PE_GCS_TRINIDAD_1903       4302 /* Trinidad 1903                      */
#define PE_GCS_TRUCIAL_COAST_1948  4303 /* Trucial Coast 1948                 */
#define PE_GCS_VOIROL_1875         4304 /* Voirol 1875                        */
#define PE_GCS_VOIROL_1875_PARIS   4811 /* Voirol 1875 (Paris)                */
#define PE_GCS_VOIROL_UNIFIE_1960  4305 /* Voirol Unifie 1960                 */
#define PE_GCS_VOIROL_UNIFIE_1960_PARIS 4812 /* Voirol Unifie 1960 (Paris)    */
#define PE_GCS_WGS_1972            4322 /* WGS 1972                           */
#define PE_GCS_WGS_1972_BE         4324 /* WGS 1972 Transit Broadcast Ephemer.*/
#define PE_GCS_WGS_1984            4326 /* WGS 1984                           */
#define PE_GCS_YACARE              4309 /* Yacare                             */
#define PE_GCS_YOFF                4310 /* Yoff                               */
#define PE_GCS_ZANDERIJ            4311 /* Zanderij                           */
#define PE_GCS_EUROPEAN_1979       (4201+33000) /* European 1979              */
#define PE_GCS_EVEREST_BANGLADESH  (4202+33000) /* Everest - Bangladesh       */
#define PE_GCS_EVEREST_INDIA_NEPAL (4203+33000) /* Everest - India and Nepal  */
#define PE_GCS_HJORSEY_1955        (4204+33000) /* Hjorsey 1955               */
#define PE_GCS_HONG_KONG_1963      (4205+33000) /* Hong Kong 1963             */
#define PE_GCS_OMAN                (4206+33000) /* Oman                       */
#define PE_GCS_S_ASIA_SINGAPORE    (4207+33000) /* South Asia Singapore       */
#define PE_GCS_AYABELLE            (4208+33000) /* Ayabelle Lighthouse        */
#define PE_GCS_BISSAU              (4209+33000) /* Bissau                     */
#define PE_GCS_DABOLA              (4210+33000) /* Dabola                     */
#define PE_GCS_POINT58             (4211+33000) /* Point 58                   */
#define PE_GCS_BEACON_E_1945       (4212+33000) /* Astro Beacon E 1945        */
#define PE_GCS_TERN_ISLAND_1961    (4213+33000) /* Tern Island Astro 1961     */
#define PE_GCS_ASTRO_1952          (4214+33000) /* Astronomical Station 1952  */
#define PE_GCS_BELLEVUE            (4215+33000) /* Bellevue IGN               */
#define PE_GCS_CANTON_1966         (4216+33000) /* Canton Astro 1966          */
#define PE_GCS_CHATHAM_ISLAND_1971 (4217+33000) /* Chatham Island Astro 1971  */
#define PE_GCS_DOS_1968            (4218+33000) /* DOS 1968                   */
#define PE_GCS_EASTER_ISLAND_1967  (4219+33000) /* Easter Island 1967         */
#define PE_GCS_GUAM_1963           (4220+33000) /* Guam 1963                  */
#define PE_GCS_GUX_1               (4221+33000) /* GUX 1 Astro                */
#define PE_GCS_JOHNSTON_ISLAND_1961 (4222+33000) /* Johnston Island 1961      */
#define PE_GCS_KUSAIE_1951         (4259+33000) /* Kusaie Astro 1951          */
#define PE_GCS_MIDWAY_1961         (4224+33000) /* Midway Astro 1961          */
#define PE_GCS_OLD_HAWAIIAN        (4225+33000) /* Old Hawaiian               */
#define PE_GCS_PITCAIRN_1967       (4226+33000) /* Pitcairn Astro 1967        */
#define PE_GCS_SANTO_DOS_1965      (4227+33000) /* Santo DOS 1965             */
#define PE_GCS_VITI_LEVU_1916      (4228+33000) /* Viti Levu 1916             */
#define PE_GCS_WAKE_ENIWETOK_1960  (4229+33000) /* Wake-Eniwetok 1960         */
#define PE_GCS_WAKE_ISLAND_1952    (4230+33000) /* Wake Island Astro 1952     */
#define PE_GCS_ANNA_1_1965         (4231+33000) /* Anna 1 Astro 1965          */
#define PE_GCS_GAN_1970            (4232+33000) /* Gan 1970                   */
#define PE_GCS_ISTS_073_1969       (4233+33000) /* ISTS 073 Astro 1969        */
#define PE_GCS_KERGUELEN_ISLAND_1949 (4234+33000) /* Kerguelen Island 1949    */
#define PE_GCS_REUNION             (4235+33000) /* Reunion                    */
#define PE_GCS_ANTIGUA_ISLAND_1943 (4236+33000) /* Antigua Island Astro 1943  */
#define PE_GCS_ASCENSION_ISLAND_1958 (4237+33000) /* Ascension Island 1958    */
#define PE_GCS_DOS_71_4           (4238+33000) /* Astro DOS 71/4              */
#define PE_GCS_CAPE_CANAVERAL     (4239+33000) /* Cape Canaveral              */
#define PE_GCS_FORT_THOMAS_1955   (4240+33000) /* Fort Thomas 1955            */
#define PE_GCS_GRACIOSA_1948      (4241+33000) /* Graciosa Base SW 1948       */
#define PE_GCS_ISTS_061_1968      (4242+33000) /* ISTS 061 Astro 1968         */
#define PE_GCS_LC5_1961           (4243+33000) /* L.C. 5 Astro 1961           */
#define PE_GCS_MONTSERRAT_ISLAND_1958 (4244+33000) /* Montserrat Astro 1958   */
#define PE_GCS_OBSERV_METEOR_1939 (4245+33000) /* Observ. Meteorologico 1939  */
#define PE_GCS_PICO_DE_LAS_NIEVES (4246+33000) /* Pico de Las Nieves          */
#define PE_GCS_PORTO_SANTO_1936   (4247+33000) /* Porto Santo 1936            */
#define PE_GCS_PUERTO_RICO        (4248+33000) /* Puerto Rico                 */
#define PE_GCS_SAO_BRAZ           (4249+33000) /* Sao Braz                    */
#define PE_GCS_SELVAGEM_GRANDE_1938 (4250+33000) /* Selvagem Grande 1938      */
#define PE_GCS_TRISTAN_1968       (4251+33000) /* Tristan Astro 1968          */
#define PE_GCS_SAMOA_1962         (4252+33000) /* American Samoa 1962         */
#define PE_GCS_CAMP_AREA          (4253+33000) /* Camp Area Astro             */
#define PE_GCS_DECEPTION_ISLAND   (4254+33000) /* Deception Island            */
#define PE_GCS_GUNUNG_SEGARA      (4255+33000) /* Gunung Segara               */
#define PE_GCS_INDIAN_1960        (4256+33000) /* Indian 1960                 */
#define PE_GCS_S42_HUNGARY        (4257+33000) /* S-42 Hungary                */
#define PE_GCS_S_JTSK             (4258+33000) /* S-JTSK                      */
#define PE_GCS_ALASKAN_ISLANDS    (4260+33000) /* Alaskan Islands             */

/*----------------------------------------------------------------------------*/
/*                            P A R A M E T E R S                             */
/*----------------------------------------------------------------------------*/
#define PE_PAR_FALSE_EASTING         3082 /* X0                               */
#define PE_PAR_FALSE_NORTHING        3083 /* Y0                               */
#define PE_PAR_CENTRAL_MERIDIAN      3088 /* LAM0                             */
#define PE_PAR_STANDARD_PARALLEL_1   3078 /* PHI1                             */
#define PE_PAR_STANDARD_PARALLEL_2   3079 /* PHI2                             */
#define PE_PAR_SCALE_FACTOR          3093 /* K0                               */
#define PE_PAR_CENTRAL_PARALLEL      3089 /* PHI0                             */
#define PE_PAR_LONGITUDE_OF_ORIGIN   3080 /* LAM0                             */
#define PE_PAR_LATITUDE_OF_ORIGIN    3081 /* PHI0                             */
#define PE_PAR_AZIMUTH               3094 /* ALPHA                            */
#define PE_PAR_LATITUDE_OF_1ST      (3081+33000) /* PHI1                      */
#define PE_PAR_LATITUDE_OF_2ND      (3082+33000) /* PHI2                      */
#define PE_PAR_LONGITUDE_OF_1ST     (3083+33000) /* LAM1                      */
#define PE_PAR_LONGITUDE_OF_2ND     (3084+33000) /* LAM2                      */
#define PE_PAR_X_AXIS_TRANSLATION   (3071+33000) /* DX                        */
#define PE_PAR_Y_AXIS_TRANSLATION   (3072+33000) /* DY                        */
#define PE_PAR_Z_AXIS_TRANSLATION   (3073+33000) /* DZ                        */
#define PE_PAR_X_AXIS_ROTATION      (3074+33000) /* RX                        */
#define PE_PAR_Y_AXIS_ROTATION      (3075+33000) /* RY                        */
#define PE_PAR_Z_AXIS_ROTATION      (3076+33000) /* RZ                        */
#define PE_PAR_SCALE_DIFFERENCE     (3077+33000) /* DS                        */
#define PE_PAR_NAME_DATASET        103078        /* ND                        */

/*----------------------------------------------------------------------------*/
/*                       M A P   P R O J E C T I O N S                        */
/*----------------------------------------------------------------------------*/
#define PE_PRJ_PLATE_CARREE             43001 /* Plate Carree                 */
#define PE_PRJ_EQUIDISTANT_CYLINDRICAL  43002 /* Equidistant Cylindrical      */
#define PE_PRJ_MILLER_CYLINDRICAL       43003 /* Miller Cylindrical           */
#define PE_PRJ_MERCATOR                 43004 /* Mercator                     */
#define PE_PRJ_GAUSS_KRUGER             43005 /* Gauss-Kruger                 */
#define PE_PRJ_TRANSVERSE_MERCATOR      43006 /* Transverse Mercator = 43005  */
#define PE_PRJ_ALBERS                   43007 /* Albers                       */
#define PE_PRJ_SINUSOIDAL               43008 /* Sinusoidal                   */
#define PE_PRJ_MOLLWEIDE                43009 /* Mollweide                    */
#define PE_PRJ_ECKERT_VI                43010 /* Eckert VI                    */
#define PE_PRJ_ECKERT_V                 43011 /* Eckert V                     */
#define PE_PRJ_ECKERT_IV                43012 /* Eckert IV                    */
#define PE_PRJ_ECKERT_III               43013 /* Eckert III                   */
#define PE_PRJ_ECKERT_II                43014 /* Eckert II                    */
#define PE_PRJ_ECKERT_I                 43015 /* Eckert I                     */
#define PE_PRJ_GALL_STEREOGRAPHIC       43016 /* Gall Stereographic           */
#define PE_PRJ_BEHRMANN                 43017 /* Behrmann                     */
#define PE_PRJ_WINKEL_I                 43018 /* Winkel I                     */
#define PE_PRJ_WINKEL_II                43019 /* Winkel II                    */
#define PE_PRJ_LAMBERT_CONFORMAL_CONIC  43020 /* Lambert Conformal Conic      */
#define PE_PRJ_POLYCONIC                43021 /* Polyconic                    */
#define PE_PRJ_QUARTIC_AUTHALIC         43022 /* Quartic Authalic             */
#define PE_PRJ_LOXIMUTHAL               43023 /* Loximuthal                   */
#define PE_PRJ_BONNE                    43024 /* Bonne                        */
#define PE_PRJ_HOTINE                   43025 /* Hotine                       */
#define PE_PRJ_STEREOGRAPHIC            43026 /* Stereographic                */
#define PE_PRJ_EQUIDISTANT_CONIC        43027 /* Equidistant Conic            */
#define PE_PRJ_CASSINI                  43028 /* Cassini                      */
#define PE_PRJ_VAN_DER_GRINTEN_I        43029 /* Van der Grinten I            */
#define PE_PRJ_ROBINSON                 43030 /* Robinson                     */
#define PE_PRJ_TWO_POINT_EQUIDISTANT    43031 /* Two-Point Equidistant        */
#define PE_PRJ_AZIMUTHAL_EQUIDISTANT    43032 /* Azimuthal Equidistant        */

/*----------------------------------------------------------------------------*/
/*          P R O J E C T E D   C O O R D I N A T E   S Y S T E M S           */
/*                           (W O R L D   W I D E)                            */
/*----------------------------------------------------------------------------*/
#define PE_PCS_WORLD_PLATE_CARREE             54001 /* Plate Carree           */
#define PE_PCS_WORLD_EQUIDISTANT_CYLINDRICAL  54002 /* Equidistant Cyl.       */
#define PE_PCS_WORLD_MILLER_CYLINDRICAL       54003 /* Miller Cylindrical     */
#define PE_PCS_WORLD_MERCATOR                 54004 /* Mercator               */
#define PE_PCS_WORLD_SINUSOIDAL               54008 /* Sinusoidal             */
#define PE_PCS_WORLD_MOLLWEIDE                54009 /* Mollweide              */
#define PE_PCS_WORLD_ECKERT_VI                54010 /* Eckert VI              */
#define PE_PCS_WORLD_ECKERT_V                 54011 /* Eckert V               */
#define PE_PCS_WORLD_ECKERT_IV                54012 /* Eckert IV              */
#define PE_PCS_WORLD_ECKERT_III               54013 /* Eckert III             */
#define PE_PCS_WORLD_ECKERT_II                54014 /* Eckert II              */
#define PE_PCS_WORLD_ECKERT_I                 54015 /* Eckert I               */
#define PE_PCS_WORLD_GALL_STEREOGRAPHIC       54016 /* Gall Stereographic     */
#define PE_PCS_WORLD_BEHRMANN                 54017 /* Behrmann               */
#define PE_PCS_WORLD_WINKEL_I                 54018 /* Winkel I               */
#define PE_PCS_WORLD_WINKEL_II                54019 /* Winkel II              */
#define PE_PCS_WORLD_POLYCONIC                54021 /* Polyconic              */
#define PE_PCS_WORLD_QUARTIC_AUTHALIC         54022 /* Quartic Authalic       */
#define PE_PCS_WORLD_LOXIMUTHAL               54023 /* Loximuthal             */
#define PE_PCS_WORLD_BONNE                    54024 /* Bonne                  */
#define PE_PCS_WORLD_HOTINE                   54025 /* Hotine                 */
#define PE_PCS_WORLD_STEREOGRAPHIC            54026 /* Stereographic          */
#define PE_PCS_WORLD_EQUIDISTANT_CONIC        54027 /* Equidistant Conic      */
#define PE_PCS_WORLD_CASSINI                  54028 /* Cassini                */
#define PE_PCS_WORLD_VAN_DER_GRINTEN_I        54029 /* Van der Grinten I      */
#define PE_PCS_WORLD_ROBINSON                 54030 /* Robinson               */
#define PE_PCS_WORLD_TWO_POINT_EQUIDISTANT    54031 /* Two-Point Equidistant  */
#define PE_PCS_WORLD_AZIMUTHAL_EQUIDISTANT    54032 /* Azimuthal Equidistant  */

/*----------------------------------------------------------------------------*/
/*          P R O J E C T E D   C O O R D I N A T E   S Y S T E M S           */
/*                          (S P H E R E   O N L Y)                           */
/*----------------------------------------------------------------------------*/
#define PE_PCS_SPHERE_PLATE_CARREE             53001 /* Plate Carree          */
#define PE_PCS_SPHERE_EQUIDISTANT_CYLINDRICAL  53002 /* Equidistant Cyl.      */
#define PE_PCS_SPHERE_MILLER_CYLINDRICAL       53003 /* Miller Cylindrical    */
#define PE_PCS_SPHERE_MERCATOR                 53004 /* Mercator              */
#define PE_PCS_SPHERE_SINUSOIDAL               53008 /* Sinusoidal            */
#define PE_PCS_SPHERE_MOLLWEIDE                53009 /* Mollweide             */
#define PE_PCS_SPHERE_ECKERT_VI                53010 /* Eckert VI             */
#define PE_PCS_SPHERE_ECKERT_V                 53011 /* Eckert V              */
#define PE_PCS_SPHERE_ECKERT_IV                53012 /* Eckert IV             */
#define PE_PCS_SPHERE_ECKERT_III               53013 /* Eckert III            */
#define PE_PCS_SPHERE_ECKERT_II                53014 /* Eckert II             */
#define PE_PCS_SPHERE_ECKERT_I                 53015 /* Eckert I              */
#define PE_PCS_SPHERE_GALL_STEREOGRAPHIC       53016 /* Gall Stereographic    */
#define PE_PCS_SPHERE_BEHRMANN                 53017 /* Behrmann              */
#define PE_PCS_SPHERE_WINKEL_I                 53018 /* Winkel I              */
#define PE_PCS_SPHERE_WINKEL_II                53019 /* Winkel II             */
#define PE_PCS_SPHERE_POLYCONIC                53021 /* Polyconic             */
#define PE_PCS_SPHERE_QUARTIC_AUTHALIC         53022 /* Quartic Authalic      */
#define PE_PCS_SPHERE_LOXIMUTHAL               53023 /* Loximuthal            */
#define PE_PCS_SPHERE_BONNE                    53024 /* Bonne                 */
#define PE_PCS_SPHERE_HOTINE                   53025 /* Hotine                */
#define PE_PCS_SPHERE_STEREOGRAPHIC            53026 /* Stereographic         */
#define PE_PCS_SPHERE_EQUIDISTANT_CONIC        53027 /* Equidistant Conic     */
#define PE_PCS_SPHERE_CASSINI                  53028 /* Cassini               */
#define PE_PCS_SPHERE_VAN_DER_GRINTEN_I        53029 /* Van der Grinten I     */
#define PE_PCS_SPHERE_ROBINSON                 53030 /* Robinson              */
#define PE_PCS_SPHERE_TWO_POINT_EQUIDISTANT    53031 /* Two-Point Equidistant */
#define PE_PCS_SPHERE_AZIMUTHAL_EQUIDISTANT    53032 /* Azimuthal Equidistant */

/*----------------------------------------------------------------------------*/
/*          P R O J E C T E D   C O O R D I N A T E   S Y S T E M S           */
/*----------------------------------------------------------------------------*/
#define PE_PCS_WGS_1984_UTM_1N    32601 /* WGS 1984 UTM Zone 1N               */
#define PE_PCS_WGS_1984_UTM_2N    32602 /* WGS 1984 UTM Zone 2N               */
#define PE_PCS_WGS_1984_UTM_3N    32603 /* WGS 1984 UTM Zone 3N               */
#define PE_PCS_WGS_1984_UTM_4N    32604 /* WGS 1984 UTM Zone 4N               */
#define PE_PCS_WGS_1984_UTM_5N    32605 /* WGS 1984 UTM Zone 5N               */
#define PE_PCS_WGS_1984_UTM_6N    32606 /* WGS 1984 UTM Zone 6N               */
#define PE_PCS_WGS_1984_UTM_7N    32607 /* WGS 1984 UTM Zone 7N               */
#define PE_PCS_WGS_1984_UTM_8N    32608 /* WGS 1984 UTM Zone 8N               */
#define PE_PCS_WGS_1984_UTM_9N    32609 /* WGS 1984 UTM Zone 9N               */
#define PE_PCS_WGS_1984_UTM_10N   32610 /* WGS 1984 UTM Zone 10N              */
#define PE_PCS_WGS_1984_UTM_11N   32611 /* WGS 1984 UTM Zone 11N              */
#define PE_PCS_WGS_1984_UTM_12N   32612 /* WGS 1984 UTM Zone 12N              */
#define PE_PCS_WGS_1984_UTM_13N   32613 /* WGS 1984 UTM Zone 13N              */
#define PE_PCS_WGS_1984_UTM_14N   32614 /* WGS 1984 UTM Zone 14N              */
#define PE_PCS_WGS_1984_UTM_15N   32615 /* WGS 1984 UTM Zone 15N              */
#define PE_PCS_WGS_1984_UTM_16N   32616 /* WGS 1984 UTM Zone 16N              */
#define PE_PCS_WGS_1984_UTM_17N   32617 /* WGS 1984 UTM Zone 17N              */
#define PE_PCS_WGS_1984_UTM_18N   32618 /* WGS 1984 UTM Zone 18N              */
#define PE_PCS_WGS_1984_UTM_19N   32619 /* WGS 1984 UTM Zone 19N              */
#define PE_PCS_WGS_1984_UTM_20N   32620 /* WGS 1984 UTM Zone 20N              */
#define PE_PCS_WGS_1984_UTM_21N   32621 /* WGS 1984 UTM Zone 21N              */
#define PE_PCS_WGS_1984_UTM_22N   32622 /* WGS 1984 UTM Zone 22N              */
#define PE_PCS_WGS_1984_UTM_23N   32623 /* WGS 1984 UTM Zone 23N              */
#define PE_PCS_WGS_1984_UTM_24N   32624 /* WGS 1984 UTM Zone 24N              */
#define PE_PCS_WGS_1984_UTM_25N   32625 /* WGS 1984 UTM Zone 25N              */
#define PE_PCS_WGS_1984_UTM_26N   32626 /* WGS 1984 UTM Zone 26N              */
#define PE_PCS_WGS_1984_UTM_27N   32627 /* WGS 1984 UTM Zone 27N              */
#define PE_PCS_WGS_1984_UTM_28N   32628 /* WGS 1984 UTM Zone 28N              */
#define PE_PCS_WGS_1984_UTM_29N   32629 /* WGS 1984 UTM Zone 29N              */
#define PE_PCS_WGS_1984_UTM_30N   32630 /* WGS 1984 UTM Zone 30N              */
#define PE_PCS_WGS_1984_UTM_31N   32631 /* WGS 1984 UTM Zone 31N              */
#define PE_PCS_WGS_1984_UTM_32N   32632 /* WGS 1984 UTM Zone 32N              */
#define PE_PCS_WGS_1984_UTM_33N   32633 /* WGS 1984 UTM Zone 33N              */
#define PE_PCS_WGS_1984_UTM_34N   32634 /* WGS 1984 UTM Zone 34N              */
#define PE_PCS_WGS_1984_UTM_35N   32635 /* WGS 1984 UTM Zone 35N              */
#define PE_PCS_WGS_1984_UTM_36N   32636 /* WGS 1984 UTM Zone 36N              */
#define PE_PCS_WGS_1984_UTM_37N   32637 /* WGS 1984 UTM Zone 37N              */
#define PE_PCS_WGS_1984_UTM_38N   32638 /* WGS 1984 UTM Zone 38N              */
#define PE_PCS_WGS_1984_UTM_39N   32639 /* WGS 1984 UTM Zone 39N              */
#define PE_PCS_WGS_1984_UTM_40N   32640 /* WGS 1984 UTM Zone 40N              */
#define PE_PCS_WGS_1984_UTM_41N   32641 /* WGS 1984 UTM Zone 41N              */
#define PE_PCS_WGS_1984_UTM_42N   32642 /* WGS 1984 UTM Zone 42N              */
#define PE_PCS_WGS_1984_UTM_43N   32643 /* WGS 1984 UTM Zone 43N              */
#define PE_PCS_WGS_1984_UTM_44N   32644 /* WGS 1984 UTM Zone 44N              */
#define PE_PCS_WGS_1984_UTM_45N   32645 /* WGS 1984 UTM Zone 45N              */
#define PE_PCS_WGS_1984_UTM_46N   32646 /* WGS 1984 UTM Zone 46N              */
#define PE_PCS_WGS_1984_UTM_47N   32647 /* WGS 1984 UTM Zone 47N              */
#define PE_PCS_WGS_1984_UTM_48N   32648 /* WGS 1984 UTM Zone 48N              */
#define PE_PCS_WGS_1984_UTM_49N   32649 /* WGS 1984 UTM Zone 49N              */
#define PE_PCS_WGS_1984_UTM_50N   32650 /* WGS 1984 UTM Zone 50N              */
#define PE_PCS_WGS_1984_UTM_51N   32651 /* WGS 1984 UTM Zone 51N              */
#define PE_PCS_WGS_1984_UTM_52N   32652 /* WGS 1984 UTM Zone 52N              */
#define PE_PCS_WGS_1984_UTM_53N   32653 /* WGS 1984 UTM Zone 53N              */
#define PE_PCS_WGS_1984_UTM_54N   32654 /* WGS 1984 UTM Zone 54N              */
#define PE_PCS_WGS_1984_UTM_55N   32655 /* WGS 1984 UTM Zone 55N              */
#define PE_PCS_WGS_1984_UTM_56N   32656 /* WGS 1984 UTM Zone 56N              */
#define PE_PCS_WGS_1984_UTM_57N   32657 /* WGS 1984 UTM Zone 57N              */
#define PE_PCS_WGS_1984_UTM_58N   32658 /* WGS 1984 UTM Zone 58N              */
#define PE_PCS_WGS_1984_UTM_59N   32659 /* WGS 1984 UTM Zone 59N              */
#define PE_PCS_WGS_1984_UTM_60N   32660 /* WGS 1984 UTM Zone 60N              */
#define PE_PCS_WGS_1984_UTM_1S    32701 /* WGS 1984 UTM Zone 1S               */
#define PE_PCS_WGS_1984_UTM_2S    32702 /* WGS 1984 UTM Zone 2S               */
#define PE_PCS_WGS_1984_UTM_3S    32703 /* WGS 1984 UTM Zone 3S               */
#define PE_PCS_WGS_1984_UTM_4S    32704 /* WGS 1984 UTM Zone 4S               */
#define PE_PCS_WGS_1984_UTM_5S    32705 /* WGS 1984 UTM Zone 5S               */
#define PE_PCS_WGS_1984_UTM_6S    32706 /* WGS 1984 UTM Zone 6S               */
#define PE_PCS_WGS_1984_UTM_7S    32707 /* WGS 1984 UTM Zone 7S               */
#define PE_PCS_WGS_1984_UTM_8S    32708 /* WGS 1984 UTM Zone 8S               */
#define PE_PCS_WGS_1984_UTM_9S    32709 /* WGS 1984 UTM Zone 9S               */
#define PE_PCS_WGS_1984_UTM_10S   32710 /* WGS 1984 UTM Zone 10S              */
#define PE_PCS_WGS_1984_UTM_11S   32711 /* WGS 1984 UTM Zone 11S              */
#define PE_PCS_WGS_1984_UTM_12S   32712 /* WGS 1984 UTM Zone 12S              */
#define PE_PCS_WGS_1984_UTM_13S   32713 /* WGS 1984 UTM Zone 13S              */
#define PE_PCS_WGS_1984_UTM_14S   32714 /* WGS 1984 UTM Zone 14S              */
#define PE_PCS_WGS_1984_UTM_15S   32715 /* WGS 1984 UTM Zone 15S              */
#define PE_PCS_WGS_1984_UTM_16S   32716 /* WGS 1984 UTM Zone 16S              */
#define PE_PCS_WGS_1984_UTM_17S   32717 /* WGS 1984 UTM Zone 17S              */
#define PE_PCS_WGS_1984_UTM_18S   32718 /* WGS 1984 UTM Zone 18S              */
#define PE_PCS_WGS_1984_UTM_19S   32719 /* WGS 1984 UTM Zone 19S              */
#define PE_PCS_WGS_1984_UTM_20S   32720 /* WGS 1984 UTM Zone 20S              */
#define PE_PCS_WGS_1984_UTM_21S   32721 /* WGS 1984 UTM Zone 21S              */
#define PE_PCS_WGS_1984_UTM_22S   32722 /* WGS 1984 UTM Zone 22S              */
#define PE_PCS_WGS_1984_UTM_23S   32723 /* WGS 1984 UTM Zone 23S              */
#define PE_PCS_WGS_1984_UTM_24S   32724 /* WGS 1984 UTM Zone 24S              */
#define PE_PCS_WGS_1984_UTM_25S   32725 /* WGS 1984 UTM Zone 25S              */
#define PE_PCS_WGS_1984_UTM_26S   32726 /* WGS 1984 UTM Zone 26S              */
#define PE_PCS_WGS_1984_UTM_27S   32727 /* WGS 1984 UTM Zone 27S              */
#define PE_PCS_WGS_1984_UTM_28S   32728 /* WGS 1984 UTM Zone 28S              */
#define PE_PCS_WGS_1984_UTM_29S   32729 /* WGS 1984 UTM Zone 29S              */
#define PE_PCS_WGS_1984_UTM_30S   32730 /* WGS 1984 UTM Zone 30S              */
#define PE_PCS_WGS_1984_UTM_31S   32731 /* WGS 1984 UTM Zone 31S              */
#define PE_PCS_WGS_1984_UTM_32S   32732 /* WGS 1984 UTM Zone 32S              */
#define PE_PCS_WGS_1984_UTM_33S   32733 /* WGS 1984 UTM Zone 33S              */
#define PE_PCS_WGS_1984_UTM_34S   32734 /* WGS 1984 UTM Zone 34S              */
#define PE_PCS_WGS_1984_UTM_35S   32735 /* WGS 1984 UTM Zone 35S              */
#define PE_PCS_WGS_1984_UTM_36S   32736 /* WGS 1984 UTM Zone 36S              */
#define PE_PCS_WGS_1984_UTM_37S   32737 /* WGS 1984 UTM Zone 37S              */
#define PE_PCS_WGS_1984_UTM_38S   32738 /* WGS 1984 UTM Zone 38S              */
#define PE_PCS_WGS_1984_UTM_39S   32739 /* WGS 1984 UTM Zone 39S              */
#define PE_PCS_WGS_1984_UTM_40S   32740 /* WGS 1984 UTM Zone 40S              */
#define PE_PCS_WGS_1984_UTM_41S   32741 /* WGS 1984 UTM Zone 41S              */
#define PE_PCS_WGS_1984_UTM_42S   32742 /* WGS 1984 UTM Zone 42S              */
#define PE_PCS_WGS_1984_UTM_43S   32743 /* WGS 1984 UTM Zone 43S              */
#define PE_PCS_WGS_1984_UTM_44S   32744 /* WGS 1984 UTM Zone 44S              */
#define PE_PCS_WGS_1984_UTM_45S   32745 /* WGS 1984 UTM Zone 45S              */
#define PE_PCS_WGS_1984_UTM_46S   32746 /* WGS 1984 UTM Zone 46S              */
#define PE_PCS_WGS_1984_UTM_47S   32747 /* WGS 1984 UTM Zone 47S              */
#define PE_PCS_WGS_1984_UTM_48S   32748 /* WGS 1984 UTM Zone 48S              */
#define PE_PCS_WGS_1984_UTM_49S   32749 /* WGS 1984 UTM Zone 49S              */
#define PE_PCS_WGS_1984_UTM_50S   32750 /* WGS 1984 UTM Zone 50S              */
#define PE_PCS_WGS_1984_UTM_51S   32751 /* WGS 1984 UTM Zone 51S              */
#define PE_PCS_WGS_1984_UTM_52S   32752 /* WGS 1984 UTM Zone 52S              */
#define PE_PCS_WGS_1984_UTM_53S   32753 /* WGS 1984 UTM Zone 53S              */
#define PE_PCS_WGS_1984_UTM_54S   32754 /* WGS 1984 UTM Zone 54S              */
#define PE_PCS_WGS_1984_UTM_55S   32755 /* WGS 1984 UTM Zone 55S              */
#define PE_PCS_WGS_1984_UTM_56S   32756 /* WGS 1984 UTM Zone 56S              */
#define PE_PCS_WGS_1984_UTM_57S   32757 /* WGS 1984 UTM Zone 57S              */
#define PE_PCS_WGS_1984_UTM_58S   32758 /* WGS 1984 UTM Zone 58S              */
#define PE_PCS_WGS_1984_UTM_59S   32759 /* WGS 1984 UTM Zone 59S              */
#define PE_PCS_WGS_1984_UTM_60S   32760 /* WGS 1984 UTM Zone 60S              */

#define PE_PCS_WGS_1972_UTM_1N    32201 /* WGS 1972 UTM Zone 1N               */
#define PE_PCS_WGS_1972_UTM_2N    32202 /* WGS 1972 UTM Zone 2N               */
#define PE_PCS_WGS_1972_UTM_3N    32203 /* WGS 1972 UTM Zone 3N               */
#define PE_PCS_WGS_1972_UTM_4N    32204 /* WGS 1972 UTM Zone 4N               */
#define PE_PCS_WGS_1972_UTM_5N    32205 /* WGS 1972 UTM Zone 5N               */
#define PE_PCS_WGS_1972_UTM_6N    32206 /* WGS 1972 UTM Zone 6N               */
#define PE_PCS_WGS_1972_UTM_7N    32207 /* WGS 1972 UTM Zone 7N               */
#define PE_PCS_WGS_1972_UTM_8N    32208 /* WGS 1972 UTM Zone 8N               */
#define PE_PCS_WGS_1972_UTM_9N    32209 /* WGS 1972 UTM Zone 9N               */
#define PE_PCS_WGS_1972_UTM_10N   32210 /* WGS 1972 UTM Zone 10N              */
#define PE_PCS_WGS_1972_UTM_11N   32211 /* WGS 1972 UTM Zone 11N              */
#define PE_PCS_WGS_1972_UTM_12N   32212 /* WGS 1972 UTM Zone 12N              */
#define PE_PCS_WGS_1972_UTM_13N   32213 /* WGS 1972 UTM Zone 13N              */
#define PE_PCS_WGS_1972_UTM_14N   32214 /* WGS 1972 UTM Zone 14N              */
#define PE_PCS_WGS_1972_UTM_15N   32215 /* WGS 1972 UTM Zone 15N              */
#define PE_PCS_WGS_1972_UTM_16N   32216 /* WGS 1972 UTM Zone 16N              */
#define PE_PCS_WGS_1972_UTM_17N   32217 /* WGS 1972 UTM Zone 17N              */
#define PE_PCS_WGS_1972_UTM_18N   32218 /* WGS 1972 UTM Zone 18N              */
#define PE_PCS_WGS_1972_UTM_19N   32219 /* WGS 1972 UTM Zone 19N              */
#define PE_PCS_WGS_1972_UTM_20N   32220 /* WGS 1972 UTM Zone 20N              */
#define PE_PCS_WGS_1972_UTM_21N   32221 /* WGS 1972 UTM Zone 21N              */
#define PE_PCS_WGS_1972_UTM_22N   32222 /* WGS 1972 UTM Zone 22N              */
#define PE_PCS_WGS_1972_UTM_23N   32223 /* WGS 1972 UTM Zone 23N              */
#define PE_PCS_WGS_1972_UTM_24N   32224 /* WGS 1972 UTM Zone 24N              */
#define PE_PCS_WGS_1972_UTM_25N   32225 /* WGS 1972 UTM Zone 25N              */
#define PE_PCS_WGS_1972_UTM_26N   32226 /* WGS 1972 UTM Zone 26N              */
#define PE_PCS_WGS_1972_UTM_27N   32227 /* WGS 1972 UTM Zone 27N              */
#define PE_PCS_WGS_1972_UTM_28N   32228 /* WGS 1972 UTM Zone 28N              */
#define PE_PCS_WGS_1972_UTM_29N   32229 /* WGS 1972 UTM Zone 29N              */
#define PE_PCS_WGS_1972_UTM_30N   32230 /* WGS 1972 UTM Zone 30N              */
#define PE_PCS_WGS_1972_UTM_31N   32231 /* WGS 1972 UTM Zone 31N              */
#define PE_PCS_WGS_1972_UTM_32N   32232 /* WGS 1972 UTM Zone 32N              */
#define PE_PCS_WGS_1972_UTM_33N   32233 /* WGS 1972 UTM Zone 33N              */
#define PE_PCS_WGS_1972_UTM_34N   32234 /* WGS 1972 UTM Zone 34N              */
#define PE_PCS_WGS_1972_UTM_35N   32235 /* WGS 1972 UTM Zone 35N              */
#define PE_PCS_WGS_1972_UTM_36N   32236 /* WGS 1972 UTM Zone 36N              */
#define PE_PCS_WGS_1972_UTM_37N   32237 /* WGS 1972 UTM Zone 37N              */
#define PE_PCS_WGS_1972_UTM_38N   32238 /* WGS 1972 UTM Zone 38N              */
#define PE_PCS_WGS_1972_UTM_39N   32239 /* WGS 1972 UTM Zone 39N              */
#define PE_PCS_WGS_1972_UTM_40N   32240 /* WGS 1972 UTM Zone 40N              */
#define PE_PCS_WGS_1972_UTM_41N   32241 /* WGS 1972 UTM Zone 41N              */
#define PE_PCS_WGS_1972_UTM_42N   32242 /* WGS 1972 UTM Zone 42N              */
#define PE_PCS_WGS_1972_UTM_43N   32243 /* WGS 1972 UTM Zone 43N              */
#define PE_PCS_WGS_1972_UTM_44N   32244 /* WGS 1972 UTM Zone 44N              */
#define PE_PCS_WGS_1972_UTM_45N   32245 /* WGS 1972 UTM Zone 45N              */
#define PE_PCS_WGS_1972_UTM_46N   32246 /* WGS 1972 UTM Zone 46N              */
#define PE_PCS_WGS_1972_UTM_47N   32247 /* WGS 1972 UTM Zone 47N              */
#define PE_PCS_WGS_1972_UTM_48N   32248 /* WGS 1972 UTM Zone 48N              */
#define PE_PCS_WGS_1972_UTM_49N   32249 /* WGS 1972 UTM Zone 49N              */
#define PE_PCS_WGS_1972_UTM_50N   32250 /* WGS 1972 UTM Zone 50N              */
#define PE_PCS_WGS_1972_UTM_51N   32251 /* WGS 1972 UTM Zone 51N              */
#define PE_PCS_WGS_1972_UTM_52N   32252 /* WGS 1972 UTM Zone 52N              */
#define PE_PCS_WGS_1972_UTM_53N   32253 /* WGS 1972 UTM Zone 53N              */
#define PE_PCS_WGS_1972_UTM_54N   32254 /* WGS 1972 UTM Zone 54N              */
#define PE_PCS_WGS_1972_UTM_55N   32255 /* WGS 1972 UTM Zone 55N              */
#define PE_PCS_WGS_1972_UTM_56N   32256 /* WGS 1972 UTM Zone 56N              */
#define PE_PCS_WGS_1972_UTM_57N   32257 /* WGS 1972 UTM Zone 57N              */
#define PE_PCS_WGS_1972_UTM_58N   32258 /* WGS 1972 UTM Zone 58N              */
#define PE_PCS_WGS_1972_UTM_59N   32259 /* WGS 1972 UTM Zone 59N              */
#define PE_PCS_WGS_1972_UTM_60N   32260 /* WGS 1972 UTM Zone 60N              */
#define PE_PCS_WGS_1972_UTM_1S    32301 /* WGS 1972 UTM Zone 1S               */
#define PE_PCS_WGS_1972_UTM_2S    32302 /* WGS 1972 UTM Zone 2S               */
#define PE_PCS_WGS_1972_UTM_3S    32303 /* WGS 1972 UTM Zone 3S               */
#define PE_PCS_WGS_1972_UTM_4S    32304 /* WGS 1972 UTM Zone 4S               */
#define PE_PCS_WGS_1972_UTM_5S    32305 /* WGS 1972 UTM Zone 5S               */
#define PE_PCS_WGS_1972_UTM_6S    32306 /* WGS 1972 UTM Zone 6S               */
#define PE_PCS_WGS_1972_UTM_7S    32307 /* WGS 1972 UTM Zone 7S               */
#define PE_PCS_WGS_1972_UTM_8S    32308 /* WGS 1972 UTM Zone 8S               */
#define PE_PCS_WGS_1972_UTM_9S    32309 /* WGS 1972 UTM Zone 9S               */
#define PE_PCS_WGS_1972_UTM_10S   32310 /* WGS 1972 UTM Zone 10S              */
#define PE_PCS_WGS_1972_UTM_11S   32311 /* WGS 1972 UTM Zone 11S              */
#define PE_PCS_WGS_1972_UTM_12S   32312 /* WGS 1972 UTM Zone 12S              */
#define PE_PCS_WGS_1972_UTM_13S   32313 /* WGS 1972 UTM Zone 13S              */
#define PE_PCS_WGS_1972_UTM_14S   32314 /* WGS 1972 UTM Zone 14S              */
#define PE_PCS_WGS_1972_UTM_15S   32315 /* WGS 1972 UTM Zone 15S              */
#define PE_PCS_WGS_1972_UTM_16S   32316 /* WGS 1972 UTM Zone 16S              */
#define PE_PCS_WGS_1972_UTM_17S   32317 /* WGS 1972 UTM Zone 17S              */
#define PE_PCS_WGS_1972_UTM_18S   32318 /* WGS 1972 UTM Zone 18S              */
#define PE_PCS_WGS_1972_UTM_19S   32319 /* WGS 1972 UTM Zone 19S              */
#define PE_PCS_WGS_1972_UTM_20S   32320 /* WGS 1972 UTM Zone 20S              */
#define PE_PCS_WGS_1972_UTM_21S   32321 /* WGS 1972 UTM Zone 21S              */
#define PE_PCS_WGS_1972_UTM_22S   32322 /* WGS 1972 UTM Zone 22S              */
#define PE_PCS_WGS_1972_UTM_23S   32323 /* WGS 1972 UTM Zone 23S              */
#define PE_PCS_WGS_1972_UTM_24S   32324 /* WGS 1972 UTM Zone 24S              */
#define PE_PCS_WGS_1972_UTM_25S   32325 /* WGS 1972 UTM Zone 25S              */
#define PE_PCS_WGS_1972_UTM_26S   32326 /* WGS 1972 UTM Zone 26S              */
#define PE_PCS_WGS_1972_UTM_27S   32327 /* WGS 1972 UTM Zone 27S              */
#define PE_PCS_WGS_1972_UTM_28S   32328 /* WGS 1972 UTM Zone 28S              */
#define PE_PCS_WGS_1972_UTM_29S   32329 /* WGS 1972 UTM Zone 29S              */
#define PE_PCS_WGS_1972_UTM_30S   32330 /* WGS 1972 UTM Zone 30S              */
#define PE_PCS_WGS_1972_UTM_31S   32331 /* WGS 1972 UTM Zone 31S              */
#define PE_PCS_WGS_1972_UTM_32S   32332 /* WGS 1972 UTM Zone 32S              */
#define PE_PCS_WGS_1972_UTM_33S   32333 /* WGS 1972 UTM Zone 33S              */
#define PE_PCS_WGS_1972_UTM_34S   32334 /* WGS 1972 UTM Zone 34S              */
#define PE_PCS_WGS_1972_UTM_35S   32335 /* WGS 1972 UTM Zone 35S              */
#define PE_PCS_WGS_1972_UTM_36S   32336 /* WGS 1972 UTM Zone 36S              */
#define PE_PCS_WGS_1972_UTM_37S   32337 /* WGS 1972 UTM Zone 37S              */
#define PE_PCS_WGS_1972_UTM_38S   32338 /* WGS 1972 UTM Zone 38S              */
#define PE_PCS_WGS_1972_UTM_39S   32339 /* WGS 1972 UTM Zone 39S              */
#define PE_PCS_WGS_1972_UTM_40S   32340 /* WGS 1972 UTM Zone 40S              */
#define PE_PCS_WGS_1972_UTM_41S   32341 /* WGS 1972 UTM Zone 41S              */
#define PE_PCS_WGS_1972_UTM_42S   32342 /* WGS 1972 UTM Zone 42S              */
#define PE_PCS_WGS_1972_UTM_43S   32343 /* WGS 1972 UTM Zone 43S              */
#define PE_PCS_WGS_1972_UTM_44S   32344 /* WGS 1972 UTM Zone 44S              */
#define PE_PCS_WGS_1972_UTM_45S   32345 /* WGS 1972 UTM Zone 45S              */
#define PE_PCS_WGS_1972_UTM_46S   32346 /* WGS 1972 UTM Zone 46S              */
#define PE_PCS_WGS_1972_UTM_47S   32347 /* WGS 1972 UTM Zone 47S              */
#define PE_PCS_WGS_1972_UTM_48S   32348 /* WGS 1972 UTM Zone 48S              */
#define PE_PCS_WGS_1972_UTM_49S   32349 /* WGS 1972 UTM Zone 49S              */
#define PE_PCS_WGS_1972_UTM_50S   32350 /* WGS 1972 UTM Zone 50S              */
#define PE_PCS_WGS_1972_UTM_51S   32351 /* WGS 1972 UTM Zone 51S              */
#define PE_PCS_WGS_1972_UTM_52S   32352 /* WGS 1972 UTM Zone 52S              */
#define PE_PCS_WGS_1972_UTM_53S   32353 /* WGS 1972 UTM Zone 53S              */
#define PE_PCS_WGS_1972_UTM_54S   32354 /* WGS 1972 UTM Zone 54S              */
#define PE_PCS_WGS_1972_UTM_55S   32355 /* WGS 1972 UTM Zone 55S              */
#define PE_PCS_WGS_1972_UTM_56S   32356 /* WGS 1972 UTM Zone 56S              */
#define PE_PCS_WGS_1972_UTM_57S   32357 /* WGS 1972 UTM Zone 57S              */
#define PE_PCS_WGS_1972_UTM_58S   32358 /* WGS 1972 UTM Zone 58S              */
#define PE_PCS_WGS_1972_UTM_59S   32359 /* WGS 1972 UTM Zone 59S              */
#define PE_PCS_WGS_1972_UTM_60S   32360 /* WGS 1972 UTM Zone 60S              */

#define PE_PCS_NAD_1927_BLM_14N      32074 /* NAD 1927 BLM Zone 14N           */
#define PE_PCS_NAD_1927_BLM_15N      32075 /* NAD 1927 BLM Zone 15N           */
#define PE_PCS_NAD_1927_BLM_16N      32076 /* NAD 1927 BLM Zone 16N           */
#define PE_PCS_NAD_1927_BLM_17N      32077 /* NAD 1927 BLM Zone 17N           */

#define PE_PCS_NAD_1927_UTM_3N    26703 /* NAD 1927 UTM Zone 3N               */
#define PE_PCS_NAD_1927_UTM_4N    26704 /* NAD 1927 UTM Zone 4N               */
#define PE_PCS_NAD_1927_UTM_5N    26705 /* NAD 1927 UTM Zone 5N               */
#define PE_PCS_NAD_1927_UTM_6N    26706 /* NAD 1927 UTM Zone 6N               */
#define PE_PCS_NAD_1927_UTM_7N    26707 /* NAD 1927 UTM Zone 7N               */
#define PE_PCS_NAD_1927_UTM_8N    26708 /* NAD 1927 UTM Zone 8N               */
#define PE_PCS_NAD_1927_UTM_9N    26709 /* NAD 1927 UTM Zone 9N               */
#define PE_PCS_NAD_1927_UTM_10N   26710 /* NAD 1927 UTM Zone 10N              */
#define PE_PCS_NAD_1927_UTM_11N   26711 /* NAD 1927 UTM Zone 11N              */
#define PE_PCS_NAD_1927_UTM_12N   26712 /* NAD 1927 UTM Zone 12N              */
#define PE_PCS_NAD_1927_UTM_13N   26713 /* NAD 1927 UTM Zone 13N              */
#define PE_PCS_NAD_1927_UTM_14N   26714 /* NAD 1927 UTM Zone 14N              */
#define PE_PCS_NAD_1927_UTM_15N   26715 /* NAD 1927 UTM Zone 15N              */
#define PE_PCS_NAD_1927_UTM_16N   26716 /* NAD 1927 UTM Zone 16N              */
#define PE_PCS_NAD_1927_UTM_17N   26717 /* NAD 1927 UTM Zone 17N              */
#define PE_PCS_NAD_1927_UTM_18N   26718 /* NAD 1927 UTM Zone 18N              */
#define PE_PCS_NAD_1927_UTM_19N   26719 /* NAD 1927 UTM Zone 19N              */
#define PE_PCS_NAD_1927_UTM_20N   26720 /* NAD 1927 UTM Zone 20N              */
#define PE_PCS_NAD_1927_UTM_21N   26721 /* NAD 1927 UTM Zone 21N              */
#define PE_PCS_NAD_1927_UTM_22N   26722 /* NAD 1927 UTM Zone 22N              */

#define PE_PCS_NAD_1983_UTM_3N    26903 /* NAD 1983 UTM Zone 3N               */
#define PE_PCS_NAD_1983_UTM_4N    26904 /* NAD 1983 UTM Zone 4N               */
#define PE_PCS_NAD_1983_UTM_5N    26905 /* NAD 1983 UTM Zone 5N               */
#define PE_PCS_NAD_1983_UTM_6N    26906 /* NAD 1983 UTM Zone 6N               */
#define PE_PCS_NAD_1983_UTM_7N    26907 /* NAD 1983 UTM Zone 7N               */
#define PE_PCS_NAD_1983_UTM_8N    26908 /* NAD 1983 UTM Zone 8N               */
#define PE_PCS_NAD_1983_UTM_9N    26909 /* NAD 1983 UTM Zone 9N               */
#define PE_PCS_NAD_1983_UTM_10N   26910 /* NAD 1983 UTM Zone 10N              */
#define PE_PCS_NAD_1983_UTM_11N   26911 /* NAD 1983 UTM Zone 11N              */
#define PE_PCS_NAD_1983_UTM_12N   26912 /* NAD 1983 UTM Zone 12N              */
#define PE_PCS_NAD_1983_UTM_13N   26913 /* NAD 1983 UTM Zone 13N              */
#define PE_PCS_NAD_1983_UTM_14N   26914 /* NAD 1983 UTM Zone 14N              */
#define PE_PCS_NAD_1983_UTM_15N   26915 /* NAD 1983 UTM Zone 15N              */
#define PE_PCS_NAD_1983_UTM_16N   26916 /* NAD 1983 UTM Zone 16N              */
#define PE_PCS_NAD_1983_UTM_17N   26917 /* NAD 1983 UTM Zone 17N              */
#define PE_PCS_NAD_1983_UTM_18N   26918 /* NAD 1983 UTM Zone 18N              */
#define PE_PCS_NAD_1983_UTM_19N   26919 /* NAD 1983 UTM Zone 19N              */
#define PE_PCS_NAD_1983_UTM_20N   26920 /* NAD 1983 UTM Zone 20N              */
#define PE_PCS_NAD_1983_UTM_21N   26921 /* NAD 1983 UTM Zone 21N              */
#define PE_PCS_NAD_1983_UTM_22N   26922 /* NAD 1983 UTM Zone 22N              */
#define PE_PCS_NAD_1983_UTM_23N   26923 /* NAD 1983 UTM Zone 23N              */

#define PE_PCS_ETRS_1989_UTM_28N   25828 /* ETRS 1989 UTM Zone 28N            */
#define PE_PCS_ETRS_1989_UTM_29N   25829 /* ETRS 1989 UTM Zone 29N            */
#define PE_PCS_ETRS_1989_UTM_30N   25830 /* ETRS 1989 UTM Zone 30N            */
#define PE_PCS_ETRS_1989_UTM_31N   25831 /* ETRS 1989 UTM Zone 31N            */
#define PE_PCS_ETRS_1989_UTM_32N   25832 /* ETRS 1989 UTM Zone 32N            */
#define PE_PCS_ETRS_1989_UTM_33N   25833 /* ETRS 1989 UTM Zone 33N            */
#define PE_PCS_ETRS_1989_UTM_34N   25834 /* ETRS 1989 UTM Zone 34N            */
#define PE_PCS_ETRS_1989_UTM_35N   25835 /* ETRS 1989 UTM Zone 35N            */
#define PE_PCS_ETRS_1989_UTM_36N   25836 /* ETRS 1989 UTM Zone 36N            */
#define PE_PCS_ETRS_1989_UTM_37N   25837 /* ETRS 1989 UTM Zone 37N            */
#define PE_PCS_ETRS_1989_UTM_38N   25838 /* ETRS 1989 UTM Zone 38N            */

#define PE_PCS_PULKOVO_1942_GK_4     28404 /* Pulkovo 1942 GK Zone 4          */
#define PE_PCS_PULKOVO_1942_GK_5     28405 /* Pulkovo 1942 GK Zone 5          */
#define PE_PCS_PULKOVO_1942_GK_6     28406 /* Pulkovo 1942 GK Zone 6          */
#define PE_PCS_PULKOVO_1942_GK_7     28407 /* Pulkovo 1942 GK Zone 7          */
#define PE_PCS_PULKOVO_1942_GK_8     28408 /* Pulkovo 1942 GK Zone 8          */
#define PE_PCS_PULKOVO_1942_GK_9     28409 /* Pulkovo 1942 GK Zone 9          */
#define PE_PCS_PULKOVO_1942_GK_10    28410 /* Pulkovo 1942 GK Zone 10         */
#define PE_PCS_PULKOVO_1942_GK_11    28411 /* Pulkovo 1942 GK Zone 11         */
#define PE_PCS_PULKOVO_1942_GK_12    28412 /* Pulkovo 1942 GK Zone 12         */
#define PE_PCS_PULKOVO_1942_GK_13    28413 /* Pulkovo 1942 GK Zone 13         */
#define PE_PCS_PULKOVO_1942_GK_14    28414 /* Pulkovo 1942 GK Zone 14         */
#define PE_PCS_PULKOVO_1942_GK_15    28415 /* Pulkovo 1942 GK Zone 15         */
#define PE_PCS_PULKOVO_1942_GK_16    28416 /* Pulkovo 1942 GK Zone 16         */
#define PE_PCS_PULKOVO_1942_GK_17    28417 /* Pulkovo 1942 GK Zone 17         */
#define PE_PCS_PULKOVO_1942_GK_18    28418 /* Pulkovo 1942 GK Zone 18         */
#define PE_PCS_PULKOVO_1942_GK_19    28419 /* Pulkovo 1942 GK Zone 19         */
#define PE_PCS_PULKOVO_1942_GK_20    28420 /* Pulkovo 1942 GK Zone 20         */
#define PE_PCS_PULKOVO_1942_GK_21    28421 /* Pulkovo 1942 GK Zone 21         */
#define PE_PCS_PULKOVO_1942_GK_22    28422 /* Pulkovo 1942 GK Zone 22         */
#define PE_PCS_PULKOVO_1942_GK_23    28423 /* Pulkovo 1942 GK Zone 23         */
#define PE_PCS_PULKOVO_1942_GK_24    28424 /* Pulkovo 1942 GK Zone 24         */
#define PE_PCS_PULKOVO_1942_GK_25    28425 /* Pulkovo 1942 GK Zone 25         */
#define PE_PCS_PULKOVO_1942_GK_26    28426 /* Pulkovo 1942 GK Zone 26         */
#define PE_PCS_PULKOVO_1942_GK_27    28427 /* Pulkovo 1942 GK Zone 27         */
#define PE_PCS_PULKOVO_1942_GK_28    28428 /* Pulkovo 1942 GK Zone 28         */
#define PE_PCS_PULKOVO_1942_GK_29    28429 /* Pulkovo 1942 GK Zone 29         */
#define PE_PCS_PULKOVO_1942_GK_30    28430 /* Pulkovo 1942 GK Zone 30         */
#define PE_PCS_PULKOVO_1942_GK_31    28431 /* Pulkovo 1942 GK Zone 31         */
#define PE_PCS_PULKOVO_1942_GK_32    28432 /* Pulkovo 1942 GK Zone 32         */
#define PE_PCS_PULKOVO_1942_GK_4N    28464 /* Pulkovo 1942 GK Zone 4N         */
#define PE_PCS_PULKOVO_1942_GK_5N    28465 /* Pulkovo 1942 GK Zone 5N         */
#define PE_PCS_PULKOVO_1942_GK_6N    28466 /* Pulkovo 1942 GK Zone 6N         */
#define PE_PCS_PULKOVO_1942_GK_7N    28467 /* Pulkovo 1942 GK Zone 7N         */
#define PE_PCS_PULKOVO_1942_GK_8N    28468 /* Pulkovo 1942 GK Zone 8N         */
#define PE_PCS_PULKOVO_1942_GK_9N    28469 /* Pulkovo 1942 GK Zone 9N         */
#define PE_PCS_PULKOVO_1942_GK_10N   28470 /* Pulkovo 1942 GK Zone 10N        */
#define PE_PCS_PULKOVO_1942_GK_11N   28471 /* Pulkovo 1942 GK Zone 11N        */
#define PE_PCS_PULKOVO_1942_GK_12N   28472 /* Pulkovo 1942 GK Zone 12N        */
#define PE_PCS_PULKOVO_1942_GK_13N   28473 /* Pulkovo 1942 GK Zone 13N        */
#define PE_PCS_PULKOVO_1942_GK_14N   28474 /* Pulkovo 1942 GK Zone 14N        */
#define PE_PCS_PULKOVO_1942_GK_15N   28475 /* Pulkovo 1942 GK Zone 15N        */
#define PE_PCS_PULKOVO_1942_GK_16N   28476 /* Pulkovo 1942 GK Zone 16N        */
#define PE_PCS_PULKOVO_1942_GK_17N   28477 /* Pulkovo 1942 GK Zone 17N        */
#define PE_PCS_PULKOVO_1942_GK_18N   28478 /* Pulkovo 1942 GK Zone 18N        */
#define PE_PCS_PULKOVO_1942_GK_19N   28479 /* Pulkovo 1942 GK Zone 19N        */
#define PE_PCS_PULKOVO_1942_GK_20N   28480 /* Pulkovo 1942 GK Zone 20N        */
#define PE_PCS_PULKOVO_1942_GK_21N   28481 /* Pulkovo 1942 GK Zone 21N        */
#define PE_PCS_PULKOVO_1942_GK_22N   28482 /* Pulkovo 1942 GK Zone 22N        */
#define PE_PCS_PULKOVO_1942_GK_23N   28483 /* Pulkovo 1942 GK Zone 23N        */
#define PE_PCS_PULKOVO_1942_GK_24N   28484 /* Pulkovo 1942 GK Zone 24N        */
#define PE_PCS_PULKOVO_1942_GK_25N   28485 /* Pulkovo 1942 GK Zone 25N        */
#define PE_PCS_PULKOVO_1942_GK_26N   28486 /* Pulkovo 1942 GK Zone 26N        */
#define PE_PCS_PULKOVO_1942_GK_27N   28487 /* Pulkovo 1942 GK Zone 27N        */
#define PE_PCS_PULKOVO_1942_GK_28N   28488 /* Pulkovo 1942 GK Zone 28N        */
#define PE_PCS_PULKOVO_1942_GK_29N   28489 /* Pulkovo 1942 GK Zone 29N        */
#define PE_PCS_PULKOVO_1942_GK_30N   28490 /* Pulkovo 1942 GK Zone 30N        */
#define PE_PCS_PULKOVO_1942_GK_31N   28491 /* Pulkovo 1942 GK Zone 31N        */
#define PE_PCS_PULKOVO_1942_GK_32N   28492 /* Pulkovo 1942 GK Zone 32N        */

#define PE_PCS_PULKOVO_1995_GK_4     20004 /* Pulkovo 1995 GK Zone 4          */
#define PE_PCS_PULKOVO_1995_GK_5     20005 /* Pulkovo 1995 GK Zone 5          */
#define PE_PCS_PULKOVO_1995_GK_6     20006 /* Pulkovo 1995 GK Zone 6          */
#define PE_PCS_PULKOVO_1995_GK_7     20007 /* Pulkovo 1995 GK Zone 7          */
#define PE_PCS_PULKOVO_1995_GK_8     20008 /* Pulkovo 1995 GK Zone 8          */
#define PE_PCS_PULKOVO_1995_GK_9     20009 /* Pulkovo 1995 GK Zone 9          */
#define PE_PCS_PULKOVO_1995_GK_10    20010 /* Pulkovo 1995 GK Zone 10         */
#define PE_PCS_PULKOVO_1995_GK_11    20011 /* Pulkovo 1995 GK Zone 11         */
#define PE_PCS_PULKOVO_1995_GK_12    20012 /* Pulkovo 1995 GK Zone 12         */
#define PE_PCS_PULKOVO_1995_GK_13    20013 /* Pulkovo 1995 GK Zone 13         */
#define PE_PCS_PULKOVO_1995_GK_14    20014 /* Pulkovo 1995 GK Zone 14         */
#define PE_PCS_PULKOVO_1995_GK_15    20015 /* Pulkovo 1995 GK Zone 15         */
#define PE_PCS_PULKOVO_1995_GK_16    20016 /* Pulkovo 1995 GK Zone 16         */
#define PE_PCS_PULKOVO_1995_GK_17    20017 /* Pulkovo 1995 GK Zone 17         */
#define PE_PCS_PULKOVO_1995_GK_18    20018 /* Pulkovo 1995 GK Zone 18         */
#define PE_PCS_PULKOVO_1995_GK_19    20019 /* Pulkovo 1995 GK Zone 19         */
#define PE_PCS_PULKOVO_1995_GK_20    20020 /* Pulkovo 1995 GK Zone 20         */
#define PE_PCS_PULKOVO_1995_GK_21    20021 /* Pulkovo 1995 GK Zone 21         */
#define PE_PCS_PULKOVO_1995_GK_22    20022 /* Pulkovo 1995 GK Zone 22         */
#define PE_PCS_PULKOVO_1995_GK_23    20023 /* Pulkovo 1995 GK Zone 23         */
#define PE_PCS_PULKOVO_1995_GK_24    20024 /* Pulkovo 1995 GK Zone 24         */
#define PE_PCS_PULKOVO_1995_GK_25    20025 /* Pulkovo 1995 GK Zone 25         */
#define PE_PCS_PULKOVO_1995_GK_26    20026 /* Pulkovo 1995 GK Zone 26         */
#define PE_PCS_PULKOVO_1995_GK_27    20027 /* Pulkovo 1995 GK Zone 27         */
#define PE_PCS_PULKOVO_1995_GK_28    20028 /* Pulkovo 1995 GK Zone 28         */
#define PE_PCS_PULKOVO_1995_GK_29    20029 /* Pulkovo 1995 GK Zone 29         */
#define PE_PCS_PULKOVO_1995_GK_30    20030 /* Pulkovo 1995 GK Zone 30         */
#define PE_PCS_PULKOVO_1995_GK_31    20031 /* Pulkovo 1995 GK Zone 31         */
#define PE_PCS_PULKOVO_1995_GK_32    20032 /* Pulkovo 1995 GK Zone 32         */
#define PE_PCS_PULKOVO_1995_GK_4N    20064 /* Pulkovo 1995 GK Zone 4N         */
#define PE_PCS_PULKOVO_1995_GK_5N    20065 /* Pulkovo 1995 GK Zone 5N         */
#define PE_PCS_PULKOVO_1995_GK_6N    20066 /* Pulkovo 1995 GK Zone 6N         */
#define PE_PCS_PULKOVO_1995_GK_7N    20067 /* Pulkovo 1995 GK Zone 7N         */
#define PE_PCS_PULKOVO_1995_GK_8N    20068 /* Pulkovo 1995 GK Zone 8N         */
#define PE_PCS_PULKOVO_1995_GK_9N    20069 /* Pulkovo 1995 GK Zone 9N         */
#define PE_PCS_PULKOVO_1995_GK_10N   20070 /* Pulkovo 1995 GK Zone 10N        */
#define PE_PCS_PULKOVO_1995_GK_11N   20071 /* Pulkovo 1995 GK Zone 11N        */
#define PE_PCS_PULKOVO_1995_GK_12N   20072 /* Pulkovo 1995 GK Zone 12N        */
#define PE_PCS_PULKOVO_1995_GK_13N   20073 /* Pulkovo 1995 GK Zone 13N        */
#define PE_PCS_PULKOVO_1995_GK_14N   20074 /* Pulkovo 1995 GK Zone 14N        */
#define PE_PCS_PULKOVO_1995_GK_15N   20075 /* Pulkovo 1995 GK Zone 15N        */
#define PE_PCS_PULKOVO_1995_GK_16N   20076 /* Pulkovo 1995 GK Zone 16N        */
#define PE_PCS_PULKOVO_1995_GK_17N   20077 /* Pulkovo 1995 GK Zone 17N        */
#define PE_PCS_PULKOVO_1995_GK_18N   20078 /* Pulkovo 1995 GK Zone 18N        */
#define PE_PCS_PULKOVO_1995_GK_19N   20079 /* Pulkovo 1995 GK Zone 19N        */
#define PE_PCS_PULKOVO_1995_GK_20N   20080 /* Pulkovo 1995 GK Zone 20N        */
#define PE_PCS_PULKOVO_1995_GK_21N   20081 /* Pulkovo 1995 GK Zone 21N        */
#define PE_PCS_PULKOVO_1995_GK_22N   20082 /* Pulkovo 1995 GK Zone 22N        */
#define PE_PCS_PULKOVO_1995_GK_23N   20083 /* Pulkovo 1995 GK Zone 23N        */
#define PE_PCS_PULKOVO_1995_GK_24N   20084 /* Pulkovo 1995 GK Zone 24N        */
#define PE_PCS_PULKOVO_1995_GK_25N   20085 /* Pulkovo 1995 GK Zone 25N        */
#define PE_PCS_PULKOVO_1995_GK_26N   20086 /* Pulkovo 1995 GK Zone 26N        */
#define PE_PCS_PULKOVO_1995_GK_27N   20087 /* Pulkovo 1995 GK Zone 27N        */
#define PE_PCS_PULKOVO_1995_GK_28N   20088 /* Pulkovo 1995 GK Zone 28N        */
#define PE_PCS_PULKOVO_1995_GK_29N   20089 /* Pulkovo 1995 GK Zone 29N        */
#define PE_PCS_PULKOVO_1995_GK_30N   20090 /* Pulkovo 1995 GK Zone 30N        */
#define PE_PCS_PULKOVO_1995_GK_31N   20091 /* Pulkovo 1995 GK Zone 31N        */
#define PE_PCS_PULKOVO_1995_GK_32N   20092 /* Pulkovo 1995 GK Zone 32N        */

#define PE_PCS_BEIJING_1954_GK_13    21413 /* Beijing 1954 GK Zone 13         */
#define PE_PCS_BEIJING_1954_GK_14    21414 /* Beijing 1954 GK Zone 14         */
#define PE_PCS_BEIJING_1954_GK_15    21415 /* Beijing 1954 GK Zone 15         */
#define PE_PCS_BEIJING_1954_GK_16    21416 /* Beijing 1954 GK Zone 16         */
#define PE_PCS_BEIJING_1954_GK_17    21417 /* Beijing 1954 GK Zone 17         */
#define PE_PCS_BEIJING_1954_GK_18    21418 /* Beijing 1954 GK Zone 18         */
#define PE_PCS_BEIJING_1954_GK_19    21419 /* Beijing 1954 GK Zone 19         */
#define PE_PCS_BEIJING_1954_GK_20    21420 /* Beijing 1954 GK Zone 20         */
#define PE_PCS_BEIJING_1954_GK_21    21421 /* Beijing 1954 GK Zone 21         */
#define PE_PCS_BEIJING_1954_GK_22    21422 /* Beijing 1954 GK Zone 22         */
#define PE_PCS_BEIJING_1954_GK_23    21423 /* Beijing 1954 GK Zone 23         */
#define PE_PCS_BEIJING_1954_GK_13N   21473 /* Beijing 1954 GK Zone 13N        */
#define PE_PCS_BEIJING_1954_GK_14N   21474 /* Beijing 1954 GK Zone 14N        */
#define PE_PCS_BEIJING_1954_GK_15N   21475 /* Beijing 1954 GK Zone 15N        */
#define PE_PCS_BEIJING_1954_GK_16N   21476 /* Beijing 1954 GK Zone 16N        */
#define PE_PCS_BEIJING_1954_GK_17N   21477 /* Beijing 1954 GK Zone 17N        */
#define PE_PCS_BEIJING_1954_GK_18N   21478 /* Beijing 1954 GK Zone 18N        */
#define PE_PCS_BEIJING_1954_GK_19N   21479 /* Beijing 1954 GK Zone 19N        */
#define PE_PCS_BEIJING_1954_GK_20N   21480 /* Beijing 1954 GK Zone 20N        */
#define PE_PCS_BEIJING_1954_GK_21N   21481 /* Beijing 1954 GK Zone 21N        */
#define PE_PCS_BEIJING_1954_GK_22N   21482 /* Beijing 1954 GK Zone 22N        */
#define PE_PCS_BEIJING_1954_GK_23N   21483 /* Beijing 1954 GK Zone 23N        */

#define PE_PCS_ED_1950_UTM_28N   23028 /* European Datum 1950 UTM Zone 28N    */
#define PE_PCS_ED_1950_UTM_29N   23029 /* European Datum 1950 UTM Zone 29N    */
#define PE_PCS_ED_1950_UTM_30N   23030 /* European Datum 1950 UTM Zone 30N    */
#define PE_PCS_ED_1950_UTM_31N   23031 /* European Datum 1950 UTM Zone 31N    */
#define PE_PCS_ED_1950_UTM_32N   23032 /* European Datum 1950 UTM Zone 32N    */
#define PE_PCS_ED_1950_UTM_33N   23033 /* European Datum 1950 UTM Zone 33N    */
#define PE_PCS_ED_1950_UTM_34N   23034 /* European Datum 1950 UTM Zone 34N    */
#define PE_PCS_ED_1950_UTM_35N   23035 /* European Datum 1950 UTM Zone 35N    */
#define PE_PCS_ED_1950_UTM_36N   23036 /* European Datum 1950 UTM Zone 36N    */
#define PE_PCS_ED_1950_UTM_37N   23037 /* European Datum 1950 UTM Zone 37N    */
#define PE_PCS_ED_1950_UTM_38N   23038 /* European Datum 1950 UTM Zone 38N    */

#define PE_PCS_ATS_1977_UTM_19N   2219  /* ATS 1977 UTM Zone 19N              */
#define PE_PCS_ATS_1977_UTM_20N   2220  /* ATS 1977 UTM Zone 20N              */

#define PE_PCS_KKJ_FINLAND_1      2391 /* Finland Zone 1                      */
#define PE_PCS_KKJ_FINLAND_2      2392 /* Finland Zone 2                      */
#define PE_PCS_KKJ_FINLAND_3      2393 /* Finland Zone 3                      */
#define PE_PCS_KKJ_FINLAND_4      2394 /* Finland Zone 4                      */

#define PE_PCS_SAD_1969_UTM_18N   29118 /* South American 1969 UTM Zone 18N   */
#define PE_PCS_SAD_1969_UTM_19N   29119 /* South American 1969 UTM Zone 19N   */
#define PE_PCS_SAD_1969_UTM_20N   29120 /* South American 1969 UTM Zone 20N   */
#define PE_PCS_SAD_1969_UTM_21N   29121 /* South American 1969 UTM Zone 21N   */
#define PE_PCS_SAD_1969_UTM_22N   29122 /* South American 1969 UTM Zone 22N   */
#define PE_PCS_SAD_1969_UTM_17S   29177 /* South American 1969 UTM Zone 17S   */
#define PE_PCS_SAD_1969_UTM_18S   29178 /* South American 1969 UTM Zone 18S   */
#define PE_PCS_SAD_1969_UTM_19S   29179 /* South American 1969 UTM Zone 19S   */
#define PE_PCS_SAD_1969_UTM_20S   29180 /* South American 1969 UTM Zone 20S   */
#define PE_PCS_SAD_1969_UTM_21S   29181 /* South American 1969 UTM Zone 21S   */
#define PE_PCS_SAD_1969_UTM_22S   29182 /* South American 1969 UTM Zone 22S   */
#define PE_PCS_SAD_1969_UTM_23S   29183 /* South American 1969 UTM Zone 23S   */
#define PE_PCS_SAD_1969_UTM_24S   29184 /* South American 1969 UTM Zone 24S   */
#define PE_PCS_SAD_1969_UTM_25S   29185 /* South American 1969 UTM Zone 25S   */

#define PE_PCS_AGD_1966_AMG_48   20248 /* AGD 1966 AMG Zone 48                */
#define PE_PCS_AGD_1966_AMG_49   20249 /* AGD 1966 AMG Zone 49                */
#define PE_PCS_AGD_1966_AMG_50   20250 /* AGD 1966 AMG Zone 50                */
#define PE_PCS_AGD_1966_AMG_51   20251 /* AGD 1966 AMG Zone 51                */
#define PE_PCS_AGD_1966_AMG_52   20252 /* AGD 1966 AMG Zone 52                */
#define PE_PCS_AGD_1966_AMG_53   20253 /* AGD 1966 AMG Zone 53                */
#define PE_PCS_AGD_1966_AMG_54   20254 /* AGD 1966 AMG Zone 54                */
#define PE_PCS_AGD_1966_AMG_55   20255 /* AGD 1966 AMG Zone 55                */
#define PE_PCS_AGD_1966_AMG_56   20256 /* AGD 1966 AMG Zone 56                */
#define PE_PCS_AGD_1966_AMG_57   20257 /* AGD 1966 AMG Zone 57                */
#define PE_PCS_AGD_1966_AMG_58   20258 /* AGD 1966 AMG Zone 58                */

#define PE_PCS_AGD_1984_AMG_48   20348 /* AGD 1984 AMG Zone 48                */
#define PE_PCS_AGD_1984_AMG_49   20349 /* AGD 1984 AMG Zone 49                */
#define PE_PCS_AGD_1984_AMG_50   20350 /* AGD 1984 AMG Zone 50                */
#define PE_PCS_AGD_1984_AMG_51   20351 /* AGD 1984 AMG Zone 51                */
#define PE_PCS_AGD_1984_AMG_52   20352 /* AGD 1984 AMG Zone 52                */
#define PE_PCS_AGD_1984_AMG_53   20353 /* AGD 1984 AMG Zone 53                */
#define PE_PCS_AGD_1984_AMG_54   20354 /* AGD 1984 AMG Zone 54                */
#define PE_PCS_AGD_1984_AMG_55   20355 /* AGD 1984 AMG Zone 55                */
#define PE_PCS_AGD_1984_AMG_56   20356 /* AGD 1984 AMG Zone 56                */
#define PE_PCS_AGD_1984_AMG_57   20357 /* AGD 1984 AMG Zone 57                */
#define PE_PCS_AGD_1984_AMG_58   20358 /* AGD 1984 AMG Zone 58                */

#define PE_PCS_GDA_1994_MGA_48   28348 /* GDA 1994 MGA Zone 48                */
#define PE_PCS_GDA_1994_MGA_49   28349 /* GDA 1994 MGA Zone 49                */
#define PE_PCS_GDA_1994_MGA_50   28350 /* GDA 1994 MGA Zone 50                */
#define PE_PCS_GDA_1994_MGA_51   28351 /* GDA 1994 MGA Zone 51                */
#define PE_PCS_GDA_1994_MGA_52   28352 /* GDA 1994 MGA Zone 52                */
#define PE_PCS_GDA_1994_MGA_53   28353 /* GDA 1994 MGA Zone 53                */
#define PE_PCS_GDA_1994_MGA_54   28354 /* GDA 1994 MGA Zone 54                */
#define PE_PCS_GDA_1994_MGA_55   28355 /* GDA 1994 MGA Zone 55                */
#define PE_PCS_GDA_1994_MGA_56   28356 /* GDA 1994 MGA Zone 56                */
#define PE_PCS_GDA_1994_MGA_57   28357 /* GDA 1994 MGA Zone 57                */
#define PE_PCS_GDA_1994_MGA_58   28358 /* GDA 1994 MGA Zone 58                */

#define PE_PCS_NAD_1927_AL_E    26729 /* NAD 1927 SPCS Zone Alabama East      */
#define PE_PCS_NAD_1927_AL_W    26730 /* NAD 1927 SPCS Zone Alabama West      */
#define PE_PCS_NAD_1927_AK_1    26731 /* NAD 1927 SPCS Zone Alaska 1          */
#define PE_PCS_NAD_1927_AK_2    26732 /* NAD 1927 SPCS Zone Alaska 2          */
#define PE_PCS_NAD_1927_AK_3    26733 /* NAD 1927 SPCS Zone Alaska 3          */
#define PE_PCS_NAD_1927_AK_4    26734 /* NAD 1927 SPCS Zone Alaska 4          */
#define PE_PCS_NAD_1927_AK_5    26735 /* NAD 1927 SPCS Zone Alaska 5          */
#define PE_PCS_NAD_1927_AK_6    26736 /* NAD 1927 SPCS Zone Alaska 6          */
#define PE_PCS_NAD_1927_AK_7    26737 /* NAD 1927 SPCS Zone Alaska 7          */
#define PE_PCS_NAD_1927_AK_8    26738 /* NAD 1927 SPCS Zone Alaska 8          */
#define PE_PCS_NAD_1927_AK_9    26739 /* NAD 1927 SPCS Zone Alaska 9          */
#define PE_PCS_NAD_1927_AK_10   26740 /* NAD 1927 SPCS Zone Alaska 10         */
#define PE_PCS_NAD_1927_AZ_E    26748 /* NAD 1927 SPCS Zone Arizona East      */
#define PE_PCS_NAD_1927_AZ_C    26749 /* NAD 1927 SPCS Zone Arizona Central   */
#define PE_PCS_NAD_1927_AZ_W    26750 /* NAD 1927 SPCS Zone Arizona West      */
#define PE_PCS_NAD_1927_AR_N    26751 /* NAD 1927 SPCS Zone Arkansas North    */
#define PE_PCS_NAD_1927_AR_S    26752 /* NAD 1927 SPCS Zone Arkansas South    */
#define PE_PCS_NAD_1927_CA_I    26741 /* NAD 1927 SPCS Zone California I      */
#define PE_PCS_NAD_1927_CA_II   26742 /* NAD 1927 SPCS Zone California II     */
#define PE_PCS_NAD_1927_CA_III  26743 /* NAD 1927 SPCS Zone California II     */
#define PE_PCS_NAD_1927_CA_IV   26744 /* NAD 1927 SPCS Zone California IV     */
#define PE_PCS_NAD_1927_CA_V    26745 /* NAD 1927 SPCS Zone California V      */
#define PE_PCS_NAD_1927_CA_VI   26746 /* NAD 1927 SPCS Zone California VI     */
#define PE_PCS_NAD_1927_CA_VII  26747 /* NAD 1927 SPCS Zone California VII    */
#define PE_PCS_NAD_1927_CO_N    26753 /* NAD 1927 SPCS Zone Colorado North    */
#define PE_PCS_NAD_1927_CO_C    26754 /* NAD 1927 SPCS Zone Colorado Central  */
#define PE_PCS_NAD_1927_CO_S    26755 /* NAD 1927 SPCS Zone Colorado South    */
#define PE_PCS_NAD_1927_CT      26756 /* NAD 1927 SPCS Zone Connecticut       */
#define PE_PCS_NAD_1927_DE      26757 /* NAD 1927 SPCS Zone Delaware          */
#define PE_PCS_NAD_1927_FL_E    26758 /* NAD 1927 SPCS Zone Florida East      */
#define PE_PCS_NAD_1927_FL_W    26759 /* NAD 1927 SPCS Zone Florida West      */
#define PE_PCS_NAD_1927_FL_N    26760 /* NAD 1927 SPCS Zone Florida North     */
#define PE_PCS_NAD_1927_GA_E    26766 /* NAD 1927 SPCS Zone Georgia East      */
#define PE_PCS_NAD_1927_GA_W    26767 /* NAD 1927 SPCS Zone Georgia West      */
#define PE_PCS_NAD_1927_HI_1    26761 /* NAD 1927 SPCS Zone Hawaii 1          */
#define PE_PCS_NAD_1927_HI_2    26762 /* NAD 1927 SPCS Zone Hawaii 2          */
#define PE_PCS_NAD_1927_HI_3    26763 /* NAD 1927 SPCS Zone Hawaii 3          */
#define PE_PCS_NAD_1927_HI_4    26764 /* NAD 1927 SPCS Zone Hawaii 4          */
#define PE_PCS_NAD_1927_HI_5    26765 /* NAD 1927 SPCS Zone Hawaii 5          */
#define PE_PCS_NAD_1927_ID_E    26768 /* NAD 1927 SPCS Zone Idaho East        */
#define PE_PCS_NAD_1927_ID_C    26769 /* NAD 1927 SPCS Zone Idaho Central     */
#define PE_PCS_NAD_1927_ID_W    26770 /* NAD 1927 SPCS Zone Idaho West        */
#define PE_PCS_NAD_1927_IL_E    26771 /* NAD 1927 SPCS Zone Illinois East     */
#define PE_PCS_NAD_1927_IL_W    26772 /* NAD 1927 SPCS Zone Illinois West     */
#define PE_PCS_NAD_1927_IN_E    26773 /* NAD 1927 SPCS Zone Indiana East      */
#define PE_PCS_NAD_1927_IN_W    26774 /* NAD 1927 SPCS Zone Indiana West      */
#define PE_PCS_NAD_1927_IA_N    26775 /* NAD 1927 SPCS Zone Iowa North        */
#define PE_PCS_NAD_1927_IA_S    26776 /* NAD 1927 SPCS Zone Iowa South        */
#define PE_PCS_NAD_1927_KS_N    26777 /* NAD 1927 SPCS Zone Kansas North      */
#define PE_PCS_NAD_1927_KS_S    26778 /* NAD 1927 SPCS Zone Kansas South      */
#define PE_PCS_NAD_1927_KY_N    26779 /* NAD 1927 SPCS Zone Kentucky North    */
#define PE_PCS_NAD_1927_KY_S    26780 /* NAD 1927 SPCS Zone Kentucky South    */
#define PE_PCS_NAD_1927_LA_N    26781 /* NAD 1927 SPCS Zone Louisiana North   */
#define PE_PCS_NAD_1927_LA_S    26782 /* NAD 1927 SPCS Zone Louisiana South   */
#define PE_PCS_NAD_1927_ME_E    26783 /* NAD 1927 SPCS Zone Maine East        */
#define PE_PCS_NAD_1927_ME_W    26784 /* NAD 1927 SPCS Zone Maine West        */
#define PE_PCS_NAD_1927_MD      26785 /* NAD 1927 SPCS Zone Maryland          */
#define PE_PCS_NAD_1927_MA_M    26786 /* NAD 1927 SPCS Zone Mass. Mainland    */
#define PE_PCS_NAD_1927_MA_I    26787 /* NAD 1927 SPCS Zone Mass. Island      */
#define PE_PCS_NAD_1927_MI_N    26788 /* NAD 1927 SPCS Zone Michigan North    */
#define PE_PCS_NAD_1927_MI_C    26789 /* NAD 1927 SPCS Zone Michigan Central  */
#define PE_PCS_NAD_1927_MI_S    26790 /* NAD 1927 SPCS Zone Michigan South    */
#define PE_PCS_NAD_1927_MN_N    26791 /* NAD 1927 SPCS Zone Minnesota North   */
#define PE_PCS_NAD_1927_MN_C    26792 /* NAD 1927 SPCS Zone Minnesota Central */
#define PE_PCS_NAD_1927_MN_S    26793 /* NAD 1927 SPCS Zone Minnesota South   */
#define PE_PCS_NAD_1927_MS_E    26794 /* NAD 1927 SPCS Zone Mississippi East  */
#define PE_PCS_NAD_1927_MS_W    26795 /* NAD 1927 SPCS Zone Mississippi West  */
#define PE_PCS_NAD_1927_MO_E    26796 /* NAD 1927 SPCS Zone Missouri East     */
#define PE_PCS_NAD_1927_MO_C    26797 /* NAD 1927 SPCS Zone Missouri Central  */
#define PE_PCS_NAD_1927_MO_W    26798 /* NAD 1927 SPCS Zone Missouri West     */
#define PE_PCS_NAD_1927_MT_N    32001 /* NAD 1927 SPCS Zone Montana North     */
#define PE_PCS_NAD_1927_MT_C    32002 /* NAD 1927 SPCS Zone Montana Central   */
#define PE_PCS_NAD_1927_MT_S    32003 /* NAD 1927 SPCS Zone Montana South     */
#define PE_PCS_NAD_1927_NE_N    32005 /* NAD 1927 SPCS Zone Nebraska North    */
#define PE_PCS_NAD_1927_NE_S    32006 /* NAD 1927 SPCS Zone Nebraska South    */
#define PE_PCS_NAD_1927_NV_E    32007 /* NAD 1927 SPCS Zone Nevada East       */
#define PE_PCS_NAD_1927_NV_C    32008 /* NAD 1927 SPCS Zone Nevada Central    */
#define PE_PCS_NAD_1927_NV_W    32009 /* NAD 1927 SPCS Zone Nevada West       */
#define PE_PCS_NAD_1927_NH      32010 /* NAD 1927 SPCS Zone New Hampshire     */
#define PE_PCS_NAD_1927_NJ      32011 /* NAD 1927 SPCS Zone New Jersey        */
#define PE_PCS_NAD_1927_NM_E    32012 /* NAD 1927 SPCS Zone New Mexico East   */
#define PE_PCS_NAD_1927_NM_C    32013 /* NAD 1927 SPCS Zone New Mexico Cent.  */
#define PE_PCS_NAD_1927_NM_W    32014 /* NAD 1927 SPCS Zone New Mexico West   */
#define PE_PCS_NAD_1927_NY_E    32015 /* NAD 1927 SPCS Zone New York East     */
#define PE_PCS_NAD_1927_NY_C    32016 /* NAD 1927 SPCS Zone New York Central  */
#define PE_PCS_NAD_1927_NY_W    32017 /* NAD 1927 SPCS Zone New York West     */
#define PE_PCS_NAD_1927_NY_LI   32018 /* NAD 1927 SPCS Zone NY Long Island    */
#define PE_PCS_NAD_1927_NC      32019 /* NAD 1927 SPCS Zone North Carolina    */
#define PE_PCS_NAD_1927_ND_N    32020 /* NAD 1927 SPCS Zone North Dakota N    */
#define PE_PCS_NAD_1927_ND_S    32021 /* NAD 1927 SPCS Zone North Dakota S    */
#define PE_PCS_NAD_1927_OH_N    32022 /* NAD 1927 SPCS Zone Ohio North        */
#define PE_PCS_NAD_1927_OH_S    32023 /* NAD 1927 SPCS Zone Ohio South        */
#define PE_PCS_NAD_1927_OK_N    32024 /* NAD 1927 SPCS Zone Oklahoma North    */
#define PE_PCS_NAD_1927_OK_S    32025 /* NAD 1927 SPCS Zone Oklahoma South    */
#define PE_PCS_NAD_1927_OR_N    32026 /* NAD 1927 SPCS Zone Oregon North      */
#define PE_PCS_NAD_1927_OR_S    32027 /* NAD 1927 SPCS Zone Oregon South      */
#define PE_PCS_NAD_1927_PA_N    32028 /* NAD 1927 SPCS Zone Pennsylvania N    */
#define PE_PCS_NAD_1927_PA_S    32029 /* NAD 1927 SPCS Zone Pennsylvania S    */
#define PE_PCS_NAD_1927_RI      32030 /* NAD 1927 SPCS Zone Rhode Island      */
#define PE_PCS_NAD_1927_SC_N    32031 /* NAD 1927 SPCS Zone South Carolina N  */
#define PE_PCS_NAD_1927_SC_S    32033 /* NAD 1927 SPCS Zone South Carolina S  */
#define PE_PCS_NAD_1927_SD_N    32034 /* NAD 1927 SPCS Zone South Dakota N    */
#define PE_PCS_NAD_1927_SD_S    32035 /* NAD 1927 SPCS Zone South Dakota S    */
#define PE_PCS_NAD_1927_TN      32036 /* NAD 1927 SPCS Zone Tennessee         */
#define PE_PCS_NAD_1927_TX_N    32037 /* NAD 1927 SPCS Zone Texas North       */
#define PE_PCS_NAD_1927_TX_NC   32038 /* NAD 1927 SPCS Zone Texas North Cent. */
#define PE_PCS_NAD_1927_TX_C    32039 /* NAD 1927 SPCS Zone Texas Central     */
#define PE_PCS_NAD_1927_TX_SC   32040 /* NAD 1927 SPCS Zone Texas South Cent. */
#define PE_PCS_NAD_1927_TX_S    32041 /* NAD 1927 SPCS Zone Texas South       */
#define PE_PCS_NAD_1927_UT_N    32042 /* NAD 1927 SPCS Zone Utah North        */
#define PE_PCS_NAD_1927_UT_C    32043 /* NAD 1927 SPCS Zone Utah Central      */
#define PE_PCS_NAD_1927_UT_S    32044 /* NAD 1927 SPCS Zone Utah South        */
#define PE_PCS_NAD_1927_VT      32045 /* NAD 1927 SPCS Zone Vermont           */
#define PE_PCS_NAD_1927_VA_N    32046 /* NAD 1927 SPCS Zone Virginia North    */
#define PE_PCS_NAD_1927_VA_S    32047 /* NAD 1927 SPCS Zone Virginia South    */
#define PE_PCS_NAD_1927_WA_N    32048 /* NAD 1927 SPCS Zone Washington North  */
#define PE_PCS_NAD_1927_WA_S    32049 /* NAD 1927 SPCS Zone Washington South  */
#define PE_PCS_NAD_1927_WV_N    32050 /* NAD 1927 SPCS Zone West Virginia N   */
#define PE_PCS_NAD_1927_WV_S    32051 /* NAD 1927 SPCS Zone West Virginia S   */
#define PE_PCS_NAD_1927_WI_N    32052 /* NAD 1927 SPCS Zone Wisconsin North   */
#define PE_PCS_NAD_1927_WI_C    32053 /* NAD 1927 SPCS Zone Wisconsin Central */
#define PE_PCS_NAD_1927_WI_S    32054 /* NAD 1927 SPCS Zone Wisconsin South   */
#define PE_PCS_NAD_1927_WY_E    32055 /* NAD 1927 SPCS Zone Wyoming I East    */
#define PE_PCS_NAD_1927_WY_EC   32056 /* NAD 1927 SPCS Zone Wyoming II EC     */
#define PE_PCS_NAD_1927_WY_WC   32057 /* NAD 1927 SPCS Zone Wyoming III WC    */
#define PE_PCS_NAD_1927_WY_W    32058 /* NAD 1927 SPCS Zone Wyoming IV West   */
#define PE_PCS_NAD_1927_PR      32059 /* NAD 1927 SPCS Zone Puerto Rico       */
#define PE_PCS_NAD_1927_VI      32060 /* NAD 1927 SPCS Zone St. Croix         */
#define PE_PCS_NAD_1927_GU      (32061+33000) /* NAD 1927 SPCS Zone Guam      */

#define PE_PCS_NAD_MICH_MI_E_OLD  26801 /* NAD Michigan                       */
#define PE_PCS_NAD_MICH_MI_C_OLD  26802 /* NAD Michigan                       */
#define PE_PCS_NAD_MICH_MI_W_OLD  26803 /* NAD Michigan                       */
#define PE_PCS_NAD_MICH_MI_N      26811 /* NAD Michigan                       */
#define PE_PCS_NAD_MICH_MI_C      26812 /* NAD Michigan                       */
#define PE_PCS_NAD_MICH_MI_S      26813 /* NAD Michigan                       */

#define PE_PCS_NAD_1983_AL_E    26929 /* NAD 1983 SPCS Zone Alabama East      */
#define PE_PCS_NAD_1983_AL_W    26930 /* NAD 1983 SPCS Zone Alabama West      */
#define PE_PCS_NAD_1983_AK_1    26931 /* NAD 1983 SPCS Zone Alaska 1          */
#define PE_PCS_NAD_1983_AK_2    26932 /* NAD 1983 SPCS Zone Alaska 2          */
#define PE_PCS_NAD_1983_AK_3    26933 /* NAD 1983 SPCS Zone Alaska 3          */
#define PE_PCS_NAD_1983_AK_4    26934 /* NAD 1983 SPCS Zone Alaska 4          */
#define PE_PCS_NAD_1983_AK_5    26935 /* NAD 1983 SPCS Zone Alaska 5          */
#define PE_PCS_NAD_1983_AK_6    26936 /* NAD 1983 SPCS Zone Alaska 6          */
#define PE_PCS_NAD_1983_AK_7    26937 /* NAD 1983 SPCS Zone Alaska 7          */
#define PE_PCS_NAD_1983_AK_8    26938 /* NAD 1983 SPCS Zone Alaska 8          */
#define PE_PCS_NAD_1983_AK_9    26939 /* NAD 1983 SPCS Zone Alaska 9          */
#define PE_PCS_NAD_1983_AK_10   26940 /* NAD 1983 SPCS Zone Alaska 10         */
#define PE_PCS_NAD_1983_AZ_E    26948 /* NAD 1983 SPCS Zone Arizona East      */
#define PE_PCS_NAD_1983_AZ_C    26949 /* NAD 1983 SPCS Zone Arizona Central   */
#define PE_PCS_NAD_1983_AZ_W    26950 /* NAD 1983 SPCS Zone Arizona West      */
#define PE_PCS_NAD_1983_AR_N    26951 /* NAD 1983 SPCS Zone Arkansas North    */
#define PE_PCS_NAD_1983_AR_S    26952 /* NAD 1983 SPCS Zone Arkansas South    */
#define PE_PCS_NAD_1983_CA_I    26941 /* NAD 1983 SPCS Zone California I      */
#define PE_PCS_NAD_1983_CA_II   26942 /* NAD 1983 SPCS Zone California II     */
#define PE_PCS_NAD_1983_CA_III  26943 /* NAD 1983 SPCS Zone California III    */
#define PE_PCS_NAD_1983_CA_IV   26944 /* NAD 1983 SPCS Zone California IV     */
#define PE_PCS_NAD_1983_CA_V    26945 /* NAD 1983 SPCS Zone California V      */
#define PE_PCS_NAD_1983_CA_VI   26946 /* NAD 1983 SPCS Zone California VI     */
#define PE_PCS_NAD_1983_CO_N    26953 /* NAD 1983 SPCS Zone Colorado North    */
#define PE_PCS_NAD_1983_CO_C    26954 /* NAD 1983 SPCS Zone Colorado Central  */
#define PE_PCS_NAD_1983_CO_S    26955 /* NAD 1983 SPCS Zone Colorado South    */
#define PE_PCS_NAD_1983_CT      26956 /* NAD 1983 SPCS Zone Connecticut       */
#define PE_PCS_NAD_1983_DE      26957 /* NAD 1983 SPCS Zone Delaware          */
#define PE_PCS_NAD_1983_FL_E    26958 /* NAD 1983 SPCS Zone Florida East      */
#define PE_PCS_NAD_1983_FL_W    26959 /* NAD 1983 SPCS Zone Florida West      */
#define PE_PCS_NAD_1983_FL_N    26960 /* NAD 1983 SPCS Zone Florida North     */
#define PE_PCS_NAD_1983_GA_E    26966 /* NAD 1983 SPCS Zone Georgia East      */
#define PE_PCS_NAD_1983_GA_W    26967 /* NAD 1983 SPCS Zone Georgia West      */
#define PE_PCS_NAD_1983_HI_1    26961 /* NAD 1983 SPCS Zone Hawaii Zone 1     */
#define PE_PCS_NAD_1983_HI_2    26962 /* NAD 1983 SPCS Zone Hawaii Zone 2     */
#define PE_PCS_NAD_1983_HI_3    26963 /* NAD 1983 SPCS Zone Hawaii Zone 3     */
#define PE_PCS_NAD_1983_HI_4    26964 /* NAD 1983 SPCS Zone Hawaii Zone 4     */
#define PE_PCS_NAD_1983_HI_5    26965 /* NAD 1983 SPCS Zone Hawaii Zone 5     */
#define PE_PCS_NAD_1983_ID_E    26968 /* NAD 1983 SPCS Zone Idaho East        */
#define PE_PCS_NAD_1983_ID_C    26969 /* NAD 1983 SPCS Zone Idaho Central     */
#define PE_PCS_NAD_1983_ID_W    26970 /* NAD 1983 SPCS Zone Idaho West        */
#define PE_PCS_NAD_1983_IL_E    26971 /* NAD 1983 SPCS Zone Illinois East     */
#define PE_PCS_NAD_1983_IL_W    26972 /* NAD 1983 SPCS Zone Illinois West     */
#define PE_PCS_NAD_1983_IN_E    26973 /* NAD 1983 SPCS Zone Indiana East      */
#define PE_PCS_NAD_1983_IN_W    26974 /* NAD 1983 SPCS Zone Indiana West      */
#define PE_PCS_NAD_1983_IA_N    26975 /* NAD 1983 SPCS Zone Iowa North        */
#define PE_PCS_NAD_1983_IA_S    26976 /* NAD 1983 SPCS Zone Iowa South        */
#define PE_PCS_NAD_1983_KS_N    26977 /* NAD 1983 SPCS Zone Kansas North      */
#define PE_PCS_NAD_1983_KS_S    26978 /* NAD 1983 SPCS Zone Kansas South      */
#define PE_PCS_NAD_1983_KY_N    26979 /* NAD 1983 SPCS Zone Kentucky North    */
#define PE_PCS_NAD_1983_KY_S    26980 /* NAD 1983 SPCS Zone Kentucky South    */
#define PE_PCS_NAD_1983_LA_N    26981 /* NAD 1983 SPCS Zone Louisiana North   */
#define PE_PCS_NAD_1983_LA_S    26982 /* NAD 1983 SPCS Zone Louisiana South   */
#define PE_PCS_NAD_1983_ME_E    26983 /* NAD 1983 SPCS Zone Maine East        */
#define PE_PCS_NAD_1983_ME_W    26984 /* NAD 1983 SPCS Zone Maine West        */
#define PE_PCS_NAD_1983_MD      26985 /* NAD 1983 SPCS Zone Maryland          */
#define PE_PCS_NAD_1983_MA_M    26986 /* NAD 1983 SPCS Zone Mass. Mainland    */
#define PE_PCS_NAD_1983_MA_I    26987 /* NAD 1983 SPCS Zone Mass. Island      */
#define PE_PCS_NAD_1983_MI_N    26988 /* NAD 1983 SPCS Zone Michigan North    */
#define PE_PCS_NAD_1983_MI_C    26989 /* NAD 1983 SPCS Zone Michigan Central  */
#define PE_PCS_NAD_1983_MI_S    26990 /* NAD 1983 SPCS Zone Michigan South    */
#define PE_PCS_NAD_1983_MN_N    26991 /* NAD 1983 SPCS Zone Minnesota North   */
#define PE_PCS_NAD_1983_MN_C    26992 /* NAD 1983 SPCS Zone Minnesota Central */
#define PE_PCS_NAD_1983_MN_S    26993 /* NAD 1983 SPCS Zone Minnesota South   */
#define PE_PCS_NAD_1983_MS_E    26994 /* NAD 1983 SPCS Zone Mississippi East  */
#define PE_PCS_NAD_1983_MS_W    26995 /* NAD 1983 SPCS Zone Mississippi West  */
#define PE_PCS_NAD_1983_MO_E    26996 /* NAD 1983 SPCS Zone Missouri East     */
#define PE_PCS_NAD_1983_MO_C    26997 /* NAD 1983 SPCS Zone Missouri Central  */
#define PE_PCS_NAD_1983_MO_W    26998 /* NAD 1983 SPCS Zone Missouri West     */
#define PE_PCS_NAD_1983_MT      32100 /* NAD 1983 SPCS Zone Montana           */
#define PE_PCS_NAD_1983_NE      32104 /* NAD 1983 SPCS Zone Nebraska          */
#define PE_PCS_NAD_1983_NV_E    32107 /* NAD 1983 SPCS Zone Nevada East       */
#define PE_PCS_NAD_1983_NV_C    32108 /* NAD 1983 SPCS Zone Nevada Central    */
#define PE_PCS_NAD_1983_NV_W    32109 /* NAD 1983 SPCS Zone Nevada West       */
#define PE_PCS_NAD_1983_NH      32110 /* NAD 1983 SPCS Zone New Hampshire     */
#define PE_PCS_NAD_1983_NJ      32111 /* NAD 1983 SPCS Zone New Jersey        */
#define PE_PCS_NAD_1983_NM_E    32112 /* NAD 1983 SPCS Zone New Mexico East   */
#define PE_PCS_NAD_1983_NM_C    32113 /* NAD 1983 SPCS Zone New Mexico Cent.  */
#define PE_PCS_NAD_1983_NM_W    32114 /* NAD 1983 SPCS Zone New Mexico West   */
#define PE_PCS_NAD_1983_NY_E    32115 /* NAD 1983 SPCS Zone New York East     */
#define PE_PCS_NAD_1983_NY_C    32116 /* NAD 1983 SPCS Zone New York Central  */
#define PE_PCS_NAD_1983_NY_W    32117 /* NAD 1983 SPCS Zone New York West     */
#define PE_PCS_NAD_1983_NY_LI   32118 /* NAD 1983 SPCS Zone NY Long Island    */
#define PE_PCS_NAD_1983_NC      32119 /* NAD 1983 SPCS Zone North Carolina    */
#define PE_PCS_NAD_1983_ND_N    32120 /* NAD 1983 SPCS Zone North Dakota N    */
#define PE_PCS_NAD_1983_ND_S    32121 /* NAD 1983 SPCS Zone North Dakota S    */
#define PE_PCS_NAD_1983_OH_N    32122 /* NAD 1983 SPCS Zone Ohio North        */
#define PE_PCS_NAD_1983_OH_S    32123 /* NAD 1983 SPCS Zone Ohio South        */
#define PE_PCS_NAD_1983_OK_N    32124 /* NAD 1983 SPCS Zone Oklahoma North    */
#define PE_PCS_NAD_1983_OK_S    32125 /* NAD 1983 SPCS Zone Oklahoma South    */
#define PE_PCS_NAD_1983_OR_N    32126 /* NAD 1983 SPCS Zone Oregon North      */
#define PE_PCS_NAD_1983_OR_S    32127 /* NAD 1983 SPCS Zone Oregon South      */
#define PE_PCS_NAD_1983_PA_N    32128 /* NAD 1983 SPCS Zone Pennsylvania N    */
#define PE_PCS_NAD_1983_PA_S    32129 /* NAD 1983 SPCS Zone Pennsylvania S    */
#define PE_PCS_NAD_1983_RI      32130 /* NAD 1983 SPCS Zone Rhode Island      */
#define PE_PCS_NAD_1983_SC      32133 /* NAD 1983 SPCS Zone South Carolina    */
#define PE_PCS_NAD_1983_SD_N    32134 /* NAD 1983 SPCS Zone South Dakota N    */
#define PE_PCS_NAD_1983_SD_S    32135 /* NAD 1983 SPCS Zone South Dakota S    */
#define PE_PCS_NAD_1983_TN      32136 /* NAD 1983 SPCS Zone Tennessee         */
#define PE_PCS_NAD_1983_TX_N    32137 /* NAD 1983 SPCS Zone Texas North       */
#define PE_PCS_NAD_1983_TX_NC   32138 /* NAD 1983 SPCS Zone Texas North Cent. */
#define PE_PCS_NAD_1983_TX_C    32139 /* NAD 1983 SPCS Zone Texas Central     */
#define PE_PCS_NAD_1983_TX_SC   32140 /* NAD 1983 SPCS Zone Texas South Cent. */
#define PE_PCS_NAD_1983_TX_S    32141 /* NAD 1983 SPCS Zone Texas South       */
#define PE_PCS_NAD_1983_UT_N    32142 /* NAD 1983 SPCS Zone Utah North        */
#define PE_PCS_NAD_1983_UT_C    32143 /* NAD 1983 SPCS Zone Utah Central      */
#define PE_PCS_NAD_1983_UT_S    32144 /* NAD 1983 SPCS Zone Utah South        */
#define PE_PCS_NAD_1983_VT      32145 /* NAD 1983 SPCS Zone Vermont           */
#define PE_PCS_NAD_1983_VA_N    32146 /* NAD 1983 SPCS Zone Virginia North    */
#define PE_PCS_NAD_1983_VA_S    32147 /* NAD 1983 SPCS Zone Virginia South    */
#define PE_PCS_NAD_1983_WA_N    32148 /* NAD 1983 SPCS Zone Washington North  */
#define PE_PCS_NAD_1983_WA_S    32149 /* NAD 1983 SPCS Zone Washington South  */
#define PE_PCS_NAD_1983_WV_N    32150 /* NAD 1983 SPCS Zone West Virginia N   */
#define PE_PCS_NAD_1983_WV_S    32151 /* NAD 1983 SPCS Zone West Virginia S   */
#define PE_PCS_NAD_1983_WI_N    32152 /* NAD 1983 SPCS Zone Wisconsin North   */
#define PE_PCS_NAD_1983_WI_C    32153 /* NAD 1983 SPCS Zone Wisconsin Central */
#define PE_PCS_NAD_1983_WI_S    32154 /* NAD 1983 SPCS Zone Wisconsin South   */
#define PE_PCS_NAD_1983_WY_E    32155 /* NAD 1983 SPCS Zone Wyoming I East    */
#define PE_PCS_NAD_1983_WY_EC   32156 /* NAD 1983 SPCS Zone Wyoming II EC     */
#define PE_PCS_NAD_1983_WY_WC   32157 /* NAD 1983 SPCS Zone Wyoming III WC    */
#define PE_PCS_NAD_1983_WY_W    32158 /* NAD 1983 SPCS Zone Wyoming IV West   */
#define PE_PCS_NAD_1983_PR_VI   32161 /* NAD 1983 SPCS Zone PR & St. Croix    */
#define PE_PCS_NAD_1983_GU      (32161+33000) /* NAD 1983 SPCS Zone Guam      */

#define PE_PCS_ADINDAN_UTM_37N         20137 /* Adindan UTM Zone 37N          */
#define PE_PCS_ADINDAN_UTM_38N         20138 /* Adindan UTM Zone 38N          */
#define PE_PCS_AFGOOYE_UTM_38N         20538 /* Afgooye UTM Zone 38N          */
#define PE_PCS_AFGOOYE_UTM_39N         20539 /* Afgooye UTM Zone 39N          */
#define PE_PCS_AIN_EL_ABD_UTM_37N      20437 /* Ain el Abd 1970 UTM Zone 37N  */
#define PE_PCS_AIN_EL_ABD_UTM_38N      20438 /* Ain el Abd 1970 UTM Zone 38N  */
#define PE_PCS_AIN_EL_ABD_UTM_39N      20439 /* Ain el Abd 1970 UTM Zone 39N  */
#define PE_PCS_ARATU_UTM_22S           20822 /* Aratu UTM Zone 22S            */
#define PE_PCS_ARATU_UTM_23S           20823 /* Aratu UTM Zone 23S            */
#define PE_PCS_ARATU_UTM_24S           20824 /* Aratu UTM Zone 24S            */
#define PE_PCS_BATAVIA_UTM_48S         21148 /* Batavia UTM Zone 48S          */
#define PE_PCS_BATAVIA_UTM_49S         21149 /* Batavia UTM Zone 49S          */
#define PE_PCS_BATAVIA_UTM_50S         21150 /* Batavia UTM Zone 50S          */
#define PE_PCS_BOGOTA_UTM_17N          21817 /* Bogota UTM Zone 17N           */
#define PE_PCS_BOGOTA_UTM_18N          21818 /* Bogota UTM Zone 18N           */
#define PE_PCS_CAMACUPA_UTM_32S        22032 /* Camacupa UTM Zone 32S         */
#define PE_PCS_CAMACUPA_UTM_33S        22033 /* Camacupa UTM Zone 33S         */
#define PE_PCS_CARTHAGE_UTM_32N        22332 /* Carthage UTM Zone 32N         */
#define PE_PCS_CORREGO_ALEGRE_UTM_23S  22523 /* Corrego Alegre UTM Zone 23S   */
#define PE_PCS_CORREGO_ALEGRE_UTM_24S  22524 /* Corrego Alegre UTM Zone 24S   */
#define PE_PCS_DATUM_73_UTM_ZONE_29N   27429 /* Datum 73 UTM Zone 29N         */
#define PE_PCS_DOUALA_UTM_32N          22832 /* Douala UTM Zone 32N           */
#define PE_PCS_FAHUD_UTM_39N           23239 /* Fahud UTM Zone 39N            */
#define PE_PCS_FAHUD_UTM_40N           23240 /* Fahud UTM Zone 40N            */
#define PE_PCS_GAROUA_UTM_33N          23433 /* Garoua UTM Zone 33N           */
#define PE_PCS_GGRS_1987_GREEK_GRID    2100  /* Greek Grid                    */
#define PE_PCS_ID_1974_UTM_46N         23846 /* Indonesia 1974 UTM Zone 46N   */
#define PE_PCS_ID_1974_UTM_47N         23847 /* Indonesia 1974 UTM Zone 47N   */
#define PE_PCS_ID_1974_UTM_48N         23848 /* Indonesia 1974 UTM Zone 48N   */
#define PE_PCS_ID_1974_UTM_49N         23849 /* Indonesia 1974 UTM Zone 49N   */
#define PE_PCS_ID_1974_UTM_50N         23850 /* Indonesia 1974 UTM Zone 50N   */
#define PE_PCS_ID_1974_UTM_51N         23851 /* Indonesia 1974 UTM Zone 51N   */
#define PE_PCS_ID_1974_UTM_52N         23852 /* Indonesia 1974 UTM Zone 52N   */
#define PE_PCS_ID_1974_UTM_53N         23853 /* Indonesia 1974 UTM Zone 53N   */
#define PE_PCS_ID_1974_UTM_46S         23886 /* Indonesia 1974 UTM Zone 46S   */
#define PE_PCS_ID_1974_UTM_47S         23887 /* Indonesia 1974 UTM Zone 47S   */
#define PE_PCS_ID_1974_UTM_48S         23888 /* Indonesia 1974 UTM Zone 48S   */
#define PE_PCS_ID_1974_UTM_49S         23889 /* Indonesia 1974 UTM Zone 49S   */
#define PE_PCS_ID_1974_UTM_50S         23890 /* Indonesia 1974 UTM Zone 50S   */
#define PE_PCS_ID_1974_UTM_51S         23891 /* Indonesia 1974 UTM Zone 51S   */
#define PE_PCS_ID_1974_UTM_52S         23892 /* Indonesia 1974 UTM Zone 52S   */
#define PE_PCS_ID_1974_UTM_53S         23893 /* Indonesia 1974 UTM Zone 53S   */
#define PE_PCS_ID_1974_UTM_54S         23894 /* Indonesia 1974 UTM Zone 54S   */
#define PE_PCS_INDIAN_1954_UTM_47N     23947 /* Indian 1954 UTM Zone 47N      */
#define PE_PCS_INDIAN_1954_UTM_48N     23948 /* Indian 1954 UTM Zone 48N      */
#define PE_PCS_INDIAN_1975_UTM_47N     24047 /* Indian 1975 UTM Zone 47N      */
#define PE_PCS_INDIAN_1975_UTM_48N     24048 /* Indian 1975 UTM Zone 48N      */
#define PE_PCS_KERTAU_UTM_47N          24547 /* Kertau UTM Zone 47N           */
#define PE_PCS_KERTAU_UTM_48N          24548 /* Kertau UTM Zone 48N           */
#define PE_PCS_LA_CANOA_UTM_20N        24720 /* La Canoa UTM Zone 20N         */
#define PE_PCS_LA_CANOA_UTM_21N        24721 /* La Canoa UTM Zone 21N         */
#define PE_PCS_LOME_UTM_31N            25231 /* Lome UTM Zone 31N             */
#define PE_PCS_MPORALOKO_UTM_32N       26632 /* M'poraloko UTM Zone 32N       */
#define PE_PCS_MPORALOKO_UTM_32S       26692 /* M'poraloko UTM Zone 32S       */
#define PE_PCS_MALONGO_1987_UTM_32S    25932 /* Malongo 1987 UTM Zone 32S     */
#define PE_PCS_MASSAWA_UTM_37N         26237 /* Massawa UTM Zone 37N          */
#define PE_PCS_MHAST_UTM_32S           26432 /* Mhast UTM Zone 32S            */
#define PE_PCS_MINNA_UTM_31N           26331 /* Minna UTM Zone 31N            */
#define PE_PCS_MINNA_UTM_32N           26332 /* Minna UTM Zone 32N            */
#define PE_PCS_NAHRWAN_1967_UTM_38N    27038 /* Nahrwan 1967 UTM Zone 38N     */
#define PE_PCS_NAHRWAN_1967_UTM_39N    27039 /* Nahrwan 1967 UTM Zone 39N     */
#define PE_PCS_NAHRWAN_1967_UTM_40N    27040 /* Nahrwan 1967 UTM Zone 40N     */
#define PE_PCS_NGN_UTM_38N             31838 /* NGN UTM Zone 38N              */
#define PE_PCS_NGN_UTM_39N             31839 /* NGN UTM Zone 39N              */
#define PE_PCS_NORD_SAHARA_UTM_29N     30729 /* Nord Sahara 1959 UTM Zone 29N */
#define PE_PCS_NORD_SAHARA_UTM_30N     30730 /* Nord Sahara 1959 UTM Zone 30N */
#define PE_PCS_NORD_SAHARA_UTM_31N     30731 /* Nord Sahara 1959 UTM Zone 31N */
#define PE_PCS_NORD_SAHARA_UTM_32N     30732 /* Nord Sahara 1959 UTM Zone 32N */
#define PE_PCS_NAPARIMA_1972_UTM_20N   27120 /* Naparima 1972 UTM Zone 20N    */
#define PE_PCS_POINTE_NOIRE_UTM_32S    28232 /* Pointe Noire UTM Zone 32S     */
#define PE_PCS_PSAD_1956_UTM_18N   24818 /* Prov. S. Amer. Datum UTM Zone 18N */
#define PE_PCS_PSAD_1956_UTM_19N   24819 /* Prov. S. Amer. Datum UTM Zone 19N */
#define PE_PCS_PSAD_1956_UTM_20N   24820 /* Prov. S. Amer. Datum UTM Zone 20N */
#define PE_PCS_PSAD_1956_UTM_21N   24821 /* Prov. S. Amer. Datum UTM Zone 21N */
#define PE_PCS_PSAD_1956_UTM_17S   24877 /* Prov. S. Amer. Datum UTM Zone 17S */
#define PE_PCS_PSAD_1956_UTM_18S   24878 /* Prov. S. Amer. Datum UTM Zone 18S */
#define PE_PCS_PSAD_1956_UTM_19S   24879 /* Prov. S. Amer. Datum UTM Zone 19S */
#define PE_PCS_PSAD_1956_UTM_20S   24880 /* Prov. S. Amer. Datum UTM Zone 20S */
#define PE_PCS_SAPPER_HILL_UTM_20S     29220 /* Sapper Hill 1943 UTM Zone 20S */
#define PE_PCS_SAPPER_HILL_UTM_21S     29221 /* Sapper Hill 1943 UTM Zone 21S */
#define PE_PCS_SCHWARZECK_UTM_33S      29333 /* Schwarzeck UTM Zone 33S       */
#define PE_PCS_SUDAN_UTM_35N           29635 /* Sudan UTM Zone 35N            */
#define PE_PCS_SUDAN_UTM_36N           29636 /* Sudan UTM Zone 36N            */
#define PE_PCS_TANANARIVE_UTM_38S      29738 /* Tananarive 1925 UTM Zone 38S  */
#define PE_PCS_TANANARIVE_UTM_39S      29739 /* Tananarive 1925 UTM Zone 39S  */
#define PE_PCS_TC_1948_UTM_39N       30339 /* Trucial Coast 1948 UTM Zone 39N */
#define PE_PCS_TC_1948_UTM_40N       30340 /* Trucial Coast 1948 UTM Zone 40N */
#define PE_PCS_TIMBALAI_1948_UTM_49N   29849 /* Timbalai 1948 UTM Zone 49N    */
#define PE_PCS_TIMBALAI_1948_UTM_50N   29850 /* Timbalai 1948 UTM Zone 50N    */
#define PE_PCS_YOFF_1972_UTM_28N       31028 /* Yoff 1972 UTM Zone 28N        */
#define PE_PCS_ZANDERIJ_1972_UTM_21N   31121 /* Zanderij 1972 UTM Zone 21N    */

#define PE_PCS_KUDAMS_KTM                 31900 /* Kuwait Utility KTM         */
#define PE_PCS_LUZON_PHILIPPINES_I        25391 /* Philippines Zone I         */
#define PE_PCS_LUZON_PHILIPPINES_II       25392 /* Philippines Zone II        */
#define PE_PCS_LUZON_PHILIPPINES_III      25393 /* Philippines Zone III       */
#define PE_PCS_LUZON_PHILIPPINES_IV       25394 /* Philippines Zone IV        */
#define PE_PCS_LUZON_PHILIPPINES_V        25395 /* Philippines Zone V         */
#define PE_PCS_MGI_FERRO_AUSTRIA_WEST     31291 /* Austria (Ferro) West Zone  */
#define PE_PCS_MGI_FERRO_AUSTRIA_CENTRAL  31292 /* Austria (Ferro) Cent. Zone */
#define PE_PCS_MGI_FERRO_AUSTRIA_EAST     31293 /* Austria (Ferro) East Zone  */
#define PE_PCS_MONTE_MARIO_ROME_ITALY_1   26591 /* Monte Mario (Rome) Italy 1 */
#define PE_PCS_MONTE_MARIO_ROME_ITALY_2   26592 /* Monte Mario (Rome) Italy 2 */
#define PE_PCS_C_INCHAUSPE_ARGENTINA_1    22191 /* Argentina Zone 1           */
#define PE_PCS_C_INCHAUSPE_ARGENTINA_2    22192 /* Argentina Zone 2           */
#define PE_PCS_C_INCHAUSPE_ARGENTINA_3    22193 /* Argentina Zone 3           */
#define PE_PCS_C_INCHAUSPE_ARGENTINA_4    22194 /* Argentina Zone 4           */
#define PE_PCS_C_INCHAUSPE_ARGENTINA_5    22195 /* Argentina Zone 5           */
#define PE_PCS_C_INCHAUSPE_ARGENTINA_6    22196 /* Argentina Zone 6           */
#define PE_PCS_C_INCHAUSPE_ARGENTINA_7    22197 /* Argentina Zone 7           */
#define PE_PCS_DHDN_GERMANY_1             31491 /* Germany Zone 1             */
#define PE_PCS_DHDN_GERMANY_2             31492 /* Germany Zone 2             */
#define PE_PCS_DHDN_GERMANY_3             31493 /* Germany Zone 3             */
#define PE_PCS_DHDN_GERMANY_4             31494 /* Germany Zone 4             */
#define PE_PCS_DHDN_GERMANY_5             31495 /* Germany Zone 5             */
#define PE_PCS_AIN_EL_ABD_BAHRAIN_GRID    20499 /* Bahrain State Grid         */
#define PE_PCS_BOGOTA_COLOMBIA_WEST       21891 /* Colombia West Zone         */
#define PE_PCS_BOGOTA_COLOMBIA_BOGOTA     21892 /* Colombia Bogota Zone       */
#define PE_PCS_BOGOTA_COLOMBIA_E_CENTRAL  21893 /* Colombia E Central Zone    */
#define PE_PCS_BOGOTA_COLOMBIA_EAST       21894 /* Colombia East Zone         */
#define PE_PCS_EGYPT_RED_BELT             22992 /* Egypt Red Belt             */
#define PE_PCS_EGYPT_PURPLE_BELT          22993 /* Egypt Purple Belt          */
#define PE_PCS_EGYPT_EXT_PURPLE_BELT      22994 /* Egypt Extended Purple Belt */
#define PE_PCS_LEIGON_GHANA_GRID          25000 /* Ghana Metre Grid           */
#define PE_PCS_TM65_IRISH_GRID            29900 /* Irish National Grid        */
#define PE_PCS_NZGD_1949_NORTH_ISLAND     27291 /* New Zealand North Island   */
#define PE_PCS_NZGD_1949_SOUTH_ISLAND     27292 /* New Zealand South Island   */
#define PE_PCS_MINNA_NIGERIA_WEST_BELT    26391 /* Nigeria West Belt          */
#define PE_PCS_MINNA_NIGERIA_MID_BELT     26392 /* Nigeria Mid Belt           */
#define PE_PCS_MINNA_NIGERIA_EAST_BELT    26393 /* Nigeria East Belt          */
#define PE_PCS_PSAD_1956_PERU_WEST        24891 /* Peru West Zone             */
#define PE_PCS_PSAD_1956_PERU_CENTRAL     24892 /* Peru Central Zone          */
#define PE_PCS_PSAD_1956_PERU_EAST        24893 /* Peru East Zone             */
#define PE_PCS_LISBON_PORTUGUESE_GRID     20700 /* Portuguese National Grid   */
#define PE_PCS_QATAR_GRID                 28600 /* Qatar National Grid        */
#define PE_PCS_OSGB_1936_BRITISH_GRID     27700 /* British National Grid      */
#define PE_PCS_RT38_STOCKHOLM_SWEDISH_GRID  30800 /* Swedish National Grid    */

#define PE_PCS_VOIROL_N_ALGERIE_ANCIENNE  30491 /* Nord Algerie ancienne      */
#define PE_PCS_VOIROL_S_ALGERIE_ANCIENNE  30492 /* Nord Algerie ancienne      */
#define PE_PCS_VOIROL_UNIFIE_N_ALGERIE    30591 /* Nord Algerie               */
#define PE_PCS_VOIROL_UNIFIE_S_ALGERIE    30592 /* Nord Algerie               */
#define PE_PCS_ATF_NORD_DE_GUERRE         27500 /* Nord de Guerre             */
#define PE_PCS_NTF_FRANCE_I               27581 /* France I                   */
#define PE_PCS_NTF_FRANCE_II              27582 /* France II                  */
#define PE_PCS_NTF_FRANCE_III             27583 /* France III                 */
#define PE_PCS_NTF_FRANCE_IV              27584 /* France IV                  */
#define PE_PCS_NTF_NORD_FRANCE            27591 /* Nord France                */
#define PE_PCS_NTF_CENTRE_FRANCE          27592 /* Centre France              */
#define PE_PCS_NTF_SUD_FRANCE             27593 /* Sud France                 */
#define PE_PCS_NTF_CORSE                  27594 /* Corse                      */
#define PE_PCS_KALIANPUR_INDIA_0          24370 /* India Zone 0               */
#define PE_PCS_KALIANPUR_INDIA_I          24371 /* India Zone I               */
#define PE_PCS_KALIANPUR_INDIA_IIA        24372 /* India Zone IIa             */
#define PE_PCS_KALIANPUR_INDIA_IIB        24382 /* India Zone IIb             */
#define PE_PCS_KALIANPUR_INDIA_IIIA       24373 /* India Zone IIIa            */
#define PE_PCS_KALIANPUR_INDIA_IIIB       24383 /* India Zone IIIb            */
#define PE_PCS_KALIANPUR_INDIA_IVA        24374 /* India Zone IVa             */
#define PE_PCS_KALIANPUR_INDIA_IVB        24384 /* India Zone IVb             */
#define PE_PCS_JAMAICA_1875_OLD_GRID      24100 /* Jamaica 1875 Old Grid      */
#define PE_PCS_JAD_1969_JAMAICA_GRID      24200 /* Jamaica Grid               */
#define PE_PCS_MERCHICH_NORD_MAROC        26191 /* Nord Maroc                 */
#define PE_PCS_MERCHICH_SUD_MAROC         26192 /* Sud Maroc                  */
#define PE_PCS_MERCHICH_SAHARA            26193 /* Sahara                     */
#define PE_PCS_CARTHAGE_NORD_TUNISIE      22391 /* Nord Tunisie               */
#define PE_PCS_CARTHAGE_SUD_TUNISIE       22392 /* Sud Tunisie                */
#define PE_PCS_KOC_LAMBERT                24600 /* Kuwait Oil Co - Lambert    */
#define PE_PCS_BELGE_LAMBERT_1950         21500 /* Belge Lambert 1950         */
#define PE_PCS_DEALUL_PISCULUI_1933_STEREO_33  31600 /* Stereo 1933           */
#define PE_PCS_DEALUL_PISCULUI_1970_STEREO_70  31700 /* Stereo 1970           */

/*----------------------------------------------------------------------------*/
/*                               M E T H O D S                                */
/*----------------------------------------------------------------------------*/
#define PE_MTH_LONGITUDE_ROTATION      9601 /* Longitude Rotation             */
#define PE_MTH_GEOCENTRIC_TRANSLATION  9603 /* Geocentric Translation (3-par.)*/
#define PE_MTH_MOLODENSKY              9604 /* Molodensky                     */
#define PE_MTH_MOLODENSKY_ABRIDGED     9605 /* Abridged Molodensky            */
#define PE_MTH_POSITION_VECTOR         9606 /* Position Vector (7-par.)       */
#define PE_MTH_COORDINATE_FRAME        9607 /* Coordinate Frame (7-par.)      */
#define PE_MTH_BURSA_WOLF             (9607+33000) /* Bursa-Wolf              */
#define PE_MTH_NADCON                  9613 /* NADCON                         */
#define PE_MTH_HARN                  109613 /* HARN (HPGN)                    */

/*----------------------------------------------------------------------------*/
/*            G E O G R A P H I C   T R A N S F O R M A T I O N S             */
/*----------------------------------------------------------------------------*/
#define PE_GT_AMERSFOORT_TO_WGS_1984        8012 /* Netherlands               */
#define PE_GT_ETRS_1989_TO_WGS_1984         8049 /* Europe                    */
#define PE_GT_GDA_1994_TO_WGS_1984          8050 /* Australia                 */
#define PE_GT_ED_1987_TO_WGS_1984_1         8137 /* North Sea south of 62     */
                                                 /* deg N (UK,Denmark,Germany,*/
                                                 /* Norway) and Netherlands   */
                                                 /* (offshore)                */
#define PE_GT_ED_1950_TO_ED_1987_2          8138 /* Norway (offshore north of */
                                                 /* 65 deg N)                 */
#define PE_GT_WGS_1972_TO_WGS_1984_1        8140 /* World                     */
#define PE_GT_WGS_1972_TO_WGS_1984_2        8141 /* World                     */
#define PE_GT_AGD_1984_TO_WGS_1984_2        8139 /* Australia                 */

#define PE_GT_ADINDAN_TO_WGS_1984_1         8000 /* Mean for Ethiopia and     */
                                                 /* Sudan                     */
#define PE_GT_ADINDAN_TO_WGS_1984_2         8001 /* Burkina Faso              */
#define PE_GT_ADINDAN_TO_WGS_1984_3         8002 /* Cameroon                  */
#define PE_GT_ADINDAN_TO_WGS_1984_4         8003 /* Ethiopia                  */
#define PE_GT_ADINDAN_TO_WGS_1984_5         8004 /* Mali                      */
#define PE_GT_ADINDAN_TO_WGS_1984_6         8005 /* Senegal                   */
#define PE_GT_ADINDAN_TO_WGS_1984_7         8006 /* Sudan                     */
#define PE_GT_AFGOOYE_TO_WGS_1984           8007 /* Somalia                   */
#define PE_GT_AGD_1966_TO_WGS_1984          8008 /* Australia                 */
#define PE_GT_AGD_1984_TO_WGS_1984_1        8009 /* Australia                 */
#define PE_GT_AIN_EL_ABD_TO_WGS_1984_1      8010 /* Bahrain                   */
#define PE_GT_AIN_EL_ABD_TO_WGS_1984_2      8011 /* Saudi Arabia              */
#define PE_GT_ARC_1950_TO_WGS_1984_1        8013 /* Mean for Botswana, Malawi,*/
                                                 /* Swaziland, Zaire, Zambia, */
                                                 /* and Zimbabwe              */
#define PE_GT_ARC_1950_TO_WGS_1984_2        8014 /* Botswana                  */
#define PE_GT_ARC_1950_TO_WGS_1984_3        8015 /* Burundi                   */
#define PE_GT_ARC_1950_TO_WGS_1984_4        8016 /* Lesotho                   */
#define PE_GT_ARC_1950_TO_WGS_1984_5        8017 /* Malawi                    */
#define PE_GT_ARC_1950_TO_WGS_1984_6        8018 /* Swaziland                 */
#define PE_GT_ARC_1950_TO_WGS_1984_7        8019 /* Zaire                     */
#define PE_GT_ARC_1950_TO_WGS_1984_8        8020 /* Zambia                    */
#define PE_GT_ARC_1950_TO_WGS_1984_9        8021 /* Zimbabwe                  */
#define PE_GT_ARC_1960_TO_WGS_1984          8022 /* Mean for Kenya and        */
                                                 /* Tanzania                  */
#define PE_GT_BATAVIA_TO_WGS_1984           8023 /* Indonesia (Sumatra)       */
#define PE_GT_BERMUDA_1957_TO_WGS_1984      8024 /* Bermuda                   */
#define PE_GT_BOGOTA_TO_WGS_1984            8025 /* Columbia                  */
#define PE_GT_BUKIT_RIMPAH_TO_WGS_1984      8026 /* Indonesia (Bangka and     */
                                                 /* Belitung Islands          */
#define PE_GT_CAMPO_INCHAUSPE_TO_WGS_1984   8027 /* Argentina                 */
#define PE_GT_CAPE_TO_WGS_1984_1            8028 /* South Africa              */
#define PE_GT_CAPE_TO_WGS_1984_2            8029 /* South Africa              */
#define PE_GT_CARTHAGE_TO_WGS_1984          8030 /* Tunisia                   */
#define PE_GT_CHUA_TO_WGS_1984              8031 /* Paraguay                  */
#define PE_GT_CORREGO_ALEGRE_TO_WGS_1984    8032 /* Brazil                    */
#define PE_GT_ED_1950_TO_WGS_1984_1         8033 /* Mean for Austria, Belgium,*/
                                                 /* Denmark, Finland, France, */
                                                 /* Germany (West), Gibraltar,*/
                                                 /* Greece, Italy, Luxembourg,*/
                                                 /* Netherlands, Norway,      */
                                                 /* Spain, Sweden,            */
                                                 /* Switzerland, and Portugal */
#define PE_GT_ED_1950_TO_WGS_1984_2         8034 /* Mean for Austria, Denmark,*/
                                                 /* France, Germany (West),   */
                                                 /* Netherlands,              */
                                                 /* and Switzerland           */
#define PE_GT_ED_1950_TO_WGS_1984_3         8035 /* Mean for Iraq, Israel,    */
                                                 /* Jordan, Kuwait, Lebanon,  */
                                                 /* Saudi Arabia, and Syria   */
#define PE_GT_ED_1950_TO_WGS_1984_4         8036 /* Cyprus                    */
#define PE_GT_ED_1950_TO_WGS_1984_5         8037 /* Egypt                     */
#define PE_GT_ED_1950_TO_WGS_1984_6         8038 /* Ireland, United Kingdom   */
#define PE_GT_ED_1950_TO_WGS_1984_7         8039 /* Finland, Norway           */
#define PE_GT_ED_1950_TO_WGS_1984_8         8040 /* Greece                    */
#define PE_GT_ED_1950_TO_WGS_1984_9         8041 /* Iran                      */
#define PE_GT_ED_1950_TO_WGS_1984_10        8042 /* Italy (Sardinia)          */
#define PE_GT_ED_1950_TO_WGS_1984_11        8043 /* Italy (Sicily)            */
#define PE_GT_ED_1950_TO_WGS_1984_12        8044 /* Malta                     */
#define PE_GT_ED_1950_TO_WGS_1984_13        8045 /* Portugal, Spain           */
#define PE_GT_ED_1950_TO_WGS_1984_14        8148 /* Tunisia                   */
#define PE_GT_EGYPT_1907_TO_WGS_1984        8048 /* Egypt                     */
#define PE_GT_GGRS_1987_TO_WGS_1984         8181 /* Greek GRS                 */
#define PE_GT_HUNGARIAN_1972_TO_ETRS_1989_1 8182 /* Hungarian to ETRS 1989    */
#define PE_GT_NZGD_1949_TO_WGS_1984         8051 /* New Zealand               */
#define PE_GT_HU_TZU_SHAN_TO_WGS_1984       8052 /* Taiwan                    */
#define PE_GT_INDIAN_1954_TO_WGS_1984       8053 /* Thailand, Vietnam         */
#define PE_GT_INDIAN_1975_TO_WGS_1984       8054 /* Thailand                  */
#define PE_GT_KALIANPUR_TO_WGS_1984_1       8055 /* Bangladesh                */
#define PE_GT_KALIANPUR_TO_WGS_1984_2       8056 /* Indian, Nepal             */
#define PE_GT_KALIANPUR_TO_WGS_1984_3       8150 /* Pakistan                  */
#define PE_GT_KANDAWALA_TO_WGS_1984         8057 /* Sri Lanka                 */
#define PE_GT_KERTAU_TO_WGS_1984            8058 /* West Malaysia, Singapore  */
#define PE_GT_LEIGON_TO_WGS_1984            8059 /* Ghana                     */
#define PE_GT_LIBERIA_1964_TO_WGS_1984      8060 /* Liberia                   */
#define PE_GT_LUZON_1911_TO_WGS_1984_1      8061 /* Philippines (excluding    */
                                                 /* Mindanao)                 */
#define PE_GT_LUZON_1911_TO_WGS_1984_2      8062 /* Philippines (Mindanao)    */
#define PE_GT_MPORALOKO_TO_WGS_1984         8063 /* Gabon                     */
#define PE_GT_MAHE_1971_TO_WGS_1984         8064 /* Mahe Island               */
#define PE_GT_MASSAWA_TO_WGS_1984           8065 /* Ethiopia (Eritrea)        */
#define PE_GT_MERCHICH_TO_WGS_1984          8066 /* Morocco                   */
#define PE_GT_MINNA_TO_WGS_1984_1           8067 /* Cameroon                  */
#define PE_GT_MINNA_TO_WGS_1984_2           8068 /* Nigeria                   */
#define PE_GT_MONTE_MARIO_TO_WGS_1984       8069 /* Italy (Sardinia)          */
#define PE_GT_NAD_1927_TO_WGS_1984_1        8070 /* Mean for Antigua, Barbados*/
                                                 /* Barbuda, Caicos Islands,  */
                                                 /* Cuba, Dominican Republic, */
                                                 /* Grand Cayman, Jamaica,    */
                                                 /* and Turks Islands         */
#define PE_GT_NAD_1927_TO_WGS_1984_2        8071 /* Mean for Belize,          */
                                                 /* Costa Rica, El Salvador,  */
                                                 /* Guatemala, Honduras,      */
                                                 /* and Nicaragua             */
#define PE_GT_NAD_1927_TO_WGS_1984_3        8072 /* Mean for Canada           */
#define PE_GT_NAD_1927_TO_WGS_1984_4        8073 /* Mean for United States    */
                                                 /* (CONUS)                   */
#define PE_GT_NAD_1927_TO_WGS_1984_5        8074 /* Mean for United States    */
                                                 /* (CONUS East of Mississippi*/
                                                 /* River including MN, MO,   */
                                                 /* and LA)                   */
#define PE_GT_NAD_1927_TO_WGS_1984_6        8075 /* Mean for United States    */
                                                 /* (CONUS West of Mississippi*/
                                                 /* River)                    */
#define PE_GT_NAD_1927_TO_WGS_1984_7        8076 /* United States (Alaska)    */
#define PE_GT_NAD_1927_TO_WGS_1984_8        8077 /* Bahamas (except San       */
                                                 /* Salvador Island)          */
#define PE_GT_NAD_1927_TO_WGS_1984_9        8078 /* Bahamas (San Salvador     */
                                                 /* Island)                   */
#define PE_GT_NAD_1927_TO_WGS_1984_10       8079 /* Canada (Alberta, British  */
                                                 /* Columbia)                 */
#define PE_GT_NAD_1927_TO_WGS_1984_11       8080 /* Canada (Manitoba, Ontario)*/
#define PE_GT_NAD_1927_TO_WGS_1984_12       8081 /* Canada (New Brunswick,    */
                                                 /* Newfoundland, Nova Scotia,*/
                                                 /* and Quebec)               */
#define PE_GT_NAD_1927_TO_WGS_1984_13       8082 /* Canada (Northwest         */
                                                 /* Territories, Saskatchewan)*/
#define PE_GT_NAD_1927_TO_WGS_1984_14       8083 /* Canada (Yukon)            */
#define PE_GT_NAD_1927_TO_WGS_1984_15       8084 /* Panama (Canal Zone)       */
#define PE_GT_NAD_1927_TO_WGS_1984_16       8085 /* Cuba                      */
#define PE_GT_NAD_1927_TO_WGS_1984_17       8086 /* Greenland (Hayes          */
                                                 /* Peninsula)                */
#define PE_GT_NAD_1927_TO_WGS_1984_18       8087 /* Mexico                    */
#define PE_GT_NAD_1927_TO_WGS_1984_21       8152 /* United States (Alaska -   */
                                                 /* Aleutians East of 180E)   */
#define PE_GT_NAD_1927_TO_WGS_1984_22       8153 /* United States (Alaska -   */
                                                 /* Aleutians West of 180E)   */
#define PE_GT_NAD_1983_TO_WGS_1984_1        8088 /* Canada, Central America,  */
                                                 /* Mexico, and United States */
                                                 /* (Alaska, CONUS)           */
#define PE_GT_NAD_1983_TO_WGS_1984_2        8154 /* United States (Alaska -   */
                                                 /* Aleutians)                */
#define PE_GT_NAD_1983_TO_WGS_1984_3        8155 /* United States (Hawaii)    */
#define PE_GT_NAHRWAN_1967_TO_WGS_1984_1    8089 /* Oman (Nasirah Island)     */
#define PE_GT_NAHRWAN_1967_TO_WGS_1984_2    8090 /* Saudi Arabia              */
#define PE_GT_NAHRWAN_1967_TO_WGS_1984_3    8091 /* United Arab Emirates      */
#define PE_GT_NAPARIMA_1972_TO_WGS_1984     8092 /* Trinidad & Tobago         */
#define PE_GT_NTF_TO_WGS_1984               8093 /* France                    */
#define PE_GT_OSGB_1936_TO_WGS_1984_1       8095 /* Mean for UK (England,     */
                                                 /* Scotland, Wales, and      */
                                                 /* Isle of Man)              */
#define PE_GT_OSGB_1936_TO_WGS_1984_2       8096 /* UK (England)              */
#define PE_GT_OSGB_1936_TO_WGS_1984_3       8097 /* UK (England, Wales, and   */
                                                 /* Isle of Man)              */
#define PE_GT_OSGB_1936_TO_WGS_1984_4       8098 /* UK (Scotland, including   */
                                                 /* Shetland Islands)         */
#define PE_GT_OSGB_1936_TO_WGS_1984_5       8099 /* UK (Wales)                */
#define PE_GT_POINTE_NOIRE_TO_WGS_1984      8100 /* Congo                     */
#define PE_GT_PSAD_1956_TO_WGS_1984_1       8101 /* Mean for Bolivia, Chile,  */
                                                 /* Colombia, Ecuador, Guyana,*/
                                                 /* Peru, and Venezuela       */
#define PE_GT_PSAD_1956_TO_WGS_1984_2       8102 /* Bolivia                   */
#define PE_GT_PSAD_1956_TO_WGS_1984_3       8103 /* Chile (Northern, near     */
                                                 /* 19 deg S                  */
#define PE_GT_PSAD_1956_TO_WGS_1984_4       8104 /* Chile (Southern, near     */
                                                 /* 43 deg S                  */
#define PE_GT_PSAD_1956_TO_WGS_1984_5       8105 /* Colombia                  */
#define PE_GT_PSAD_1956_TO_WGS_1984_6       8106 /* Ecuador                   */
#define PE_GT_PSAD_1956_TO_WGS_1984_7       8107 /* Guyana                    */
#define PE_GT_PSAD_1956_TO_WGS_1984_8       8108 /* Peru                      */
#define PE_GT_PSAD_1956_TO_WGS_1984_9       8109 /* Venezuela                 */
#define PE_GT_QATAR_TO_WGS_1984             8110 /* Qatar                     */
#define PE_GT_QORNOQ_TO_WGS_1984            8111 /* Greenland (South)         */
#define PE_GT_SAD_1969_TO_WGS_1984_1        8112 /* Mean for Argentina,       */
                                                 /* Bolivia, Brazil, Chile,   */
                                                 /* Colombia, Ecuador, Guyana,*/
                                                 /* Paraguay, Peru, Trinidad &*/
                                                 /* Tobago, and Venezuela     */
#define PE_GT_SAD_1969_TO_WGS_1984_2        8113 /* Argentina                 */
#define PE_GT_SAD_1969_TO_WGS_1984_3        8114 /* Bolivia                   */
#define PE_GT_SAD_1969_TO_WGS_1984_4        8115 /* Brazil                    */
#define PE_GT_SAD_1969_TO_WGS_1984_5        8116 /* Chile                     */
#define PE_GT_SAD_1969_TO_WGS_1984_6        8117 /* Colombia                  */
#define PE_GT_SAD_1969_TO_WGS_1984_7        8118 /* Ecuador                   */
#define PE_GT_SAD_1969_TO_WGS_1984_8        8119 /* Ecuador (Baltra,          */
                                                 /* Galapagos)                */
#define PE_GT_SAD_1969_TO_WGS_1984_9        8120 /* Guyana                    */
#define PE_GT_SAD_1969_TO_WGS_1984_10       8121 /* Paraguay                  */
#define PE_GT_SAD_1969_TO_WGS_1984_11       8122 /* Peru                      */
#define PE_GT_SAD_1969_TO_WGS_1984_12       8123 /* Trinidad & Tobago         */
#define PE_GT_SAD_1969_TO_WGS_1984_13       8124 /* Venezuela                 */
#define PE_GT_SAPPER_HILL_1943_TO_WGS_1984  8125 /* Falkland Islands (East    */
                                                 /* Falkland Island)          */
#define PE_GT_SCHWARZECK_TO_WGS_1984        8126 /* Namibia                   */
#define PE_GT_SCHWARZECK_TO_WGS_1984_2      8180 /* Namibia                   */
#define PE_GT_TANANARIVE_1925_TO_WGS_1984   8127 /* Madagascar                */
#define PE_GT_TIMBALAI_1948_TO_WGS_1984     8128 /* Brunei, Malaysia (Sabah,  */
                                                 /* Sarawak)                  */
#define PE_GT_TM65_TO_WGS_1984              8129 /* Ireland                   */
#define PE_GT_TOKYO_TO_WGS_1984_1           8130 /* Mean for Japan, Korea,    */
                                                 /* and Okinawa               */
#define PE_GT_TOKYO_TO_WGS_1984_2           8131 /* Japan                     */
#define PE_GT_TOKYO_TO_WGS_1984_3           8132 /* Korea                     */
#define PE_GT_TOKYO_TO_WGS_1984_4           8133 /* Okinawa                   */
#define PE_GT_YACARE_TO_WGS_1984            8134 /* Uruguay                   */
#define PE_GT_ZANDERIJ_TO_WGS_1984          8135 /* Suriname                  */
#define PE_GT_HERAT_NORTH_TO_WGS_1984       8149 /* Afghanistan               */
#define PE_GT_INDONESIAN_1974_TO_WGS_1984   8151 /* Indonesia                 */
#define PE_GT_NORD_SAHARA_1959_TO_WGS_1984  8156 /* Algeria                   */
#define PE_GT_PULKOVO_1942_TO_WGS_1984      8157 /* Russia                    */
#define PE_GT_VOIROL_UNIFIE_1960_TO_WGS_1984  8158 /* Algeria                 */
#define PE_GT_FAHUD_TO_WGS_1984             8159 /* Oman                      */

/* -------------------------------------------------------------------------- */
/*                  Transforms by Longitude Rotation Method                   */
/*              Prime Meridians of (non-Greenwich) to Greenwich               */
/* -------------------------------------------------------------------------- */

#define PE_GT_BATAVIA_JAKARTA_TO_BATAVIA       8172 /* Batavia (Jakarta) to   */
                                                    /* Batavia                */
#define PE_GT_BELGE_1950_BRUSSELS_TO_BELGE_1950  8168
                                                    /* Belge 1950(Brussels) to*/
                                                    /* to Belge 1950          */
#define PE_GT_BERN_1898_BERN_TO_BERN_1898      8161 /* Bern 1898 (Bern) to    */
                                                    /* Bern 1898              */
#define PE_GT_BOGOTA_BOGOTA_TO_BOGOTA          8162 /* Bogota (Bogota) to     */
                                                    /* Bogota                 */
#define PE_GT_GREEK_ATHENS_TO_GREEK            8179 /* Greek(Athens) to Greek */
#define PE_GT_LISBON_LISBON_TO_LISBON          8163 /* Lisbon (Lisbon) to     */
                                                    /* Lisbon                 */
#define PE_GT_MAKASSAR_JAKARTA_TO_MAKASSAR     8164 /* Makassar (Jakarta) to  */
                                                    /* Makassar               */
#define PE_GT_MGI_FERRO_TO_MGI                 8165 /* MGI (Ferro) to MGI     */
#define PE_GT_MONTE_MARIO_ROME_TO_MONTE_MARIO  8166 /* Monte Mario (Rome) to  */
                                                    /* Monte Mario            */
#define PE_GT_NTF_PARIS_TO_NTF                 8160 /* NTF (Paris) to NTF     */
#define PE_GT_PADANG_1884_JAKARTA_TO_PADANG_1884  8167
                                                    /* Padang 1884(Jakarta) to*/
                                                    /* to Padang 1884         */
#define PE_GT_RT38_STOCKHOLM_TO_RT38           8173 /* RT38 (Stockholm) to    */
                                                    /* RT38                   */
#define PE_GT_TANANARIVE_1925_PARIS_TO_TANANARIVE_1925  8169
                                                /* Tananarive 1925 (Paris) to */
                                                /* Tananarive 1925            */
#define PE_GT_VOIROL_1875_PARIS_TO_VOIROL_1875 8170 /* Voirol 1875 (Paris) to */
                                                    /* Voirol 1875            */
#define PE_GT_VOIROL_UNIFIE_1960_PARIS_TO_VOIROL_UNIFIE_1960  8171
                                             /* Voirol Unifie 1960 (Paris) to */
                                             /* Voirol Unifie 1960            */

/* -------------------------------------------------------------------------- */
/*                  Transforms by NADCON/HARN Grid Method                     */
/* -------------------------------------------------------------------------- */

/* NAD27 - NAD83 grid-based transformations */

#define PE_GT_NAD_1927_TO_NAD_1983_NADCON 108001 /* NAD27 to NAD83 - CONUS    */
#define PE_GT_NAD_1927_TO_NAD_1983_AK     108002 /* NAD27 to NAD83 - Alaska   */
#define PE_GT_NAD_1927_TO_NAD_1983_PRVI   108003 /* NAD27 to NAD83 -          */
                                                 /*   Puerto Rico-Virgin I.   */
/* Old non-NAD27 - NAD83 grid-based transformations */
#define PE_GT_OLD_HAWAIIAN_TO_NAD_1983    108004 /* Old Hawaiian to NAD83     */
#define PE_GT_ST_GEORGE_TO_NAD_1983       108005 /* St. George I. to NAD83    */
#define PE_GT_ST_LAWRENCE_TO_NAD_1983     108006 /* St. Lawrence I. to NAD83  */
#define PE_GT_ST_PAUL_TO_NAD_1983         108007 /* St. Paul I. to NAD83      */

/* HARN - HPGN grid-based transformations */

#define PE_GT_NAD_1983_TO_HARN_AL  108101 /* Alabama HARN                     */
#define PE_GT_NAD_1983_TO_HARN_AZ  108102 /* Arizona HARN                     */
#define PE_GT_NAD_1983_TO_HARN_CN  108103 /* California North HARN -above 36N */
#define PE_GT_NAD_1983_TO_HARN_CS  108104 /* California South HARN -below 37N */
#define PE_GT_NAD_1983_TO_HARN_CO  108105 /* Colorado HARN                    */
#define PE_GT_NAD_1983_TO_HARN_GA  108106 /* Georgia HARN                     */
#define PE_GT_NAD_1983_TO_HARN_FL  108107 /* Florida HARN                     */
#define PE_GT_NAD_1983_TO_HARN_KS  108108 /* Kansas HARN                      */
#define PE_GT_NAD_1983_TO_HARN_KY  108109 /* Kentucky HARN                    */
#define PE_GT_NAD_1983_TO_HARN_LA  108110 /* Louisiana HARN                   */
#define PE_GT_NAD_1983_TO_HARN_MD  108111 /* Maryland & Delaware HARN         */
#define PE_GT_NAD_1983_TO_HARN_ME  108112 /* Maine HARN                       */
#define PE_GT_NAD_1983_TO_HARN_MI  108113 /* Michigan HARN                    */
#define PE_GT_NAD_1983_TO_HARN_MS  108114 /* Mississippi HARN                 */
#define PE_GT_NAD_1983_TO_HARN_EM  108115 /* Idaho & Montana HARN - E of 113W */
#define PE_GT_NAD_1983_TO_HARN_WM  108116 /* Idaho & Montana HARN - W of 113W */
#define PE_GT_NAD_1983_TO_HARN_NB  108117 /* Nebraska HARN                    */
#define PE_GT_NAD_1983_TO_HARN_NV  108118 /* Nevada HARN                      */
#define PE_GT_NAD_1983_TO_HARN_NE  108119 /* New England -CT,MA,NH,RI,VT HARN */
#define PE_GT_NAD_1983_TO_HARN_NM  108120 /* New Mexico HARN                  */
#define PE_GT_NAD_1983_TO_HARN_OH  108121 /* Ohio HARN                        */
#define PE_GT_NAD_1983_TO_HARN_OK  108122 /* Oklahoma HARN                    */
#define PE_GT_NAD_1983_TO_HARN_PV  108123 /* Puerto Rico & Virgin Islnds HARN */
#define PE_GT_NAD_1983_TO_HARN_TN  108124 /* Tennessee HARN                   */
#define PE_GT_NAD_1983_TO_HARN_ET  108125 /* Texas HARN - E of 100W           */
#define PE_GT_NAD_1983_TO_HARN_WT  108126 /* Texas HARN - W of 100W           */
#define PE_GT_NAD_1983_TO_HARN_VA  108127 /* Virginia HARN                    */
#define PE_GT_NAD_1983_TO_HARN_UT  108128 /* Virginia HARN                    */
#define PE_GT_NAD_1983_TO_HARN_WO  108129 /* Washington & Oregon HARN         */
#define PE_GT_NAD_1983_TO_HARN_WV  108130 /* West Virginia HARN               */
#define PE_GT_NAD_1983_TO_HARN_WI  108131 /* Wisconsin HARN                   */
#define PE_GT_NAD_1983_TO_HARN_WY  108132 /* Wyoming HARN                     */

/*----------------------------------------------------------------------------*/
/* Symbolic names for array sph[] indexes:                                    */
/*----------------------------------------------------------------------------*/
#define PE_SPH_A   0 /* Semimajor axis                                        */
#define PE_SPH_E2  1 /* Eccentricity squared                                  */

/*----------------------------------------------------------------------------*/
/* Symbolic names for array parm[] indexes:                                   */
/*    Projections                                                             */
/*----------------------------------------------------------------------------*/
#define PE_PARM_X0     0 /* False easting                                     */
#define PE_PARM_Y0     1 /* False northing                                    */

#define PE_PARM_LAM0   2 /* Central meridian, Longitude of origin             */
#define PE_PARM_LON0   2 /* = PE_PARM_LAM0                                    */
#define PE_PARM_PHI0   6 /* Central parallel, Latitude of origin              */
#define PE_PARM_LAT0   6 /* = PE_PARM_PHI0                                    */

#define PE_PARM_PHI1   3 /* Standard parallel 1, Latitude of 1st point        */
#define PE_PARM_LAT1   3 /* = PE_PARM_PHI1                                    */
#define PE_PARM_LAM1   8 /* Longitude of 1st point                            */
#define PE_PARM_LON1   8 /* = PE_PARM_LAM1                                    */

#define PE_PARM_PHI2   4 /* Standard parallel 2, Latitude of 2nd point        */
#define PE_PARM_LAT2   4 /* = PE_PARM_PHI2                                    */
#define PE_PARM_LAM2   9 /* Longitude of 2nd point                            */
#define PE_PARM_LON2   9 /* = PE_PARM_LAM2                                    */

#define PE_PARM_K0     5 /* Scale factor                                      */
#define PE_PARM_ALPHA  7 /* Azimuth                                           */

/*----------------------------------------------------------------------------*/
/* Symbolic names for array coord[] indexes:                                  */
/*----------------------------------------------------------------------------*/
#define PE_COORD_LAM   0 /* Longitude                                         */
#define PE_COORD_LON   0 /* = PE_COORD_LAM                                    */

#define PE_COORD_PHI   1 /* Latitude                                          */
#define PE_COORD_LAT   1 /* = PE_COORD_PHI                                    */

#define PE_COORD_X     0 /* X-coordinate                                      */
#define PE_COORD_Y     1 /* Y-coordinate                                      */

#define PE_COORD_H     2 /* Height                                            */

/*----------------------------------------------------------------------------*/
/* Symbolic names for array parm[] indexes:                                   */
/*   Geographic Transforms                                                    */
/*----------------------------------------------------------------------------*/
#define PE_PARM_DX  0 /* X-axis translation                                   */
#define PE_PARM_DY  1 /* Y-axis translation                                   */
#define PE_PARM_DZ  2 /* Z-axis translation                                   */
#define PE_PARM_RX  3 /* X-axis rotation                                      */
#define PE_PARM_RY  4 /* Y-axis rotation                                      */
#define PE_PARM_RZ  5 /* Z-axis rotation                                      */
#define PE_PARM_DS  6 /* Scale difference                                     */
#define PE_PARM_ND 15 /* Name of Dataset for Grid based Geog transforms       */

/*----------------------------------------------------------------------------*/

#define PE_EPS   3.55271367880050092935562E-15  /* 2^(-48) */
#define PE_EPSS  0.000000059604644775390625     /* 2^(-24) */
#define PE_PI    3.14159265358979323846264
#define PE_PI2   1.57079632679489661923132
#define PE_PI4   0.785398163397448309615661

#define PE_SPHERE(e2) ((e2) < PE_EPS)
#define PE_ABS(a)     (((a) < 0) ? -(a) : (a))
#define PE_SGN(a,b)   (((b) >= 0) ? PE_ABS(a) : -PE_ABS(a))
#define PE_EQ(a,b)    (PE_ABS((a)-(b)) <= PE_EPS*(1+(PE_ABS(a)+PE_ABS(b))/2))
#define PE_ZERO(a)    (PE_ABS(a) <= PE_EPS)

/*----------------------------------------------------------------------------*/
/* Symbolic names for kinds of a horizon:                                     */
/*----------------------------------------------------------------------------*/
#define PE_HORIZON_RECT  0
#define PE_HORIZON_POLY  1


#endif
