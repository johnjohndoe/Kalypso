/* 
	Copyright 1989-1996 XVT Software. All rights reserved.
	May be used freely by licensed and registered users of XVT.
	May be distributed in source form only when embedded in an XVT 
	user's application.

	url.h - header that defines all default resources (dialogs, menus,
           about box, accelerators, ...) and resource file preamble.

	$Revision: 1.4 $
*/

#ifndef XVT_INCL_URL
#define XVT_INCL_URL

/* System specific resource file preamble */


#define NO_INCLUDES
#scan "xvt.h"

/* Include specified language file. */
#if   defined(LANG_AFR_MRMN)    /* Afrikaans Mac */
#include "uafrmrmn.h"

#elif defined(LANG_ALB_MRMN)    /* Albanian Mac */        
#include "ualbmrmn.h"

#elif defined(LANG_AMH_MRMN)    /* Amharic Mac */            
#include "uamhmrmn.h"

#elif defined(LANG_ARA_MARA)    /* Arabic bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_ARM_MRMN)    /* Armenian Mac */
#include "uarmmrmn.h"
#elif defined(LANG_ARM_MCE)     /* Armenian Mac */
#include "uarmmce.h"

#elif defined(LANG_ASM_MRMN)    /* Assamese Mac */
#include "uasmmrmn.h"

#elif defined(LANG_AZE_MARA)    /* Azerbaijani bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_BAH_MRMN)    /* Bahasa Indonesia bidirect Mac */
#include "ubahmrmn.h"

#elif defined(LANG_BEL_MCYR)    /* Belorussian Mac */
#include "ubelmcyr.h"

#elif defined(LANG_BEN_MRMN)    /* Bengali Mac */
#include "ubenmrmn.h"

#elif defined(LANG_BIH_MRMN)    /* Bihari Mac */
#include "ubihmrmn.h"

#elif defined(LANG_BUL_MCYR)    /* Bulgarian Mac */
#include "ubulmcyr.h"

#elif defined(LANG_BUR_MRMN)    /* Burmese Mac */
#include "uburmrmn.h"

#elif defined(LANG_CAT_MRMN)    /* Catalan Mac */
#include "ucatmrmn.h"

#elif defined(LANG_CHE_MRMN)    /* Chewa Mac */
#include "uchemrmn.h"

#elif defined(LANG_CHI_MCHI)    /* Chinese Mac */
#include "uchimchi.h"

#elif defined(LANG_CHU_MRMN)    /* Chuang Mac */
#include "uchumrmn.h"

#elif defined(LANG_CRO_MRMN)    /* Croatian Mac */
#include "ucromrmn.h"
#elif defined(LANG_CRO_MCRO)    /* Croatian Mac */
#include "ucromcro.h"

#elif defined(LANG_CZE_MRMN)    /* Czech Mac */
#include "uczemrmn.h"

#elif defined(LANG_DAN_MRMN)    /* Danish Mac */
#include "udanmrmn.h"

#elif defined(LANG_DAR_MARA)    /* Dari Persian bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_DUT_MRMN)    /* Dutch Mac */
#include "udutmrmn.h"

#elif defined(LANG_DZO_MRMN)    /* Dzongkha Mac */
#include "udzomrmn.h"

#elif defined(LANG_EST_MRMN)    /* Estonian Mac */
#include "uestmrmn.h"

#elif defined(LANG_EWE_MRMN)    /* Ewe Mac */
#include "uewemrmn.h"

#elif defined(LANG_FAR_MARA)    /* Farsi bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_FIJ_MRMN)    /* Fijian Mac */
#include "ufijmrmn.h"

#elif defined(LANG_FIN_MRMN)    /* Finnish Mac */
#include "ufinmrmn.h"

#elif defined(LANG_FLE_MRMN)    /* Flemish Mac */
#include "uflemrmn.h"

#elif defined(LANG_FRE_MRMN)    /* French Mac */
#include "ufremrmn.h"
#elif defined(LANG_FRE_IS1)     /* French Motif */
#include "ufreis1.h"
#elif defined(LANG_FRE_D850)    /* French DOS */
#include "ufred850.h"
#elif defined(LANG_FRE_W52)     /* French Windows */
#include "ufrew52.h"

#elif defined(LANG_FUL_MRMN)    /* Fulani Mac */
#include "ufulmrmn.h"

#elif defined(LANG_GAL_MRMN)    /* Galla Mac */
#include "ugalmrmn.h"

#elif defined(LANG_GEO_MRMN)    /* Georgian Mac */
#include "ugeomrmn.h"

#elif defined(LANG_GER_IS1)     /* German Motif */
#include "ugeris1.h"
#elif defined(LANG_GER_MRMN)    /* German Mac */
#include "ugermrmn.h"
#elif defined(LANG_GER_D850)    /* German DOS */
#include "ugerd850.h"
#elif defined(LANG_GER_W52)     /* German Windows */
#include "ugerw52.h"

#elif defined(LANG_GRE_MGRE)    /* Greek Mac */
#include "ugremgre.h"

#elif defined(LANG_GRL_MRMN)    /* Greenlandic Mac */
#include "ugrlmrmn.h"

#elif defined(LANG_GUJ_MRMN)    /* Gujarati Mac */
#include "ugujmrmn.h"

#elif defined(LANG_HAU_MRMN)    /* Hausa Mac */
#include "uhaumrmn.h"

#elif defined(LANG_HEB_MHEB)    /* Hebrew bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_HIN_MRMN)    /* Hindi Mac */
#include "uhinmrmn.h"

#elif defined(LANG_IBO_MRMN)    /* Ibo Mac */
#include "uibomrmn.h"

#elif defined(LANG_ICE_MRMN)    /* Icelandic Mac */
#include "uicemrmn.h"
#elif defined(LANG_ICE_MICE)    /* Icelandic Mac */
#include "uicemice.h"

#elif defined(LANG_IRI_MRMN)    /* Irish Gaelic Mac */
#include "uirimrmn.h"

#elif defined(LANG_ITA_MRMN)    /* Italian Mac */
#include "uitamrmn.h"
#elif defined(LANG_ITA_IS1)     /* Italian Motif */
#include "uitais1.h"
#elif defined(LANG_ITA_D850)    /* Italian DOS */
#include "uitad850.h"
#elif defined(LANG_ITA_W52)     /* Italian Windows */
#include "uitaw52.h"

#elif defined (LANG_JPN_SJIS)   /* Japanese all */
#include "ujpnsjis.h"
#elif defined (LANG_JPN_UJA)    /* Japanese EUC Motif */
#include "ujpnuja.h"

#elif defined(LANG_JAV_MRMN)    /* Javanese Mac */
#include "ujavmrmn.h"

#elif defined(LANG_KAN_MRMN)    /* Kanarese Mac */
#include "ukanmrmn.h"

#elif defined(LANG_KAS_MARA)    /* Kashmiri Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_KAZ_MCYR)    /* Kazakh Mac */
#include "ukazmcyr.h"

#elif defined(LANG_KHM_MRMN)    /* Khmer Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_KIR_MCYR)    /* Kirghiz Mac */
#include "ukirmcyr.h"

#elif defined(LANG_KOR_MKCS)    /* Korean Mac */
#include "ukormkcs.h"

#elif defined(LANG_KUR_MARA)    /* Kurdish bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_LAO_MRMN)    /* Laotian Mac */

#elif defined(LANG_LAT_MRMN)    /* Latin Mac */
#include "ulatmrmn.h"

#elif defined(LANG_LTV_MRMN)    /* Latvian Mac */
#include "ultvmrmn.h"

#elif defined(LANG_LIT_MRMN)    /* Lithuanian Mac */
#include "ulitmrmn.h"

#elif defined(LANG_LUX_MRMN)    /* Luxembourgian Mac */
#include "uluxmrmn.h"

#elif defined(LANG_MAC_MRMN)    /* Macedonian Mac */
#include "umacmrmn.h"
#elif defined(LANG_MAC_MCYR)    /* Macedonian Mac */
#include "umacmcyr.h"

#elif defined(LANG_MAD_MRMN)    /* Madurese Mac */

#elif defined(LANG_MAG_MRMN)    /* Magyar Mac */
#include "umagmrmn.h"

#elif defined(LANG_MLG_MRMN)    /* Malagasy Mac */
#include "umlgmrmn.h"

#elif defined(LANG_MLY_MARA)    /* Malay bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_MLM_MRMN)    /* Malayalam Mac */
#include "umlmmrmn.h"

#elif defined(LANG_MLD_MRMN)    /* Maldivian Mac */
#include "umacmrmn.h"

#elif defined(LANG_MLT_MRMN)    /* Maltese Mac */
#include "umltmrmn.h"

#elif defined(LANG_MAO_MRMN)    /* Maori Mac */
#include "umaomrmn.h"

#elif defined(LANG_MAR_MRMN)    /* Marathi Mac */
#include "umarmrmn.h"

#elif defined(LANG_MOL_MCYR)    /* Moldavian Mac */
#include "umolmcyr.h"

#elif defined(LANG_MON_MRMN)    /* Monegasque Mac */
#include "umonmrmn.h"

#elif defined(LANG_MNG_MCYR)    /* Mongolian top/bot Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_NAU_MRMN)    /* Nauruan Mac */
#include "unaumrmn.h"

#elif defined(LANG_NEP_MRMN)    /* Nepali Mac */
#include "unepmrmn.h"

#elif defined(LANG_NOR_MRMN)    /* Norwegian Mac */
#include "unormrmn.h"
#elif defined(LANG_NOR_IS1)     /* Norwegian Motif */
#include "unoris1.h"
#elif defined(LANG_NOR_D865)    /* Norwegian DOS */
#include "unord865.h"
#elif defined(LANG_NOR_W52)     /* Norwegian Windows */
#include "unorw52.h"

#elif defined(LANG_ORI_MRMN)    /* Oriya Mac */
#include "uorimrmn.h"

#elif defined(LANG_PAS_MARA)    /* Pashto bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_PID_MRMN)    /* Pidgin Mac */
#include "upidmrmn.h"

#elif defined(LANG_POL_MRMN)    /* Polish Mac */
#include "upolmrmn.h"

#elif defined(LANG_POR_MRMN)    /* Portuguese Mac */
#include "upormrmn.h"

#elif defined(LANG_PUN_MRMN)    /* Punjabi Mac */
#include "upunmrmn.h"

#elif defined(LANG_ROM_MRMN)    /* Romanian Mac */
#include "urommrmn.h"

#elif defined(LANG_RMH_MRMN)    /* Romansch Mac */
#include "urmhmrmn.h"

#elif defined(LANG_RUA_MRMN)    /* Ruanda Mac */
#include "uruamrmn.h"

#elif defined(LANG_RUN_MRMN)    /* Rundi Mac */
#include "urunmrmn.h"

#elif defined(LANG_RUS_MCYR)    /* Russian Mac */
#include "urusmcyr.h"
#elif defined(LANG_RUS_IS1)     /* Russian Motif */
#include "urusis1.h"
#elif defined(LANG_RUS_D866)    /* Russian DOS */
#include "urusd866.h"
#elif defined(LANG_RUS_W51)     /* Russian Windows */
#include "urusw51.h"

#elif defined(LANG_SAM_MRMN)    /* Sami Mac */
#include "usammrmn.h"

#elif defined(LANG_SMN_MRMN)    /* Samoan Mac */
#include "usmnmrmn.h"

#elif defined(LANG_SAN_MRMN)    /* Sango Mac */
#include "usanmrmn.h"

#elif defined(LANG_SNK_MTHA)    /* Sanskrit Mac */
#include "usnkmtha.h"

#elif defined(LANG_SER_MCYR)    /* Serbian Mac */
#include "usermcyr.h"

#elif defined(LANG_SES_MRMN)    /* Sesotho Mac */
#include "usesmrmn.h"

#elif defined(LANG_SET_MRMN)    /* Setswana Mac */
#include "usetmrmn.h"

#elif defined(LANG_SHO_MRMN)    /* Shona Mac */
#include "ushomrmn.h"

#elif defined(LANG_SND_MARA)    /* Sindhi bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_SNH_MRMN)    /* Sinhalese Mac */
#include "usnhmrmn.h"

#elif defined(LANG_SLO_MRMN)    /* Slovak Mac */
#include "uslomrmn.h"

#elif defined(LANG_SLN_MRMN)    /* Slovenian Mac */
#include "uslnmrmn.h"

#elif defined(LANG_SOM_MRMN)    /* Somali Mac */
#include "usommrmn.h"

#elif defined(LANG_SPA_IS1)     /* Spanish Motif */
#include "uspais1.h"
#elif defined(LANG_SPA_MRMN)    /* Spanish Mac */
#include "uspamrmn.h"
#elif defined(LANG_SPA_D850)    /* Spanish DOS */
#include "uspad850.h"
#elif defined(LANG_SPA_W52)     /* Spanish Windows */
#include "uspaw52.h"

#elif defined(LANG_SUN_MRMN)    /* Sundanese Mac */
#include "usunmrmn.h"

#elif defined(LANG_SWA_MRMN)    /* Swahili Mac */
#include "uswamrmn.h"

#elif defined(LANG_SWZ_MRMN)    /* Swazi Mac */
#include "uswzmrmn.h"

#elif defined(LANG_SWE_MRMN)    /* Swedish Mac */
#include "uswemrmn.h"
#elif defined(LANG_SWE_IS1)     /* Swedish Motif */
#include "usweis1.h"
#elif defined(LANG_SWE_D850)    /* Swedish DOS */
#include "uswed850.h"
#elif defined(LANG_SWE_W52)     /* Swedish Windows */
#include "uswew52.h"

#elif defined(LANG_TAD_MCYR)    /* Tadzhik Mac */
#include "utadmcyr.h"

#elif defined(LANG_TAG_MRMN)    /* Tagalog Mac */
#include "utagmrmn.h"

#elif defined(LANG_TAK_MRMN)    /* Taki-Taki Mac */
#include "utakmrmn.h"

#elif defined(LANG_TAM_MRMN)    /* Tamil Mac */
#include "utammrmn.h"

#elif defined(LANG_TEL_MRMN)    /* Telugu Mac */
#include "utelmrmn.h"

#elif defined(LANG_THA_MTHA)    /* Thai Mac */
#include "uthamtha.h"

#elif defined(LANG_TIB_MRMN)    /* Tibetan Mac */
#include "utibmrmn.h"

#elif defined(LANG_TIG_MRMN)    /* Tigre Mac */
#include "utigmrmn.h"

#elif defined(LANG_TGR_MRMN)    /* Tigrinya Mac */
#include "utgrmrmn.h"

#elif defined(LANG_TON_MRMN)    /* Tongan Mac */
#include "utonmrmn.h"

#elif defined(LANG_TSW_MRMN)    /* Tswana Mac */
#include "utswmrmn.h"

#elif defined(LANG_TUR_MRMN)    /* Turkish Mac */
#include "uturmrmn.h"
#elif defined(LANG_TUR_MTUR)    /* Turkish Mac */
#include "uturmtur.h"

#elif defined(LANG_TRK_MCYR)    /* Turkmen Mac */
#include "utrkmcyr.h"

#elif defined(LANG_TUV_MRMN)    /* Tuvaluan Mac */
#include "utuvmrmn.h"

#elif defined(LANG_UKR_MCYR)    /* Ukrainian Mac */
#include "uukrmcyr.h"

#elif defined(LANG_URD_MARA)    /* Urdu bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_UZB_MCYR)    /* Uzbek Mac */
#include "uuzbmcyr.h"

#elif defined(LANG_VEN_MRMN)    /* Venda Mac */
#include "uvenmrmn.h"

#elif defined(LANG_VIE_MRMN)    /* Vietnamese Mac */
#include "uviemrmn.h"

#elif defined(LANG_XHO_MRMN)    /* Xhosa Mac */
#include "uxhomrmn.h"

#elif defined(LANG_YID_MHEB)    /* Yiddish bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_YOR_MYOR)    /* Yoruba Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_ZUL_MZUL)    /* Zulu Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#else
#include "uengasc.h"            /* Default to English/ASCII */
#endif

#include "xvt_strs.h"           /* shared string resources for all PTKs */
#include "url_plat.h"
#include "xvt_help.url"


/* reset UNITS to default state */
Units Pixels

#endif
