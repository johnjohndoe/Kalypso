'---------------------------------------------------------------------------
'   Interface file for XVT applications
'   Copyright 1993-1996 XVT Software. All rights reserved.
'
'   WARNING:  Do not use anything found in this file unless
'   documented elsewhere.
'---------------------------------------------------------------------------
'
' This is a set of "common" help topics.
' This file can simply be included in another help source file.
'
'---------------------------------------------------------------------------
' I18N - NOTES:
'		 You should not need to make any changes to this file.
'---------------------------------------------------------------------------


/* Include specified language file. */
#if   defined(LANG_AFR_MRMN)    /* Afrikaans Mac */
#include "hafrmrmn.csh"

#elif defined(LANG_ALB_MRMN)    /* Albanian Mac */        
#include "halbmrmn.csh"

#elif defined(LANG_AMH_MRMN)    /* Amharic Mac */            
#include "hamhmrmn.csh"

#elif defined(LANG_ARA_MARA)    /* Arabic bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_ARM_MRMN)    /* Armenian Mac */
#include "harmmrmn.csh"
#elif defined(LANG_ARM_MCE)     /* Armenian Mac */
#include "harmmce.csh"

#elif defined(LANG_ASM_MRMN)    /* Assamese Mac */
#include "hasmmrmn.csh"

#elif defined(LANG_AZE_MARA)    /* Azerbaijani bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_BAH_MRMN)    /* Bahasa Indonesia bidirect Mac */
#include "hbahmrmn.csh"

#elif defined(LANG_BEL_MCYR)    /* Belorussian Mac */
#include "hbelmcyr.csh"

#elif defined(LANG_BEN_MRMN)    /* Bengali Mac */
#include "hbenmrmn.csh"

#elif defined(LANG_BIH_MRMN)    /* Bihari Mac */
#include "hbihmrmn.csh"

#elif defined(LANG_BUL_MCYR)    /* Bulgarian Mac */
#include "hbulmcyr.csh"

#elif defined(LANG_BUR_MRMN)    /* Burmese Mac */
#include "hburmrmn.csh"

#elif defined(LANG_CAT_MRMN)    /* Catalán Mac */
#include "hcatmrmn.csh"

#elif defined(LANG_CHE_MRMN)    /* Chewa Mac */
#include "hchemrmn.csh"

#elif defined(LANG_CHI_MCHI)    /* Chinese Mac */
#include "hchimchi.csh"

#elif defined(LANG_CHU_MRMN)    /* Chuang Mac */
#include "hchumrmn.csh"

#elif defined(LANG_CRO_MRMN)    /* Croatian Mac */
#include "hcromrmn.csh"
#elif defined(LANG_CRO_MCRO)    /* Croatian Mac */
#include "hcromcro.csh"

#elif defined(LANG_CZE_MRMN)    /* Czech Mac */
#include "hczemrmn.csh"

#elif defined(LANG_DAN_MRMN)    /* Danish Mac */
#include "hdanmrmn.csh"

#elif defined(LANG_DAR_MARA)    /* Dari Persian bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_DUT_MRMN)    /* Dutch Mac */
#include "hdutmrmn.csh"

#elif defined(LANG_DZO_MRMN)    /* Dzongkha Mac */
#include "hdzomrmn.csh"

#elif defined(LANG_EST_MRMN)    /* Estonian Mac */
#include "hestmrmn.csh"

#elif defined(LANG_EWE_MRMN)    /* Ewé Mac */
#include "hewemrmn.csh"

#elif defined(LANG_FAR_MARA)    /* Farsi bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_FIJ_MRMN)    /* Fijian Mac */
#include "hfijmrmn.csh"

#elif defined(LANG_FIN_MRMN)    /* Finnish Mac */
#include "hfinmrmn.csh"

#elif defined(LANG_FLE_MRMN)    /* Flemish Mac */
#include "hflemrmn.csh"

#elif defined(LANG_FRE_MRMN)    /* French Mac */
#include "hfremrmn.csh"
#elif defined(LANG_FRE_IS1)     /* French Motif */
#include "hfreis1.csh"
#elif defined(LANG_FRE_D850)    /* French DOS */
#include "hfred850.csh"
#elif defined(LANG_FRE_W52)     /* French Windows */
#include "hfrew52.csh"

#elif defined(LANG_FUL_MRMN)    /* Fulani Mac */
#include "hfulmrmn.csh"

#elif defined(LANG_GAL_MRMN)    /* Galla Mac */
#include "hgalmrmn.csh"

#elif defined(LANG_GEO_MRMN)    /* Georgian Mac */
#include "hgeomrmn.csh"

#elif defined(LANG_GER_IS1)     /* German Motif */
#include "hgeris1.csh"
#elif defined(LANG_GER_MRMN)    /* German Mac */
#include "hgermrmn.csh"
#elif defined(LANG_GER_D850)    /* German DOS */
#include "hgerd850.csh"
#elif defined(LANG_GER_W52)     /* German Windows */
#include "hgerw52.csh"

#elif defined(LANG_GRE_MGRE)    /* Greek Mac */
#include "hgremgre.csh"

#elif defined(LANG_GRL_MRMN)    /* Greenlandic Mac */
#include "hgrlmrmn.csh"

#elif defined(LANG_GUJ_MRMN)    /* Gujarati Mac */
#include "hgujmrmn.csh"

#elif defined(LANG_HAU_MRMN)    /* Hausa Mac */
#include "hhaumrmn.csh"

#elif defined(LANG_HEB_MHEB)    /* Hebrew bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_HIN_MRMN)    /* Hindi Mac */
#include "hhinmrmn.csh"

#elif defined(LANG_IBO_MRMN)    /* Ibo Mac */
#include "hibomrmn.csh"

#elif defined(LANG_ICE_MRMN)    /* Icelandic Mac */
#include "hicemrmn.csh"
#elif defined(LANG_ICE_MICE)    /* Icelandic Mac */
#include "hicemice.csh"

#elif defined(LANG_IRI_MRMN)    /* Irish Gaelic Mac */
#include "hirimrmn.csh"

#elif defined(LANG_ITA_MRMN)    /* Italian Mac */
#include "hitamrmn.csh"
#elif defined(LANG_ITA_IS1)     /* Italian Motif */
#include "hitais1.csh"
#elif defined(LANG_ITA_D850)    /* Italian DOS */
#include "hitad850.csh"
#elif defined(LANG_ITA_W52)     /* Italian Windows */
#include "hitaw52.csh"

#elif defined(LANG_JPN_SJIS)    /* Japanese all */
#include "hjpnsjis.csh"
#elif defined(LANG_JPN_UJA)     /* Japanese EUC Motif */
#include "hjpnuja.csh"

#elif defined(LANG_JAV_MRMN)    /* Javanese Mac */
#include "hjavmrmn.csh"

#elif defined(LANG_KAN_MRMN)    /* Kanarese Mac */
#include "hkanmrmn.csh"

#elif defined(LANG_KAS_MARA)    /* Kashmiri Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_KAZ_MCYR)    /* Kazakh Mac */
#include "hkazmcyr.csh"

#elif defined(LANG_KHM_MRMN)    /* Khmer Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_KIR_MCYR)    /* Kirghiz Mac */
#include "hkirmcyr.csh"

#elif defined(LANG_KOR_MKCS)    /* Korean Mac */
#include "hkormkcs.csh"

#elif defined(LANG_KUR_MARA)    /* Kurdish bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_LAO_MRMN)    /* Laotian Mac */

#elif defined(LANG_LAT_MRMN)    /* Latin Mac */
#include "hlatmrmn.csh"

#elif defined(LANG_LTV_MRMN)    /* Latvian Mac */
#include "hltvmrmn.csh"

#elif defined(LANG_LIT_MRMN)    /* Lithuanian Mac */
#include "hlitmrmn.csh"

#elif defined(LANG_LUX_MRMN)    /* Luxembourgian Mac */
#include "hluxmrmn.csh"

#elif defined(LANG_MAC_MRMN)    /* Macedonian Mac */
#include "hmacmrmn.csh"
#elif defined(LANG_MAC_MCYR)    /* Macedonian Mac */
#include "hmacmcyr.csh"

#elif defined(LANG_MAD_MRMN)    /* Madurese Mac */

#elif defined(LANG_MAG_MRMN)    /* Magyar Mac */
#include "hmagmrmn.csh"

#elif defined(LANG_MLG_MRMN)    /* Malagasy Mac */
#include "hmlgmrmn.csh"

#elif defined(LANG_MLY_MARA)    /* Malay bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_MLM_MRMN)    /* Malayalam Mac */
#include "hmlmmrmn.csh"

#elif defined(LANG_MLD_MRMN)    /* Maldivian Mac */
#include "hmacmrmn.csh"

#elif defined(LANG_MLT_MRMN)    /* Maltese Mac */
#include "hmltmrmn.csh"

#elif defined(LANG_MAO_MRMN)    /* Maori Mac */
#include "hmaomrmn.csh"

#elif defined(LANG_MAR_MRMN)    /* Marathi Mac */
#include "hmarmrmn.csh"

#elif defined(LANG_MOL_MCYR)    /* Moldavian Mac */
#include "hmolmcyr.csh"

#elif defined(LANG_MON_MRMN)    /* Monégasque Mac */
#include "hmonmrmn.csh"

#elif defined(LANG_MNG_MCYR)    /* Mongolian top/bot Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_NAU_MRMN)    /* Nauruan Mac */
#include "hnaumrmn.csh"

#elif defined(LANG_NEP_MRMN)    /* Nepali Mac */
#include "hnepmrmn.csh"

#elif defined(LANG_NOR_MRMN)    /* Norwegian Mac */
#include "hnormrmn.csh"
#elif defined(LANG_NOR_IS1)     /* Norwegian Motif */
#include "hnoris1.csh"
#elif defined(LANG_NOR_D865)    /* Norwegian DOS */
#include "hnord865.csh"
#elif defined(LANG_NOR_W52)     /* Norwegian Windows */
#include "hnorw52.csh"

#elif defined(LANG_ORI_MRMN)    /* Oriya Mac */
#include "horimrmn.csh"

#elif defined(LANG_PAS_MARA)    /* Pashto bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_PID_MRMN)    /* Pidgin Mac */
#include "hpidmrmn.csh"

#elif defined(LANG_POL_MRMN)    /* Polish Mac */
#include "hpolmrmn.csh"

#elif defined(LANG_POR_MRMN)    /* Portuguese Mac */
#include "hpormrmn.csh"

#elif defined(LANG_PUN_MRMN)    /* Punjabi Mac */
#include "hpunmrmn.csh"

#elif defined(LANG_ROM_MRMN)    /* Romanian Mac */
#include "hrommrmn.csh"

#elif defined(LANG_RMH_MRMN)    /* Romansch Mac */
#include "hrmhmrmn.csh"

#elif defined(LANG_RUA_MRMN)    /* Ruanda Mac */
#include "hruamrmn.csh"

#elif defined(LANG_RUN_MRMN)    /* Rundi Mac */
#include "hrunmrmn.csh"

#elif defined(LANG_RUS_MCYR)    /* Russian Mac */
#include "hrusmcyr.csh"
#elif defined(LANG_RUS_IS1)     /* Russian Motif */
#include "hrusis1.csh"
#elif defined(LANG_RUS_D866)    /* Russian DOS */
#include "hrusd866.csh"
#elif defined(LANG_RUS_W51)     /* Russian Windows */
#include "hrusw51.csh"

#elif defined(LANG_SAM_MRMN)    /* Sami Mac */
#include "hsammrmn.csh"

#elif defined(LANG_SMN_MRMN)    /* Samoan Mac */
#include "hsmnmrmn.csh"

#elif defined(LANG_SAN_MRMN)    /* Sango Mac */
#include "hsanmrmn.csh"

#elif defined(LANG_SNK_MTHA)    /* Sanskrit Mac */
#include "hsnkmtha.csh"

#elif defined(LANG_SER_MCYR)    /* Serbian Mac */
#include "hsermcyr.csh"

#elif defined(LANG_SES_MRMN)    /* Sesotho Mac */
#include "hsesmrmn.csh"

#elif defined(LANG_SET_MRMN)    /* Setswana Mac */
#include "hsetmrmn.csh"

#elif defined(LANG_SHO_MRMN)    /* Shona Mac */
#include "hshomrmn.csh"

#elif defined(LANG_SND_MARA)    /* Sindhi bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_SNH_MRMN)    /* Sinhalese Mac */
#include "hsnhmrmn.csh"

#elif defined(LANG_SLO_MRMN)    /* Slovak Mac */
#include "hslomrmn.csh"

#elif defined(LANG_SLN_MRMN)    /* Slovenian Mac */
#include "hslnmrmn.csh"

#elif defined(LANG_SOM_MRMN)    /* Somali Mac */
#include "hsommrmn.csh"

#elif defined(LANG_SPA_IS1)     /* Spanish Motif */
#include "hspais1.csh"
#elif defined(LANG_SPA_MRMN)    /* Spanish Mac */
#include "hspamrmn.csh"
#elif defined(LANG_SPA_D850)    /* Spanish DOS */
#include "hspad850.csh"
#elif defined(LANG_SPA_W52)     /* Spanish Windows */
#include "hspaw52.csh"

#elif defined(LANG_SUN_MRMN)    /* Sundanese Mac */
#include "hsunmrmn.csh"

#elif defined(LANG_SWA_MRMN)    /* Swahili Mac */
#include "hswamrmn.csh"

#elif defined(LANG_SWZ_MRMN)    /* Swazi Mac */
#include "hswzmrmn.csh"

#elif defined(LANG_SWE_MRMN)    /* Swedish Mac */
#include "hswemrmn.csh"
#elif defined(LANG_SWE_IS1)     /* Swedish Motif */
#include "hsweis1.csh"
#elif defined(LANG_SWE_D850)    /* Swedish DOS */
#include "hswed850.csh"
#elif defined(LANG_SWE_W52)     /* Swedish Windows */
#include "hswew52.csh"

#elif defined(LANG_TAD_MCYR)    /* Tadzhik Mac */
#include "htadmcyr.csh"

#elif defined(LANG_TAG_MRMN)    /* Tagalog Mac */
#include "htagmrmn.csh"

#elif defined(LANG_TAK_MRMN)    /* Taki-Taki Mac */
#include "htakmrmn.csh"

#elif defined(LANG_TAM_MRMN)    /* Tamil Mac */
#include "htammrmn.csh"

#elif defined(LANG_TEL_MRMN)    /* Telugu Mac */
#include "htelmrmn.csh"

#elif defined(LANG_THA_MTHA)    /* Thai Mac */
#include "hthamtha.csh"

#elif defined(LANG_TIB_MRMN)    /* Tibetan Mac */
#include "htibmrmn.csh"

#elif defined(LANG_TIG_MRMN)    /* Tigre Mac */
#include "htigmrmn.csh"

#elif defined(LANG_TGR_MRMN)    /* Tigrinya Mac */
#include "htgrmrmn.csh"

#elif defined(LANG_TON_MRMN)    /* Tongan Mac */
#include "htonmrmn.csh"

#elif defined(LANG_TSW_MRMN)    /* Tswana Mac */
#include "htswmrmn.csh"

#elif defined(LANG_TUR_MRMN)    /* Turkish Mac */
#include "hturmrmn.csh"
#elif defined(LANG_TUR_MTUR)    /* Turkish Mac */
#include "hturmtur.csh"

#elif defined(LANG_TRK_MCYR)    /* Turkmen Mac */
#include "htrkmcyr.csh"

#elif defined(LANG_TUV_MRMN)    /* Tuvaluan Mac */
#include "htuvmrmn.csh"

#elif defined(LANG_UKR_MCYR)    /* Ukrainian Mac */
#include "hukrmcyr.csh"

#elif defined(LANG_URD_MARA)    /* Urdu bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_UZB_MCYR)    /* Uzbek Mac */
#include "huzbmcyr.csh"

#elif defined(LANG_VEN_MRMN)    /* Venda Mac */
#include "hvenmrmn.csh"

#elif defined(LANG_VIE_MRMN)    /* Vietnamese Mac */
#include "hviemrmn.csh"

#elif defined(LANG_XHO_MRMN)    /* Xhosa Mac */
#include "hxhomrmn.csh"

#elif defined(LANG_YID_MHEB)    /* Yiddish bidirect Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_YOR_MYOR)    /* Yoruba Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#elif defined(LANG_ZUL_MZUL)    /* Zulu Mac */
#include "LANGUAGE_NOT_SUPPORTED"

#else
#include "hengasc.csh"		/* Default to English/ASCII */
#endif

