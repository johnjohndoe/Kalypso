/***************************************************
					Headerdatei zu CONFIGURATION.CPP

  Hilfsfunktionen zum Auslesen der
  Konfigurationseinstellungen in der WSPWIN.INI

****************************************************/



#ifndef _CONFIGURATION_H_
#define _CONFIGURATION_H_

// Defines
#define CONF_INI_FILE_NAME    TEXT("WSPWIN.INI")

#define CONF_SECTION_NAME     TEXT("WSPWIN")

#define CONF_KEY_SORT_DIR     TEXT("SORTRICHTUNG")
#define CONF_VAL_VORWAERTS    TEXT("VORWAERTS")
#define CONF_VAL_RUECKWAERTS  TEXT("RUECKWAERTS")

#define CONF_KEY_VERZEIGT     TEXT("VERZWEIGSORT")
#define CONF_VAL_AUTO         TEXT("AUTO")
#define CONF_VAL_HAND         TEXT("HAND")     


BOOL GetSortStrangVorwaerts(); // Aufnehm.cpp, wspd136.cpp, wspw120
BOOL GetSortVerzweigt(); // ebenso



#endif _CONFIGURATION_H_