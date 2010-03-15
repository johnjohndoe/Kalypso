////////////
// read.h //
/////////////

#ifndef _READ_H_INCLUDED_
#define _READ_H_INCLUDED_

int read_start_berechnung(void); // wspd203, wspm001
void read_anfang_ende(void); // qwert, wspd136, d200, d215, d305, d307, w122
int name_doppelt_profile_schon_da(void); // wspm001
void menue_alles_sperren(void); // wspm001, wspw122
void menue_alles_oeffnen(void); // dito
int prof_in_slist2(char auswahl_dateien[256]); // aufnehm, wspm001
void zaehler_aus_ber_lesen(void); // wspm001
void start_lwa2b(void); // wspm001

#endif _READ_H_INCLUDED_