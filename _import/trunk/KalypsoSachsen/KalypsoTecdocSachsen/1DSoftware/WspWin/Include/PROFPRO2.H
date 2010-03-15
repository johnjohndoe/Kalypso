#ifndef _PROFPRO2_H_
#define _PROFPRO2_H_

void profil_nr_ermitteln(void); // oft
void ermittle_profile_slist(void); // wspd210
void ermittle_loesch_profile(void); // wspd135, wspm001
void test_auf_andere_zustandsdatei(void); // hier
void sortiere_profproj_neu(void); // wspd135
void profile_nach_profproj_schreiben(void); // lese_str, wspwin.cpp
void schluessel_nach_profil(char zielname[100]); // aufnehm, wspd136
void schluessel_einlesen(char namederdatei[100]); // dito
void lese_projektbezeichnung(char *text); // util2, wspd222
void schreibe_projektbezeichnung(char *text); // dito


#endif // _PROFPRO2_H_