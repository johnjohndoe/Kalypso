////////////
// rauh.h //
////////////

#ifndef _RAUH_H_INCLUDED_
#define _RAUH_H_INCLUDED_

struct plot
{
  char laenge [20];
  char hoehe [20];
  char name [100];
  char ursprungsname[100];
};

void lese_datenbank_in_slist(char *db_pfadn); // wspw136
void schreibe_datenbank(char *db_pfadptr); // wspw136
void parameter_aus_selected_bew(char *selected2, char *bemerkung2,char *wert1, char *wert2, char *wert3); // wspw136
void schreibe_input_plot_datei(int); // wspd162, wspm001
void starte_plot_programm(int); // dito
int wahl(void); // dito


int masstab_holen(void); // wspd162, d218


#endif _RAUH_H_INCLUDED_