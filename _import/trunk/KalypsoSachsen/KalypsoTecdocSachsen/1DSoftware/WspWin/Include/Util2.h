#ifndef _UTIL2_H_
#define _UTIL2_H_

struct st_daten;

void savedat(st_daten *daten);
void schreibe_bat(void);
void read_varianten (char *string);
void AddNewZeile(char str[90], double km);//Dick 16.07.99
int copy_file(char alte_datei[100], char neue_datei[100]);
void Delete_Zeile(void);
void Delete_203(void);
void loesche_alle_mit_strname(void);
void loesche_alle_in_dath(void);
void slashn_entfernen(char *string);
void pruefe_errorlog(char *which_file);
void loesche_alle_mit_log(void);
void start_wspr(void);
void Init_Steuerdaten_LWA(st_daten *daten);

int copy_fileb(char alte_datei[100], char neue_datei[100]);
void  slashn_entfernenb(char *string);


#endif // _UTIL2_H_