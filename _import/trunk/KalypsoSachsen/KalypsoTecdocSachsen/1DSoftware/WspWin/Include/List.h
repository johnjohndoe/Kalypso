/**********************************

  Version: WIN95  Stand: 24.09.1996 

***********************************/

#ifndef _LIST_H_INCLUDED_
#define _LIST_H_INCLUDED_

struct Rauheits_Klassen;

/*------------------------------------------------------------*/
class List // in flaeche.h, global.h, paint.h, global_vars.h, dis_prof.cpp, flaeche.cpp, printer.cpp,
          // wspd140, wspd144, wspd137, wspd141, wsdp142, wspd143, wspd155, wspd156, wspd157, 
          // wspd158, wspd159, wsplist
{
 protected:
  Profildatei *ptr_profil,*ptr_anfang,*ptr_ende,
				*ptr_rueck; //Bley 20.10.2000 (Spiegelbild)
  Koord *pp,*p_h,*p_t, 
	    *p_rueck; //Bley 20.10.2000 (Spiegelbild)

  Koord  *zoom_left_x,         //Zeiger auf GELAENDE-Koord. für Zoomen
			*zoom_right_x;

  WSP_SLIST *slist_dummy;
  int  dummy_typ;

public:
  List();
  ~List(void);
  Profildatei* get_anfang(void); // oft
  Koord* get_zoom_ptr(int typ); // paint.cpp
  void DeleteList(void); // oft
  void DeleteNode( int nummer, int ds_info[TYPE_SIZE], int typ[TYPE_SIZE] ); // oft
  void BauwerkansEnde( int nummer, int* ds_info, int* typ ); // readlp
  void MakeNewNode(int n1); // oft
  int  MakeWriteInfo(FILE *in); // dito
  int  ExistStation(double station,int ds_nr); // oft, laengs.dll
  int  ExistStationX(double station,int ds_nr); // nuer hier
  void MakeSaveInfo(FILE *out); // hier, laengs.dll
  void MakeNewKoord(int anzahl); // oft, laengs.dll
  void NewKoord(int ds_nummer, int Pos = -1); // oft, laengs.dll
  int  PositionSuchen(int ds_num,int pos); // hier, laengs
  void Koord_Update(int grafik); // paint, wspw116, wspw120
  void InitKoord(int ,double ); // util, waspila
  void WriteTypDaten(int nummer,int typ,char*); // oft, laengs.dll
  void CopyStation(int ds_nummer); // util, waspila, laengs.dll
  void Delete_Min_Koord(int ds_nr); // util
  void DeleteKoord(int ds); // hier, laengs.dll
  int  DeleteKoord(int pos,int ds_nummer,double x,double y); // dito
  void Sort(double x,double y,unsigned ds_not); // wspd151
  void Koord_Einf(double x,double y,int Pos,int datensatz=-1); // oft, laengs.dll
  int Koord_Einf_Sorted(double x,double y,int Pos); // hier
  int WriteX(FILE *in,int n1); // readprof, laengs.dll
  int WriteY(FILE *in,int n1); // dito

  int  GetInfoline2(int ,BRUECKE *); // oft
  int  GetInfoline2(int , int, buhne *); // oft
  void SaveInfoline2(int,int, buhne *); // oft
  void SaveInfoline2(int ds_nummer,BRUECKE *br); // oft

  int  GetInfoline2(int ,WEHR *); // oft
  void SaveInfoline2(int,WEHR *); // oft

  double GetInfoline2(int ds_nummer); // oft
  void   SaveInfoline2(int ds_nummer,double gefaelle); // oft
  int  Infoline2(int ds_nummer,char *profiltyp,BOOLEAN op); //ARMCO84 // wspd156
  int  GetSonderProfilTyp(char * ); // oft, laengs.dll
  int ReadSonderProfil(FILE *in,int typ); // readprof, laengs.dll
  void Update_Scr(int pos,Scroller *pscr); //hier, paint
  void GetScrollDaten(Scroller *pscr); // oft
  void GetDateninfo(WINDOW lwin); // oft
  void GetMinMax(MinMax *ptr,int datensatz); // oft, laengs.dll
  int  Get_Station_Num(double station,int nr); // dito
  double Get_Station_Hoehe(double station); // oft
  int Exist_Station_Hoehe(double station,double hoehe); // wspd140
  int Exist_Station_Hoehe_Allg(double station,double hoehe,int datensatz); // hier
  double Get_Num_Station(int ds_nummer,int *position,int correct); // oft
  int Get_Num_Station_Zoom(ZOOM *info,int direction); // paint, wspw116
  int Get_Station_pos(double station,int pos,int offset); // hier
  int Get_Station_pos_allg(double station,int pos,int datensatz); // hier, paint, laengs.dll
  int Get_Station_pos_num(double station,int pos,int datensatz); // hier laengs.dll

  void Change_Station(double x_new,double x_old,int anzahl,int position); // hier, laengs
  void Change_Station2(double x_new,double x_old,int anzahl,int position,int ds_nummer=-1); // hier
  void Change_Station_Allg(double x_new,double x_old,int anzahl,int position); // hier
  void SaveScrollDaten(WINDOW win,Scroller *scr,char *str,int typ); // hie, wspw116, wspw120
  void SaveSonderprofildaten(Scroller *scr,  int id); // oft
  void GetDatenInfo3(char *str, int ds); // oft
  void SaveDatenInfo3(char *str, int ds); // wspw130
  int GetProfilTyp(void); // readprof, laengs.dll

  void SaveXY(FILE *out,int n1); // readprof
  void SaveSonderProfil(FILE *out,int nr,int typ); // readprof, laengs.dll
  int	ChangeNumber(double station,double y,int ds_nr); // hier
  int ChangeNumber2(double station,double y,int ds_nr,int pos); // hier
  void Edit_Gelaende_Bereiche(int datensatz,double faktor,int typ,MMP *mmp); // wspd110, waspila
  void Set_Zoom_Marker(ZOOM *info); // paint, wspw116, wspw117
  int Get_Plot_Options(int ds,SLIST werte,int art,int *); // wspd161
  int Save_Plot_Options(int ds,SLIST werte,int art,int *); // dito

  int change_rauheiten(FILE *protokoll_file,char file[15], double links,double mitte,double rechts,int rauh_change_typ);
  bool Get_Rauheit( double* links, double* mitte, double* rechts, int* rauh_typ ); // wspd162

  int check_durch_bereiche(int ds,int action); // readprof, laengs.cpp
  int check_nwrinne(int ds); // readprof
  int check_sort(void); // readprof

  int Check_Gel1_Gel2_Daten(List*); // flaeche, volume1
  int ExistGelaende2Daten(void); // dito, wspm001
  int ExistFlaecheDaten(void); // flaeche, wspm001
  int CopyGel2ToGel1Daten(List*); // flaeche
  int CopyGelDatenToLaengspro(LAENGSPROFIL*); // flaeche
  int CopyFlaecheToProfil(LAENGSPROFIL*); // flaeche
  int SaveSummeToFlaeche(char *); // flaeche
  void GetInfolineFlaeche(S_FLAECHE*,int); // wspd153
  void GetInfolineSecGel(char*,int); // wspd163
  int CopyGelDatenToGelaende(GELAENDE*,int,int); // volume1
  int ExistDatensatzTyp(int); // oft

  int AddDummySList(char *); // hier, laengs
  int WriteDummySList(FILE *out,int typ = -1 ); // readprof, laengs
  int ReadDummySList(FILE *in,int anzahl,int typ); // dito
  void DelDummySList(); // readprof

  int ListWspLpData(int pos,double *x,double *y,int *n_vzk,double *wsp_fix); // wsplp2qp
  int ListSucheStation(double station,double abstand, int anzahl_koord, int anzahl_straenge); //Bley 9.11.2000, wsplp2qp, laengs.dll
  int InsertQpWspInLp(int nr,double wsp,char *name,char *BerVariante,int datensatz); // wsplp2qp
  int ExtractStationsString(int ds,char *str,int datensatz); // disprof
  int ExtractInfo2String(int nr,char *str);  // dis_prof, wspd166, wspw116
  int FindWspQpData(char *filename,char *BerVariante); // wsplp2qp

  int ProfilVerl(WSP_PROFIL_LISTE *pWPL,int ver); // profverl

  double hole_station(int editwindow,Scroller *pscr); // wspw116
  void schluessel_holen(int position,char *typ, char *vzkt, char *pkt); // wspw116
 
  Koord* HoleDatensatz(int typ); //Dick 22.09.98 // oft, laengs.dll
  Profildatei* HoleDatenblock( int typ );

  int ConcatDatensatz(WSP_PROFIL_LISTE *pWPL); // laengs2
  int CopyDatenL(Koord *ptr1,Koord *ptr2); // util

  void GetPlotDatenInfo3(int ds);//Dick 9.12.98 // readprof
  int WritePlotDatenInfo3(int ds);//Dick 10.12.98 // dito
  void ChangePlotDatenInfo3(int ds);//Dick 10.12.98 // readprof

  double Koord_Interpolieren(double x,int pos); // wspw116, wspw120
  int GetUGWsp(int nr,double wsp,double *ugx1,double *ugx2); // wsplp2qp
  int GetInterpKoord(double x,double *y,int datensatz); // wsplp2qp

  void Mehrfeldoptimierung(void); // wspm001, laengs.dll

  int BuildBauwerk(int anzahl); // readprof, laengs.dll

  int  num_station_in_ds(double station,int ds);
  void spiegelbild(void);

  void LeseWerteInPtr(FILE* start_file,int anzahl_punkte); // waspila
  int GetBuhnenKoord(int editwindow, Scroller *pscr); // wspw116
  int Errechne_Buhnen_Neigung(int,int, buhne*); // wspw116, wspd337
  void GaussKruegerInterpolieren(Scroller *pscr); // wspw116
  void Sort_In_Ds(double x,double,int ds_nr,int pre_x);

// aus listdll übernommene Funktionen
  void MakeSaveInfoDLL(FILE *out);
  void SetPtrAnfang( Profildatei* prf );
  void WSP_Differenz(); // identisch zu list.cpp
  int vgl_ds_nr(double station);
  void GetLPlotInfo( MinMax *pMinMax,double *abstand_hoehe,double *abstand_breite );
  int ds_check_daten(int *typ,int *ds_info,WSP_SLIST *slist_comment);

  double GetSohltiefe();
  Rauheits_Klassen GetRauheitsKlassen();
  void SetRauheitsKlassen( const Rauheits_Klassen& rK );

private:
  void DeleteStation(Rem_Station *station);
  int  Get_First_Station(double station ,int nr);
  int  num_station_leer_in_ds(double station,int ds);
  void Update_ds_info(int ds_nr);
};

#endif _LIST_H_INCLUDED_
