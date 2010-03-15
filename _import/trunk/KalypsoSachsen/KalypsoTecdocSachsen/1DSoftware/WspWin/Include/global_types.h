//
// globale Typedefinitionen für wspwin
//

#ifndef _GLOBAL_TYPES_H_
#define _GLOBAL_TYPES_H_


#include "global_defs.h"



// Vorwaertsdeklarationen
class List;

class CharBuffer
{
public:
  CharBuffer( size_t size )
  {
    m_buffer = new char[size];
    m_buffer[0] = '\0';
  };
  ~CharBuffer() { delete[] m_buffer; };

  operator char*() { return m_buffer; };

private:
  char* m_buffer;
};

/*! @struct Rauheits_Klassen
 *  Kleiner Container um die drei Raheitswerte zu fassen
*/
struct Rauheits_Klassen
{
  double vorland_links, vorland_rechts, flussschlauch;
  int typ;
};


//
// aus SList.h
//

typedef struct _WSP_SLIST
{
	char *string;
	struct _WSP_SLIST *next;
}WSP_SLIST;


//
// Typen aus flaeche.h
//

typedef struct _STR_DATA
  {
	FILE_SPEC file;
	short     anzahl_profil_dat_entries,
				 anzahl_strang_entries;
	char      *str_zustand,
				 *str_gewaesser;
	struct STRANG    *ptr_anfang;
	WSP_SLIST *prof_datei;
  }STR_DATA;


typedef struct _GELAENDE
  {
	 short    index;
	 double   y;             // Y-Wert
	 double   z;             // Z-Wert bzw. Flächenwert
	 struct _GELAENDE *next_ds;      //Zeiger auf das folgende Element
	 struct _GELAENDE *pre_ds;       //Zeiger auf das vorhergehende Element
  }GELAENDE;

typedef struct _LAENGSPROFIL
	  {
		double   station;
		GELAENDE *querprof1,
				 *querprof2,
				 *flaeche;
		double   summe,
				 sum_auftrag,
				 sum_abtrag;
	  }LAENGSPROFIL;

//
// Typen aus zoom_dll.h
//

typedef struct _ZOOM
  {
	int     level;      //0= kein ZOOM , 1=ZOOM
	int     mouse_pos_left_x , //Position von mouse_down
			  mouse_pos_right_x;
	int     pos_station_min,   //Nr. der 1.Station in aktueller Instanz
			  pos_station_max;   //Nr. der letzten Station in aktueller Instanz

	int     min_old,           //Nr. der Station in alter Instanz
			  max_old;           //Nr. der letzten Station in alter Instanz

	double  station_min,     // station[m] der 1.Station in erster Instanz
			  station_max;     // station[m] der letzten Station in erster Instanz
	int     edit;            // wurde etwas geändert ?
	int     datensatz;       // Datensatz - Nr.
	int     ds_info[TYPE_SIZE];     // retten von ds_info
	int     typ[TYPE_SIZE];         // retten von typ
  }ZOOM;


//
// Typen aus list_dll.h
//

typedef struct _options
  {
	 char pre[130];
   double koord[130];
  }options;

typedef struct _Rem_Station  // enthält den Stationswert der durch editieren ungültig
  {                // geworden ist
	 int nummer;
   int typ,
     anzahl;
   double x,
			  y;
  }Rem_Station;

typedef struct _Scroller
  {
  int    scrollpos;
  int    anzahl;
  int    datensatz;
  int    z_offset;
  double x[15];
  double y[15];
  double z[15];
  double lastX; // die x Position vor dem ersten angezeigten Element, BCE_NAN, falls es das erste ist
  }Scroller;

typedef struct _MinMax
  {
  int    posMinX, posMaxX;
  int    posMinY, posMaxY;
  double minX, maxX;
  double minY, maxY;
  double distanceX, distanceY;
  double  sec_maxY;    // max y-Wert 2.gewaehlter Datensatz
  }MinMax;

typedef struct _MMP
  {
  double horizontal,  //aktuelle Maus-Position in [m]
			 vertikal,
       hor_m_down;  //Maus-Position in [m] bei E_MOUSE_DOWN
  
  int mouse_v,       //aktuelle Maus-Position
    mouse_h,
    mouse_down_v,  // Maus-Position bei E_MOUSE_DOWN
    mouse_down_h;
  int position,
    position_mouse_down,
    position_mouse_up,
    ds_nummer;
  RCT last_rct;
  BOOLEAN active;
  }MMP;

typedef struct _BRUECKE
{
  char sohle[15],
		  breite[15],
      rauheit[15],
      abflusszahl[15],//Dick 23.08.99
      beiwert[15],
      typ[15],
      neigung[15];
  
  BOOLEAN rehbock;
}BRUECKE;

typedef struct _WEHR
  {
		char typ[20],
      kote[20],
      breite[20] ,
      beiwert[20],
      hoehe[20],
      ausuferungshoehe[20];
    char wehrtyp[20],
      bwert[130];  // !!Gesamtlänge <150Zeichen
  }WEHR;

typedef struct _S_FLAECHE
  { 
  char s[15],
			 s_auf[15],
       s_ab[15];
  }S_FLAECHE;

typedef struct _Koord
  {
	 int    ds_nr;        //Datensatz Nummer
   double  x;            // x-Wert
   double  y;            // y-Wert
   unsigned short x_pnt,  // x-Wert als Grafikkoordinate
					y_pnt;  // y-Wert als Grafikkoordinate
   int    pre_x;
   int    pre_y;
   struct _Koord *next_ds;      //Zeiger auf das folgende Element
   struct _Koord *pre_ds;        //Zeiger auf das vorhergehende Element
   int status;         //Vermerke für Zoomfunktionen
   char  attr;         //->List:ds_check_daten
  }Koord;

typedef struct _Profildatei
  {
  Koord *datensatz;
  struct _Profildatei *next;
  char daten_info[3][251];        //3 Infozeilen vor Datensatz //Dick 2.12.98 auf 251
  int  ds_nummer;
  int  profiltyp;
  int status;          //Vermerke für Zoomfunktionen
  }Profildatei;


//
// Typen aus wsplist1.h
//

typedef struct _PROFILDATA
  {
	WSP_SLIST *slist_header,
				 *slist_comment;
	int	*ds_info, *typ, anzahl_ds;
	BOOLEAN   exist_plot,
				 exist_gel2,
				 exist_comment;
	FILE_SPEC file;
  }PROFILDATA;

 typedef struct _WSP_PROFIL_LISTE
  {
   short index;
   long  window;  //dient dem temp. speichern von Fensteradressen(z.B. listbutton)
   List *PList;
   void *dummy;
   struct _PROFILDATA *data;
   _WSP_PROFIL_LISTE *PListNext;
  }WSP_PROFIL_LISTE;



//
// Typen aus Strang.h
//

struct STRANG
{
  int    nummer;
	double anfang,            //Anfangsprofil  Station -km
         ende;              //Endprofil Station -km
  int    strang_vzk;        //Verzweigungskennung
  int		 strang_pk;
  char   name_anfang[15],   // Dateiname der Profildatei
         name_ende[15];
  char   abstand_links[25], // Profilabstand   vorher 11 Zeichen
         abstand_rechts[25],
         abstand_fluss[25];
  double abstand;
  STRANG* next;
};


// Typen aus laengs2.h

typedef struct _LWA_DATA1   // normale Zeile -kein Sonderprofil oder Datenblock 2
  {
	double station,
			 sohlhoehe,
			 ufer_links,
			 ufer_rechts,
			 wsp,
			 abfluss,
			 abstand,
			 wsp_breite;
	int	 ibk,
		 idp,
		 iva,
		 ivz;
   char   mfb[3];
   double mfbint;
   //Neu Datensätze 22.09.99 Dick
   double schlepp_span,
          ausufer_links,
          ausufer_rechts,
          energiehoehe;
   //Ende Neu


  }LWA_DATA1;

typedef struct _LWA_DATA2  // Zeile2 bei Sonderprofil
  {
	double station,
			 dkuk,
			 dkok;
	char   text[41],
			 kenng[5];
  } LWA_DATA2;


// Typen aus Util2.h

struct st_daten
{
  char info[60];
  double anfang,ende;
  char eich[9];
  char he[10];
  char hgralle[9];
  char normalle[10];
  char wtau[7];
  char wasser[9];
  char hoehe[21];
  char gefaelle[10];
  int sel_index;
  char q[21];//Dick 8.07.99 20->21 wegen '/0'
  char qplot[7];
  char lplot[7];
  char nasall[8];
  char nasabs[8];
  char qwv [6];
  char wqbez[7];
  char kalmin [9];
  
  int werte[13];
  char qmin[11],
		  qstep[11],
      qmax[11];
  int abfluss;
  float w_anfang;
  char wehran[8];
  int ia,ncar,nhyd,idat,nfrou,iauto;
  char epsh[10],epsv[10],rny[14],cwr[12];
  int schiess;
  int ifp, idr;
 	int ipunkt, nposey, nbeta, iform, inn;
  char sm[20], izmax[10];
  int hmo;//Dick 22.09.98
  int wsf_q;//Dick 4.02.99
  int wsf_l;//Dick 4.02.99
  //Zusatz für BCE-Version
  int u_grenze;
  int kalinin;
  //Grenzwerte //Dick 28.09.99
  double dhwmax;//max.Differenz zw.WSP
  double vfmax;//max. Fließge.
  double hzvmax;//max. örtl. Verluste
  double faklhg;//max. Abstand beim Fließwechsel als Faktor
  double ffmax;//max.Fließausschnitt
} ;


struct ZEILE
{
  double  station; //Dick 16.07.99
  char   line[90];
  ZEILE *next_zeile;      //Zeiger auf das folgende Element
};

struct ABSTAENDE
{
	 double pabstand1;
   double pabstand2;
   double pabstand3;
   ABSTAENDE *next_abstand;
};

// aus list.h

typedef struct _BUHNE      
  {
	char hoehe_links[15],
		 neig_links_rueck [15],
		 neig_links_vorne [15],
		 hoehe_rechts[15],
		 neig_rechts_rueck [15],
		 neig_rechts_vorne [15],
		 lage [5];  //links/rechts
	struct _BUHNE *next_ds;
}buhne; 

// aus QList.h

typedef struct _QSatz
{
  double  x;
  double  y;
  double  z;//Dick 2.02.99 WSFixirungen
  int ds_nr;
  int optimiert;
  struct _QSatz *next_ds;      //Zeiger auf das folgende Element
}QSatz;

typedef struct _QWertDatei
{
  QSatz *datensatz;
  struct _QWertDatei *next;
  char info[22] ;      //  Infozeile vor Datensatz Dick 8.07.99 11->21
  double hoehe;
  int  anzahl;
}QWertDatei ;

typedef struct _TABELLE
{
		int index;
    double anfang,ende;
    struct _TABELLE *next;
} TABELLE;



#endif // _GLOBAL_TYPES_H_


