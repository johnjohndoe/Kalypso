#include "stdafx.h"

#include "commResource.h"
#include "images.h"

CCommonImageList::CHelpArray CCommonImageList::theLists;

int CCommonImageList::commonImagesMap[NIMAGES] = 
{ 
  IDB_PROJECT,
    IDB_WATER, 
    IDB_STATE,
    IDB_LSECTION,
    IDB_CSECTION,
    IDB_TEXT,
    IDB_MAP,
    IDB_MAPLAYER,
    IDB_IMLAYER,
    IDB_FOLDER,
    IDB_GELAENDEHOEHE,
    IDB_TRENNFLAECHEN,
    IDB_RAUHIGKEIT,
    IDB_DURCHST_BEREICH,
    IDB_UK_BRUECKE,
    IDB_OK_BRUECKE,
    IDB_BEWUCHS,
    IDB_OK_WEHRS,
    IDB_KREISSEGM,
    IDB_OK_GELAENDE,
    IDB_KASTEN,
    IDB_GAUSS,
    IDB_RECHTSWERT,
    IDB_HOCHWERT,
    IDB_PUNKT_NR,
    IDB_WSP_HOEHE,
    IDB_WASSERSPIEGEL,
    IDB_BORDVOLL,
    IDB_MAUL,
    IDB_EIPROFIL,
    IDB_KREIS,
    IDB_TRAPEZ,
    IDB_SOHLHOEHE,
    IDB_LAENGE,
    IDB_ABFLUSS, 
    IDB_WSP_BREITE,
    IDB_BOESCHUNG_LINKS,
    IDB_BOESCHUNG_RECHTS,
    IDB_BAUWERK, 
    IDB_WSP_MESSUNG,
    IDB_DEICH_LINKS,
    IDB_DEICH_RECHTS,
    IDB_TEXT,
    IDB_CALC,
    IDB_LSECTION_SAVED,
    IDB_CSECTION_SAVED,
    IDB_SECTION_DISABLED,
    IDB_TEXT3,
    IDB_FILLING,
    IDB_TABELLE,
    IDB_STEMPEL,
    IDB_STATION,
    IDB_TITEL,
    IDB_HEIGHT,
    IDB_RAHMEN
};

int CCommonImageList::commonIconsMap[NICONS] = 
{
  IDB_PRINT,
    IDB_ZOOMIN,
    IDB_ZOOMOUT,
    IDB_PRINT_PROPS,
    IDB_KARTE,
    IDB_SCALEBAR,
    IDB_LOGO,
    IDB_TEXT2,
    IDB_LEGEND,
    IDB_SELECT,
    IDB_DELETE,
    IDB_OBJECT_PROPS,
    IDB_ZORDER_BACK,
    IDB_ZORDER_FRONT,
    IDB_CLOSE_DOC
};


CCommonImageList::CCommonImageList( const BOOL bIcons )
// der Standardkonstruktor, lädt alle Bitmaps in die Liste
// Parameter:
//        const BOOL bIcons: falls TRUE wird die IconList erstellt
//                            sonst die ImageList
{
  // je nachdem ob die IconList erstellt werden soll die Variablen initialisieren
  int count = bIcons ? NICONS : NIMAGES;

  int* map = bIcons ? (int*)commonIconsMap : (int*)commonImagesMap;

  int width = bIcons ? 16 : 20;
  int height = bIcons ? 16 : 13;
  COLORREF colorMask = bIcons ? RGB( 198, 198, 198 ) : RGB( 255, 255, 255 );
  int flags = ILC_COLOR32 | ILC_MASK;

  CImageList::Create( width, height, flags, count, 5 );

  for( int i = 0; i < count; i++ )
	{
    CBitmap* pBmp = new CBitmap;
    pBmp->LoadBitmap( map[i] );
    Add( pBmp, colorMask );
    delete pBmp;
  }
} // Standardkonstruktor

CImageList* CCommonImageList::GetList( const BOOL bIcons )
// gibt sich selbst ( nach CImageList gecastet ) zurück
// falls das einzige Objekt dieser Klasse noch nicht existiert wird es erzeugt
// und initialisiert
// Parameter:
//      const BOOL bIcons: falls TRUE, wird die Iconslist zurückgegeben, sonst die Images
{
  int index = bIcons ? 0 : 1;
  
  CCommonImageList* pList = theLists[index];

  if( pList == NULL )
  {
    pList = new CCommonImageList( bIcons );
    theLists[index] = pList;
  };

  
  return pList;
}
