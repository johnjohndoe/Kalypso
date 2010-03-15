// images.h

#ifndef _IMAGES_H_INCLUDED_
#define _IMAGES_H_INCLUDED_

//////////////////////////////////////////////////////////////////////////////
// Die Klasse CommonImageList stellt den Wspwin Anwendungen zwei Listen
// von 'Bildchen' zur Verfügung:
// - die ImageList: Bitmaps der Grösse 20 x 13
// - die IconList: Bitmaps der Grösse 15 x 15
//////////////////////////////////////////////////////////////////////////////
// Es gibt max zwei Instancen dieser Klasse: die ImageList und die IconList
// Um zu gewährleisten, dass es max eine Instanz jeder Sorte gibt, wurde
// auf das 'Singleton Idom' zurückgegriffen
//////////////////////////////////////////////////////////////////////////////
// Anwendungen die die CCommonImageList benutzen, können das direkt tun
// d.h. einfach durch CCommonImageList::GetImageList die Liste holen
// bzw. durch CCommonImageList::GetIconList
// Die Anwendung muss aber am Ende ( z.B. in ExitInstance ) die statische
// Funktion CCommonImageList::DestroyLists aufrufen um die Listen zu zerstören
/////////////////////////////////////////////////////////////////////////////////

// Die ImageList

#define IMAGE_PROJECT                     0
#define IMAGE_WATER                       1
#define IMAGE_STATE                       2
#define IMAGE_LSECTION                    3
#define IMAGE_CSECTION                    4
#define IMAGE_TEXT_WSPMAP                 5
#define IMAGE_MAP                         6
#define IMAGE_MAPLAYER                    7
#define IMAGE_IMLAYER                     8
#define IMAGE_FOLDER                      9
#define IMAGE_GELAENDEHOEHE               10
#define IMAGE_TRENNFLAECHEN               11
#define IMAGE_RAUHIGKEIT                  12
#define IMAGE_DURCHST_BEREICH             13
#define IMAGE_UK_BRUECKE                  14
#define IMAGE_OK_BRUECKE                  15
#define IMAGE_BEWUCHS                     16
#define IMAGE_OK_WEHRS                    17
#define IMAGE_KREISSEGM                   18
#define IMAGE_OK_GELAENDE                 19
#define IMAGE_KASTEN                      20
#define IMAGE_GAUSS                       21
#define IMAGE_RECHTSWERT                  22
#define IMAGE_HOCHWERT                    23
#define IMAGE_PUNKT_NR                    24
#define IMAGE_WSP_HOEHE                   25
#define IMAGE_WASSERSPIEGEL               26
#define IMAGE_BORDVOLL                    27
#define IMAGE_MAUL                        28
#define IMAGE_EIPROFIL                    29
#define IMAGE_KREIS                       30
#define IMAGE_TRAPEZ                      31
#define IMAGE_SOHLHOEHE                   32
#define IMAGE_LAENGE                      33
#define IMAGE_ABFLUSS                     34
#define IMAGE_WSP_BREITE                  35
#define IMAGE_BOESCHUNG_LINKS             36
#define IMAGE_BOESCHUNG_RECHTS            37
#define IMAGE_BAUWERK                     38
#define IMAGE_WSP_MESSUNG                 39
#define IMAGE_DEICH_LINKS                 40
#define IMAGE_DEICH_RECHTS                41
#define IMAGE_TEXT                        42
#define IMAGE_CALC                        43
#define IMAGE_LSECTION_SAVED              44
#define IMAGE_CSECTION_SAVED              45
#define IMAGE_SECTION_DISABLED            46
#define IMAGE_TEXT3                       47
#define IMAGE_FILLING                     48
#define IMAGE_TABELLE                     49
#define IMAGE_STEMPEL                     50
#define IMAGE_STATIONIERUNG               51
#define IMAGE_TITEL                       52
#define IMAGE_HEIGHT                      53
#define IMAGE_RAHMEN                      54

#define NIMAGES 55


// die IconList

#define ICON_PRINT                       0
#define ICON_ZOOMIN                      1
#define ICON_ZOOMOUT                     2
#define ICON_PRINT_PROPS                 3
#define ICON_KARTE                       4
#define ICON_SCALEBAR                    5
#define ICON_LOGO                        6
#define ICON_TEXT2                       7
#define ICON_LEGEND                      8
#define ICON_SELECT                      9
#define ICON_DELETE                      10
#define ICON_OBJECT_PROPS                11
#define ICON_ZORDER_BACK                 12
#define ICON_ZORDER_FRONT                13
#define ICON_CLOSE_DOC                   14

#define NICONS 15


class CCommonImageList : public CImageList
{
  // kleine Hilfsklasse, die dafür sorgt, dass die ImageLists automatisch entsorgt werden
  static class CHelpArray : public CTypedPtrArray<CPtrArray, CCommonImageList*>
  {
  public:
    CHelpArray()
    {
      SetAtGrow( 0, NULL );
      SetAtGrow( 1, NULL );
    }

    ~CHelpArray()
    {
      for( int i = 0; i < GetSize(); i++ )
        delete GetAt( i );
    }
  }; // class CHelpArray

private:
  CCommonImageList( const BOOL bIcons ); // protected, damit niemand selbst eine Instanz dieser Klasse schaffen kann

public:
  static CImageList* GetList( const BOOL bIcons );

private:
  static int commonImagesMap[NIMAGES]; // Zuordnung Bildnummer -> ResourceID
  static int commonIconsMap[NICONS];  // dito für Icons
  static CHelpArray theLists;
}; // class CommonImageList


#endif _IMAGES_H_INCLUDED_