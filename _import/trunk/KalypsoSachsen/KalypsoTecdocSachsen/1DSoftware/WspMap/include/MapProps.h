// MapPops.h: Schnittstelle für die Klasse CMapData.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPPROPS_H__88065F43_4861_11D5_BE27_00104BB3E525__INCLUDED_)
#define AFX_MAPPROPS_H__88065F43_4861_11D5_BE27_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


// Vorwärtsdeklarationen
class CLayerData;
class CMapLayout;

class CMapProperties : public CObject  
{
  // Konstruktion
public:
	CMapProperties();
	virtual ~CMapProperties();

  // Deklarationen
  DECLARE_SERIAL( CMapProperties );
  friend class CMapDoc;

  // Attribute
public:
  CDockState dockState;
  CRect windowRect;
  WORD windowState;

protected:
  CMapLayout* m_mapLayout; // das Layout der Karte fürs Drucken
  CTypedPtrArray<CPtrArray, CLayerData*> m_layerData;  // für jede CLayer::LayerType einen Block an typspezifischen Daten
  CMap<UINT, UINT, CRect, CRect&> barRects;

public:
  CTypedPtrArray<CPtrArray, CLayerData*>* GetLayerData() { return &m_layerData; };
  CRect GetWindowRect() const { return windowRect; };
  CRect GetWindowRect( UINT barID ) const;
  void SetWindowRect( const UINT barID, const CRect& barRect );

  CDockState* GetDockState() { return &dockState; };
  WORD GetWindowState() { return windowState; };

  CMapLayout* GetMapLayout() { return m_mapLayout; };

  // operationen
public:
  void Serialize( CArchive& ar );
  void Initialize();
  void DeleteContents();
  BOOL SafeToFile( LPCTSTR filePath );
  BOOL LoadFromFile( LPCTSTR filePath );
};

#endif // !defined(AFX_MAPPROPS_H__88065F43_4861_11D5_BE27_00104BB3E525__INCLUDED_)
