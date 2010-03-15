// MapDoc.h : Schnittstelle der Klasse CMapDoc
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPDOC_H__27A882CB_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_)
#define AFX_MAPDOC_H__27A882CB_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_


#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "layer.h"
#include "mapdocdata.h"
#include "mapLayer.h"
#include "mapDocListener.h"
#include "profilauswahl.h"

class CLayerData;
class CMapProperties;
class CMapLayout;
class CZTable;
class CImageLayer;
class CProfilAuswahl;
struct NMPROJECTMNG;
class CProfilModel;

namespace BCE
{
  class Hmo;
};

class CMapDoc : public CDocument
{
  typedef CMap<CLayer*, CLayer*, CUIntArray*, CUIntArray*> UpdateProjectData;
  typedef std::set<IMapDocListener*> ListenerSet;

public:
  class cancel_action {}; // Exception für 'Abbrechen' bei mehreren Aktionen

protected: // Nur aus Serialisierung erzeugen
	CMapDoc();
	DECLARE_DYNCREATE(CMapDoc)
	virtual void DeleteContents();

// Operationen
public:
  std::vector<bool> AddProfiles( CrossSectionArray* cSections, StatesArray* states );
  CMapLayer* CreateMapLayer( const CLayer::LayerType typ, const CString& strFileSuffix );
  void RemoveProfiles( const CStringArray& fileNames );
  BOOL SaveProfileModifications( CrossSectionArray* cSections );
  void UpdateWSPVernetzung();
  void AddLayer( CLayer* layer, const bool bRefreshViews );
	void RemoveLayer( CLayer* layer, const bool bRefreshViews );
	CMapLayer* GetMapLayer(LPDISPATCH lpDispatch);
	CImageLayer* GetImageLayer(LPDISPATCH lpDispatch);
	CLayer* GetLayer(int i);
	int GetLayerCount();
	void EditLayerProperties(CLayer *layer);
  CLayer* GetActiveLayer() { return m_pActiveLayer; }
	void SetActiveLayer(LPDISPATCH layer);
	short GetMapUnit() { return m_nMapUnit; }
	void SetMapUnit(short n) { m_nMapUnit = n; }
	short GetScaleUnit() { return m_nScaleUnit; }
	void SetScaleUnit(short n);
  int  GetScale() { return m_nScale; }
	void SetScale(int n) { m_nScale = n; }
	short GetScreenUnit() { return m_nScreenUnit; }
	void SetScreenUnit(short n);
  CLayer* GetOverviewLayer() { return m_pOverviewLayer; }
  CLayerArray* GetLayers() { return &m_layers; };
  BOOL GetShowOverview() { return m_bShowOverview; };
  void SetShowOverview( BOOL b );
  void SetOverview( LPDISPATCH layer );
	void SetExtent(CMoRectangle extent);
	CMoRectangle GetExtent();
  CMoRectangle GetFullExtent();
	int GetLayerIndex(CLayer *layer);
  void SetProject( Project* project );
  CLayerData* GetLayerData( CLayer::LayerType type );

  CMapLayout* GetMapLayout();

  CMapProperties* GetMapProperties( ) { return m_mapProperties; };
  BOOL SafeMapProperties( LPCTSTR filePath );
  BOOL LoadMapProperties( LPCTSTR filePath );
  BOOL ParseCommand( NMPROJECTMNG* command );
  void GetWindowState();
  void MoveObjectsOutwards( CMapLayer* layer );
  BOOL CutThemeToProfilLines( CMapLayer* mapLayer, const CLayer::LayerType type, const CString& feature, 
                              const CString& constraint, const BOOL bDeleteTheme, 
                              const CString& reli = TEXT("RE") );
  BOOL CutThemeToProfilPoints( CMapLayer* mapLayer, const CString& feature, CZTable* zTable );
  
  BOOL ExtendProfile( CMoLine& line, const int profilID );

  void RemoveDataFromMap( const CArray<BOOL, BOOL>& removeIDs );

  void EditProfile( const long profilID );

  BOOL TinCutLayer( CMapLayer* wspLayer );
  void RecalcProfileDistances( CMapLayer* lineLayer );
  int CreateRiverLine();
  CMoGeoEvent MarkObject( LPDISPATCH dispObj, const CLayer::LayerType& type );
  void GenerateProfile( CMoLine profilLine, CMapLayer* hmoLayer, bool bReportnoPoints, int* pLastCutAnswer, State* state, const double* station, const CString& comment );
  void GenerateProfiles( CMapLayer* pMapLayer );

  void CopyLayer( CLayer* pLayer );

  CProfilModel* ReadProfil( const int profilID );
  BOOLEAN CreateHmoFromWsp( CMapLayer* wspLayer, const CString& hmoName, const CString& shapeName );

  void SetFullExtent();

private:
  void ReadAsTrenntype( CMapLayer* layer, const CString& sqlExpr, CProfilModel* model, const int type );

  // Publisher Pattern
public:
  void AddMapDocListener( IMapDocListener* listener );
  void RemoveMapDocListener( IMapDocListener* listener );
  void FireMapDocChanged( const long type, CLayer* layer );

private:
  ListenerSet m_listeners;

public:
  // Attribute
  CMoMap* GetMap();
  CString GetMapPath() const { return m_mapDir; };

  WSPVernetzungsInfo* GetProfileNet() { return m_netz; };
  void SetProfileNet( WSPVernetzungsInfo* netz );

  void GetDataBlockTypes( CArray<int, int>& dbArray  );
  void GetProfileNames( CStringArray& profilNames, CStringArray& stateNames );

  void SetProfilAuswahl( CProfilAuswahl* profilAuswahl ) { m_profilAuswahl = profilAuswahl; };
  CProfilAuswahl* GetProfilAuswahl() { return m_profilAuswahl; };

  CMoRectangle GetStartExtent() const { return m_extent; };


  // Actives Profil
public:
  void SetActiveProfile( const long featureID, const bool bRefreshViews );
  long GetActiveProfile() const { return m_activeProfileID; };

  bool IsShowActiveProfile() const { return m_fShowActiveProfile; };
  void SetShowActiveProfile( bool bShow ) { m_fShowActiveProfile = bShow; };

  double GetProfileCursor() const { return m_profilCursorPos; };
  void SetProfilCursor( const double cursorPos );

  CProfilModel* GetActiveProfilModel();

private:
  long m_activeProfileID; 
  bool m_fShowActiveProfile;
  double m_profilCursorPos;

protected:
  CMapLayer* CreateAndAddLayer( const CLayer::LayerType& typ, const CString& strFileNameSuffix, const bool bRefreshViews );
  BOOL GenerateProfile( CMoLine& river, CMoLine& line, CMapLayer* hmoLayer, bool bReportNoPoints, int* pLastCutAnswer, State* state, const double* station, const int* vzk, const CString& comment );
  void FlushLayers();
  BOOL AddTrippleToProfile( TripleArray* tripArray, const int profilID, 
                            CMoPoint& startPoint, CProgressCtrl* progressCtrl = NULL );
  CMapLayer* MakeUGrenzenFromWaterLevel(CMapLayer* levelLayer);
  int ChangeProfilIDs( const std::map<long, CString>& idMap, const CString& profilFile, CProgressCtrl* progressCtrl = NULL );

  CProfilAuswahl* m_profilAuswahl;
  CMapProperties* m_mapProperties;  // Eigenschaften der Karte ( insbesondere, alle welche gespeichert werden sollen )
  CMoRectangle m_extent;
	BOOL m_bShowOverview;
  short m_nMapUnit;
  short m_nScaleUnit;
  int   m_nScale;	
  short m_nScreenUnit;
  CLayer* m_pActiveLayer;
  CLayer* m_pOverviewLayer;
  
  WSPVernetzungsInfo* m_netz; // das aktuelle netz

	CLayerArray m_layers;	// all map layers
  CString m_mapDir;

// Überladungen
	// Vom Klassenassistenten generierte Überladungen virtueller Funktionen
	//{{AFX_VIRTUAL(CMapDoc)
	public:
	virtual void Serialize(CArchive& ar);
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual BOOL SaveModified();
	virtual void OnCloseDocument();
	//}}AFX_VIRTUAL
  public:
    virtual HMENU GetDefaultMenu();
protected:
  virtual BOOL OnNewDocument();

private:
	void ShowVolumeDialog( const CString& logFileName );

// Implementierung
public:
	virtual ~CMapDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

// Generierte Message-Map-Funktionen
protected:
	//{{AFX_MSG(CMapDoc)
	afx_msg void OnViewOverview();
	afx_msg void OnUpdateViewOverview(CCmdUI* pCmdUI);
	//}}AFX_MSG

  afx_msg void OnSafeDefaultProperties();

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio fügt zusätzliche Deklarationen unmittelbar vor der vorhergehenden Zeile ein.

#endif // !defined(AFX_MAPDOC_H__27A882CB_3E7F_11D3_A4B9_0080ADAC5D6B__INCLUDED_)
