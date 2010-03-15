// Layer.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef LAYER_H
#define LAYER_H

class CLayer : public CObject
{
	DECLARE_SERIAL(CLayer);

protected:
    CLayer(); // soll nur beim Dynamischen Kreieren benutzt werden
public:
	CLayer( const CString& strBaseDirectory );
	CLayer( const CString& strBaseDirectory, const LPDISPATCH pDispatch, const BOOL bAutoRelease = TRUE );
	CLayer( const CString& strBaseDirectory, const CLayer& dispatchSrc, const BOOL bAutoRelease = TRUE);
	~CLayer();

  virtual void Serialize(CArchive& ar);
  virtual void SerializeProperties( CArchive& arM, BOOL bOnlyProps ) {};

  virtual BOOL ShowPropertyDialog( CMoMap& pMap, CWnd* pWnd = NULL ) { return FALSE; };
  
  void LoadProperties( const CString& path, CMoMap& map );
  void SaveProperties( const CString& path );

	enum LayerType
	{
    image,       // Rasterdaten
		profilLines,
    profilPoints,
    trennflaechen,
    durchst_bereiche,
    modellgrenzen,
    bordvoll,
		waterLines,
    user_RO,      // unveränderbare (Map)Layer (z.B.: importierte ArcInfoCovers)
    user_RW,      // veränderbare (Map)Layer (z.B.: neu-erzeugte Layer)
    waterLevel,
    hmo,          // Ansicht eines (als HMO vorliegenden) Digitalen Höhenmodels
    buhnen,
    festpunkte,    // Profilnullpunkte
    waterLevelC,   // Kopien von waterLevel's zur Bearbeitung
    flussachse    // die Flussachse
	}; // LayerType

  LPDISPATCH GetDispatch() { return m_dispatchDriver.m_lpDispatch; }
  
  virtual BOOL SetGeoDatasetName(const CString& path) {	m_strGeoDatasetName = path; return TRUE;};
  virtual const CString GetGeoDatasetName() { return m_strGeoDatasetName; };
  virtual const CString GetFullGeoDatasetName();
  virtual void SetBaseDirectory( const CString& strBaseDirectory ) { m_strBaseDirectory = strBaseDirectory; };
  const CString& GetBaseDirectory() const { return m_strBaseDirectory; };

  const BOOL LoadIsValid() { return m_strGeoDatasetName != ""; };
  
  void SetType( LayerType type ) { m_nType = type; }
  LayerType GetType() { return m_nType; }

	virtual CString GetName();
	virtual void SetName(LPCTSTR);
	virtual CMoRectangle GetExtent();
	virtual void SetExtent(LPDISPATCH);
	virtual BOOL GetVisible();
	virtual void SetVisible(BOOL);
	virtual long GetLayerType();
	virtual CString GetTag();
	virtual void SetTag(LPCTSTR);
	virtual BOOL GetValid();
	virtual void SetValid(BOOL);

	COleDispatchDriver m_dispatchDriver;

protected:
	CString m_strGeoDatasetName;
  CString m_strBaseDirectory;
	LayerType m_nType;
};

#endif // LAYER_H