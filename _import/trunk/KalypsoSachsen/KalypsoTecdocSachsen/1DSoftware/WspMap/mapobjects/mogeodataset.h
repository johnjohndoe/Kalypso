#if !defined(AFX_MOGEODATASET_H__CC25FE47_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOGEODATASET_H__CC25FE47_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoGeoDataset 

class CMoGeoDataset : public COleDispatchDriver
{
public:
	CMoGeoDataset() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoGeoDataset(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoGeoDataset(const CMoGeoDataset& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	CString GetName();
	void SetName(LPCTSTR);
	BOOL GetAllowSharing();
	void SetAllowSharing(BOOL);
	BOOL GetHasZ();
	void SetHasZ(BOOL);
	BOOL GetHasMeasure();
	void SetHasMeasure(BOOL);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MOGEODATASET_H__CC25FE47_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
