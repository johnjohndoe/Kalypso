#if !defined(AFX_MODATACONNECTION_H__CC25FE65_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MODATACONNECTION_H__CC25FE65_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht ändern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre Änderungen überschrieben.


// Dispatch-Schnittstellen, auf die von dieser Schnittstelle verwiesen wird
class CMoGeoDataset;

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoDataConnection 

class CMoDataConnection : public COleDispatchDriver
{
public:
	CMoDataConnection() {}		// Ruft den Standardkonstruktor für COleDispatchDriver auf
	CMoDataConnection(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoDataConnection(const CMoDataConnection& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	BOOL GetConnected();
	void SetConnected(BOOL);
	CString GetDatabase();
	void SetDatabase(LPCTSTR);
	CString GetServer();
	void SetServer(LPCTSTR);
	CString GetUser();
	void SetUser(LPCTSTR);
	CString GetPassword();
	void SetPassword(LPCTSTR);
	LPDISPATCH GetGeoDatasets();
	void SetGeoDatasets(LPDISPATCH);
	long GetConnection();
	void SetConnection(long);
	long GetConnectError();
	void SetConnectError(long);
	long GetExtendedError();
	void SetExtendedError(long);
	CString GetExtendedErrorString();
	void SetExtendedErrorString(LPCTSTR);

// Operationen
public:
	BOOL Connect();
	void Disconnect();
	CMoGeoDataset FindGeoDataset(LPCTSTR Name);
	CMoGeoDataset AddGeoDataset(LPCTSTR Name, long shapeType, LPDISPATCH TableDesc, const VARIANT& HasZ, const VARIANT& HasMeasure);
	BOOL DeleteGeoDataset(LPCTSTR Name);
	LPDISPATCH FindCoordinateSystem(LPCTSTR Name);
	void ClearConnectError();
	LPDISPATCH FindArcInfoCoordinateSystem(LPCTSTR Name);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_MODATACONNECTION_H__CC25FE65_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
