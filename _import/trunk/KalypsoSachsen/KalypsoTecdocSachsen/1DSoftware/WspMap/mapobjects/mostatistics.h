#if !defined(AFX_MOSTATISTICS_H__CC25FE51_21E0_11D6_B2A0_00104BB3E525__INCLUDED_)
#define AFX_MOSTATISTICS_H__CC25FE51_21E0_11D6_B2A0_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// Mit Microsoft Visual C++ automatisch erstellte IDispatch-Kapselungsklasse(n). 

// HINWEIS: Die Inhalte dieser Datei nicht �ndern. Wenn Microsoft Visual C++
// diese Klasse erneuert, werden Ihre �nderungen �berschrieben.

/////////////////////////////////////////////////////////////////////////////
// Wrapper-Klasse CMoStatistics 

class CMoStatistics : public COleDispatchDriver
{
public:
	CMoStatistics() {}		// Ruft den Standardkonstruktor f�r COleDispatchDriver auf
	CMoStatistics(LPDISPATCH pDispatch) : COleDispatchDriver(pDispatch) {}
	CMoStatistics(const CMoStatistics& dispatchSrc) : COleDispatchDriver(dispatchSrc) {}

// Attribute
public:
	double GetMax();
	void SetMax(double);
	double GetMin();
	void SetMin(double);
	double GetMean();
	void SetMean(double);
	double GetStdDev();
	void SetStdDev(double);
	double GetSum();
	void SetSum(double);
	long GetCount();
	void SetCount(long);

// Operationen
public:
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ f�gt unmittelbar vor der vorhergehenden Zeile zus�tzliche Deklarationen ein.

#endif // AFX_MOSTATISTICS_H__CC25FE51_21E0_11D6_B2A0_00104BB3E525__INCLUDED_
