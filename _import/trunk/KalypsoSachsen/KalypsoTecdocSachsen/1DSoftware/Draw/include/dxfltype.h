// DXFLType.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DXFLTYPE_H
#define DXFLTYPE_H

class CDXFZeichnung;

class CDXFLType : public CObject
{
public:
	friend ostream& operator<<(ostream& os, CDXFLType &lt);
	friend istream& operator>>(istream& is, CDXFLType &lt);
	CDXFLType(CDXFZeichnung* pOwner = NULL);
	~CDXFLType();

	void GetBeschreibung(CString& str) { str = m_beschreibung; }
	void GetName(CString& name) { name = m_name; }
	CDXFZeichnung* GetOwner() { return m_pOwner; }
	void Set(CString& name, CString& bes, int num, double laenge);
	void SetStrich(CArray<double, double>& sl);

protected:
	CDXFZeichnung* m_pOwner;
	CString m_name;
	CString m_beschreibung;
	int m_nNum;
	double m_dMusterLaenge;
	CArray<double, double> m_strichlaenge;
};

#endif // DXFLTYPE_H