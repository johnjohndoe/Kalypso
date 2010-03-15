// Datenbank.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef DATENBANK_H
#define DATENBANK_H

class Datenbank : public CObject
{
public:
	 Datenbank(int type);
	 ~Datenbank();

	 int GetType();
	 void SetType(int type);

	 void GetName(int i, CString& name);
	 void SetName(int i, CString& name);
	 double GetVar1(int i);
	 void SetVar1(int i, double value);
     double GetVar2(int i);
	 void SetVar2(int i, double value);
     double GetVar3(int i);
	 void SetVar3(int i, double value);
     CString GetDatabankName();
     void SetDatabankName(CString databankname);
     BOOL Load();
     BOOL Save();
     int GetSize();
     void AddName(CString name);

	enum Type { ueberfallbeiwert, rauheit_ks, rauheit_kst, bewuchs };

protected:
	Type m_nType;
    CString m_DatabankName;
	CStringArray m_names;
	CArray<double, double> m_vars1;
	CArray<double, double> m_vars2;
	CArray<double, double> m_vars3;

	friend istream& operator>>(istream& is, Datenbank &dat);
	friend ostream& operator<<(ostream& os, Datenbank &dat);
};

#endif // DATENBANK_H