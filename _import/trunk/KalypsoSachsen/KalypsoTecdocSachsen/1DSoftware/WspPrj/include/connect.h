// Connect.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef CONNECT_H
#define CONNECT_H

class istream;
class ostream;

class Connection : public CObject
{
public:
	 Connection();
	 ~Connection();

	 double GetAnfStation();
	 double GetEndStation();
	 double GetVorlandLinks();
	 double GetFluss();
	 double GetVorlandRechts();
	 CString GetAnfProf();
	 CString GetEndProf();

	 void SetAnfStation(double x);
	 void SetEndStation(double x);
	 void SetVorlandLinks(double x);
	 void SetFluss(double x);
	 void SetVorlandRechts(double x);
	 void SetAnfProf(CString& file);
	 void SetEndProf(CString& file);

protected:
	friend istream& operator>>(istream& is, Connection &con);
	friend ostream& operator<<(ostream& os, Connection &con);

protected:
	double m_dAnfStation;
	double m_dEndStation;
	double m_dVorL;
	double m_dFluss;
	double m_dVorR;
	CString m_fileAnfProf;
	CString m_fileEndProf;
};

#endif // CONNECT_H