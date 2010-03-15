#include "stdafx.h"


#include "connect.h"

  ////////////////////////////
  //  Klasse  Connection
  ///////////////////////////

/* The Default Constructor */
Connection::Connection()
{
	m_dAnfStation = m_dEndStation = 0;
	m_dVorL = m_dFluss = m_dVorR = 0;
}

Connection::~Connection()
{
}

double Connection::GetAnfStation()
{
	return m_dAnfStation;
}

void Connection::SetAnfStation(double x)
{
	m_dAnfStation = x;
}

double Connection::GetEndStation()
{
	return m_dEndStation;
}

void Connection::SetEndStation(double x)
{
	m_dEndStation = x;
}

double Connection::GetVorlandLinks()
{
	return m_dVorL;
}

void Connection::SetVorlandLinks(double x)
{
	m_dVorL = x;
}

double Connection::GetFluss()
{
	return m_dFluss;
}

void Connection::SetFluss(double x)
{
	m_dFluss = x;
}

double Connection::GetVorlandRechts()
{
	return m_dVorR;
}

void Connection::SetVorlandRechts(double x)
{
	m_dVorR = x;
}

CString Connection::GetAnfProf()
{
	return m_fileAnfProf;
}

void Connection::SetAnfProf(CString& file)
{
	m_fileAnfProf = file;
}

CString Connection::GetEndProf()
{
	return m_fileEndProf;
}

void Connection::SetEndProf(CString& file)
{
	m_fileEndProf = file;
}

istream& operator>>(istream& is, Connection &con)
{
	char buffer[LINE_SIZE];
	CString str;
	
	is >> con.m_dAnfStation;
	is >> con.m_dEndStation;
	is >> con.m_dVorL;
	is >> con.m_dFluss;
	is >> con.m_dVorR;
	is.getline(buffer, LINE_SIZE, '\n');
	str = buffer;
	str.TrimLeft();
	str.TrimRight();
	con.m_fileAnfProf = str.Left(str.Find(' '));
	con.m_fileEndProf = str.Right(str.GetLength()-str.Find(' ')-1);
	return is;
}

ostream& operator<<(ostream& os, Connection &con)
{
	os.setf(ios::fixed);
	os.precision(6);
	os << con.m_dAnfStation;
	os << " " << con.m_dEndStation;
	os.precision(4);
	os << " " << con.m_dVorL;
	os << " " << con.m_dFluss;
	os << " " << con.m_dVorR;
	os << " " << con.m_fileAnfProf;
	os << " " << con.m_fileEndProf << endl;
	return os;
}
