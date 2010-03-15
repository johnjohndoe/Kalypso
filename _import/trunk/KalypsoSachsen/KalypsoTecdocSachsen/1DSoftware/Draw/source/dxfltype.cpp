#include "stdafx.h"

#include "dxflayer.h"

#include "dxfltype.h"

  ////////////////////////////
  //  Klasse  CDXFLType
  ///////////////////////////

/* The Default Constructor */
CDXFLType::CDXFLType(CDXFZeichnung* pOwner)
{
	m_pOwner = pOwner;
	m_nNum = 0;
	m_dMusterLaenge = 0;
}

CDXFLType::~CDXFLType()
{
}

ostream& operator<<(ostream& os, CDXFLType &lt)
{
	int i;

	ASSERT(lt.m_pOwner!=NULL);

	os << setw(WSIZE_3) << "0" << endl;
	os << "LTYPE" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << lt.m_name << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << "64" << endl;
	os << setw(WSIZE_3) << "3" << endl;
	os << lt.m_beschreibung << endl;
	os << setw(WSIZE_3) << "72" << endl;
	os << "65" << endl;
	os << setw(WSIZE_3) << "73" << endl;
	os << lt.m_nNum << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << lt.m_dMusterLaenge << endl;
	for (i=0; i<lt.m_strichlaenge.GetSize(); i++)
	{
		os << setw(WSIZE_3) << "49" << endl;
		os << lt.m_strichlaenge[i] << endl;
	}
	return os;
}

istream& operator>>(istream& is, CDXFLType &lt)
{
	char buffer[100];
	int code = 1;
	int i = 0;
	double sl;
	
	ASSERT(lt.m_pOwner!=NULL);

	while (code!=0 && !is.eof())
	{
		is >> code;
		if (code==0)
		{
			is.putback('0');
			break;
		}
		is.getline(buffer, 100, '\n');
		switch (code)
		{
			case 2:		// LType name
				is.getline(buffer, 100, '\n');
				lt.m_name = buffer;
				lt.m_name.TrimLeft();
				lt.m_name.TrimRight();
				break;

			case 70:
				is.getline(buffer, 100, '\n');
				break;

			case 3:	// Beschreibung
				is.getline(buffer, 100, '\n');
				lt.m_beschreibung = buffer;
				lt.m_beschreibung.TrimLeft();
				lt.m_beschreibung.TrimRight();
				break;

			case 72:
				is.getline(buffer, 100, '\n');
				break;

			case 73:	// Anzahl Strichlaengen-Elemente
				is >> lt.m_nNum;
				is.getline(buffer, 100, '\n');
				break;

			case 40:	// Musterlaenge
				is >> lt.m_dMusterLaenge;
				is.getline(buffer, 100, '\n');
				break;

			case 49:	// Strichlaenge
				is >> sl;
				is.getline(buffer, 100, '\n');
				lt.m_strichlaenge.SetAtGrow(i++, sl);
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}
	return is;
}

void CDXFLType::Set(CString& name, CString& bes, int num, double laenge)
{
	m_name = name;
	m_beschreibung = bes;
	m_nNum = num;
	m_dMusterLaenge = laenge;
}

void CDXFLType::SetStrich(CArray<double, double>& sl)
{
	int i;

	m_strichlaenge.RemoveAll();
	for (i=0; i<sl.GetSize(); i++)
		m_strichlaenge.SetAtGrow(i, sl[i]);
}
