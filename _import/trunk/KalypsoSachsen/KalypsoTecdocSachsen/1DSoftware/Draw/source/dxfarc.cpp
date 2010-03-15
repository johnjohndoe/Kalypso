#include "stdafx.h"

#include "dxflayer.h"
#include "dxfzeich.h"

#include "dxfarc.h"

  ////////////////////////////
  //  Klasse  CDXFArc
  ///////////////////////////

/* The Default Constructor */
CDXFArc::CDXFArc(CDXFLayer* pOwner) : CDXFEntity(pOwner)
{
//	m_pOwner = pOwner;
	m_dX = m_dY = m_dZ = 0;
	m_rad = 0;
	m_dAnfW = m_dEndW = 0;
}

CDXFArc::~CDXFArc()
{
}

ostream& operator<<(ostream& os, CDXFArc &ar)
{
	CString str;
	
	ASSERT(ar.m_pOwner!=NULL);

	os.setf(ios::fixed);
	os << setw(WSIZE_3) << "0" << endl;
	os << setw(WSIZE_4) << "ARC" << endl;
	os << setw(WSIZE_3) << "8" << endl;
	ar.m_pOwner->GetName(str);
	os << str << endl;
	if (!ar.m_LType.IsEmpty())
	{
		os << setw(WSIZE_3) << "6" << endl;
		os << ar.m_LType << endl;
	}
	os << setw(WSIZE_3) << "39" << endl;
	os << ar.m_nWidth << endl;
	os << setw(WSIZE_3) << "62" << endl;
	os << ar.m_nColor << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << ar.m_dX << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << ar.m_dY << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << ar.m_dZ << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << ar.m_rad << endl;
	os << setw(WSIZE_3) << "50" << endl;
	os << ar.m_dAnfW << endl;
	os << setw(WSIZE_3) << "51" << endl;
	os << ar.m_dEndW << endl;
	return os;
}

istream& operator>>(istream& is, CDXFArc &ar)
{
	int code = 1;
	char buffer[100];
	CDXFLayer *ly;
	CString name, str;

	ASSERT(ar.m_pOwner!=NULL);

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
			case 8:		// Layer name
				is.getline(buffer, 100, '\n');
				name = buffer;
				name.TrimLeft();
				name.TrimRight();
				ar.m_pOwner->GetName(str);
				if (str.IsEmpty())
				{
					ly = ar.m_pOwner->GetOwner()->FindLayer(name);
					if (ly==NULL)
						ar.m_pOwner->SetName(name);
					else
					{
						delete ar.m_pOwner;
						ar.m_pOwner = ly;
					}
				}
				break;

			case 10:	// X
				is >> ar.m_dX;
				is.getline(buffer, 100, '\n');
				break;

			case 20:	// Y
				is >> ar.m_dY;
				is.getline(buffer, 100, '\n');
				break;

			case 30:	// Z
				is >> ar.m_dZ;
				is.getline(buffer, 100, '\n');
				break;

			case 40:	// rad
				is >> ar.m_rad;
				is.getline(buffer, 100, '\n');
				break;

			case 50:	// anfang winkel
				is >> ar.m_dAnfW;
				is.getline(buffer, 100, '\n');
				break;

			case 51:	// end winkel
				is >> ar.m_dEndW;
				is.getline(buffer, 100, '\n');
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}

	return is;
}

void CDXFArc::Set( const double dx, const double dy, double rad, double anfw, double endw)
{
	m_dX = dx;
	m_dY = dy;
	m_rad = rad;
	m_dAnfW = anfw;
	m_dEndW = endw;
}