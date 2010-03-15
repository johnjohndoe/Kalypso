#include "stdafx.h"

#include "dxflayer.h"
#include "dxfzeich.h"

#include "dxfpunkt.h"

  ////////////////////////////
  //  Klasse  CDXFPunkt
  ///////////////////////////

/* The Default Constructor */
CDXFPunkt::CDXFPunkt(CDXFLayer* pOwner) : CDXFEntity(pOwner)
{
//	m_pOwner = pOwner;
	m_dX = m_dY = m_dZ = 0;
}

CDXFPunkt::~CDXFPunkt()
{
}

ostream& operator<<(ostream& os, CDXFPunkt &pt)
{
	CString str;
	ASSERT(pt.m_pOwner!=NULL);

	os.setf(ios::fixed);
	os << setw(WSIZE_3) << "0" << endl;
	os << setw(WSIZE_4) << "POINT" << endl;
	os << setw(WSIZE_3) << "8" << endl;
	pt.m_pOwner->GetName(str);
	os << str << endl;
	if (!pt.m_LType.IsEmpty())
	{
		os << setw(WSIZE_3) << "6" << endl;
		os << pt.m_LType << endl;
	}
	os << setw(WSIZE_3) << "39" << endl;
	os << pt.m_nWidth << endl;
	os << setw(WSIZE_3) << "62" << endl;
	os << pt.m_nColor << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << pt.m_dX << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << pt.m_dY << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << pt.m_dZ << endl;
	return os;
}

istream& operator>>(istream& is, CDXFPunkt &pt)
{
	int code = 1;
	char buffer[100];
	CDXFLayer *ly;
	CString name, str;

	ASSERT(pt.m_pOwner!=NULL);

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
				pt.m_pOwner->GetName(str);
				if (str.IsEmpty())
				{
					ly = pt.m_pOwner->GetOwner()->FindLayer(name);
					if (ly==NULL)
						pt.m_pOwner->SetName(name);
					else
					{
						delete pt.m_pOwner;
						pt.m_pOwner = ly;
					}
				}
				break;

			case 10:	// X
				is >> pt.m_dX;
				is.getline(buffer, 100, '\n');
				break;

			case 20:	// Y
				is >> pt.m_dY;
				is.getline(buffer, 100, '\n');
				break;

			case 30:	// Z
				is >> pt.m_dZ;
				is.getline(buffer, 100, '\n');
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}

	return is;
}