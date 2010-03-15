#include "stdafx.h"

#include "dxflayer.h"
#include "dxfzeich.h"

#include "dxfkreis.h"

  ////////////////////////////
  //  Klasse  CDXFKreis
  ///////////////////////////

/* The Default Constructor */
CDXFKreis::CDXFKreis(CDXFLayer* pOwner) : CDXFEntity(pOwner)
{
//	m_pOwner = pOwner;
	m_dX = m_dY = m_dZ = 0;
	m_rad = 0;
}

CDXFKreis::~CDXFKreis()
{
}

ostream& operator<<(ostream& os, CDXFKreis &kr)
{
	CString str;
	
	ASSERT(kr.m_pOwner!=NULL);

	os.setf(ios::fixed);
	os << setw(WSIZE_3) << "0" << endl;
	os << setw(WSIZE_4) << "CIRCLE" << endl;
	os << setw(WSIZE_3) << "8" << endl;
	kr.m_pOwner->GetName(str);
	os << str << endl;
	if (!kr.m_LType.IsEmpty())
	{
		os << setw(WSIZE_3) << "6" << endl;
		os << kr.m_LType << endl;
	}
	os << setw(WSIZE_3) << "39" << endl;
	os << kr.m_nWidth << endl;
	os << setw(WSIZE_3) << "62" << endl;
	os << kr.m_nColor << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << kr.m_dX << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << kr.m_dY << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << kr.m_dZ << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << kr.m_rad << endl;
	return os;
}

istream& operator>>(istream& is, CDXFKreis &kr)
{
	int code = 1;
	char buffer[100];
	CDXFLayer *ly;
	CString name, str;

	ASSERT(kr.m_pOwner!=NULL);

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
				kr.m_pOwner->GetName(str);
				if (str.IsEmpty())
				{
					ly = kr.m_pOwner->GetOwner()->FindLayer(name);
					if (ly==NULL)
						kr.m_pOwner->SetName(name);
					else
					{
						delete kr.m_pOwner;
						kr.m_pOwner = ly;
					}
				}
				break;

			case 10:	// X
				is >> kr.m_dX;
				is.getline(buffer, 100, '\n');
				break;

			case 20:	// Y
				is >> kr.m_dY;
				is.getline(buffer, 100, '\n');
				break;

			case 30:	// Z
				is >> kr.m_dZ;
				is.getline(buffer, 100, '\n');
				break;

			case 40:	// rad
				is >> kr.m_rad;
				is.getline(buffer, 100, '\n');
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}

	return is;
}

void CDXFKreis::Set( const double posX, const double posY, const double rad )
{
	m_dX = posX;
	m_dY = posY;
	m_rad = rad;
}