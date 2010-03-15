#include "stdafx.h"

#include "dxflayer.h"
#include "dxfzeich.h"

#include "dxfblkin.h"

  ////////////////////////////
  //  Klasse  CDXFBlockInsert
  ///////////////////////////

/* The Default Constructor */
CDXFBlockInsert::CDXFBlockInsert(CDXFLayer* pOwner) : CDXFEntity(pOwner)
{
//	m_pOwner = pOwner;
	m_dX = m_dY = m_dZ = 0;
	m_dXMaszstab = m_dYMaszstab = m_dZMaszstab = 1.0;
	m_dDrehWkl = 0;
}

CDXFBlockInsert::~CDXFBlockInsert()
{
}

ostream& operator<<(ostream& os, CDXFBlockInsert &bi)
{
	ASSERT(bi.m_pOwner!=NULL);

	os << setw(WSIZE_3) << "0" << endl;
	os << "INSERT" << endl;
	os << setw(WSIZE_3) << "8" << endl;
	os << "VONLAYER" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << bi.m_blockname << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << bi.m_dX << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << bi.m_dY << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << bi.m_dZ << endl;
	os << setw(WSIZE_3) << "41" << endl;
	os << bi.m_dXMaszstab << endl;
	os << setw(WSIZE_3) << "42" << endl;
	os << bi.m_dYMaszstab << endl;
	os << setw(WSIZE_3) << "43" << endl;
	os << bi.m_dZMaszstab << endl;
	os << setw(WSIZE_3) << "50" << endl;
	os << bi.m_dDrehWkl << endl;
	return os;
}

istream& operator>>(istream& is, CDXFBlockInsert &bi)
{
	int code = 1;
	char buffer[100];
	CDXFLayer *ly;
	CString name, str;

	ASSERT(bi.m_pOwner!=NULL);

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
			case 2:		// Block name
				is.getline(buffer, 100, '\n');
				bi.m_blockname = buffer;
				bi.m_blockname.TrimLeft();
				bi.m_blockname.TrimRight();
				break;

			case 8:		// Layer name
				is.getline(buffer, 100, '\n');
				name = buffer;
				name.TrimLeft();
				name.TrimRight();
				bi.m_pOwner->GetName(str);
				if (str.IsEmpty())
				{
					ly = bi.m_pOwner->GetOwner()->FindLayer(name);
					if (ly==NULL)
						bi.m_pOwner->SetName(name);
					else
					{
						delete bi.m_pOwner;
						bi.m_pOwner = ly;
					}
				}
				break;

			case 10:	// X
				is >> bi.m_dX;
				is.getline(buffer, 100, '\n');
				break;

			case 20:	// Y
				is >> bi.m_dY;
				is.getline(buffer, 100, '\n');
				break;

			case 30:	// Z
				is >> bi.m_dZ;
				is.getline(buffer, 100, '\n');
				break;

			case 41:	// XMaszstabfaktor
				is >> bi.m_dXMaszstab;
				is.getline(buffer, 100, '\n');
				break;

			case 42:	// YMaszstabfaktor
				is >> bi.m_dYMaszstab;
				is.getline(buffer, 100, '\n');
				break;

			case 43:	// ZMaszstabfaktor
				is >> bi.m_dZMaszstab;
				is.getline(buffer, 100, '\n');
				break;

			case 50:
				is >> bi.m_dDrehWkl;
				is.getline(buffer, 100, '\n');
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}

	return is;
}

void CDXFBlockInsert::Set( const double posX, const double posY, 
                           const double scale /*= 1.0 */, const double wkl /* = 0.0 */ )
{
	m_dX = posX;
	m_dY = posY;
	m_dXMaszstab = m_dYMaszstab = m_dZMaszstab = scale;
	m_dDrehWkl = wkl;
}