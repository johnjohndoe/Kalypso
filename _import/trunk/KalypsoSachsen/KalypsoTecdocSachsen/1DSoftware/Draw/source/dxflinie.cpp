#include "stdafx.h"

#include "dxflayer.h"
#include "dxfzeich.h"

#include "dxflinie.h"

  ////////////////////////////
  //  Klasse  CDXFLinie
  ///////////////////////////

/* The Default Constructor */
CDXFLinie::CDXFLinie(CDXFLayer* pOwner) : CDXFEntity(pOwner)
{
//	m_pOwner = pOwner;
	m_dXA = m_dYA = m_dZA = 0;
	m_dXE = m_dYE = m_dZE = 0;
}

CDXFLinie::~CDXFLinie()
{
}

ostream& operator<<(ostream& os, CDXFLinie &ln)
{
	CString str;
	
	ASSERT(ln.m_pOwner!=NULL);

	os.setf(ios::fixed);
	os << setw(WSIZE_3) << "0" << endl;
	os << setw(WSIZE_4) << "LINE" << endl;
	os << setw(WSIZE_3) << "8" << endl;
	ln.m_pOwner->GetName(str);
	os << str << endl;
	if (!ln.m_LType.IsEmpty())
	{
		os << setw(WSIZE_3) << "6" << endl;
		os << ln.m_LType << endl;
	}
	os << setw(WSIZE_3) << "39" << endl;
	os << ln.m_nWidth << endl;
	os << setw(WSIZE_3) << "62" << endl;
	os << ln.m_nColor << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << ln.m_dXA << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << ln.m_dYA << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << ln.m_dZA << endl;
	os << setw(WSIZE_3) << "11" << endl;
	os << ln.m_dXE << endl;
	os << setw(WSIZE_3) << "21" << endl;
	os << ln.m_dYE << endl;
	os << setw(WSIZE_3) << "31" << endl;
	os << ln.m_dZE << endl;
	return os;
}

istream& operator>>(istream& is, CDXFLinie &ln)
{
	int code = 1;
	char buffer[100];
	CDXFLayer *ly;
	CString name, str;

	ASSERT(ln.m_pOwner!=NULL);

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
				ln.m_pOwner->GetName(str);
				if (str.IsEmpty())
				{
					ly = ln.m_pOwner->GetOwner()->FindLayer(name);
					if (ly==NULL)
						ln.m_pOwner->SetName(name);
					else
					{
						delete ln.m_pOwner;
						ln.m_pOwner = ly;
					}
				}
				break;

			case 10:	// XAnf
				is >> ln.m_dXA;
				is.getline(buffer, 100, '\n');
				break;

			case 20:	// YAnf
				is >> ln.m_dYA;
				is.getline(buffer, 100, '\n');
				break;

			case 30:	// ZAnf
				is >> ln.m_dZA;
				is.getline(buffer, 100, '\n');
				break;

			case 11:	// XEnd
				is >> ln.m_dXE;
				is.getline(buffer, 100, '\n');
				break;

			case 21:	// YEnd
				is >> ln.m_dYE;
				is.getline(buffer, 100, '\n');
				break;

			case 31:	// ZEnd
				is >> ln.m_dZE;
				is.getline(buffer, 100, '\n');
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}

	return is;
}