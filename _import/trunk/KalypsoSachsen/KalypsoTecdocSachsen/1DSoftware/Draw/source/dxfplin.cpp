#include "stdafx.h"

#include "dxflayer.h"
#include "dxfzeich.h"

#include "dxfplin.h"

  ////////////////////////////
  //  Klasse  CDXFPolylinie
  ///////////////////////////

/* The Default Constructor */
CDXFPolylinie::CDXFPolylinie(CDXFLayer* pOwner) : CDXFEntity(pOwner)
{
//	m_pOwner = pOwner;
	m_dAnfBreite = m_dEndBreite = 0;
}

CDXFPolylinie::~CDXFPolylinie()
{
}

ostream& operator<<(ostream& os, CDXFPolylinie &pl)
{
	CString str;
	POSITION pos;
  CDoublePoint crd;
	ASSERT(pl.m_pOwner!=NULL);

	if (pl.m_coords.GetCount()>0)
	{
		os.setf(ios::fixed);
		os << setw(WSIZE_3) << "0" << endl;
		os << "POLYLINE" << endl;
		os << setw(WSIZE_3) << "8" << endl;
		pl.m_pOwner->GetName(str);
		os << str << endl;
		if (!pl.m_LType.IsEmpty())
		{
			os << setw(WSIZE_3) << "6" << endl;
			os << pl.m_LType << endl;
		}
		os << setw(WSIZE_3) << "39" << endl;
		os << pl.m_nWidth << endl;
		os << setw(WSIZE_3) << "62" << endl;
		os << pl.m_nColor << endl;
		os << setw(WSIZE_3) << "66" << endl;
		os << setw(WSIZE_3) << "1" << endl;
		os << setw(WSIZE_3) << "10" << endl;
		os.setf(ios::fixed);
		os.precision(1);
		os << setw(WSIZE_4) << 0 << endl;
		os << setw(WSIZE_3) << "20" << endl;
		os << setw(WSIZE_4) << 0 << endl;
		os << setw(WSIZE_3) << "30" << endl;
		os << setw(WSIZE_4) << 0 << endl;
		if (pl.m_dAnfBreite!=0 || pl.m_dEndBreite!=0)
		{
			os << setw(WSIZE_3) << "40" << endl;
			os << pl.m_dAnfBreite << endl;
			os << setw(WSIZE_3) << "41" << endl;
			os << pl.m_dEndBreite << endl;
		}
		os.setf(!ios::fixed);
		pos = pl.m_coords.GetHeadPosition();
		while(pos!=NULL)
		{
			crd = pl.m_coords.GetNext(pos);
			os << setw(WSIZE_3) << "0" << endl;
			os << "VERTEX" << endl;
			os << setw(WSIZE_3) << "8" << endl;
			pl.m_pOwner->GetName(str);
			os << str << endl;
			os << setw(WSIZE_3) << "10" << endl;
			os << crd.x << endl;
			os << setw(WSIZE_3) << "20" << endl;
			os << crd.y << endl;
			os << setw(WSIZE_3) << "30" << endl;
			os << "0.0" << endl;
		}
		os << setw(WSIZE_3) << "0" << endl;
		os << "SEQEND" << endl;
		os << setw(WSIZE_3) << "8" << endl;
		os << "0" << endl;
	}
	return os;
}

istream& operator>>(istream& is, CDXFPolylinie &pl)
{
	int key, code = 1;
	char buffer[100];
	CDXFLayer *ly;
	CString name, str;
	CDoublePoint crd;
	double X, Y;

	ASSERT(pl.m_pOwner!=NULL);

	while (code!=99 && !is.eof())
	{
		is >> code;
		is.getline(buffer, 100, '\n');
		switch (code)
		{
			case 0:
				is.getline(buffer, 100, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				key = 0;
				if (str=="VERTEX")
					key = 1;
				else if (str=="SEQEND")
					key = 2;
				switch (key)
				{
					case 1:
						code = 1;
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
								case 10:
									is >> X;
									crd.SetPoint( X, crd. y );
									is.getline(buffer, 100, '\n');
									break;

								case 20:
									is >> Y;
									crd.SetPoint( crd.x, Y );
									is.getline(buffer, 100, '\n');
									break;

								case 30:
									is.getline(buffer, 100, '\n');
									break;
								
								default:
									is.getline(buffer, 100, '\n');
									break;
							}
						}
						pl.AddCoord( crd.x, crd.y );
						break;

					case 2:
						code = 99;
						break;

					default:
						break;
				}
				break;

			case 8:		// Layer name
				is.getline(buffer, 100, '\n');
				name = buffer;
				name.TrimLeft();
				name.TrimRight();
				pl.m_pOwner->GetName(str);
				if (str.IsEmpty())
				{
					ly = pl.m_pOwner->GetOwner()->FindLayer(name);
					if (ly==NULL)
						pl.m_pOwner->SetName(name);
					else
					{
						delete pl.m_pOwner;
						pl.m_pOwner = ly;
					}
				}
				break;

			case 10:
				is.getline(buffer, 100, '\n');
				break;

			case 20:
				is.getline(buffer, 100, '\n');
				break;

			case 30:	// Erhebung
				is.getline(buffer, 100, '\n');
				break;

			case 40:	// Anfangsbreite
				is >> pl.m_dAnfBreite;
				is.getline(buffer, 100, '\n');
				break;

			case 41:	// Anfangsbreite
				is >> pl.m_dEndBreite;
				is.getline(buffer, 100, '\n');
				break;

			case 66:	// Kontrollpunkte flag
				is.getline(buffer, 100, '\n');
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}

	return is;
}

void CDXFPolylinie::AddCoord( const double crdX, const double crdY )
{
	m_coords.AddTail( CDoublePoint( crdX, crdY ) );
}