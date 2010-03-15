#include "stdafx.h"

#include "dxflayer.h"
#include "dxfzeich.h"

#include "dxftext.h"

  ////////////////////////////
  //  Klasse  CDXFText
  ///////////////////////////

/* The Default Constructor */
CDXFText::CDXFText( CDXFLayer* pOwner ) : CDXFEntity( pOwner )
{
	m_dX = m_dY = m_dZ = 0;
	m_dXAus = m_dYAus = m_dZAus = 0;
	m_dXMaszstab = m_dNWkl = 0;
	m_nHPos = m_nVPos = 0;
	m_dHoehe = 0;
	m_dWkl = 0;
}

CDXFText::~CDXFText()
{
}

ostream& operator<<(ostream& os, CDXFText &tx)
{
	CString str;
	
	ASSERT(tx.m_pOwner!=NULL);

	os.setf(ios::fixed);
	os << setw(WSIZE_3) << "0" << endl;
	os << "TEXT" << endl;
	os << setw(WSIZE_3) << "8" << endl;
	tx.m_pOwner->GetName(str);
	os << str << endl;
	if (!tx.m_LType.IsEmpty())
	{
		os << setw(WSIZE_3) << "6" << endl;
		os << tx.m_LType << endl;
	}
	os << setw(WSIZE_3) << "39" << endl;
	os << tx.m_nWidth << endl;
	os << setw(WSIZE_3) << "62" << endl;
	os << tx.m_nColor << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << tx.m_dX << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << tx.m_dY << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << tx.m_dZ << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << tx.m_dHoehe << endl;
	os << setw(WSIZE_3) << "1" << endl;
	os << tx.m_text << endl;
	os << setw(WSIZE_3) << "50" << endl;
	os << tx.m_dWkl << endl;
	if (tx.m_dXMaszstab!=0)
	{
		os << setw(WSIZE_3) << "41" << endl;
		os << tx.m_dXMaszstab << endl;
	}
	if (tx.m_dNWkl!=0)
	{
		os << setw(WSIZE_3) << "51" << endl;
		os << tx.m_dNWkl << endl;
	}
	if (tx.m_nHPos!=0 || tx.m_nVPos!=0)
	{
		os << setw(WSIZE_3) << "72" << endl;
		os << tx.m_nHPos << endl;
		os << setw(WSIZE_3) << "73" << endl;
		os << tx.m_nVPos << endl;
		os << setw(WSIZE_3) << "11" << endl;
		os << tx.m_dXAus << endl;
		os << setw(WSIZE_3) << "21" << endl;
		os << tx.m_dYAus << endl;
		os << setw(WSIZE_3) << "31" << endl;
		os << tx.m_dZAus << endl;
	}

	return os;
}

istream& operator>>(istream& is, CDXFText &tx)
{
	int code = 1;
	char buffer[100];
	CDXFLayer *ly;
	CString name, str;

	ASSERT(tx.m_pOwner!=NULL);

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
			case 1:		// Text
				is.getline(buffer, 100, '\n');
				tx.m_text = buffer;
				tx.m_text.TrimLeft();
				tx.m_text.TrimRight();
				break;

			case 8:		// Layer name
				is.getline(buffer, 100, '\n');
				name = buffer;
				name.TrimLeft();
				name.TrimRight();
				tx.m_pOwner->GetName(str);
				if (str.IsEmpty())
				{
					ly = tx.m_pOwner->GetOwner()->FindLayer(name);
					if (ly==NULL)
						tx.m_pOwner->SetName(name);
					else
					{
						delete tx.m_pOwner;
						tx.m_pOwner = ly;
					}
				}
				break;

			case 10:	// X
				is >> tx.m_dX;
				is.getline(buffer, 100, '\n');
				break;

			case 11:	// X Ausrichtungspunkt
				is >> tx.m_dXAus;
				is.getline(buffer, 100, '\n');
				break;

			case 20:	// Y
				is >> tx.m_dY;
				is.getline(buffer, 100, '\n');
				break;

			case 21:	// Y Ausrichtungspunkt
				is >> tx.m_dYAus;
				is.getline(buffer, 100, '\n');
				break;

			case 30:	// Z
				is >> tx.m_dZ;
				is.getline(buffer, 100, '\n');
				break;

			case 31:	// Z Ausrichtungspunkt
				is >> tx.m_dZAus;
				is.getline(buffer, 100, '\n');
				break;

			case 40:	// Hoehe
				is >> tx.m_dHoehe;
				is.getline(buffer, 100, '\n');
				break;

			case 41:	// X-Maszstabsfaktor
				is >> tx.m_dXMaszstab;
				is.getline(buffer, 100, '\n');
				break;

			case 50:	// Drehwinkel
				is >> tx.m_dWkl;
				is.getline(buffer, 100, '\n');
				break;

			case 51:	// Neigungswinkel
				is >> tx.m_dNWkl;
				is.getline(buffer, 100, '\n');
				break;

			case 72:	// horizontale Positionierung
				is >> tx.m_nHPos;
				is.getline(buffer, 100, '\n');
				break;

			case 73:	// vertikale Positionierung
				is >> tx.m_nVPos;
				is.getline(buffer, 100, '\n');
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}

	return is;
}

void CDXFText::Set( const CString& text, const double posX, const double posY, const double h, const double w )
{
	m_text = text;
	m_dX = posX;
	m_dY = posY;
	m_dHoehe = h;
	m_dWkl = w;
}

void CDXFText::SetJust( const int hpos, const int vpos, const double crdX, const double crdY )
{
	m_nHPos = hpos;
	m_nVPos = vpos;
	m_dXAus = crdX;
	m_dYAus = crdY;
}

