#include "stdafx.h"

#include "dxftext.h"
#include "dxfplin.h"
#include "dxflinie.h"
#include "dxfkreis.h"
#include "dxfarc.h"
#include "dxfpunkt.h"
#include "dxflayer.h"

#include "dxfblock.h"

  ////////////////////////////
  //  Klasse  CDXFBlock
  ///////////////////////////

/* The Default Constructor */
CDXFBlock::CDXFBlock(CDXFLayer* pOwner)
{
	m_pOwner = pOwner;
	m_dX = m_dY = m_dZ = 0;
}

CDXFBlock::~CDXFBlock()
{
	POSITION pos;
	CDXFText *tx;
	CDXFLinie *ln;
	CDXFPolylinie *pl;
	CDXFKreis *kr;
	CDXFArc *ar;
	CDXFPunkt *pt;

	pos = m_text.GetHeadPosition();
	while(pos!=NULL)
	{
		tx = m_text.GetNext(pos);
		delete tx;
	}
	m_text.RemoveAll();
	pos = m_linien.GetHeadPosition();
	while(pos!=NULL)
	{
		ln = m_linien.GetNext(pos);
		delete ln;
	}
	m_linien.RemoveAll();
	pos = m_polylinien.GetHeadPosition();
	while(pos!=NULL)
	{
		pl = m_polylinien.GetNext(pos);
		delete pl;
	}
	m_polylinien.RemoveAll();
	pos = m_kreise.GetHeadPosition();
	while(pos!=NULL)
	{
		kr = m_kreise.GetNext(pos);
		delete kr;
	}
	m_kreise.RemoveAll();
	pos = m_arcs.GetHeadPosition();
	while(pos!=NULL)
	{
		ar = m_arcs.GetNext(pos);
		delete ar;
	}
	m_arcs.RemoveAll();
	pos = m_punkte.GetHeadPosition();
	while(pos!=NULL)
	{
		pt = m_punkte.GetNext(pos);
		delete pt;
	}
	m_punkte.RemoveAll();
}

ostream& operator<<(ostream& os, CDXFBlock &bl)
{
	POSITION pos;
	CDXFText *tx;
	CDXFLinie *ln;
	CDXFPolylinie *pl;
	CDXFKreis *kr;
	CDXFArc *ar;
	CDXFPunkt *pt;

	ASSERT(bl.m_pOwner!=NULL);

	os << setw(WSIZE_3) << "0" << endl;
	os << "BLOCK" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << bl.m_name << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "64" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << bl.m_dX << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << bl.m_dY << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << bl.m_dZ << endl;
	pos = bl.m_text.GetHeadPosition();
	while(pos!=NULL)
	{
		tx = bl.m_text.GetNext(pos);
		os << *tx;
	}
	pos = bl.m_linien.GetHeadPosition();
	while(pos!=NULL)
	{
		ln = bl.m_linien.GetNext(pos);
		os << *ln;
	}
	pos = bl.m_polylinien.GetHeadPosition();
	while(pos!=NULL)
	{
		pl = bl.m_polylinien.GetNext(pos);
		os << *pl;
	}
	pos = bl.m_kreise.GetHeadPosition();
	while(pos!=NULL)
	{
		kr = bl.m_kreise.GetNext(pos);
		os << *kr;
	}
	pos = bl.m_arcs.GetHeadPosition();
	while(pos!=NULL)
	{
		ar = bl.m_arcs.GetNext(pos);
		os << *ar;
	}
	pos = bl.m_punkte.GetHeadPosition();
	while(pos!=NULL)
	{
		pt = bl.m_punkte.GetNext(pos);
		os << *pt;
	}
	os << setw(WSIZE_3) << "0" << endl;
	os << "ENDBLK" << endl;

	return os;
}

istream& operator>>(istream& is, CDXFBlock &bl)
{
	CDXFText *tx;
	CDXFLinie *ln;
	CDXFPolylinie *pl;
	CDXFKreis *kr;
	CDXFArc *ar;
	CDXFPunkt *pt;
	char buffer[100];
	int key, code = 0;
	CString str;

	ASSERT(bl.m_pOwner!=NULL);

	while (code!=-1 && !is.eof())
	{
		is >> code;
		is.getline(buffer, 100, '\n');
		switch (code)
		{
			case 0:	// Entity or End of Block
				is.getline(buffer, 100, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				key = 0;
				if (str=="ENDBLK")
					key = 1;
				else if (str=="LINE")
					key = 2;
				else if (str=="CIRCLE")
					key = 3;
				else if (str=="POLYLINE")
					key = 4;
				else if (str=="TEXT")
					key = 5;
				else if (str=="POINT")
					key = 6;
				else if (str=="ARC")
					key = 7;
				switch (key)
				{
					case 1:		// ENDBLK
						is.getline(buffer, 100, '\n');
						is.getline(buffer, 100, '\n');
						break;
				
					case 2:		// LINE
						ln = new CDXFLinie(bl.m_pOwner);
						is >> *ln;
						bl.m_pOwner = ln->GetOwner();
						bl.AddLinie(ln);
						break;

					case 3:		// CIRCLE
						kr = new CDXFKreis(bl.m_pOwner);
						is >> *kr;
						bl.m_pOwner = kr->GetOwner();
						bl.AddKreis(kr);
						break;
	
					case 4:		// POLYLINE
						pl = new CDXFPolylinie(bl.m_pOwner);
						is >> *pl;
						bl.m_pOwner = pl->GetOwner();
						bl.AddPolylinie(pl);
						break;

					case 5:		// TEXT
						tx = new CDXFText(bl.m_pOwner);
						is >> *tx;
						bl.m_pOwner = tx->GetOwner();
						bl.AddText(tx);
						break;

					case 6:		// POINT
						pt = new CDXFPunkt(bl.m_pOwner);
						is >> *pt;
						bl.m_pOwner = pt->GetOwner();
						bl.AddPunkt(pt);
						break;
	
					case 7:		// ARC
						ar = new CDXFArc(bl.m_pOwner);
						is >> *ar;
						bl.m_pOwner = ar->GetOwner();
						bl.AddArc(ar);
						break;
	
					default:
						break;
				}
				break;

			case 2:		// Block name
				is.getline(buffer, 100, '\n');
				bl.m_name = buffer;
				bl.m_name.TrimLeft();
				bl.m_name.TrimRight();
				break;

			case 8:
				is.getline(buffer, 100, '\n');
				break;

			case 10:	// X Coord
				is >> bl.m_dX;
				is.getline(buffer, 100, '\n');
				break;

			case 20:	// Y Coord
				is >> bl.m_dY;
				is.getline(buffer, 100, '\n');
				break;

			case 30:	// Z Coord
				is >> bl.m_dZ;
				is.getline(buffer, 100, '\n');
				break;

			case 70:	// Flag
				is.getline(buffer, 100, '\n');
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}

	return is;
}

void CDXFBlock::AddText(CDXFText* tx)
{
	m_text.AddTail(tx);
}

void CDXFBlock::AddLinie(CDXFLinie* ln)
{
	m_linien.AddTail(ln);
}

void CDXFBlock::AddPolylinie(CDXFPolylinie* pl)
{
	m_polylinien.AddTail(pl);
}

void CDXFBlock::AddKreis(CDXFKreis* kr)
{
	m_kreise.AddTail(kr);
}

void CDXFBlock::AddArc(CDXFArc* ar)
{
	m_arcs.AddTail(ar);
}

void CDXFBlock::AddPunkt(CDXFPunkt* pt)
{
	m_punkte.AddTail(pt);
}

void CDXFBlock::DrawLinie( const double anfX, const double anfY, const double endX, const double endY )
{
	CDXFLinie* ln = new CDXFLinie( m_pOwner );
	ln->SetAnf( anfX, anfY );
	ln->SetEnd( endX, endY );
	AddLinie( ln );
}; // DrawLinie

void CDXFBlock::DrawCross( const double posX, const double posY, double rad )
{
	CDXFLinie* ln = new CDXFLinie(m_pOwner);
	
  ln->SetAnf( posX - rad, posY );
	ln->SetEnd( posX + rad, posY );
	AddLinie(ln);

  ln = new CDXFLinie(m_pOwner);
	ln->SetAnf( posX, posY - rad );
	ln->SetEnd( posX, posY + rad );
	AddLinie(ln);
}; // DrawCross

void CDXFBlock::DrawKreis( const double posX, const double posY, double rad )
{
	CDXFKreis* kr = new CDXFKreis( m_pOwner );
	kr->Set( posX, posY, rad );
	AddKreis(kr);
}; // DrawKreis

void CDXFBlock::DrawPunkt( const double posX, const double posY )
{
	CDXFPunkt* pt = new CDXFPunkt(m_pOwner);
	pt->Set( posX, posY );
	AddPunkt(pt);
}; // DrawPunkt

void CDXFBlock::DrawRect(double x, double y, double h, double w, double th)
{
	double theta, xs, xc, x1, y1;
	CDXFPolylinie* poly = new CDXFPolylinie(m_pOwner);
	theta = th/57.2958;
	xs = sin(theta);
	xc = cos(theta);
	poly->AddCoord( x, y );
	x1 = x-h*xs;
	y1 = x+h*xc;
	poly->AddCoord( x1, y1 );
	x1 = x1+w*xc;
	y1 = y1+w*xs;
	poly->AddCoord( x1, y1 );
	x1 = x+w*xc;
	y1 = y+w*xs;
	poly->AddCoord( x1, y1 );
	poly->AddCoord( x, y );
	AddPolylinie( poly );
}; // DrawRect