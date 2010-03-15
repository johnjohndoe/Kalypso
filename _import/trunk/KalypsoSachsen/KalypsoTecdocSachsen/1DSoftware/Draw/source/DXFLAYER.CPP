#include "stdafx.h"

#include "..\..\commonMfc\commonMfc.h"

#include "dxftext.h"
#include "dxflinie.h"
#include "dxfkreis.h"
#include "dxfarc.h"
#include "dxfpunkt.h"
#include "dxfplin.h"
#include "dxfblock.h"
#include "dxfblkin.h"
#include "dxfzeich.h"

#include "dxflayer.h"

  ////////////////////////////
  //  Klasse  CDXFLayer
  ///////////////////////////

/* The Default Constructor */
CDXFLayer::CDXFLayer(CDXFZeichnung* pOwner)
{
	m_pOwner = pOwner;
	m_outputType = DXF_TYPE_ENTITY;
}

CDXFLayer::~CDXFLayer()
{
	POSITION pos;
	CDXFText *tx;
	CDXFLinie *ln;
	CDXFKreis *kr;
	CDXFArc *ar;
	CDXFPunkt *pt;
	CDXFPolylinie *pl;
	CDXFBlock *bl;
	CDXFBlockInsert *bi;

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
	pos = m_polylinien.GetHeadPosition();
	while(pos!=NULL)
	{
		pl = m_polylinien.GetNext(pos);
		delete pl;
	}
	m_polylinien.RemoveAll();
	pos = m_blocks.GetHeadPosition();
	while(pos!=NULL)
	{
		bl = m_blocks.GetNext(pos);
		delete bl;
	}
	m_blocks.RemoveAll();
	pos = m_inserts.GetHeadPosition();
	while(pos!=NULL)
	{
		bi = m_inserts.GetNext(pos);
		delete bi;
	}
	m_inserts.RemoveAll();
}

ostream& operator<<(ostream& os, CDXFLayer &ly)
{
	POSITION pos;
	CDXFText *tx;
	CDXFLinie *ln;
	CDXFKreis *kr;
	CDXFArc *ar;
	CDXFPunkt *pt;
	CDXFPolylinie *pl;
	CDXFBlock *bl;
	CDXFBlockInsert *bi;

	switch (ly.m_outputType)
	{
		case DXF_TYPE_BLOCK:
			pos = ly.m_blocks.GetHeadPosition();
			while(pos!=NULL)
			{
				bl = ly.m_blocks.GetNext(pos);
				os << *bl;
			}
			break;

		case DXF_TYPE_ENTITY:
			pos = ly.m_text.GetHeadPosition();
			while(pos!=NULL)
			{
				tx = ly.m_text.GetNext(pos);
				os << *tx;
			}
			pos = ly.m_linien.GetHeadPosition();
			while(pos!=NULL)
			{
				ln = ly.m_linien.GetNext(pos);
				os << *ln;
			}
			pos = ly.m_kreise.GetHeadPosition();
			while(pos!=NULL)
			{
				kr = ly.m_kreise.GetNext(pos);
				os << *kr;
			}
			pos = ly.m_arcs.GetHeadPosition();
			while(pos!=NULL)
			{
				ar = ly.m_arcs.GetNext(pos);
				os << *ar;
			}
			pos = ly.m_punkte.GetHeadPosition();
			while(pos!=NULL)
			{
				pt = ly.m_punkte.GetNext(pos);
				os << *pt;
			}
			pos = ly.m_polylinien.GetHeadPosition();
			while(pos!=NULL)
			{
				pl = ly.m_polylinien.GetNext(pos);
				os << *pl;
			}
			pos = ly.m_inserts.GetHeadPosition();
			while(pos!=NULL)
			{
				bi = ly.m_inserts.GetNext(pos);
				os << *bi;
			}
			break;

		default:
			break;
	}
	
	return os;
}

void CDXFLayer::AddText(CDXFText* tx)
{
	m_text.AddTail(tx);
}

void CDXFLayer::AddLinie(CDXFLinie* ln)
{
	m_linien.AddTail(ln);
}

void CDXFLayer::AddKreis(CDXFKreis* kr)
{
	m_kreise.AddTail(kr);
}

void CDXFLayer::AddArc(CDXFArc* ar)
{
	m_arcs.AddTail(ar);
}

void CDXFLayer::AddPunkt(CDXFPunkt* pt)
{
	m_punkte.AddTail(pt);
}

void CDXFLayer::AddPolylinie(CDXFPolylinie* pl)
{
	m_polylinien.AddTail(pl);
}

void CDXFLayer::AddBlock(CDXFBlock* bl)
{
	m_blocks.AddTail(bl);
}

void CDXFLayer::AddBlockInsert(CDXFBlockInsert* bi)
{
	m_inserts.AddTail(bi);
}

///////////////////////////////////////////////////////////////
///////////	Drawing Routines
///////////////////////////////////////////////////////////////

void CDXFLayer::DrawLinie( const double anfX, const double anfY, const double endX, const double endY )
{
	CDXFLinie* ln = new CDXFLinie(this);
	ln->SetAnf( anfX, anfY );
	ln->SetEnd( endX, endY );
	AddLinie(ln);
}

void CDXFLayer::DrawKreis( const double posX, const double posY, const double rad )
{
	CDXFKreis* kr = new CDXFKreis(this);
	kr->Set( posX, posY, rad );
	AddKreis(kr);
}

void CDXFLayer::DrawPunkt( const double posX, const double posY )
{
	CDXFPunkt* pt = new CDXFPunkt(this);
	pt->Set( posX, posY );
	AddPunkt(pt);
}

void CDXFLayer::DrawFrame(double gxx, double gyx)
{
	CDXFBlock *bl;
	CDXFBlockInsert *bi;
	CDXFText *tx;
	double xt, yt, scat, ori;
	CString text, str;
	double d, br, hoin, dxy, gx, gy, blgr, gxme, gyme;
	int k;
	
	bl = new CDXFBlock(this);
	str = "FRAME";
	bl->SetName(str);
	scat = 0.75*2.5;
	gx = gxx/10;
	gy = gyx/10;
	dxy = 0.026;
	hoin = (gy-1.0)*10;
	bl->DrawRect(0, 0, gyx, gxx, 0);
	br = (gx-1.2)*10;
	for (k=1; k<=3; k++)
	{
		d = dxy*(k-1)*10;
		bl->DrawRect(7+d, 5+d, hoin, br, 0);
	}
	// Text
	// Hoch-Wert
	xt = m_pOwner->GetXOff()+2.73;
	yt = 2.50+m_pOwner->GetYOff();
	ori = 90.0;
	str.Format("%.2f", -m_pOwner->GetXAdd());
	text.FormatMessage(IDS_HOCHWERT, str);
	tx = new CDXFText(this);
	tx->Set(text, xt, yt, scat, ori);
	bl->AddText(tx);
	// Rechts-Wert
	xt = 2.5+m_pOwner->GetXOff();
	yt = m_pOwner->GetXOff()+0.1;
	ori = 0.0;
	str.Format("%.2f", -m_pOwner->GetYAdd());
	text.Format(IDS_RECHTSWERT, str);
	tx = new CDXFText(this);
	tx->Set(text, xt, yt, scat, ori);
	bl->AddText(tx);
	// KPF filename
	xt = 6.0;
	yt = 6.0;
	ori = 90.0;
	scat = 2.5;
	tx = new CDXFText(this);
	tx->Set(m_pOwner->m_kpfpath, xt, yt, scat, ori);
	bl->AddText(tx);
	// Drawing size + CDXF filename
	blgr = gx*gy/10000;
	gxme = gx/100;
	gyme = gy/100;
	xt = (gx-12)*10;
	yt = 0.15*10;
	ori = 0.0;
	text.Format("%.2f * %.2f = %.2f m**2 %s", gxme, gyme, blgr, m_pOwner->m_CDXFpath);
	tx = new CDXFText(this);
	tx->Set(text, xt, yt, scat, ori);
	bl->AddText(tx);
	AddBlock(bl);
	bi = new CDXFBlockInsert(this);
	bl->GetName(str);
	bi->SetBlockName(str);
	AddBlockInsert(bi);
}

void CDXFLayer::DrawGrid(double scale, double rseite, int anzy, int anzx)
{
	int i, k;
	double yg, xg, ygmm, xgmm, rmasst;
	CDXFBlock *block;
	CDXFBlockInsert *bi;
	CString str;
	
	block = new CDXFBlock(this);
	str.LoadString(IDS_GRID);
	block->SetName(str);
	block->DrawCross( 0, 0, 1);
	AddBlock(block);
	rmasst = 100.0/scale;
	for (i=1; i<=anzy; i++)
	{
		yg = (i-1)*rseite*rmasst;
		for (k=1; k<=anzx; k++)
		{
			xg = (k-1)*rseite*rmasst;
            if(div(i,2).rem==0)
				xg = (anzx-k)*rseite*rmasst;
			xgmm=(xg+1.0)*10.0;
			ygmm=(yg+1.0)*10.0;
			bi = new CDXFBlockInsert(this);
			block->GetName(str);
			bi->SetBlockName(str);
			bi->Set( xgmm, ygmm );
			AddBlockInsert(bi);
		}
	}
}

void CDXFLayer::DrawCross( const double posX, const double posY, const double rad )
{
	CDXFLinie* ln	= new CDXFLinie(this);

	ln->SetAnf( posX - rad, posY );
	ln->SetEnd( posX + rad, posY );
	AddLinie(ln);

  ln = new CDXFLinie(this);
	ln->SetAnf( posX, posY - rad );
	ln->SetEnd( posX, posY + rad );
	AddLinie(ln);
}; // DrawCross

int CDXFLayer::GetColor()
{
	int color = 256;

	switch (m_nColor)
	{
		case RED:
			color = 240;
			break;

		case YELLOW:
			color = 50;
			break;

		case GREEN:
			color = 80;
			break;

		case CYAN:
			color = 130;
			break;

		case BLUE:
			color = 170;
			break;

		case MAGENTA:
			color = 220;
			break;

		case GRAY:
			color = 8;
			break;

		default:
			break;
	}
	return color;
}

void CDXFLayer::GetLType(CString& str)
{
	str = m_LType;
	if (str.IsEmpty())
		str = "CONTINUOUS";
}