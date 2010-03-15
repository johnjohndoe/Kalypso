#include "stdafx.h"

#include "dxflayer.h"
#include "dxfltype.h"
#include "dxfblock.h"
#include "dxflinie.h"
#include "dxfplin.h"
#include "dxfkreis.h"
#include "dxftext.h"
#include "dxfblkin.h"

#include "dxfzeich.h"

  ////////////////////////////
  //  Klasse  CDXFZeichnung
  ///////////////////////////

/* The Default Constructor */
CDXFZeichnung::CDXFZeichnung(CDrawDoc* pOwner /*=NULL*/)
{
	m_pOwner = pOwner;
	m_pLastBlockLoaded = NULL;
	xblatt = 0;
	yblatt = 0;
	xxof = 0;
	yyof = 0;
	scale = 0;
	rseite = 0;
	km = 0;
	im = 0;
	xadd = 0;
	yadd = 0;
	SetDefaultLTypes();
}

CDXFZeichnung::~CDXFZeichnung()
{
	POSITION pos;
	CDXFLayer *ly;
	CDXFLType *lt;
	CString name;
	
	pos = m_layers.GetStartPosition();
	while(pos!=NULL)
	{
		m_layers.GetNextAssoc(pos, name, ly);
		delete ly;
	}
	m_layers.RemoveAll();
	pos = m_ltypes.GetHeadPosition();
	while(pos!=NULL)
	{
		lt = m_ltypes.GetNext(pos);
		delete lt;
	}
	m_ltypes.RemoveAll();
}

ostream& operator<<(ostream& os, CDXFZeichnung &zn)
{
	CDXFLayer *ly;
	CDXFLType *lt;
	CString name, str;
	POSITION pos;

	os.setf(ios::fixed);
	os << setw(WSIZE_3) << "0" << endl;
	os << "SECTION" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << "HEADER" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$ACADVER" << endl;
	os << setw(WSIZE_3) << "1" << endl;
	os << "AC1009" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$INSBASE" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$EXTMIN" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$EXTMAX" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << zn.xblatt << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << zn.yblatt << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "1.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$LIMMIN" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$LIMMAX" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << zn.xblatt << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << zn.yblatt << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$ORTHOMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$REGENMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$FILLMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$QTEXTMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$MIRRTEXT" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DRAGMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "2" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$LTSCALE" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "1.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$OSMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$ATTMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$TEXTSIZE" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.2" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$TRACEWID" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.05" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$TEXTSTYLE" << endl;
	os << setw(WSIZE_3) << "7" << endl;
	os << "STANDARD" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$CLAYER" << endl;
	os << setw(WSIZE_3) << "8" << endl;
	os << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$CELTYPE" << endl;
	os << setw(WSIZE_3) << "6" << endl;
	os << "BYLAYER" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$CECOLOR" << endl;
	os << setw(WSIZE_3) << "62" << endl;
	os << setw(WSIZE_6) << "256" << endl;
	// here
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMSCALE" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "1.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMASZ" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.18" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMEXO" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0625" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMDLI" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.38" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMRND" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMDLE" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMEXE" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.18" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTP" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTM" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTXT" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.18" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMCEN" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.09" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTSZ" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTOL" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMLIM" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTIH" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTOH" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMSE1" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMSE2" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTAD" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMZIN" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMBLK" << endl;
	os << setw(WSIZE_3) << "1" << endl;
	os << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMASO" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMSHO" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMPOST" << endl;
	os << setw(WSIZE_3) << "1" << endl;
	os << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMAPOST" << endl;
	os << setw(WSIZE_3) << "1" << endl;
	os << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMALT" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMALTD" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "2" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMALTF" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "25.4" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMLFAC" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "1.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTOFL" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTVP" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTIX" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMSOXD" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMSAH" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMBLK1" << endl;
	os << setw(WSIZE_3) << "1" << endl;
	os << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMBLK2" << endl;
	os << setw(WSIZE_3) << "1" << endl;
	os << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMSTYLE" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << "*UNNAMED" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMCLRD" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMCLRE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMCLRT" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMTFAC" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "1.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DIMGAP" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.09" << endl;
	// here
	os << setw(WSIZE_3) << "9" << endl;
	os << "$LUNITS" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "2" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$LUPREC" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "4" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SKETCHINC" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$FILLETRAD" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.5" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$AUNITS" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$AUPREC" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$MENU" << endl;
	os << setw(WSIZE_3) << "1" << endl;
	os << "acad" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$ELEVATION" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PELEVATION" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$THICKNESS" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$LIMCHECK" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$BLIPMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$CHAMFERA" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.5" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$CHAMFERB" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.5" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SKPOLY" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
/*	os << setw(WSIZE_3) << "9" << endl;
	os << "$TDCREATE" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "2451445.584356944" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$TDUPDATE" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "2451445.584356944" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$TDINDWG" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0000000000" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$TDUSRTIMER" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0000000000" << endl;*/
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USRTIMER" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$ANGBASE" << endl;
	os << setw(WSIZE_3) << "50" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$ANGDIR" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PDMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "34" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PDSIZE" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.875" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PLINEWID" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$COORDS" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SPLFRAME" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SPLINETYPE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "6" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SPLINESEGS" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "8" << endl;
	// here
	os << setw(WSIZE_3) << "9" << endl;
	os << "$ATTDIA" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$ATTREQ" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$HANDLING" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$HANDSEED" << endl;
	os << setw(WSIZE_3) << "5" << endl;
	os << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SURFTAB1" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "6" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SURFTAB2" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "6" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SURFTYPE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "6" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SURFU" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "6" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SURFV" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "6" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$UCSNAME" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$UCSORG" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$UCSXDIR" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "1.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$UCSYDIR" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "1.0" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "0.0" << endl;
	// bis here
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PUCSNAME" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PUCSORG" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PUCSXDIR" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "1.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PUCSYDIR" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "1.0" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "0.0" << endl;
	// bis here
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USERI1" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USERI2" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USERI3" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USERI4" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USERI5" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USERR1" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USERR2" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USERR3" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USERR4" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$USERR5" << endl;
	os << setw(WSIZE_3) << "40" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$WORLDVIEW" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SHADEDGE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "3" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$SHADEDIF" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "70" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$TILEMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "1" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$MAXACTVP" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "16" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PINSBASE" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PLIMCHECK" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PEXTMIN" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "1.0000000000E+20" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "1.0000000000E+20" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "1.0000000000E+20" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PEXTMAX" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "-1.0000000000E+20" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "-1.0000000000E+20" << endl;
	os << setw(WSIZE_3) << "30" << endl;
	os << "-1.0000000000E+20" << endl;
	// bis here
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PLIMMIN" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "0.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PLIMMAX" << endl;
	os << setw(WSIZE_3) << "10" << endl;
	os << "12.0" << endl;
	os << setw(WSIZE_3) << "20" << endl;
	os << "9.0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$UNITMODE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$VISRETAIN" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PLINEGEN" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$PSLTSCALE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "0" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$TREEDEPTH" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << setw(WSIZE_6) << "3020" << endl;
	os << setw(WSIZE_3) << "9" << endl;
	os << "$DWGCODEPAGE" << endl;
	os << setw(WSIZE_3) << "3" << endl;
	os << "iso8859-1" << endl;
	os << setw(WSIZE_3) << "0" << endl;
	os << "ENDSEC" << endl;
	os << setw(WSIZE_3) << "0" << endl;
	os << "SECTION" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << "TABLES" << endl;
	os << setw(WSIZE_3) << "0" << endl;
	os << "TABLE" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << "LTYPE" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << zn.m_ltypes.GetCount() << endl;
	pos = zn.m_ltypes.GetHeadPosition();
	while(pos!=NULL)
	{
		lt = zn.m_ltypes.GetNext(pos);
		os << *lt;
	}
	os << setw(WSIZE_3) << "0" << endl;
	os << "ENDTAB" << endl;
	os << setw(WSIZE_3) << "0" << endl;
	os << "TABLE" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << "LAYER" << endl;
	os << setw(WSIZE_3) << "70" << endl;
	os << zn.m_layers.GetCount() << endl;
	pos = zn.m_layers.GetStartPosition();
	while(pos!=NULL)
	{
		zn.m_layers.GetNextAssoc(pos, name, ly);
		os << setw(WSIZE_3) << "0" << endl;
		os << "LAYER" << endl;
		os << setw(WSIZE_3) << "2" << endl;
		ly->GetName(str);
		os << str << endl;
		os << setw(WSIZE_3) << "70" << endl;
		os << "64" << endl;
		os << setw(WSIZE_3) << "62" << endl;
		os << "7" << endl;
		os << setw(WSIZE_3) << "6" << endl;
		ly->GetLType(str);
		os << str << endl;
	}
	os << setw(WSIZE_3) << "0" << endl;
	os << "ENDTAB" << endl;
	os << setw(WSIZE_3) << "0" << endl;
	os << "ENDSEC" << endl;
	os << setw(WSIZE_3) << "0" << endl;
	os << "SECTION" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << "BLOCKS" << endl;
	pos = zn.m_layers.GetStartPosition();
	while(pos!=NULL)
	{
		zn.m_layers.GetNextAssoc(pos, name, ly);
		ly->m_outputType = DXF_TYPE_BLOCK;
		os << *ly;
	}
	os << setw(WSIZE_3) << "0" << endl;
	os << "ENDSEC" << endl;
	os << setw(WSIZE_3) << "0" << endl;
	os << "SECTION" << endl;
	os << setw(WSIZE_3) << "2" << endl;
	os << "ENTITIES" << endl;
	pos = zn.m_layers.GetStartPosition();
	while(pos!=NULL)
	{
		zn.m_layers.GetNextAssoc(pos, name, ly);
		ly->m_outputType = DXF_TYPE_ENTITY;
		os << *ly;
	}
	os << setw(WSIZE_3) << "0" << endl;
	os << "ENDSEC" << endl;
	os << setw(WSIZE_3) << "0" << endl;
	os << "EOF" << endl;
	return os;
}

istream& operator>>(istream& is, CDXFZeichnung &zn)
{
	int key, code = 0;
	char buffer[100];
	CString str;

	while (code!=99 && !is.eof())
	{
		is >> code;
		is.getline(buffer, 100, '\n');
		switch (code)
		{
			case 0:
				is.getline(buffer, 100, '\n');	// SECTION
				is.getline(buffer, 100, '\n');	// 2
				is.getline(buffer, 100, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				key = 0;
				if (str=="HEADER")
					key = 1;
				else if (str=="BLOCKS")
					key = 2;
				else if (str=="ENTITIES")
					key = 3;
				else if (str=="ENDOF")
					key = 4;
				switch (key)
				{
					case 1:
						zn.LoadHeader(is);
						break;

					case 2:
						zn.LoadBlocks(is);
						break;

					case 3:
						zn.LoadEntities(is);
						break;

					case 4:
						code = 99;
						break;

					default:
						is.getline(buffer, 100, '\n');
						break;
				}
				is.getline(buffer, 100, '\n');	// 0
				is.getline(buffer, 100, '\n');	// ENDSEC
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}

	return is;
}

void CDXFZeichnung::LoadHeader(istream& is)
{
	int key, code = 1;
	char buffer[100];
	CString str;
	
	while (code!=99 && !is.eof())
	{
		is >> code;
		if (code==0)
		{
			is.putback('0');
			code = 99;
			break;
		}
		is.getline(buffer, 100, '\n');
		switch (code)
		{
			case 0:
				is.putback('0');
				code = 99;
				break;
		
			case 9:
				is.getline(buffer, 100, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				key = 0;
				if (str=="$LIMMIN")
					key = 1;
				else if (str=="$LIMMAX")
					key = 2;
				switch (key)
				{
					case 1:
						is.getline(buffer, 100, '\n');	// 10
						is.getline(buffer, 100, '\n');
						is.getline(buffer, 100, '\n');	// 20
						is.getline(buffer, 100, '\n');
						break;

					case 2:
						is.getline(buffer, 100, '\n');	// 10
						is >> xblatt;
						is.getline(buffer, 100, '\n');
						is.getline(buffer, 100, '\n');	// 20
						is >> yblatt;
						is.getline(buffer, 100, '\n');
						break;

					default:
						is.getline(buffer, 100, '\n');
						break;
				}
				break;

			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}
}

void CDXFZeichnung::LoadBlocks(istream& is)
{
	CDXFBlock *bl;
	CDXFLayer *ly;
	char buffer[100];
	int key, code = 1;
	CString str;

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
				if (str=="ENDSEC")
					key = 1;
				else if (str=="BLOCK")
					key = 2;
				switch (key)
				{
					case 1:
						code = 99;
						break;

					case 2:
						ly = new CDXFLayer(this);
						bl = new CDXFBlock(ly);
						is >> *bl;
						ly = bl->GetOwner();
						ly->AddBlock(bl);
						m_pLastBlockLoaded = bl;
						ly->GetName(str);
						SetLayer(str, ly);
						break;

					default:
						is.getline(buffer, 100, '\n');
						break;
				}
				break;
		
			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}
}

void CDXFZeichnung::LoadEntities(istream& is)
{
	CDXFLayer *ly;
	CDXFLinie *ln;
	CDXFPolylinie *pl;
	CDXFKreis *kr;
	CDXFText *tx;
	CDXFBlockInsert *bi;
	char buffer[100];
	int key, code = 1;
	CString str;

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
				if (str=="ENDSEC")
					key = 1;
				else if (str=="LINE")
					key = 2;
				else if (str=="POLYLINE")
					key = 3;
				else if (str=="CIRCLE")
					key = 4;
				else if (str=="TEXT")
					key = 5;
				else if (str=="INSERT")
					key = 6;
				switch (key)
				{
					case 1:
						code = 99;
						break;

					case 2:
						ly = new CDXFLayer(this);
						ln = new CDXFLinie(ly);
						is >> *ln;
						ly = ln->GetOwner();
						ly->AddLinie(ln);
						ly->GetName(str);
						SetLayer(str, ly);
						break;

					case 3:
						ly = new CDXFLayer(this);
						pl = new CDXFPolylinie(ly);
						is >> *pl;
						ly = pl->GetOwner();
						ly->AddPolylinie(pl);
						ly->GetName(str);
						SetLayer(str, ly);
						break;

					case 4:
						ly = new CDXFLayer(this);
						kr = new CDXFKreis(ly);
						is >> *kr;
						ly = kr->GetOwner();
						ly->AddKreis(kr);
						ly->GetName(str);
						SetLayer(str, ly);
						break;

					case 5:
						ly = new CDXFLayer(this);
						tx = new CDXFText(ly);
						is >> *tx;
						ly = tx->GetOwner();
						ly->AddText(tx);
						ly->GetName(str);
						SetLayer(str, ly);
						break;

					case 6:
						ly = new CDXFLayer(this);
						bi = new CDXFBlockInsert(ly);
						is >> *bi;
						ly = bi->GetOwner();
						ly->AddBlockInsert(bi);
						ly->GetName(str);
						SetLayer(str, ly);
						break;

					default:
						break;
				}
				break;
		
			default:
				is.getline(buffer, 100, '\n');
				break;
		}
	}
}

void CDXFZeichnung::SetSize(double xb, double yb, double xo, double yo)
{
	xblatt = xb;
	yblatt = yb;
	xxof = xo;
	yyof = yo;
}

void CDXFZeichnung::SetParams(double sc, double rs, int k, int i, double xa, double ya)
{
	scale = sc;
	rseite = rs;
	km = k;
	im = i;
	xadd = xa;
	yadd = ya;
}

void CDXFZeichnung::SetText(CString& kpfpath, CString& CDXFpath)
{
	m_kpfpath = kpfpath;
	m_CDXFpath = CDXFpath;
}

///////////////////////////////////////////////////////////////
///////////	CDXFLayer List Routines
///////////////////////////////////////////////////////////////

void CDXFZeichnung::SetLayer( const CString& name, CDXFLayer* ly)
{
	m_layers.SetAt(name, ly);
}

CDXFLayer* CDXFZeichnung::FindLayer( const CString& name )
{
	CDXFLayer *ly;

	if (m_layers.Lookup(name, ly))
		return ly;
	else
		return NULL;
}

void CDXFZeichnung::AddLType(CDXFLType* lt)
{
	m_ltypes.AddTail(lt);
}

void CDXFZeichnung::SetDefaultLTypes()
{
	CDXFLType *lt;
	int i, j, k;

	static CString names[25] = 
	{
		"CONTINUOUS",
		"RAND",
		"RAND2",
		"RANDX2",
		"MITTE",
		"MITTE2",
		"MITTEX2",
		"STRICHPUNKT",
		"STRICHPUNKT2",
		"STRICHPUNKTX2",
		"GESTRICHELT",
		"GESTRICHELT2",
		"GESTRICHELTX2",
		"GETRENNT",
		"GETRENNT2",
		"GETRENNTX2",
		"PUNKT",
		"PUNKT2",
		"PUNKTX2",
		"VERDECKT",
		"VERDECKT2",
		"VERDECKTX2",
		"PHANTOM",
		"PHANTOM2",
		"PHANTOMX2"
	};
	
	static CString descs[25] = 
	{
		"_______________________________________________",
		"__ __ . __ __ . __ __ . __ __ . __ __ . __ __ .",
		"__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__",
		"____  ____  .  ____  ____  .  ____  ____  .  __",
		"____ _ ____ _ ____ _ ____ _ ____ _ ____ _ ____ ",
		"___ _ ___ _ ___ _ ___ _ ___ _ ___ _ ___ _ ___ _",
		"________  __  ________  __  ________  __  _____",
		"__ . __ . __ . __ . __ . __ . __ . __ . __ . __",
		"_._._._._._._._._._._._._._._._._._._._._._._._",
		"____  .  ____  .  ____  .  ____  .  ____  .  __",
		"__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __",
		"_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _",
		"____  ____  ____  ____  ____  ____  ____  ____ ",
		"____ . . ____ . . ____ . . ____ . . ____ . . __",
		"__..__..__..__..__..__..__..__..__..__..__..__.",
		"________  .  .  ________  .  .  ________  .  .",
		". . . . . . . . . . . . . . . . . . . . . . . .",
		"...............................................",
		".  .  .  .  .  .  .  .  .  .  .  .  .  .  .  . ",
		"__ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __",
		"_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _",
		"____ ____ ____ ____ ____ ____ ____ ____ ____ __",
		"______  __  __  ______  __  __  ______  __  __",
		"___ _ _ ___ _ _ ___ _ _ ___ _ _ ___ _ _ ___ _ _",
		"____________    ____    ____    ____________   "
	};

	static double data[25*2+2*9+4*6+6*9+1] =
	{
		0,	0.0,
		6,	44.45,	12.7,	-6.35,	12.7,	-6.35,	0.0,	-6.35,
		6,	22.225,	6.35,	-3.175,	6.35,	-3.175,	0.0,	-3.175,
		6,	88.9,	25.4,	-12.7,	25.4,	-12.7,	0.0,	-12.7,
		4,	50.8,	31.75,	-6.35,	6.35,	-6.35,
		4,	28.575,	19.05,	-3.175,	3.175,	-3.175,
		4,	101.6,	63.5,	-12.7,	12.7,	-12.7,
		4,	25.4,	12.7,	-6.35,	0.0,	-6.35,
		4,	12.7,	6.35,	-3.175,	0.0,	-3.175,
		4,	50.8,	25.4,	-12.7,	0.0,	-12.7,
		2,	19.05,	12.7,	-6.35,
		2,	9.525,	6.35,	-3.175, // ,	9.525
		2,	38.1,	25.4,	-12.7,
		6,	31.75,	12.7,	-6.35,	0.0,	-6.35,	0.0,	-6.35,
		6,	15.875,	6.35,	-3.175,	0.0,	-3.175,	0.0,	-3.175,
		6,	63.5,	25.4,	-12.7,	0.0,	-12.7,	0.0,	-12.7,
		2,	6.35,	0.0,	-6.35,
		2,	3.175,	0.0,	-3.175,
		2,	12.7,	0.0,	-12.7,
		2,	9.525,	6.35,	-3.175,
		2,	4.7625,	3.175,	-1.5875,
		2,	19.05,	12.7,	-6.35,
		6,	63.5,	31.75,	-6.35,	6.35,	-6.35,	6.35,	-6.35,
		6,	31.75,	15.875,	-3.175,	3.175,	-3.175,	3.175,	-3.175,
		6,	127.0,	63.5,	-12.7,	12.7,	-12.7,	12.7,	-12.7
	};

	j = 0;
	for (i=0; i<25; i++)
	{
		lt = new CDXFLType(this);
		CArray<double, double> sl;

		lt->Set(names[i], descs[i], (int)data[j], data[j+1]);
		for (k=0; k<(int)data[j]; k++)
			sl.Add(data[j+2+k]);
		lt->SetStrich(sl);
		AddLType(lt);
		j = j+2+(int)data[j];
	}
}