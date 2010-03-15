#include "stdAfx.h"

#include "batch.h"
#include "interp.h"

#include "interpol.h"

  ////////////////////////////
  //  Klasse  Interpolation
  ///////////////////////////

#define SHIFT_RIGHT		0
#define SHIFT_LOWPOINT	1
#define SHIFT_LEFT		2

#define FLAG_PROF1		0x01
#define FLAG_PROF2		0x02

/* The Default Constructor */
Interpolation::Interpolation(Project* pProject)
{
	int i, j;

	m_pProject = pProject;
	for (i=0; i<2; i++)
	{
		m_pSrc[i] = NULL;
		m_pCS[i] = NULL;
		for (j=0; j<3; j++)
		{
			m_pSrcSec[i][j] = NULL;
			m_pNewSec[j] = NULL;
		}
	}
}

Interpolation::~Interpolation()
{
	DeleteContents();
}

void Interpolation::DeleteContents()
{
	int i, j;

	for (i=0; i<2; i++)
	{
		for (j=0; j<3; j++)
		{
			if (m_pSrcSec[i][j]!=NULL)
			{
				delete m_pSrcSec[i][j];
				m_pSrcSec[i][j] = NULL;
			}
		}
		if (m_pCS[i]!=NULL)
			m_pCS[i]->FlushProfil();
	}
	for (i=0; i<3; i++)
	{
		if (m_pNewSec[i]!=NULL)
		{
			delete m_pNewSec[i];
			m_pNewSec[i] = NULL;
		}
	}
}

BOOL Interpolation::Initialize(CrossSection* src1, CrossSection* src2)
{
//	DataBlock *db;
//	Coord *crd;
	int i;
	
	if (src1->GetStation()<src2->GetStation())
	{
		m_pCS[0] = src1;
		m_pCS[1] = src2;
	}
	else
	{
		m_pCS[1] = src1;
		m_pCS[0] = src2;
	}
	for (i=0; i<2; i++)
	{
		m_dSrcSt[i] = m_pCS[i]->GetStation();
		m_pCS[i]->LoadProfil();
		m_pSrc[i] = m_pCS[i]->GetProfil();
		// find xmin of original source profils
/*		m_orgxmin[i] = 1e36;
		db = m_pSrc[i]->GetFirstDataBlock();
		while (db!=NULL)
		{
			if (db->GetType()==DST_GELAENDEHOEHE)
			{
				crd = db->GetFirstCoord();
				while (crd!=NULL)
				{
					m_orgxmin[i] = min(m_orgxmin[i], crd->dx);
					crd = db->GetNextCoord();
				}
			}
			db = m_pSrc[i]->GetNextDataBlock();
		}*/
		// now shift profil to low point
		if (!ShiftProfil(m_pSrc[i], SHIFT_LOWPOINT, 0, TRUE))
			return FALSE;
	}
	if (!SplitSources())
		return FALSE;
	return TRUE;
}

BOOL Interpolation::ShiftProfil(Profil* prof, int shift, double factor /*=0*/, BOOL bErrors /*=FALSE*/)
{
	DataBlock *db;
	Coord *crd;
	double xmin, xmax, ymin, xofymin;
	double xshift;
	
	ASSERT(prof!=NULL);

	xmin = ymin = 1e36;
	xmax = -xmin;
	xofymin = 0;
	// find xmin, ymin and xofymin
	db = prof->GetFirstDataBlock();
	while (db!=NULL)
	{
		double xlast = -1e36;

		if (db->GetType()==DST_GELAENDEHOEHE)
		{
			crd = db->GetFirstCoord();
			while (crd!=NULL)
			{
				xmin = min(xmin, crd->dx);
				xmax = max(xmax, crd->dx);
				if (bErrors && crd->dx<xlast)
				{	// Ruecksprung!!!
					CString str;
					
					str.LoadString(IDS_BACKSPRING);
					AfxMessageBox(str, MB_OK | MB_ICONSTOP);
					theApp.m_strErrors.Add(str);
					return FALSE;
				}
				if (bErrors && crd->dx==xlast)
				{	// vertikale Wand!!!
					CString str;
					
					str.LoadString(IDS_VERTICAL);
					AfxMessageBox(str, MB_OK | MB_ICONSTOP);
					theApp.m_strErrors.Add(str);
					return FALSE;
				}
				if (crd->dy<ymin)
				{
					ymin = crd->dy;
					xofymin = crd->dx;
				}
				xlast = crd->dx;
				crd = db->GetNextCoord();
			}
		}
		db = prof->GetNextDataBlock();
	}
	switch(shift)
	{
		case SHIFT_RIGHT:
			xshift = xmax-factor;
			break;

		case SHIFT_LOWPOINT:
			xshift = xofymin;
			break;

		case SHIFT_LEFT:
			xshift = xmin-factor;
			break;

		default:
			ASSERT(FALSE);
	}
	// shift all coords
	db = prof->GetFirstDataBlock();
	while (db!=NULL)
	{
		crd = db->GetFirstCoord();
		while (crd!=NULL)
		{
			crd->dx -= xshift;
			if (bErrors)
			{	// During initialization we want to round off to the fourth decimal.
				// This avoids two equal numbers being regarded as unequal due
				// to rounding errors in floating point routines.
				CString temp;
				temp.Format("%.4f", crd->dx);
				crd->dx = atof(temp);
			}
			crd = db->GetNextCoord();
		}
		db = prof->GetNextDataBlock();
	}
	return TRUE;
}

BOOL Interpolation::SplitSources()
{
	int i, j;
	
	for (i=0; i<2; i++)
	{
		if (m_pSrc[i]!=NULL)
		{
			for (j=0; j<3; j++)
			{
				if (!ExtractSection(i, j))
					return FALSE;
				ShiftProfil(m_pSrcSec[i][j], j);
				SetSectionLimits(i, j);
			}
		}
	}
	for (i=0; i<3; i++)
		UnifySections(i);

	return TRUE;
}

BOOL Interpolation::SplitProfil(Profil* prof)
{
	int j;
	
	if (prof!=NULL)
	{
		m_pSrc[0] = prof;
		for (j=0; j<3; j++)
		{
			if (!ExtractSection(0, j))
				return FALSE;
			SetSectionLimits(0, j);
		}
	}

	return TRUE;
}

BOOL Interpolation::ExtractSection(int nSrc, int nType)
{
	DataBlock *db, *newDB, *dbg = NULL;
	DataBlock *tf;
	Coord *crd, *newCrd, *crd2;
	double tf1, tf2;

	if (m_pSrcSec[nSrc][nType]!=NULL)
		delete m_pSrcSec[nSrc][nType];
	m_pSrcSec[nSrc][nType] = new Profil(NULL);
	// first get trennflaechen (if any)
	tf = m_pSrc[nSrc]->GetDataBlock(DST_TRENNFLAECHEN);
	if (tf!=NULL)
	{
		crd = tf->GetFirstCoord();
		if (crd!=NULL)
		{
			tf1 = crd->dx;
			crd = tf->GetNextCoord();
			if (crd!=NULL)
			{
				tf2 = crd->dx;
			}
			else
				tf = NULL;
		}
		else
			tf = NULL;
	}

	db = m_pSrc[nSrc]->GetFirstDataBlock();
	while (db!=NULL)
	{	// split Gelaendehoehe
		switch(db->GetType())
		{
			case DST_GELAENDEHOEHE:
				newDB = new DataBlock(m_pSrcSec[nSrc][nType]);
				newDB->SetType(db->GetType());
				switch (nType)
				{
					case VORLAND_LINKS:
						if (tf!=NULL)
						{
							crd = db->GetFirstCoord();
							while (crd!=NULL)
							{
								if (crd->dx<=tf1)
								{
									newCrd = new Coord;
									*newCrd = *crd;
									newDB->AddCoord(newCrd);
								}
								crd = db->GetNextCoord();
							}
						}
						else
						{
							crd = db->GetFirstCoord();
							if (crd!=NULL)
							{
								newCrd = new Coord;
								*newCrd = *crd;
								newDB->AddCoord(newCrd);
							}
						}
						break;
				
					case FLUSS:
						if (tf!=NULL)
						{
							crd = db->GetFirstCoord();
							while (crd!=NULL)
							{
								if (crd->dx>=tf1 && crd->dx<=tf2)
								{
									newCrd = new Coord;
									*newCrd = *crd;
									newDB->AddCoord(newCrd);
								}
								crd = db->GetNextCoord();
							}
						}
						else
						{
							crd = db->GetFirstCoord();
							while (crd!=NULL)
							{
								newCrd = new Coord;
								*newCrd = *crd;
								newDB->AddCoord(newCrd);
								crd = db->GetNextCoord();
							}
						}
						break;
				
					case VORLAND_RECHTS:
						if (tf!=NULL)
						{
							crd = db->GetFirstCoord();
							while (crd!=NULL)
							{
								if (crd->dx>=tf2)
								{
									newCrd = new Coord;
									*newCrd = *crd;
									newDB->AddCoord(newCrd);
								}
								crd = db->GetNextCoord();
							}
						}
						else
						{
							crd = db->GetLastCoord();
							if (crd!=NULL)
							{
								newCrd = new Coord;
								*newCrd = *crd;
								newDB->AddCoord(newCrd);
							}
						}
						break;
				}
				m_pSrcSec[nSrc][nType]->AddDataBlock(newDB);
				dbg = newDB;
				break;

			default:
				break;
		}
		db = m_pSrc[nSrc]->GetNextDataBlock();
	}
	if (dbg!=NULL)
	{	// split the rest (making sure the coords are identical to Gelaendehoehe)
		db = m_pSrc[nSrc]->GetFirstDataBlock();
		while (db!=NULL)
		{
			switch(db->GetType())
			{
				case DST_RAUHIGKEIT:
				case DST_RAUHIGKEIT_KST:
					newDB = new DataBlock(m_pSrcSec[nSrc][nType]);
					newDB->SetType(db->GetType());
					crd = dbg->GetFirstCoord();
					while (crd!=NULL)
					{
						Coord *lastCrd = NULL;

						int index = db->GetCoordIndex(crd->dx);
						if (index!=-1)
						{
							crd2 = db->GetCoordAt(index);
							newCrd = new Coord;
							*newCrd = *crd2;
							newDB->AddCoord(newCrd);
						}
						else
						{
							crd2 = db->GetFirstCoord();
							while (crd2!=NULL)
							{
								if (crd2->dx>crd->dx)
								{
									if (lastCrd==NULL)
									{
										newCrd = new Coord;
										newCrd->dx = crd->dx;
										newCrd->dy = crd2->dy;
										newDB->AddCoord(newCrd);
										break;
									}
									else if (lastCrd->dx<crd->dx)
									{
										newCrd = new Coord;
										newCrd->dx = crd->dx;
										newCrd->dy = lastCrd->dy-(lastCrd->dy-crd2->dy)*(crd->dx-lastCrd->dx)/(crd2->dx-lastCrd->dx);
										newDB->AddCoord(newCrd);
										break;
									}
								}
								lastCrd = crd2;
								crd2 = db->GetNextCoord();
								if (crd2==NULL)
								{
									newCrd = new Coord;
									newCrd->dx = crd->dx;
									newCrd->dy = lastCrd->dy;
									newDB->AddCoord(newCrd);
								}
							}
						}
						crd = dbg->GetNextCoord();
					}
					m_pSrcSec[nSrc][nType]->AddDataBlock(newDB);
					break;
				
				default:
					break;
			}
			db = m_pSrc[nSrc]->GetNextDataBlock();
		}
	}
	else
	{
		CString str = m_pCS[nSrc]->GetFileName();
		str.FormatMessage(IDS_ERROR_NOGELAENDE, str);
		theApp.m_strErrors.Add(str);
		return FALSE;
	}

	return TRUE;
}

void Interpolation::SetSectionLimits(int nSrc, int nType)
{
	DataBlock *db;
	Coord *crd;
	
	ASSERT(m_pSrcSec[nSrc][nType]!=NULL);

	m_xmin[nSrc][nType] = 1e36;
	m_xmax[nSrc][nType] = -1e36;
	m_ymin[nSrc][nType] = 1e36;

	db = m_pSrcSec[nSrc][nType]->GetFirstDataBlock();
	while (db!=NULL)
	{
		if (db->GetType()==DST_GELAENDEHOEHE)
		{
			crd = db->GetFirstCoord();
			while (crd!=NULL)
			{
				m_xmin[nSrc][nType] = min(m_xmin[nSrc][nType], crd->dx);
				m_xmax[nSrc][nType] = max(m_xmax[nSrc][nType], crd->dx);
				m_ymin[nSrc][nType] = min(m_ymin[nSrc][nType], crd->dy);
				crd = db->GetNextCoord();
			}
		}
		db = m_pSrcSec[nSrc][nType]->GetNextDataBlock();
	}

}

void Interpolation::UnifySections(int nType)
{
	DataBlock *db;
	Coord *crd, *newCrd;
	CList<double, double> m_xcoords[N_DSTYPES];
	POSITION pos;
	double xmin, xmax, xcoord;
	int i;

	ASSERT(m_pSrcSec[0][nType]!=NULL);
	ASSERT(m_pSrcSec[1][nType]!=NULL);

	// get common range
	xmin = max(m_xmin[0][nType], m_xmin[1][nType]);
	xmax = min(m_xmax[0][nType], m_xmax[1][nType]);
	// for each DataBlock make a list of all points
	for (i=0; i<2; i++)
	{
		db = m_pSrcSec[i][nType]->GetFirstDataBlock();
		while (db!=NULL)
		{
			crd = db->GetFirstCoord();
			while (crd!=NULL)
			{
				m_xcoords[db->GetType()].AddTail(crd->dx);
				crd = db->GetNextCoord();
			}
			db = m_pSrcSec[i][nType]->GetNextDataBlock();
		}
	}
	// increase point count in common range to match union
	for (i=0; i<2; i++)
	{
		db = m_pSrcSec[i][nType]->GetFirstDataBlock();
		while (db!=NULL)
		{
			pos = m_xcoords[db->GetType()].GetHeadPosition();
			while (pos!=NULL)
			{
				xcoord = m_xcoords[db->GetType()].GetNext(pos);
				if (xcoord>=xmin && xcoord<=xmax)
				{
					double x1, y1;
					
					x1 = y1 = 1e36;
					crd = db->GetFirstCoord();
					while (crd!=NULL)
					{
						if (xcoord>x1 && xcoord<crd->dx)
						{	// insert here
							double x2, y2;
							
							x2 = crd->dx;
							y2 = crd->dy;
							newCrd = new Coord;
							newCrd->dx = xcoord;
							if (x2==x1)
								newCrd->dy = y2;
							else
								newCrd->dy = y1-(y1-y2)*(xcoord-x1)/(x2-x1);
							db->InsertCoordAt(db->GetCurrentCoordIndex()-1, newCrd);
						}
						x1 = crd->dx;
						y1 = crd->dy;
						crd = db->GetNextCoord();
					}
				}
			}
			db = m_pSrcSec[i][nType]->GetNextDataBlock();
		}
	}
}

BOOL Interpolation::Interpolate( CrossSection* cs )
{
	m_pNew = cs;
	m_dNewSt = cs->GetStation();

	ASSERT( m_pCS[0] != NULL && m_pCS[1] != NULL );
	ASSERT( m_dNewSt > m_dSrcSt[0] && m_dNewSt < m_dSrcSt[1] );

	m_pNew->FlushProfil();
	
  CreateNewSections();
	
  if( !InterpolateNewSections() )
		return FALSE;
	
  CombineNewSections();
	
  if( !LimitPointCount() )
		return FALSE;
	
  return TRUE;
}

BOOL Interpolation::DoProfilShift(CrossSection *cs, Batch& batch, int i)
{
	double dNewBed, dNewLeft, dNewRight;
	DataBlock *db, *dbgl, *dbgf, *dbgr;
	Profil *prof = cs->GetProfil();
	ASSERT(prof!=NULL);
	int index, j;
	CString str;

	SplitProfil(prof);
	dNewBed = batch.GetBedHeight(i);
	if (batch.m_bWholeProfile)	// gesamte Profil wird verschoben (benutz nur neue Sohlhöhe)
  {
		VertShift(prof, dNewBed-m_ymin[0][1]);
    prof->SetModified();
  }
	else	// Flußschlauch und Vorland werden getrennt verschoben
	{
		dbgl = m_pSrcSec[0][0]->GetDataBlock(DST_GELAENDEHOEHE);
		dbgf = m_pSrcSec[0][1]->GetDataBlock(DST_GELAENDEHOEHE);
		dbgr = m_pSrcSec[0][2]->GetDataBlock(DST_GELAENDEHOEHE);
		if (dbgl!=NULL && dbgf!=NULL && dbgr!=NULL)
		{
			Coord *pCrd;
			Coord *pCrdL, *pCrdR;
			double diff;

			// Flussschlauch verschieben
			VertShift(m_pSrcSec[0][1], dNewBed-m_ymin[0][1]);
			db = prof->GetDataBlock(DST_BORDVOLL);
			if (db!=NULL)
			{
				// versuch Vorlandlinks zu verschieben
				pCrd = db->GetFirstCoord();
				index = dbgl->GetCoordIndex(pCrd->dx);
				if (index!=-1)
				{
					pCrdL = dbgl->GetCoordAt(index);
					if (!batch.GetLBankString(i).IsEmpty())
					{
						dNewLeft = batch.GetLBankHeight(i);
						VertShift(m_pSrcSec[0][0], dNewLeft-pCrdL->dy);
					}
				}
				else
				{
					str.LoadString(IDS_BV1_NOTINLEFT);
					theApp.m_strErrors.Add(str);
				}

				// versuch Vorlandrechts zu verschieben
				pCrd = db->GetLastCoord();
				index = dbgr->GetCoordIndex(pCrd->dx);
				if (index!=-1)
				{
					pCrdR = dbgr->GetCoordAt(index);
					if (!batch.GetRBankString(i).IsEmpty())
					{
						dNewRight = batch.GetRBankHeight(i);
						VertShift(m_pSrcSec[0][2], dNewRight-pCrdR->dy);
					}
				}
				else
				{
					str.LoadString(IDS_BV2_NOTINRIGHT);
					theApp.m_strErrors.Add(str);
				}
			
				// versuch Korrektur für Punkte zwischen Bordvoll und Trennflaechen...
				// (nur wenn Bordvollpunkt in Vorland liegt!)
				// Vorlandlinkskorrektur
				pCrdL = db->GetFirstCoord();
				pCrdR = dbgf->GetFirstCoord();
				pCrd = dbgl->GetLastCoord();
				diff = pCrd->dy-pCrdR->dy;
				if (pCrdL!=NULL && pCrdR!=NULL)
				{
					index = dbgl->GetCoordIndex(pCrdL->dx);
					if (index !=-1)
					{
						for (j=index+1; j<dbgl->GetNumCoords(); j++)
						{
							pCrd = dbgl->GetCoordAt(j);
							pCrd->dy -= diff*(pCrd->dx-pCrdL->dx)/(pCrdR->dx-pCrdL->dx);
						}
					}
				}
				
				// Vorlandlinkskorrektur
				pCrdR = db->GetLastCoord();
				pCrdL = dbgf->GetLastCoord();
				pCrd = dbgr->GetFirstCoord();
				diff = pCrd->dy-pCrdL->dy;
				if (pCrdL!=NULL && pCrdR!=NULL)
				{
					index = dbgr->GetCoordIndex(pCrdR->dx);
					if (index !=-1)
					{
						for (j=index-1; j>=0; j--)
						{
							pCrd = dbgr->GetCoordAt(j);
							pCrd->dy -= diff*(pCrdR->dx-pCrd->dx)/(pCrdR->dx-pCrdL->dx);
						}
					}
				}
			}
			else
			{
				str.LoadString(IDS_NOBORDVOLL);
				theApp.m_strErrors.Add(str);
			}

			// neue Punkthoehen in Urprofil übertragen
			db = prof->GetDataBlock(DST_GELAENDEHOEHE);
			prof->SetModified();
			if (db!=NULL)
			{
				Coord *pOldCrd, *pNewCrd;
				
				pOldCrd = db->GetFirstCoord();
				while (pOldCrd!=NULL)
				{
					index = dbgf->GetCoordIndex(pOldCrd->dx);
					if (index!=-1)
					{
						pNewCrd = dbgf->GetCoordAt(index);
						pOldCrd->dy = pNewCrd->dy;
						pOldCrd = db->GetNextCoord();
						continue;
					}
					index = dbgl->GetCoordIndex(pOldCrd->dx);
					if (index!=-1)
					{
						pNewCrd = dbgl->GetCoordAt(index);
						pOldCrd->dy = pNewCrd->dy;
						pOldCrd = db->GetNextCoord();
						continue;
					}
					index = dbgr->GetCoordIndex(pOldCrd->dx);
					if (index!=-1)
					{
						pNewCrd = dbgr->GetCoordAt(index);
						pOldCrd->dy = pNewCrd->dy;
						pOldCrd = db->GetNextCoord();
						continue;
					}
					pOldCrd = db->GetNextCoord();
				}
			}
		}
	}

	return TRUE;
}

void Interpolation::CreateNewSections()
{
	DataBlock *db, *newDB;
	Coord *crd, *newCrd;
	int i;
	double xmin, xmax;

	for (i=0; i<3; i++)
	{
		ASSERT(m_pSrcSec[0][i]!=NULL && m_pSrcSec[1][i]!=NULL);
		if (m_pNewSec[i]!=NULL)
			delete m_pNewSec[i];
		m_pNewSec[i] = new Profil(NULL);

		xmin = max(m_xmin[0][i], m_xmin[1][i]);
		xmax = min(m_xmax[0][i], m_xmax[1][i]);

		db = m_pSrcSec[0][i]->GetFirstDataBlock();
		while (db!=NULL)
		{
			switch(db->GetType())
			{
				case DST_GELAENDEHOEHE:
				case DST_RAUHIGKEIT:
				case DST_RAUHIGKEIT_KST:
					if (m_pSrcSec[1][i]->GetDataBlock(db->GetType())!=NULL)
					{
						newDB = new DataBlock(m_pNewSec[i]);
						newDB->SetType(db->GetType());
						crd = db->GetFirstCoord();
						while (crd!=NULL)
						{
							if (crd->dx>=xmin && crd->dx<=xmax)
							{
								newCrd = new Coord;
								newCrd->dx = crd->dx;
								newDB->AddCoord(newCrd);
							}
							crd = db->GetNextCoord();
						}
						m_pNewSec[i]->AddDataBlock(newDB);
					}
					break;

				default:
					break;
			}
			db = m_pSrcSec[0][i]->GetNextDataBlock();
		}
	}
}

BOOL Interpolation::InterpolateNewSections()
{
	DataBlock *db, *db1, *db2;
	Coord *crd, *crd1, *crd2, *newCrd;
	int i, j, k, index, index1, index2;
	double xmax, xmin, grad;
	CString str;

	for (i=0; i<3; i++)
	{
		ASSERT(m_pSrcSec[0][i]!=NULL && m_pSrcSec[1][i]!=NULL);
		ASSERT(m_pNewSec[i]!=NULL);

		xmin = max(m_xmin[0][i], m_xmin[1][i]);
		xmax = min(m_xmax[0][i], m_xmax[1][i]);
		db = m_pNewSec[i]->GetFirstDataBlock();
		while (db!=NULL)
		{
			if (db->GetNumCoords()==0)
			{
				db = m_pNewSec[i]->GetNextDataBlock();
				continue;
			}
			db1 = m_pSrcSec[0][i]->GetDataBlock(db->GetType());
			db2 = m_pSrcSec[1][i]->GetDataBlock(db->GetType());
			ASSERT(db1!=NULL && db2!=NULL);

			index = db->GetCoordIndex(0);
			for (j=0; j<db->GetNumCoords(); j++)
			{
				crd = db->GetCoordAt(j);
				index1 = db1->GetCoordIndex(crd->dx);
				index2 = db2->GetCoordIndex(crd->dx);
				if (index1==-1 || index2==-1)
				{
					str.FormatMessage(IDS_ERROR_INDEX, 0, i, index1, crd->dx);
					theApp.m_strErrors.Add(str);
					str.FormatMessage(IDS_ERROR_INDEX, 1, i, index2, crd->dx);
					theApp.m_strErrors.Add(str);
					return FALSE;
				}
				crd1 = db1->GetCoordAt(index1);
				crd2 = db2->GetCoordAt(index2);
				crd->dy = crd1->dy-(crd1->dy-crd2->dy)*(m_dNewSt-m_dSrcSt[0])/(m_dSrcSt[1]-m_dSrcSt[0]);
			}
			index1 = db1->GetCoordIndex(xmin);
			index2 = db2->GetCoordIndex(xmin);
			if (index1==-1 || index2==-1)
			{
				str.FormatMessage(IDS_ERROR_INDEX, 0, i, index1, xmin);
				theApp.m_strErrors.Add(str);
				str.FormatMessage(IDS_ERROR_INDEX, 1, i, index2, xmin);
				theApp.m_strErrors.Add(str);
				return FALSE;
			}
			if (index1!=0 && index2!=0)
			{
				str.FormatMessage(IDS_ERROR_INDEX, 0, i, index1, xmin);
				theApp.m_strErrors.Add(str);
				str.FormatMessage(IDS_ERROR_INDEX, 1, i, index2, xmin);
				theApp.m_strErrors.Add(str);
				return FALSE;
			}
			if (index1==0)
			{
				crd1 = db1->GetCoordAt(0);
				for (k=index2-1; k>=0; k--)
				{
					crd = db2->GetCoordAt(k+1);
					crd2 = db2->GetCoordAt(k);
					grad = (crd->dy-crd2->dy)/(crd->dx-crd2->dx);
					crd = db->GetCoordAt(0);
					newCrd = new Coord;
					newCrd->dx = crd1->dx-(crd1->dx-crd2->dx)*(m_dNewSt-m_dSrcSt[0])/(m_dSrcSt[1]-m_dSrcSt[0]);
					newCrd->dy = crd->dy-grad*(crd->dx-newCrd->dx);
					db->InsertCoordAt(0, newCrd);
				}
			}
			else
			{
				crd2 = db2->GetCoordAt(0);
				for (k=index1-1; k>=0; k--)
				{
					crd = db1->GetCoordAt(k+1);
					crd1 = db1->GetCoordAt(k);
					grad = (crd->dy-crd1->dy)/(crd->dx-crd1->dx);
					crd = db->GetCoordAt(0);
					newCrd = new Coord;
					newCrd->dx = crd1->dx-(crd1->dx-crd2->dx)*(m_dNewSt-m_dSrcSt[0])/(m_dSrcSt[1]-m_dSrcSt[0]);
					newCrd->dy = crd->dy-grad*(crd->dx-newCrd->dx);
					db->InsertCoordAt(0, newCrd);
				}
			}
			index1 = db1->GetCoordIndex(xmax);
			index2 = db2->GetCoordIndex(xmax);
			if (index1==-1 || index2==-1)
			{
				str.FormatMessage(IDS_ERROR_INDEX, 0, i, index1, xmax);
				theApp.m_strErrors.Add(str);
				str.FormatMessage(IDS_ERROR_INDEX, 1, i, index2, xmax);
				theApp.m_strErrors.Add(str);
				return FALSE;
			}
			if (index1!=db1->GetNumCoords()-1 && index2!=db2->GetNumCoords()-1)
			{
				str.FormatMessage(IDS_ERROR_INDEX, 0, i, index1, xmax);
				theApp.m_strErrors.Add(str);
				str.FormatMessage(IDS_ERROR_INDEX, 1, i, index2, xmax);
				theApp.m_strErrors.Add(str);
				return FALSE;
			}
			if (index1==db1->GetNumCoords()-1)
			{
				crd1 = db1->GetCoordAt(db1->GetNumCoords()-1);
				for (k=index2+1; k<db2->GetNumCoords(); k++)
				{
					crd = db2->GetCoordAt(k-1);
					crd2 = db2->GetCoordAt(k);
					grad = (crd2->dy-crd->dy)/(crd2->dx-crd->dx);
					crd = db->GetCoordAt(db->GetNumCoords()-1);
					newCrd = new Coord;
					newCrd->dx = crd1->dx-(crd1->dx-crd2->dx)*(m_dNewSt-m_dSrcSt[0])/(m_dSrcSt[1]-m_dSrcSt[0]);
					newCrd->dy = crd->dy-grad*(crd->dx-newCrd->dx);
					db->AddCoord(newCrd);
				}
			}
			else
			{
				crd2 = db2->GetCoordAt(db2->GetNumCoords()-1);
				for (k=index1+1; k<db1->GetNumCoords(); k++)
				{
					crd = db1->GetCoordAt(k-1);
					crd1 = db1->GetCoordAt(k);
					grad = (crd1->dy-crd->dy)/(crd1->dx-crd->dx);
					crd = db->GetCoordAt(db->GetNumCoords()-1);
					newCrd = new Coord;
					newCrd->dx = crd1->dx-(crd1->dx-crd2->dx)*(m_dNewSt-m_dSrcSt[0])/(m_dSrcSt[1]-m_dSrcSt[0]);
					newCrd->dy = crd->dy-grad*(crd->dx-newCrd->dx);
					db->AddCoord(newCrd);
				}
			}
			db = m_pNewSec[i]->GetNextDataBlock();
		}
	}
	return TRUE;
}

void Interpolation::CombineNewSections()
{
	Profil *newProf;
	DataBlock *db, *db1, *db2, *newDB;
	Coord *crd, *crd1, *crd2, *newCrd;
	int i, j;
	double xmax[3];
	double yofxmin[3];
	double yofxmax[3];
	static int nSpecials[2] =
	{
		DST_DURCHST_BEREICH,
		DST_BORDVOLL
	};
//	double shift_factor;

	newProf = new Profil(m_pNew);
	for (i=0; i<3; i++)
	{
		xmax[i] = -1e36;
		ShiftProfil(m_pNewSec[i], SHIFT_LEFT);
		db = m_pNewSec[i]->GetFirstDataBlock();
		while (db!=NULL)
		{
			crd = db->GetFirstCoord();
			while (crd!=NULL)
			{
				if (crd->dx>xmax[i])
				{
					xmax[i] = crd->dx;
					if (db->GetType()==DST_GELAENDEHOEHE)
						yofxmax[i] = crd->dy;
				}
				if (crd->dx==0)
				{
					if (db->GetType()==DST_GELAENDEHOEHE)
						yofxmin[i] = crd->dy;
				}
				crd = db->GetNextCoord();
			}
			db = m_pNewSec[i]->GetNextDataBlock();
		}
		if (i>0)
			xmax[i] += xmax[i-1];
	}
	for (i=0; i<3; i++)
	{
		db = m_pNewSec[i]->GetFirstDataBlock();
		while (db!=NULL)
		{
			BOOL bNew = FALSE;
			
			newDB = newProf->GetDataBlock(db->GetType());
			if (newDB==NULL)
			{
				newDB = new DataBlock(newProf);
				newDB->SetType(db->GetType());
				bNew = TRUE;
			}
			crd = db->GetFirstCoord();
			while (crd!=NULL)
			{
				if (i>0)
				{
					if (crd->dx==0)
					{
						crd = db->GetNextCoord();
						continue;
					}
					crd->dx += xmax[i-1];
				}
				if (db->GetType()==DST_GELAENDEHOEHE)
				{	// shift vorland y values
					if (i==0)
						crd->dy += yofxmin[1]-yofxmax[0];
					if (i==2)
						crd->dy += yofxmax[1]-yofxmin[2];
				}
				newCrd = new Coord;
				*newCrd = *crd;
				newDB->AddCoord(newCrd);
				crd = db->GetNextCoord();
			}
			if (bNew)
				newProf->AddDataBlock(newDB);
			db = m_pNewSec[i]->GetNextDataBlock();
		}
	}
	if (m_pSrc[0]->GetDataBlock(DST_TRENNFLAECHEN)!=NULL ||
		m_pSrc[1]->GetDataBlock(DST_TRENNFLAECHEN)!=NULL)
	{
		// now add Trennflaechen
		newDB = new DataBlock(newProf);
		newDB->SetType(DST_TRENNFLAECHEN);
		for (i=0; i<2; i++)
		{
			newCrd = new Coord;
			newCrd->dx = xmax[i];
			newCrd->dy = i+1;
			newDB->AddCoord(newCrd);
		}
		newProf->AddDataBlock(newDB);
	}
	m_pNew->SetProfil(newProf);
	// special cases
	for (j=0; j<2; j++)
	{
		db1 = m_pSrc[0]->GetDataBlock(nSpecials[j]);
		db2 = m_pSrc[1]->GetDataBlock(nSpecials[j]);
		if (db1!=NULL && db2!=NULL)
		{
			ShiftProfil(newProf, SHIFT_LOWPOINT);
			newDB = new DataBlock(newProf);
			newDB->SetType(nSpecials[j]);
			for (i=0; i<2; i++)
			{
				crd1 = db1->GetCoordAt(i);
				crd2 = db2->GetCoordAt(i);
				if (crd1==NULL || crd2==NULL)
				{
					delete db;
					return;
				}
				newCrd = new Coord;
				newCrd->dx = crd1->dx-(crd1->dx-crd2->dx)*(m_dNewSt-m_dSrcSt[0])/(m_dSrcSt[1]-m_dSrcSt[0]);
				// now ensure point is in existing datablocks and find gelaendehoehe
				db = newProf->GetFirstDataBlock();
				while (db!=NULL)
				{
					double x1, y1;
					int index;
					
					x1 = y1 = 1e36;
					switch(db->GetType())
					{
					case DST_GELAENDEHOEHE:
					case DST_RAUHIGKEIT:
					case DST_RAUHIGKEIT_KST:
						index = db->GetCoordIndex(newCrd->dx);
						if (index!=-1)
						{
							if (db->GetType()==DST_GELAENDEHOEHE)
							{
								crd = db->GetCoordAt(index);
								newCrd->dy = crd->dy;
							}
							break;
						}
						crd = db->GetFirstCoord();
						while (crd!=NULL)
						{
							if (newCrd->dx>x1 && newCrd->dx<crd->dx)
							{	// insert here
								double x2, y2;
								Coord *crd3;
								
								x2 = crd->dx;
								y2 = crd->dy;
								crd3 = new Coord;
								crd3->dx = newCrd->dx;
								if (x2==x1)
									crd3->dy = y2;
								else
									crd3->dy = y1-(y1-y2)*(newCrd->dx-x1)/(x2-x1);
								db->InsertCoordAt(db->GetCurrentCoordIndex()-1, crd3);
								if (db->GetType()==DST_GELAENDEHOEHE)
									newCrd->dy = crd3->dy;
								break;
							}
							x1 = crd->dx;
							y1 = crd->dy;
							crd = db->GetNextCoord();
						}
						break;
					}
					db = newProf->GetNextDataBlock();
				}
				if (nSpecials[j]==DST_BORDVOLL)
					newCrd->dy = i+1;
				newDB->AddCoord(newCrd);
			}
			newProf->AddDataBlock(newDB);
			ShiftProfil(newProf, SHIFT_LEFT);
		}
	}
	// check for repeated points when in format %.4f and remove
	CTypedPtrArray<CObArray, Coord*> removes;
	db = newProf->GetFirstDataBlock();
	while (db!=NULL)
	{
		crd1 = NULL;
		removes.RemoveAll();
		crd = db->GetFirstCoord();
		while (crd!=NULL)
		{
			if (crd1!=NULL)
			{
				if (fabs(crd1->dx-crd->dx)<0.0001)
					removes.Add(crd);
			}
			crd1 = crd;
			crd = db->GetNextCoord();
		}
		for (i=0; i<removes.GetSize(); i++)
			db->RemoveCoord(removes[i]);
		db = newProf->GetNextDataBlock();
	}

//	shift_factor = m_orgxmin[0]-(m_orgxmin[0]-m_orgxmin[1])*(m_dNewSt-m_dSrcSt[0])/(m_dSrcSt[1]-m_dSrcSt[0]);
//	ShiftProfil(newProf, SHIFT_LEFT, shift_factor);
}

BOOL Interpolation::LimitPointCount()
{
	const int count_max = 1000;  // maximale Anzahl an zugelassenen Profilpunkten
  const double limit = 0.02; // 2 Zentimeter

	Profil* newProf = m_pNew->GetProfil();
	DataBlock* db1 = newProf->GetDataBlock( DST_GELAENDEHOEHE );
	
  /*  Früher nur ausdünnen, wenn die Anzahl der Punkte zu gross ist, jetzt immer ausdünnen
  if( db1->GetNumCoords()<=count_max )
		return TRUE;
  */
	
  CTypedPtrList<CObList, Coord*> removes;
  CMapPtrToWord remove_map;
  for( int i = 1; i < db1->GetNumCoords() - 1; i++)
	{
		BOOL bOK = TRUE;
		
		Coord* crd1 = db1->GetCoordAt( i - 1 );
		Coord* crd2 = db1->GetCoordAt( i );
		Coord* crd3 = db1->GetCoordAt( i + 1 );
		double y = crd1->dy - ( crd1->dy - crd3->dy ) * ( crd2->dx - crd1->dx ) / ( crd3->dx - crd1->dx );
		if( fabs( y - crd2->dy ) > limit )
			continue;	// cannot remove this point
		
    DataBlock* db2 = newProf->GetFirstDataBlock();
		while( db2 != NULL )
		{
			if( db2->GetType() == DST_TRENNFLAECHEN && db2->GetCoordIndex( crd2->dx ) != -1 )
			{
				bOK = FALSE;	// cannot remove this point
				break;
			}
			if( db2->GetType() == DST_DURCHST_BEREICH && db2->GetCoordIndex( crd2->dx ) != -1 )
			{
				bOK = FALSE;	// cannot remove this point
				break;
			}
			db2 = newProf->GetNextDataBlock();
		}
		
    if( bOK )
		{	// remove point
			DataBlock* db2 = newProf->GetFirstDataBlock();
			while( db2 != NULL )
			{
				int index = db2->GetCoordIndex( crd2->dx );
				if( index != -1 )
				{
					removes.AddTail( db2->GetCoordAt( index ) );
					remove_map.SetAt( db2->GetCoordAt( index ), db2->GetType() );
				}
				db2 = newProf->GetNextDataBlock();
			}
		}
	}
	
  POSITION pos = removes.GetHeadPosition();
	while( pos )
	{
		Coord* crd1 = removes.GetNext( pos );
    
    WORD type;
		if( remove_map.Lookup( crd1, type ) )
		{
			DataBlock* db2 = newProf->GetDataBlock( type );
			db2->RemoveCoord( crd1 );
		}
	}
	
  if( db1->GetNumCoords() <= count_max )
		return TRUE;

  CString str( MAKEINTRESOURCE( IDS_TOOMANYPOINTS ) );
	AfxMessageBox( str, MB_OK | MB_ICONSTOP );
	theApp.m_strErrors.Add( str );
	return FALSE;
}

void Interpolation::VertShift(Profil* prof, double shift)
{
	DataBlock *db;
	Coord *pCrd;

	db = prof->GetDataBlock(DST_GELAENDEHOEHE);
	if (db!=NULL)
	{
		pCrd = db->GetFirstCoord();
		while (pCrd!=NULL)
		{
			pCrd->dy += shift;
			pCrd = db->GetNextCoord();
		}
	}
}