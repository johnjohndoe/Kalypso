// intrpsht.cpp : implementation file
//

#include "stdAfx.h"

#include "intrppg1.h"
#include "intrppg2.h"
#include "intrppg3.h"
#include "intrppg4.h"
#include "interpol.h"
#include "batch.h"
#include "interp.h"
#include "mainfrm.h"

#include "intrpsht.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CInterpSheet

IMPLEMENT_DYNAMIC(CInterpSheet, CPropertySheet)

CInterpSheet::CInterpSheet(UINT nIDCaption, CWnd* pParentWnd, UINT iSelectPage, BOOL bDemo, BOOL bBatch)
	:CPropertySheet(nIDCaption, pParentWnd, iSelectPage)
{
	m_bDemo = bDemo;
	m_bBatch = bBatch;
	if (!bBatch)
	{
		page1 = new CInterpPage1(this);
		page2 = new CInterpPage2(this);
		page3 = NULL;
		page4 = NULL;
		AddPage(page1);
		AddPage(page2);
	}
	else
	{
		page1 = NULL;
		page2 = NULL;
		page3 = new CInterpPage3(this);
		page4 = new CInterpPage4(this);
		AddPage(page3);
		AddPage(page4);
	}
	SetWizardMode();
}

CInterpSheet::CInterpSheet(LPCTSTR pszCaption, CWnd* pParentWnd, UINT iSelectPage, BOOL bDemo, BOOL bBatch)
	:CPropertySheet(pszCaption, pParentWnd, iSelectPage)
{
	m_bDemo = bDemo;
	m_bBatch = bBatch;
	if (!bBatch)
	{
		page1 = new CInterpPage1(this);
		page2 = new CInterpPage2(this);
		page3 = NULL;
		page4 = NULL;
		AddPage(page1);
		AddPage(page2);
	}
	else
	{
		page1 = NULL;
		page2 = NULL;
		page3 = new CInterpPage3(this);
		page4 = new CInterpPage4(this);
		AddPage(page3);
		AddPage(page4);
	}
	SetWizardMode();
}

CInterpSheet::~CInterpSheet()
{
	if (page1!=NULL)
		delete page1;
	if (page2!=NULL)
		delete page2;
	if (page3!=NULL)
		delete page3;
	if (page4!=NULL)
		delete page4;
}

void CInterpSheet::GetWaterName(CString& name)
{
	if (page1->sec[0]!=NULL)
		name = page1->sec[0]->GetWaterName();
	else
		name.Empty();
}

double CInterpSheet::GetMinStation()
{
	if (page1->sec[0]!=NULL && page1->sec[1]!=NULL)
		return min(page1->sec[0]->GetStation(), page1->sec[1]->GetStation());
	else
		return 0;
}

double CInterpSheet::GetMaxStation()
{
	if (page1->sec[0]!=NULL && page1->sec[1]!=NULL)
		return max(page1->sec[0]->GetStation(), page1->sec[1]->GetStation());
	else
		return 0;
}

void CInterpSheet::GetStateName(CString& name)
{
	if (page1->sec[0]!=NULL)
		name = page1->sec[0]->GetStateName();
	else
		name.Empty();
}

void CInterpSheet::Interpolate()
{
	Interpolation interp(theApp.m_pProject);
	int i;
	CrossSection *cs;
	CString str, defaultLang, temp;
	BOOL bOK = TRUE;

	if (m_bDemo)
	{
		AfxMessageBox(IDS_DEMO, MB_OK | MB_ICONEXCLAMATION);
		return;
	}
	if (!m_bBatch)
	{
		for (i=0; i<page2->m_profils.GetSize(); i++)
		{
			if (interp.Initialize(page1->sec[0], page1->sec[1]))
			{
				cs = page2->m_profils.GetAt(i);
				if (!interp.Interpolate(cs))
				{
					bOK = FALSE;
					break;
				}
				str.Format("QUERPROFIL %d", cs->GetProfilNr());
				cs->GetProfil()->SetPageDesc(1, str);
				defaultLang = setlocale(LC_NUMERIC, NULL);
				setlocale(LC_NUMERIC, "English");
				str.Format("STATION KM %.4f", cs->GetStation());
				setlocale(LC_NUMERIC, defaultLang);
				cs->GetProfil()->SetPageDesc(2, str);
				cs->SaveProfil();
				theApp.m_strFiles.Add(cs->GetFileName());
				interp.DeleteContents();
			}
			else
			{
				bOK = FALSE;
				break;
			}
		}
		if (!bOK)
		{
			CString path, rString;
			ofstream ofs;
			
			path = theApp.m_pProject->GetCalcDir();
			path += "ERROR.LOG";
			ofs.open(path, ios::out);
			if (ofs.fail())
			{
				rString.FormatMessage(IDS_ERROR_SAVEFILE, path);
				AfxMessageBox(rString, MB_ERROR);
			}
			else
			{
				for (i=0; i<theApp.m_strErrors.GetSize(); i++)
					ofs << theApp.m_strErrors[i] << endl;
				ofs.close();
			}
			((CMainFrame*)theApp.m_pMainWnd)->m_bError = TRUE;
		}
	}
	else // if !m_bBatch
	{
		CMap<int, int, int, int> usedProfilNums;
		CStringList usedFileNames;
		int dummy;

		for (i=0; i<m_batch.GetNumEntries(); i++)
		{
			CrossSection *cs1, *cs2;
			BOOL bInterp = TRUE;
			BOOL bStop = FALSE;
			BOOL bShift = FALSE;
			int profNum;

			cs1 = cs2 = NULL;
			cs = theApp.m_pState->GetFirstCrossSection();
			while (cs!=NULL && !(cs1!=NULL && bStop))
			{
				if (cs->GetStation() <= m_batch.GetStation(i) + 0.001)  // 1m Toleranz wg Rundungsfehlern
					cs1 = cs;
				else
					bStop = TRUE;
				cs = theApp.m_pState->GetNextCrossSection();
				if (!bStop)
					cs2 = cs;
			}
			if (cs1 && (fabs(cs1->GetStation() - m_batch.GetStation(i)) < 0.001)) // 1m Toleranz wg Rundungsfehlern
			{	// existierendes Profil
				bInterp = FALSE;
				if (m_batch.m_bExisting)
					bShift = TRUE;
			}
      if (bInterp && (cs1==NULL || cs2==NULL))
			{
				temp.Format("%.4f", m_batch.GetStation(i));
				str.FormatMessage(IDS_NOTBETWEEN, temp);
				theApp.m_strErrors.Add(str);
				continue;
			}
			if (fabs(m_batch.GetStation(i)-cs1->GetStation())<m_batch.m_dDistance)
			{
				bInterp = FALSE;
			}
			if (bInterp && (fabs(m_batch.GetStation(i)-cs2->GetStation())<m_batch.m_dDistance))
			{
				bInterp = FALSE;
			}
			if (bInterp)
			{
				temp.Format("%.4f", m_batch.GetStation(i));
				str.FormatMessage(IDS_INTERPOLATING, temp);
				theApp.m_strErrors.Add(str);
				
				if (interp.Initialize(cs1, cs2))
				{
					cs = new CrossSection(theApp.m_pProject);
					cs->SetStation(m_batch.GetStation(i));
					str = theApp.m_pState->GetWaterName();
					cs->SetWaterName(str);
					str = theApp.m_pState->GetName();
					cs->SetStateName(str);
					profNum = theApp.m_pProject->GetCrossSectionCount()+1;
					while (usedProfilNums.Lookup(profNum, dummy))
						profNum++;
					cs->SetProfilNr(profNum);
					str = "0";
					cs->SetVZK(0);
					cs->SetPK(str);
					cs->CreateFileName(&usedFileNames);
					str = cs->GetFileName();
					usedFileNames.AddTail(str);
					usedProfilNums.SetAt(i, 0);
					
					if (!interp.Interpolate(cs))
					{
						delete cs;
						str.LoadString(IDS_INTERP_BAD);
						theApp.m_strErrors.Add(str);
						continue;
					}
					str.LoadString(IDS_INTERP_OK);
					theApp.m_strErrors.Add(str);
					str.Format("QUERPROFIL %d", cs->GetProfilNr());
					cs->GetProfil()->SetPageDesc(1, str);
					defaultLang = setlocale(LC_NUMERIC, NULL);
					setlocale(LC_NUMERIC, "English");
					str.Format("STATION KM %.4f", cs->GetStation());
					setlocale(LC_NUMERIC, defaultLang);
					cs->GetProfil()->SetPageDesc(2, str);

					interp.DeleteContents();

					temp.Format("%.4f", m_batch.GetStation(i));
					str.FormatMessage(IDS_SHIFTING, temp);
					theApp.m_strErrors.Add(str);
					if (!interp.DoProfilShift(cs, m_batch, i))
					{
						delete cs;
						str.LoadString(IDS_SHIFT_BAD);
						theApp.m_strErrors.Add(str);
						continue;
					}
					str.LoadString(IDS_SHIFT_OK);
					theApp.m_strErrors.Add(str);

					cs->SaveProfil();
					str = cs->GetFileName();
					theApp.m_strFiles.Add(str);
					delete cs;
					continue;
				}
				else
				{
					temp.Format("%.4f", m_batch.GetStation(i));
					str.FormatMessage(IDS_NOINTERP, temp);
					theApp.m_strErrors.Add(str);
					continue;
				}
			}; // if bInterp

			if (bShift)
			{	// nur für existierende Profile
				cs1->LoadProfil();

				temp.Format("%.4f", m_batch.GetStation(i));
				str.FormatMessage(IDS_SHIFTING, temp);
				theApp.m_strErrors.Add(str);
				if (!interp.DoProfilShift(cs1, m_batch, i))
				{
					delete cs;
					str.LoadString(IDS_SHIFT_BAD);
					theApp.m_strErrors.Add(str);
					continue;
				}
				str.LoadString(IDS_SHIFT_OK);
				theApp.m_strErrors.Add(str);

				cs1->SaveProfil();
			}
		}; // for i
		CString path, rString;
		ofstream ofs;

		path = theApp.m_pProject->GetCalcDir();
		path += "INTERP.LOG";
		ofs.open(path, ios::out);
		if (ofs.fail())
		{
			rString.FormatMessage(IDS_ERROR_SAVEFILE, path);
			AfxMessageBox(rString, MB_ERROR);
		}
		else
		{
			for (i=0; i<theApp.m_strErrors.GetSize(); i++)
				ofs << theApp.m_strErrors[i] << endl;
			ofs.close();
		}
	}
}

BEGIN_MESSAGE_MAP(CInterpSheet, CPropertySheet)
	//{{AFX_MSG_MAP(CInterpSheet)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CInterpSheet message handlers

