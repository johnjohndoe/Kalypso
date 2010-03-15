// Export.cpp
//

#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "resource.h"

#include "bce/include/wspfeatures.h"
#include "commonMfc/include/version.h"

#include "calcsht.h"
#include "calcpg1.h"
#include "progdlg.h"
#include "prjsmdlg.h"
#include "lossdlg.h"
#include "optdlg.h"
#include "datenbankdlg.h"
#include "kopftxtdlg.h"
#include "wspdlg.h"
#include "export.h"
#include <cderr.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

//
//  Beachten Sie!
//
//    Wird diese DLL dynamisch an die MFC-DLLs gebunden,
//    muss bei allen von dieser DLL exportierten Funktionen,
//    die MFC-Aufrufe durchführen, das Makro AFX_MANAGE_STATE
//    direkt am Beginn der Funktion eingefügt sein.
//
//    Beispiel:
//
//    extern "C" BOOL PASCAL EXPORT ExportedFunction()
//    {
//      AFX_MANAGE_STATE(AfxGetStaticModuleState());
//    // Hier normaler Funktionsrumpf
//    }
//
//    Es ist sehr wichtig, dass dieses Makro in jeder Funktion
//    vor allen MFC-Aufrufen erscheint. Dies bedeutet, dass es
//    als erste Anweisung innerhalb der Funktion ausgeführt werden
//    muss, sogar vor jeglichen Deklarationen von Objektvariablen,
//    da ihre Konstruktoren Aufrufe in die MFC-DLL generieren
//    könnten.
//
//    Siehe MFC Technical Notes 33 und 58 für weitere
//    Details.
//


WspDlgDllFunction int DoDatenimportAcces( LPCTSTR projectPfad )
// startet den Datenimport ovn ACCESS Daten nach 'Sawall' für wspwin
// Parameter:
//          LPCTSTR projectPfad: Name des Projekts, in welches importiert wird
// Rückgabewert:
//          Fehlerkode:
//                  0: kein Fehler
//                  1: Stationsdatei wurde nicht eingegeben
//                  2: Profildatei wurde nicht eingegeben
//                  3: Projekt konnte nicht erzeugt werden
//            100 + n: fehlerkode n aus Projekt::Datenimport( ... )
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  
  // zwei Dateinamen vom Benutzer einlesen
  
  TCHAR title[MAX_PATH];
  CString stationPath, profilePath;
  CString fileFilter( MAKEINTRESOURCE( IDS_TEXT_FILE_FILTER ) );
  // zuerst die Stationsdatei
  CFileDialog stationDlg( TRUE, TEXT("TXT"), TEXT("STATION"), OFN_FILEMUSTEXIST | OFN_HIDEREADONLY, 
    fileFilter, NULL );
  lstrcpy( title, CString( MAKEINTRESOURCE( IDS_CHOOSE_STATION_FILE ) ) );
  stationDlg.m_ofn.lpstrTitle = title;
  
  if ( stationDlg.DoModal() == IDOK )
    stationPath = stationDlg.GetPathName();
  else
    return 1;
  
  // dann die Profildatei
  CFileDialog profileDlg( TRUE, TEXT("TXT"), TEXT("PROFILE"), OFN_FILEMUSTEXIST | OFN_HIDEREADONLY,
    fileFilter, NULL );
  lstrcpy( title, CString( MAKEINTRESOURCE( IDS_CHOOSE_PROFILE_FILE ) ) );
  profileDlg.m_ofn.lpstrTitle = title;
  
  if ( profileDlg.DoModal() == IDOK )
    profilePath = profileDlg.GetPathName();
  else
    return 2;
  
  // das Projekt erzeugen und die beiden Dateien importieren
  Project* project = new Project( projectPfad );
  if( !project || !project->Load() )
  {
    AfxMessageBox( IDS_PROJECT_LOAD_ERROR );
    delete project;  // kein Problem falls project = NULL
    return 3;
  };
  
  CWaitCursor wait;
  int fehler = project->ImportDataAccess( stationPath, profilePath );
  
  // Fehlermeldungen erzeugen und sonstige Fehlerbehandlung
  CString message;
  UINT msgType;
  
  switch ( fehler )
  {
  case 0:
    project->Save();
    message.LoadString( IDS_IMPORT_SUCCESFUL );
    msgType = MB_ICONINFORMATION;
    break;
    
  case 1:
    message.FormatMessage( IDS_ERROR_SAME_FILES );
    msgType = MB_ICONERROR;
    break;
    
  case 2:
    message.FormatMessage( IDS_ERROR_READFILE, stationPath );
    msgType = MB_ICONERROR;
    break;
    
  case 3:
    message.FormatMessage( IDS_ERROR_READFILE, profilePath );
    msgType = MB_ICONERROR;
    break;
    
  case 4:
    message.FormatMessage( IDS_ERROR_CHANGED_FILES );
    msgType = MB_ICONERROR;
    break;
    
  default:
    message.Format( "Error %d", fehler );
    msgType = MB_ICONERROR;
  };
  
  AfxMessageBox( message, MB_OK | msgType );
  
  // Aufräumen
  delete project;
  
  return ( fehler > 0 ? 100 + fehler : 0 );
}; // DoDatenimportAccess


WspDlgDllFunction BOOL DoLWACalcDlg(LPCSTR lpszProject, LPCSTR lpszSTRFile, char* lpszFileName, HWND hWndParent, bool bDemo)
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  CString project, STRFile, fileName, str;
  CWnd parent;
  State *st;
  Calculation *calc;
  CalcData *cd;
  CFile file;
  CFileStatus rStatus;
  BOOL bReturn = FALSE;
  BOOL bNew = FALSE;
  
  project = lpszProject;
  STRFile = lpszSTRFile;
  fileName = lpszFileName;
  
  Project proj(project);
  if (!proj.Load())
    return FALSE;
  st = proj.GetState(STRFile);
  if (st==NULL)
    return FALSE;
  if (st->GetNumCrossSections()<2)
  {
    AfxMessageBox(IDS_TOO_FEW_CSECTIONS, MB_OK | MB_ICONSTOP);
    return FALSE;
  }
  if (st->GetNumOutFlows()<1)
  {
    AfxMessageBox(IDS_TOO_FEW_OUTFLOWS, MB_OK | MB_ICONSTOP);
    return FALSE;
  }
  if (fileName.IsEmpty())
    bNew = TRUE;
  if (bNew)
  { // new calculation
    calc = new Calculation(&proj, st);
    calc->CreateFileName();
    if (st->GetNumCalculations()==0)
    {
      cd = new CalcData(calc, st, TRUE);
      calc->SetCalcData(cd);
    }
    else
    {
      Calculation *pLast;
      
      pLast = st->GetLastCalculation();
      pLast->LoadCalcData(TRUE);
      cd = pLast->GetCalcData()->Clone( pLast, st );
      pLast->FlushCalcData();
      calc->SetCalcData(cd);
    }
  }
  else
  {
    calc = st->FindCalculation(fileName);
    if (calc==NULL || !calc->LoadCalcData(TRUE))
      return FALSE;
    cd = calc->GetCalcData();
  }
  if (parent.Attach(hWndParent))
  {
    CString msg, str;
    str.LoadString(IDS_CALCDATA);
    LWACalcSheet dlg(cd, str, &parent, 0);
    BOOL bStartOK, bEndOK;
    CrossSection *cs;
    
    dlg.m_psh.dwFlags |= PSH_NOAPPLYNOW;
    if (!bNew)
    {
      if (cd->m_nSelIndex>st->GetNumOutFlows()-1)
      {
        str.LoadString(IDS_WARNING_ABFLUSS);
        msg += str;
        cd->m_nSelIndex = st->GetNumOutFlows()-1;
      }
      bStartOK = bEndOK = FALSE;
      cs = st->GetFirstCrossSection();
      while (cs!=NULL)
      {
        if (cs->GetStation()==cd->m_dAnfang)
          bStartOK = TRUE;
        if (cs->GetStation()==cd->m_dEnde)
          bEndOK = TRUE;
        cs = st->GetNextCrossSection();
      }
      if (!bStartOK)
      {
        str.LoadString(IDS_WARNING_ANFANG);
        msg += str;
      }
      if (!bEndOK)
      {
        str.LoadString(IDS_WARNING_ENDE);
        msg += str;
      }
      if (!msg.IsEmpty())
        AfxMessageBox(msg, MB_OK | MB_ICONEXCLAMATION);
    }
    
    if (dlg.DoModal()==IDOK)
    {
      if (bNew)
        st->AddCalculation(calc);
      if( bDemo )
      {
        AfxMessageBox(IDS_DEMO_NOSAVE, MB_OK | MB_ICONEXCLAMATION);
        bReturn = FALSE;
      }
      else
      {
        cd->GetInfo(str);
        calc->SetName(str);
        calc->SetStartStation(cd->m_dAnfang);
        calc->SetEndStation(cd->m_dEnde);
        //        proj.Save();
        st->Save();  // es muss doch nur der Zustand, nicht das ganze Projekt gespeichert werden
        if (bNew)
        {
          str = calc->GetFileName();
          strcpy(lpszFileName, str);                    
        }
        bReturn = TRUE;
      }
    }
    else if (bNew)
      delete calc;
    parent.Detach();
  }
  
  return bReturn;
}

WspDlgDllFunction BOOL DoBCECalcDlg(LPCSTR lpszProject, LPCSTR lpszSTRFile, char* lpszFileName, HWND hWndParent, bool bDemo)
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  CString project, STRFile, fileName, str;
  CWnd parent;
  State *st;
  Calculation *calc;
  CalcData *cd;
  CFile file;
  CFileStatus rStatus;
  BOOL bReturn = FALSE;
  BOOL bNew = FALSE;
  
  project = lpszProject;
  STRFile = lpszSTRFile;
  fileName = lpszFileName;
  Project proj(project);
  if (!proj.Load())
    return FALSE;
  st = proj.GetState(STRFile);
  if (st==NULL)
    return FALSE;
  if (st->GetNumCrossSections()<2)
  {
    AfxMessageBox(IDS_TOO_FEW_CSECTIONS, MB_OK | MB_ICONSTOP);
    return FALSE;
  }
  if (fileName.IsEmpty())
    bNew = TRUE;
  if (bNew)
  { // new calculation
    calc = new Calculation(&proj, st);
    calc->CreateFileName();
    if (st->GetNumCalculations()==0)
    {
      cd = new CalcData(calc, st, FALSE);
      calc->SetCalcData(cd);
    }
    else
    {
      Calculation *pLast;
      
      pLast = st->GetLastCalculation();
      pLast->LoadCalcData(FALSE);
      cd = pLast->GetCalcData()->Clone( pLast, st );
      pLast->FlushCalcData();
      calc->SetCalcData(cd);
    }
  }
  else
  {
    calc = st->FindCalculation(fileName);
    if (calc==NULL || !calc->LoadCalcData(FALSE))
      return FALSE;
    cd = calc->GetCalcData();
  }
  if (cd->m_nWerte[0]==1 && st->GetNumOutFlows()<1)
  {
    AfxMessageBox(IDS_TOO_FEW_OUTFLOWS, MB_OK | MB_ICONSTOP);
    return FALSE;
  }
  if (parent.Attach(hWndParent))
  {
    CString msg, str;
    str.LoadString(IDS_CALCDATA);
    BCECalcSheet dlg(cd, str, &parent, 0, bDemo);
    BOOL bStartOK, bEndOK;
    CrossSection *cs;
    
    dlg.m_psh.dwFlags |= PSH_NOAPPLYNOW;
    if (!bNew)
    {
      if (cd->m_nWerte[0]==1 && cd->m_nSelIndex>st->GetNumOutFlows()-1)
      {
        str.LoadString(IDS_WARNING_ABFLUSS);
        msg += str;
        cd->m_nSelIndex = st->GetNumOutFlows()-1;
      }
      bStartOK = bEndOK = FALSE;
      cs = st->GetFirstCrossSection();
      while (cs!=NULL)
      {
        if (cs->GetStation()==cd->m_dAnfang)
          bStartOK = TRUE;
        if (cs->GetStation()==cd->m_dEnde)
          bEndOK = TRUE;
        cs = st->GetNextCrossSection();
      }
      if (!bStartOK)
      {
        str.LoadString(IDS_WARNING_ANFANG);
        msg += str;
      }
      if (!bEndOK)
      {
        str.LoadString(IDS_WARNING_ENDE);
        msg += str;
      }
      if (!msg.IsEmpty())
        AfxMessageBox(msg, MB_OK | MB_ICONEXCLAMATION);
    }
    
    if (dlg.DoModal()==IDOK)
    {
      if (bNew)
        st->AddCalculation(calc);
      if( bDemo )
      {
        AfxMessageBox(IDS_DEMO_NOSAVE, MB_OK | MB_ICONEXCLAMATION);
        bReturn = FALSE;
      }
      else
      {
        cd->GetInfo(str);
        calc->SetName(str);
        calc->SetStartStation(cd->m_dAnfang);
        calc->SetEndStation(cd->m_dEnde);
        proj.Save();
        if (bNew)
        {
          str = calc->GetFileName();
          strcpy(lpszFileName, str);
        }
        bReturn = TRUE;
      }
    }
    else if (bNew)
      delete calc;
    parent.Detach();
  }
  return bReturn;
}

WspDlgDllFunction void DoProjectSummaryDlg(HWND hWndParent)
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  CWnd parent;
  
  if (parent.Attach(hWndParent))
  {
    ProjectSummDialog dlg(&parent);
    dlg.DoModal();
    parent.Detach();
  }
}

WspDlgDllFunction BOOL DoProgressDlg(HWND hWndParent)
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  BOOL bReturn = FALSE;
  
  if (theApp.pDlg!=NULL)
  {
    theApp.pDlg->DestroyWindow();
    delete theApp.pDlg;
  }
  theApp.parentProgress.Detach();
  if (theApp.parentProgress.Attach(hWndParent))
  {
    bReturn = TRUE;
    theApp.pDlg = new ProgressDialog(&theApp.parentProgress);;
    theApp.pDlg->Create(IDD_PROGRESS, &theApp.parentProgress);
  }
  return bReturn;
}

WspDlgDllFunction void IncProgress()
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  
  if (theApp.pDlg==NULL)
    return;
  theApp.pDlg->IncProgress();
}

WspDlgDllFunction void SetProgressTitle(LPCSTR lpszTitle)
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  
  if (theApp.pDlg==NULL)
    return;
  theApp.pDlg->SetWindowText(lpszTitle);
}

WspDlgDllFunction void SetProgressText(LPCSTR lpszText)
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  CString text = lpszText;
  
  if (theApp.pDlg==NULL)
    return;
  theApp.pDlg->SetText(text);
}

WspDlgDllFunction void EndProgressDlg()
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  
  if (theApp.pDlg==NULL)
    return;
  theApp.pDlg->DestroyWindow();
  delete theApp.pDlg;
  theApp.pDlg = NULL;
  theApp.parentProgress.Detach();
}

WspDlgDllFunction BOOL DoCaddyConvertion( LPCSTR lpszProject, HWND hWndParent, char* lpszCFGString )
// Importiert Caddy - Dateien
{
  AFX_MANAGE_STATE( AfxGetStaticModuleState() );
  
  // falls das übergebene Fenster nichts taugt, gleich zurück
  CWnd parentWnd;
  if( !parentWnd.Attach( hWndParent ) )
    return FALSE;
  
  CStringList files;
  int nRet = IDCANCEL;
  
  // falls kein CFG-String übergeben wurde, auch gleich zurück
  if( lpszCFGString == NULL )
    return FALSE;
  
  // versuchen, das angegebene Projekt zu laden
  CString project = lpszProject;
  Project proj( project );
  if( !proj.Load() )
    return FALSE;
  
  CString str;
  str.LoadString(IDS_QUEFILES_ALLFILES);
  CFileDialog fileDlg( TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_ALLOWMULTISELECT,
    str, &parentWnd );
  DWORD dwError = FNERR_BUFFERTOOSMALL;
  char *lpstrFile;
  DWORD nMaxFile = MAX_PATH * MAX_PATH;
  lpstrFile = new char[nMaxFile];
  lpstrFile[0] = '\0';
  fileDlg.m_ofn.lpstrFile = lpstrFile;
  fileDlg.m_ofn.nMaxFile = nMaxFile;
  while (nRet!=IDOK && dwError==FNERR_BUFFERTOOSMALL)
  {
    nRet = fileDlg.DoModal();
    dwError = CommDlgExtendedError();
    if (nRet!=IDOK && dwError==FNERR_BUFFERTOOSMALL)
    {
      AfxMessageBox(IDS_BUFFERTOOSMALL, MB_OK | MB_ICONINFORMATION);
      nMaxFile = MAKELONG((WORD)lpstrFile[0], (WORD)lpstrFile[1]);
      delete[] lpstrFile;
      lpstrFile = new char[nMaxFile];
      lpstrFile[0] = '\0';
      fileDlg.m_ofn.lpstrFile = lpstrFile;
      fileDlg.m_ofn.nMaxFile = nMaxFile;
    }
  }
  if( nRet == IDOK )
  {
    COleDateTime newDate;
    CString newStateName;
    CString newWaterName;
    
    StateDialog dlg( STATE_DLG_MASK_ALL, &parentWnd );
    dlg.m_oleDate = COleDateTime::GetCurrentTime();
    if( dlg.DoModal() == IDOK )
    {
      newWaterName = dlg.m_water;
      newDate = dlg.m_oleDate;
      newStateName = dlg.m_name;
      
      CString fileName, path;
      State* pState = new State( &proj );
      int i = 1;
      BOOL bOK = FALSE;
      CFile file;
      CFileStatus rStatus;
      
      
      CWaitCursor wait;
      
      pState->SetWaterName( newWaterName );
      pState->SetDate( newDate );
      pState->SetName( newStateName );
      pState->CreateFileName();
      proj.AddState( pState );
      
      POSITION pos = fileDlg.GetStartPosition();
      while( pos != NULL )
      {
        DataBlock *db = NULL;
        
        fileName = fileDlg.GetNextPathName(pos);
        
        CStdioFile file;
        if ( !file.Open( fileName, CFile::modeRead ) )
          continue;
        
        CString str, inputStr;
        while ( file.ReadString( inputStr ) )
        {
          str = inputStr;
          if (str.Left(7).CompareNoCase("STATION")==0)
          {
            CString defaultLang;
            
            // create new cross section and set info
            CrossSection* cs = new CrossSection(&proj);
            cs->SetWaterName( newWaterName );
            cs->SetStateName( newStateName );

            int km;
            double meter;
            sscanf( inputStr, "%*s%2d%lf", &km, &meter );
            
            double station = km + (meter/1000);
            str.Format( "%.4f", station );
            station = atof( str );
            cs->SetStation(station);
            str = "0";
            cs->SetPK(str);
            cs->SetVZK(0);
            cs->SetProfilNr(proj.GetCrossSectionCount()+1);
            cs->CreateFileName(&files);
            str = cs->GetFileName();
            files.AddTail(str);
            pState->AddCrossSection(cs);
            
            // create new profil and set info
            Profil* pr = new Profil(cs);
            pr->SetOriginalFile(fileName);
            str.Format( "%s %s", newWaterName, newStateName );
            pr->SetPageDesc(0, str);
            str.Format("QUERPROFIL %d", cs->GetProfilNr());
            pr->SetPageDesc(1, str);
            defaultLang = setlocale(LC_NUMERIC, NULL);
            setlocale(LC_NUMERIC, "English");
            str.Format("STATION KM %.4f", station);
            setlocale(LC_NUMERIC, defaultLang);
            pr->SetPageDesc(2, str);
            cs->SetProfil(pr);
            // create new data block and set info
            db = new DataBlock(pr);
            db->SetType(DST_GELAENDEHOEHE);
            pr->AddDataBlock(db);
            
          }
          else if( str.GetLength() > 0 && db != NULL )
          {
            double a, b;
            Coord *pCrd;
            
            sscanf( inputStr , "%lf%lf%*s", &a, &b );
            pCrd = new Coord;
            pCrd->dx = a;
            pCrd->dy = b;
            pCrd->xs = 0;
            pCrd->ys = 0;
            db->AddCoord(pCrd);
          }
        }

        file.Close();
      }
      if( pState->GetNumCrossSections() < 1 )
      {
        AfxMessageBox(IDS_NOTENOUGHPROFILS, MB_OK | MB_ICONSTOP);
        nRet = IDCANCEL;
      }
      if( pState->GetNumCrossSections() > 1000 )
      {
        AfxMessageBox( IDS_TOOMANYPROFILS, MB_OK | MB_ICONSTOP );
        nRet = IDCANCEL;
      }
      if( nRet == IDOK && !proj.Save() )
        nRet = IDCANCEL;
      if( nRet == IDOK )
      {
        fileName = pState->GetFileName();
        
        CString dateString = newDate.Format( "%d.%m.%Y" );
        _stprintf( lpszCFGString, TEXT( "%-14.14s %-13.13s %-10.10s %15.6f%15.6f   %s" ),
          newWaterName, newStateName, dateString, pState->GetStartStation(), pState->GetEndStation(), fileName );
      }
    }
  }
  delete[] lpstrFile;
    
  parentWnd.Detach();
    
  return( nRet == IDOK );
}

WspDlgDllFunction BOOL DoLossDlg( LPCSTR lpszProject, LPCSTR lpszSTRFile, HWND hWndParent )
{
  AFX_MANAGE_STATE( AfxGetStaticModuleState() );
  CWnd parent;
  BOOL bRet = TRUE;
  
  CString project = lpszProject;
  CString STRFile = lpszSTRFile;
  Project proj(project);
  if ( !proj.Load() )
    return FALSE;
  
  State* st = proj.GetState( STRFile );
  if ( !st )
    return FALSE;
  if ( parent.Attach( hWndParent ) )
  {
    LossDialog dlg( &parent, st );
    if( dlg.DoModal() == IDOK )
    {
      if ( !proj.Save() )
        bRet = FALSE;
    }
    else
      bRet = FALSE;
    parent.Detach();
  }; // if parent.Attach
  return bRet;
}

WspDlgDllFunction BOOL DoOptionsDlg(HWND hWndParent, bool bLWA, bool bFeatureSort )
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  CWnd parent;
  BOOL bRet = TRUE;
  
  if (parent.Attach(hWndParent))
  {
    CString str;
    str.LoadString(IDS_OPTIONS);
    OptionsDialog dlg( bFeatureSort, !bLWA, str, &parent );
    dlg.m_psh.dwFlags |= PSH_NOAPPLYNOW;
    if (dlg.DoModal() !=IDOK)
      bRet = FALSE;
    parent.Detach();
  }
  return bRet;
}

WspDlgDllFunction BOOL DoDirDlg(HWND hWndParent, char* lpszDir)
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  CWnd parent;
  BOOL bRet = FALSE;
  
  if (parent.Attach(hWndParent))
  {
    CString dir = lpszDir;
    DirectoryDlg dlg( NULL, dir, &parent );
    if (dlg.DoModal()==IDOK)
    {
      strcpy(lpszDir, dlg.m_strDir);
      bRet = TRUE;
    }
    parent.Detach();
  }
  return bRet;
}

WspDlgDllFunction HWND DoDatabankDlg(HWND hWndParent, char* lpszDir, int dlgtyp)
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  CWnd parent;
  HWND hWnd = NULL;
  CString str;
  
  theApp.parentDatenbank.Detach();  //für MODELES
  if (theApp.datDlg!=NULL)
  {
    theApp.datDlg->DestroyWindow();
    delete theApp.datDlg;
  }
  
  if (theApp.parentDatenbank.Attach(hWndParent))
  {
    CString dir = lpszDir;
    theApp.datDlg = new DatenbankDlg(&theApp.parentDatenbank, dir, dlgtyp);
    theApp.datDlg->Create(IDD_BEIWERT_DB, &theApp.parentDatenbank);
    hWnd = theApp.datDlg->GetSafeHwnd();
  }
  
  return hWnd;
}

WspDlgDllFunction BOOL DoKopfTxtDlg( BOOL bChangeKunde, HWND hWndParent, char* lpszDir) 
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  CWnd parent;
  BOOL bRet = TRUE;
  
  if (parent.Attach(hWndParent))
  {
    CString dir=lpszDir;
    KopfTxtDlg dlg( bChangeKunde, &parent, dir );
    if (dlg.DoModal()==IDOK)
    {
    }
    else
      bRet = FALSE;
    parent.Detach();
  }
  return bRet;
}


WspDlgDllFunction BOOL InitFeatureinfo( const char* exe_ver )
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());

	WSPFeatures* features = WSPFeatures::Instance();

	if( features->RegLoaded() )
	{
		if( features->isEnabled( "WSPWIN", "wsp_nodemo" ) )
		{
			if( !features->CheckDemoDate(features->GetDataStr("WSPWIN","DEMO_DATE")))
			{
				AfxMessageBox("Die aktuelle Lizensierung ist abgelaufen.\nDas Programm wird im DEMO-Modus gestartet.");
				features->SetDataStr("WSPWIN","DEMO_DATE","01.01.2222");//Damit diese Meldung wird nur einmal angezeigt wird
				features->DisableFeature("WSPWIN", "wsp_nodemo");
			}
			else if (!features->CheckRegVer(features->GetDataStr("WSPWIN","VERSIONS_NR"), exe_ver))
			{
				AfxMessageBox("Die vorhandene Lizenz ist nicht für diese Version gültig.\nDas Programm wird im DEMO-Modus gestartet.");
				features->SetDataStr("WSPWIN","VERSIONS_NR",exe_ver);//Damit diese Meldung wird nur einmal angezeigt wird
				features->DisableFeature("WSPWIN", "wsp_nodemo");
			}
		}
	}

	if( features->isEnabled("WSPWIN","PRODUCT_NAME")) 
		return features->RegLoaded();
	
	if(features->AddFeature("WSPWIN","PRODUCT_NAME"))
	{
		features->SetDataStr("WSPWIN","PRODUCT_NAME","WspWin");
		features->EnableFeature("WSPWIN","PRODUCT_NAME");
	}

	AfxMessageBox( "Die Produktregistrierung ist fehlgeschlagen.\nDas Programm wird im Demo-Modus gestartet." );
	return false;
	// konnte der reg-Eintrag gelesen werden, oder wurden die Defaulteinstellungen geladen ?;
};

WspDlgDllFunction BOOL GetFeature( const char* name )
{
  AFX_MANAGE_STATE( AfxGetStaticModuleState() );
  return WSPFeatures::Instance()->isEnabled("WSPWIN", name);
}; 

WspDlgDllFunction void GetFeatureVersion( char versionStr[256], HMODULE hModule ) 
{
  AFX_MANAGE_STATE( AfxGetStaticModuleState() );

  CVersion version( hModule );
  CString vStr = version.GetProductNr();

  strncpy( versionStr, vStr, 256 );
};

WspDlgDllFunction LPCTSTR GetFeatureLicence( int index ) 
{
  AFX_MANAGE_STATE(AfxGetStaticModuleState());
  if (index == 0) return WSPFeatures::Instance()->GetDataStr("HEAD","LICENCE_TEXT1");
  if (index == 1) return WSPFeatures::Instance()->GetDataStr("HEAD","LICENCE_TEXT2");
  return NULL;
};

WspDlgDllFunction time_t GetFeatureDate( HMODULE hModule )
{ 
  AFX_MANAGE_STATE( AfxGetStaticModuleState() );

  CVersion version( hModule );
  CTime time = version.GetTime();

  return time.GetTime();

};
