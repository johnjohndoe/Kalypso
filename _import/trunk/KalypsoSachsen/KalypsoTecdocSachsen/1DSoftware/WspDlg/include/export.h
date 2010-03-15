#ifndef _EXPORT_H_WSPDLG_DLL_INCLUDED_
#define _EXPORT_H_WSPDLG_DLL_INCLUDED_

#ifdef _WSPDLGDLL
#define WspDlgDllFunction __declspec(dllexport)
#else
#define WspDlgDllFunction __declspec(dllimport)
#endif

struct NMPROJECTMNG;

WspDlgDllFunction BOOL DoDatenimportAcces( LPCTSTR projectPfad );
WspDlgDllFunction BOOL DoLWACalcDlg(LPCSTR lpszProject, LPCSTR lpszSTRFile, char* lpszFileName, HWND hWndParent, bool bDemo);
WspDlgDllFunction BOOL DoBCECalcDlg(LPCSTR lpszProject, LPCSTR lpszSTRFile, char* lpszFileName, HWND hWndParent, bool bDemo);
WspDlgDllFunction void DoProjectSummaryDlg(HWND hWndParent);
WspDlgDllFunction BOOL DoProgressDlg(HWND hWndParent);
WspDlgDllFunction void IncProgress();
WspDlgDllFunction void SetProgressTitle(LPCSTR lpszTitle);
WspDlgDllFunction void SetProgressText(LPCSTR lpszText);
WspDlgDllFunction void EndProgressDlg();
WspDlgDllFunction BOOL DoCaddyConvertion(LPCSTR lpszProject, HWND hWndParent, char* lpszCFGString);
WspDlgDllFunction BOOL DoLossDlg(LPCSTR lpszProject, LPCSTR lpszSTRFile, HWND hWndParent);
WspDlgDllFunction BOOL DoOptionsDlg(HWND hWndParent, bool bLWA, bool bFeatureSort );
WspDlgDllFunction BOOL DoDirDlg(HWND hWndParent, char* lpszDir);
WspDlgDllFunction HWND DoDatabankDlg(HWND hWndParent, char* lpszDir,int dlgtyp);
WspDlgDllFunction BOOL DoKopfTxtDlg( BOOL bChangeKunde, HWND hWndParent, char* lpszDir);
WspDlgDllFunction BOOL GetFeature( const char* name );
WspDlgDllFunction BOOL InitFeatureinfo( const char* fileName );
WspDlgDllFunction void GetFeatureVersion( char versionStr[256], HMODULE hModule );
WspDlgDllFunction LPCTSTR GetFeatureLicence( int index );
WspDlgDllFunction time_t GetFeatureDate( HMODULE hModule );

// 'exportierte' Typen und efinitionen

enum DatenbankType { ueberfallbeiwert, rauheit_ks, rauheit_kst, bewuchs };

#endif _EXPORT_H_WSPDLG_DLL_INCLUDED_