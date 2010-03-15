// Plotter.h : main header file for the PLOTTER application
//

#ifndef _PLOTTER_H_INCLUDED_
#define _PLOTTER_H_INCLUDED_

#define OVERLAP_SHIFTED			0
#define OVERLAP_NOTSHOWN		1

/////////////////////////////////////////////////////////////////////////////
// CPlotterApp:
// See Plotter.cpp for the implementation of this class
//

#include "commonMfc/include/helper.h"
#include "drawobj.h"

#include "draw.h"

class CTemplate;
class Project;
class State;
class Section;
class SectionArray;

class CPlotterApp : public CDrawApp
{
public:
	CPlotterApp();

	State *m_pState;
	CTypedPtrArray<CObArray, Section*> m_Sections;
	int m_nCount;
	CTypedPtrArray<CObArray, CTemplate*> m_templates;
	int m_nTemplate;

	int GetImageType(int nType);

  Project* GetProject() { return m_pProject; };
  void DeleteProject();
	void LoadProject( CString& dir );

	void LoadTemplates();
	BOOL SaveTemplates();
	void FlushTemplates();
	void GetDrawingFileName(CString& path, Section* sec);
	void SetPrinterDeviceSettings(short& po, short& ps, short& pw, short& pl);
	void GetPrinterDeviceSettings(short& po, short& ps, short& pw, short& pl);
	void SetStandardPrinterSettings(short& po, short& ps, short& pw, short& pl);
	void GetStandardPrinterSettings(short& po, short& ps, short& pw, short& pl);

  void CreateMultiPlot( const SectionArray& secArray, BOOL bAutoFileName );
  CMultiDocTemplate* GetPlotterDocTemplate() { return m_pPlotDocTemplate; };

protected:
	virtual void LoadINISettings();
	virtual void SaveINISettings();
	
	int DoPageSetupDialog(CPageSetupDialog* pPD);

private:
	void SetDefaultIniEntries();

private:
	 Project* m_pProject;

   CMultiDocTemplate* m_pMultiPlotDocTemplate; // das DocTemplate für die MultiDocs
   CMultiDocTemplate* m_pPlotDocTemplate;  // das DocTemplate für PlotDocs
   void InitFeatureInfo();
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPlotterApp)
public:
	virtual BOOL InitInstance();
	virtual BOOL OnDDECommand(LPTSTR lpszCommand);
	virtual int ExitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CPlotterApp)
	afx_msg void OnAppAbout();
	afx_msg void OnUpdateRecentFileMenu(CCmdUI* pCmdUI);
	//}}AFX_MSG
  afx_msg void OnFileOpen() { CDrawApp::OnFileOpen(); }; // damit public wird
  afx_msg void OnFileNew() { CDrawApp::OnFileNew(); };  // damit public wird

	DECLARE_MESSAGE_MAP()

	friend class StempelDialog;

#ifdef _DEBUG
  private:
    static log4cpp::Category& m_logCat;
#endif 
};

#define GETPLOTTERAPP ((CPlotterApp*)AfxGetApp())

#define NSCALES		16
const static double dscale[NSCALES] =
{
	0,10,25,50,100,200,250,500,1000,2000,2500,5000,10000,25000,50000,100000
};




/////////////////////////////////////////////////////////////////////////////

#endif // _PLOTTER_H_INCLUDED_