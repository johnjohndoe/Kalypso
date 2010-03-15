// dirdlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "..\..\commonMfc\include\commResource.h"

#include "direct.h"
#include "dirdlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// DirectoryDlg

IMPLEMENT_DYNAMIC(DirectoryDlg, CFileDialog)

DirectoryDlg::DirectoryDlg( LPCTSTR title, LPCTSTR lpszDirectory, CWnd* pParentWnd ) :
		CFileDialog( FALSE, NULL, NULL, OFN_ENABLEHOOK | OFN_EXPLORER | OFN_HIDEREADONLY | OFN_NOCHANGEDIR,
		"Dummy|.||", pParentWnd)
{
  titleBuffer = (LPTSTR)malloc( sizeof( TCHAR ) * MAX_PATH );
  if ( title == NULL )
    lstrcpy( titleBuffer, CString( MAKEINTRESOURCE( IDS_CHOOSEDIR ) ) );
  else
    lstrcpy( titleBuffer, title );
  m_ofn.lpstrTitle = titleBuffer;

  if ( lpszDirectory )
  {
    m_ofn.lpstrInitialDir = lpszDirectory;
    m_strDir = lpszDirectory;
  };
};

DirectoryDlg::~DirectoryDlg()
{
  free( titleBuffer );
};

BEGIN_MESSAGE_MAP(DirectoryDlg, CFileDialog)
	//{{AFX_MSG_MAP(DirectoryDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BOOL DirectoryDlg::OnFileNameOK()
{
	ASSERT_VALID(this);
	CFile file;
	CFileStatus rStatus;
	CString path, msg;
	BOOL bOK;

	m_edit.GetWindowText(path);
	bOK = file.GetStatus(path, rStatus);
	if (!bOK)
	{
		msg.FormatMessage(IDS_DIRNOTEXIST, path);
		if (AfxMessageBox(msg, MB_YESNO | MB_ICONQUESTION)==IDYES)
		{
			if (CreateDirectory(path,NULL))     
				bOK = TRUE;
		}
	}
	if (bOK)
		m_strDir = path;
	return (!bOK);
}

void DirectoryDlg::OnLBSelChangedNotify(UINT, UINT, UINT)
{
	ASSERT_VALID(this);
	CString path;

	path = GetFolderPath();
	path.MakeLower();
	m_edit.SetWindowText(path);
	path += "\\dummy";
	SetControlText(1152, path);
}

void DirectoryDlg::OnInitDone()
{
	ASSERT_VALID(this);
	CRect rect;
	CString str;

	GetParent()->CenterWindow();

	HideControl(1089);
	HideControl(1136);

	// create new edit control
	GetParent()->GetDlgItem(1152)->GetWindowRect(&rect);
	GetParent()->ScreenToClient(&rect);
	LONG lStyle = ::GetWindowLong(GetParent()->GetDlgItem(1152)->GetSafeHwnd(), GWL_EXSTYLE);
	m_edit.CreateEx(lStyle, _T("EDIT"), NULL, GetParent()->GetDlgItem(1152)->GetStyle(), rect, GetParent(), 100);
	m_edit.SetFont(GetParent()->GetDlgItem(1152)->GetFont());
	// hide the old one
	HideControl(1152);

	str.LoadString(IDS_DIR_STATIC);
	SetControlText(1090, str);
	str.LoadString(IDS_FINDIN_STATIC);
	SetControlText(1091, str);
	SetControlText(IDOK, "OK");
}

void DirectoryDlg::OnFileNameChange()
{
	ASSERT_VALID(this);
	CString path;

	path = GetFolderPath();
	path.MakeLower();
	m_edit.SetWindowText(path);
	if(path.GetLength()>3)
		path += "\\dummy";
	else
		path += "dummy";
	SetControlText(1152, path);
}

void DirectoryDlg::OnFolderChange()
{
	ASSERT_VALID(this);
	CString path;

	path = GetFolderPath();
	path.MakeLower();
	m_edit.SetWindowText(path);
	if(path.GetLength()>3)
		path += "\\dummy";
	else
		path += "dummy";
	SetControlText(1152, path);
}
