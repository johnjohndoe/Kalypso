// obqrydlg.cpp: Implementierungsdatei
//

#include "stdafx.h"

#include "maplayer.h"
#include "obqrydlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CObjectQueryDialog 


CObjectQueryDialog::CObjectQueryDialog(CWnd* pParent /*=NULL*/, BOOL bEdit /*=FALSE*/)
	: CDialog(CObjectQueryDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(CObjectQueryDialog)
	m_position = _T("");
	m_numfound = _T("");
	m_layername = _T("");
	m_objecttype = _T("");
	//}}AFX_DATA_INIT
	m_bEdit = bEdit;
	m_nShapeType = -1;
}

CObjectQueryDialog::~CObjectQueryDialog()
{
}

void CObjectQueryDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CObjectQueryDialog)
	DDX_Control(pDX, IDC_COMBO1, m_features);
	DDX_Control(pDX, IDC_LIST1, m_list);
	DDX_Text(pDX, IDC_STATIC1, m_position);
	DDX_Text(pDX, IDC_STATIC2, m_numfound);
	DDX_Text(pDX, IDC_STATIC3, m_layername);
	DDX_Text(pDX, IDC_STATIC4, m_objecttype);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CObjectQueryDialog, CDialog)
	//{{AFX_MSG_MAP(CObjectQueryDialog)
	ON_CBN_SELCHANGE(IDC_COMBO1, OnSelchangeCombo1)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CObjectQueryDialog 

BOOL CObjectQueryDialog::OnInitDialog() 
{
	LV_COLUMN lvColumn;
	CRect rect;
	CString str;
	int i, j;

	CDialog::OnInitDialog();
	
	m_list.SetFullRowSel(TRUE);
	m_list.SetGridList(TRUE);
	m_list.GetClientRect(&rect);
	lvColumn.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
	lvColumn.fmt = LVCFMT_LEFT;
	lvColumn.cx = rect.Width()/3;
	str.LoadString(IDS_FIELDNAME);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 0;
	m_list.InsertColumn(0, &lvColumn);
	str.LoadString(IDS_FIELDTYPE);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 1;
	m_list.InsertColumn(1, &lvColumn);
	str.LoadString(IDS_VALUE);
	lvColumn.pszText = str.GetBuffer(str.GetLength());
	str.ReleaseBuffer();
	lvColumn.iSubItem = 2;
	m_list.InsertColumn(2, &lvColumn);

	for (i=0; i<m_queries.GetSize(); i++)
	{
		for (j=0; j<m_queries[i]->m_fieldNames.GetSize(); j++)
		{
			if (m_queries[i]->m_fieldNames[j]==MO2_FIELD_FEATUREID)
				str.Format("%d", m_queries[i]->m_longValues[j]);
		}
		m_features.AddString(str);
	}
	m_features.SetCurSel(0);
	OnSelchangeCombo1();

	switch (m_nShapeType)
	{
		case moPoint:
			m_objecttype.LoadString(IDS_POINT);
			break;

		case moLine:
			m_objecttype.LoadString(IDS_LINE);
			break;

		case moPolygon:
			m_objecttype.LoadString(IDS_POLYGON);
			break;
	}

	UpdateData(FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX-Eigenschaftenseiten sollten FALSE zurückgeben
}

void CObjectQueryDialog::OnOK() 
{
	int i, j;
	CString str, temp;

	if (!UpdateData())
		return;

	if (m_bEdit)
	{
		for (i=0; i<m_queries.GetSize(); i++)
		{
			for (j=0; j<m_queries[i]->m_fieldTypes.GetSize(); j++)
			{
				str = m_list.GetItemText(j, 2);
				switch (m_queries[i]->m_fieldTypes[j])
				{
				case moLong:
					m_queries[i]->m_longValues[j] = atoi(str);
					break;
					
				case moDouble:
					m_queries[i]->m_doubleValues[j] = atof(str);
					break;
					
				case moDate:
					m_queries[i]->m_dateValues[j].ParseDateTime(str);
					break;
					
				case moString:
					m_queries[i]->m_stringValues[j] = str;
					break;
					
				case moBoolean:
					temp.LoadString(IDS_TRUE);
					m_queries[i]->m_boolValues[j] = (BOOL)(str==temp ? TRUE : FALSE);
					break;
				}
			}
		}
	}
	
	CDialog::OnOK();
}

void CObjectQueryDialog::OnSelchangeCombo1() 
{
	LV_ITEM lvItem;
	CString str, value, data;
	CString temp1, temp2;
	int i, dataType, ctrlType;
	int nSel;

	nSel = m_features.GetCurSel();
	
	lvItem.mask = LVIF_TEXT | LVIF_PARAM | LVIF_STATE;
	lvItem.state = 0;
	lvItem.stateMask = 0;

	m_list.DeleteAllItems();

	for (i=0; i<m_queries[nSel]->m_fieldNames.GetSize(); i++)
	{
		BOOL bFeatureId = FALSE;
		str = m_queries[nSel]->m_fieldNames[i];
		if (str==MO2_FIELD_FEATUREID)
			bFeatureId = TRUE;
		lvItem.iItem = lvItem.lParam = m_list.GetItemCount();
		lvItem.iSubItem = 0;
		lvItem.pszText = str.GetBuffer(str.GetLength());
		str.ReleaseBuffer();
		lvItem.cchTextMax = str.GetLength();
		m_list.InsertItem(&lvItem);
		switch (m_queries[nSel]->m_fieldTypes[i])
		{
			case moLong:
				str.LoadString(IDS_WHOLENUM);
				dataType = INT_FIELD;
				ctrlType = LCTRLEX_EDIT;
				value.Format("%d", m_queries[nSel]->m_longValues[i]);
				break;

			case moDouble:
				str.LoadString(IDS_DECIMAL);
				dataType = DOUBLE_FIELD;
				ctrlType = LCTRLEX_EDIT;
				value.Format("%.*g", DBL_DIG, m_queries[nSel]->m_doubleValues[i]);
				break;

			case moDate:
				str.LoadString(IDS_DATE);
				dataType = DATE_FIELD;
				ctrlType = LCTRLEX_EDIT;
				value = m_queries[nSel]->m_dateValues[i].Format(VAR_DATEVALUEONLY);
				break;

			case moString:
				str.LoadString(IDS_TEXT);
				dataType = STRING_FIELD;
				ctrlType = LCTRLEX_EDIT;
				value = m_queries[nSel]->m_stringValues[i];
				break;

			case moBoolean:
				str.LoadString(IDS_BOOLEAN);
				dataType = STRING_FIELD;
				ctrlType = LCTRLEX_DROPDOWNLIST;
				temp1.LoadString(IDS_TRUE);
				temp2.LoadString(IDS_FALSE);
				data = temp1 + "\n" + temp2 + "\n";
				value = m_queries[nSel]->m_boolValues[i] ? temp1 : temp2;
				break;
		}
		m_list.SetItemText(lvItem.iItem, 1, str);
		m_list.SetItemText(lvItem.iItem, 2, value);
		if (m_bEdit && !bFeatureId)
			m_list.SetItemControl(lvItem.iItem, 2, ctrlType, ctrlType==LCTRLEX_EDIT ? NULL : (LPCSTR)data, dataType);
	}
}
