// GenPage.cpp : implementation file
//

#include "stdafx.h"

#include "..\..\wspprj\wspprj.h"

#include "plotter.h"
#include "plotdoc.h"
#include "profil.h"
#include "stempel.h"
#include "plotdocdata.h"
#include "stpldoc.h"
#include "template.h"
#include "summinfo.h"
//#include "drawvw.h"
#include "genpage.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CGeneralPage property page

CGeneralPage::CGeneralPage( CPropertyDialog *pParent, CPlotterDoc* pDoc, BOOL bTemplate /*=FALSE*/ ) 
: CPropertyPage( CGeneralPage::IDD )
{
	//{{AFX_DATA_INIT(CGeneralPage)
	m_height = 0.0;
	m_heightDefault = _T("");
	m_hintText = _T("");
	m_staticProps = _T("");
	m_staticStamp = _T("");
	m_staticFrom = _T("");
	m_staticTo = _T("");
	m_staticXScale = _T("");
	m_staticYScale = _T("");
	m_ende = 0.0;
	m_anfang = 0.0;
	//}}AFX_DATA_INIT

  m_staticScale = CString( MAKEINTRESOURCE( IDS_GPAGE_STATIC_SCALE ) );
  m_staticRange = CString( MAKEINTRESOURCE( IDS_GPAGE_STATIC_RANGE ) );
  m_staticHeight = CString( MAKEINTRESOURCE( IDS_GPAGE_STATIC_HEIGHT ) );
  m_hintText = CString( MAKEINTRESOURCE( IDS_GPAGE_HINT ) );

  m_staticProps = CString( MAKEINTRESOURCE( IDS_GPAGE_PROPS_PROPS ) );
	m_staticStamp = CString( MAKEINTRESOURCE( IDS_GPAGE_PROPS_STAMPS ) );

  m_staticTo = CString( MAKEINTRESOURCE( IDS_GPAGE_RANGE_TO ) );
  m_staticFrom = CString( MAKEINTRESOURCE( IDS_GPAGE_RANGE_FROM ) );
 
  
  m_pParent = pParent;
	m_pDoc = pDoc;

  m_bNewStempel = FALSE;
	m_bNewTemplate = FALSE;
	
  m_bTemplate = bTemplate;
} // Konstruktor


void CGeneralPage::DoDataExchange(CDataExchange* pDX)
{
	ASSERT(m_pDoc!=NULL);
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CGeneralPage)
	DDX_Control(pDX, IDC_GPAGE_RANGE_TO_EDIT, m_editTo);
	DDX_Control(pDX, IDC_GPAGE_RANGE_FROM_EDIT, m_editFrom);
	DDX_Control(pDX, IDC_GPAGE_EDIT_HEIGHT, m_editHeight);
	DDX_Control(pDX, IDC_GPAGE_RANGE_TO_CHECK, m_checkAutoTo);
	DDX_Control(pDX, IDC_GPAGE_RANGE_FROM_CHECK, m_checkAutoFrom);
	DDX_Control(pDX, IDC_GPAGE_PROPS_STAMP_COMBO, m_comboStamps);
	DDX_Control(pDX, IDC_GPAGE_DATABLOCK_COMBO, m_comboDB);
	DDX_Control(pDX, IDC_GPAGE_DATABLOCK_CHECK, m_checkDB);
	DDX_Control(pDX, IDC_GPAGE_Y_COMBO, m_comboYScale);
	DDX_Control(pDX, IDC_GPAGE_X_COMBO, m_comboXScale);
	DDX_Control(pDX, IDC_GPAGE_PROPS_TEMPLATE_COMBO, m_templCombo);
	DDX_Control(pDX, IDC_GPAGE_CHECK_HEIGHT, m_checkAutoHeight);
	DDX_Text(pDX, IDC_GPAGE_EDIT_HEIGHT, m_height);
	DDX_Text(pDX, IDC_GPAGE_HEIGHT_DEFAULT, m_heightDefault);
	DDX_Text(pDX, IDC_GPAGE_HINT, m_hintText);
	DDX_Text(pDX, IDC_GPAGE_PROPS_PROPS, m_staticProps);
	DDX_Text(pDX, IDC_GPAGE_PROPS_STAMP, m_staticStamp);
	DDX_Text(pDX, IDC_GPAGE_RANGE_FROM, m_staticFrom);
	DDX_Text(pDX, IDC_GPAGE_RANGE_TO, m_staticTo);
	DDX_Text(pDX, IDC_GPAGE_STATIC_X_SCALE, m_staticXScale);
	DDX_Text(pDX, IDC_GPAGE_STATIC_Y_SCALE, m_staticYScale);
	DDX_Text(pDX, IDC_GPAGE_RANGE_TO_EDIT, m_ende);
	DDX_Text(pDX, IDC_GPAGE_RANGE_FROM_EDIT, m_anfang);
	//}}AFX_DATA_MAP

  DDX_Text( pDX, IDC_GPAGE_STATIC_SCALE, m_staticScale );
  DDX_Text( pDX, IDC_GPAGE_STATIC_RANGE, m_staticRange );
  DDX_Text( pDX, IDC_GPAGE_STATIC_HEIGHT, m_staticHeight );
	
  // falls wird kein Template editieren müssen die Bereiche richtig gesetzt sein
  if( !m_bTemplate )
	{
    CDoubleIRect rectProfile( m_pDoc->GetProfil()->GetTotalRect() );
		DDV_MinMaxDouble( pDX, m_anfang, rectProfile.left, rectProfile.right );
		DDV_MinMaxDouble( pDX, m_ende, rectProfile.left, rectProfile.right );
	}
}


BEGIN_MESSAGE_MAP( CGeneralPage, CPropertyPage )
	//{{AFX_MSG_MAP(CGeneralPage)
	ON_CBN_SELCHANGE( IDC_GPAGE_PROPS_TEMPLATE_COMBO, OnChangeTemplate )
  ON_CBN_SELCHANGE( IDC_GPAGE_X_COMBO, OnChange )
  ON_CBN_SELCHANGE( IDC_GPAGE_Y_COMBO, OnChange )
	ON_BN_CLICKED( IDC_GPAGE_CHECK_HEIGHT, OnChange )
  ON_BN_CLICKED( IDC_GPAGE_DATABLOCK_CHECK, OnChange )
	ON_EN_CHANGE( IDC_GPAGE_EDIT_HEIGHT, OnChange )
  ON_EN_CHANGE( IDC_GPAGE_RANGE_TO_EDIT, OnChange )
  ON_EN_CHANGE( IDC_GPAGE_RANGE_FROM_EDIT, OnChange )
	ON_BN_CLICKED( IDC_GPAGE_RANGE_FROM_CHECK, OnChange )
	ON_BN_CLICKED( IDC_GPAGE_RANGE_TO_CHECK, OnChange )
	ON_CBN_SELCHANGE( IDC_GPAGE_PROPS_STAMP_COMBO, OnChangeStempel )
  ON_CBN_SELCHANGE( IDC_GPAGE_DATABLOCK_COMBO, OnChange )
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CGeneralPage message handlers

BOOL CGeneralPage::OnInitDialog() 
{
	ASSERT( m_pDoc != NULL );
	CPropertyPage::OnInitDialog();

  // Internationalisierung
  m_checkAutoFrom.SetWindowText( CString( MAKEINTRESOURCE( IDS_GPAGE_RANGE_FROM_CHECK ) ) );
  m_checkAutoTo.SetWindowText( CString( MAKEINTRESOURCE( IDS_GPAGE_RANGE_TO_CHECK ) ) );
  m_checkDB.SetWindowText( CString( MAKEINTRESOURCE( IDS_GPAGE_DATABLOCK_CHECK ) ) );
  m_checkAutoHeight.SetWindowText( CString( MAKEINTRESOURCE( IDS_GPAGE_CHECK_HEIGHT ) ) );
  GetDlgItem( IDC_GPAGE_PROPS )->SetWindowText( CString( MAKEINTRESOURCE( IDS_GPAGE_PROPS ) ) );

	m_pParent->m_nActivePages++;
	
  // Seite mit Daten aus dem Document füllen
  Update( m_pDoc );

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CGeneralPage::Update( CPlotterDoc* pDataDoc )
// alle Daten aus dem aktuellen Doc laden
// Parameter:
//        CPlotterDoc* pDataDoc: die Seite wird mit Daten aus diesem Document gefüllt
//                              ( bis auf ein paar Ausnahmen )
{
  BOOL bApplyTemp = ( pDataDoc != m_pDoc );
	
	/****** reset controls ******/
	m_comboXScale.ResetContent();
	m_comboYScale.ResetContent();
	
  // wenn ein Template angewendet wird, die Templates nicht neu füllen ( sonst gibts ne Schleife )
  if( !bApplyTemp )
  {
    m_templCombo.ResetContent();
    m_comboStamps.ResetContent();
	}
  
  /****** scale ******/
  // get actual scale and update static controls
  CDoublePoint realScale, scale;
  m_pDoc->GetRealScale( realScale );
  m_pDoc->GetScale( scale );

  CString xScale;
	if( m_bTemplate && scale.x == 0 )	// doc is a template and scale is automatic
    xScale = TEXT( "?" );
	else							// doc is not a template or scale is not automatic
    xScale.Format( "%g", realScale.x );
  m_staticXScale.FormatMessage( IDS_GPAGE_STATIC_X_SCALE, xScale );

  CString yScale;
	if( m_bTemplate && scale.y == 0 )	// doc is a template and scale is automatic
    yScale = TEXT( "?" );
	else							// doc is not a template or scale is not automatic
		yScale.Format( "%g", realScale.y );
  m_staticYScale.FormatMessage( IDS_GPAGE_STATIC_Y_SCALE, yScale );

	// get intended scale
  pDataDoc->GetScale( scale );

  int xsel = -1;
  int ysel = -1;
	// add scales to controls and decide which item is to be selected
	for( int i = 0; i < NSCALES; i++ )
	{
    CString str;
		if( i == 0 )
      str.LoadString( IDS_AUTOMATIC );
		else
			str.Format( "%g", dscale[i] );
		m_comboXScale.AddString( str );
		m_comboYScale.AddString( str );
		if( scale.x == dscale[i] )
			xsel = i;
		if( scale.y == dscale[i] )
			ysel = i;
	}
	if( xsel == -1 )	// scale is not in the list of items
	{
    CString str;
		str.Format( "%g", scale.x );
		m_comboXScale.SetWindowText( str );
	}
	else			// scale is in the list of items
		m_comboXScale.SetCurSel( xsel );
	if( ysel == -1 )	// scale is not in the list of items
	{
    CString str;
		str.Format( "%g", scale.y );
		m_comboYScale.SetWindowText( str );
	}
	else			// scale is in the list of items
		m_comboYScale.SetCurSel( ysel );
	
  /****** range ******/

  // Achtung: im Template ( = pDataDoc ) können Ausmasse angegeben sein, die sich auf dieses 
  // Doc ( m_pDoc ) nicht anwenden lassen

  // gewünschte Ausmasse
  CProfil* pSollProfil = pDataDoc->GetProfil();  
  CDoubleIRect rectSoll( pSollProfil->GetTotalRect() ); // gewünschte Ausmasse

  // vorhandene Ausmasse
  CProfil* pIstProfil = m_pDoc->GetProfil();
  CDoubleIRect rectIst( pIstProfil->GetTotalRect() );
  
  CArray<int, int> dbArray;
  pIstProfil->GetDataBlocks( dbArray );

  // die ComboBox füllen, und merken, welcher der aktuelle Index ist
  m_comboDB.ResetContent();

  int selDbType = pSollProfil->GetRangeDbType();

  CString selDbName;
  for( int dbIndex = 0; dbIndex < dbArray.GetSize(); dbIndex++ )
  {
    int dbType = dbArray[dbIndex];

    // den Namen dieses DatenBlocks rausfinden
    DataBlock db( NULL, dbType );
    CString name = db.GetDesc( 0 );
    int index = m_comboDB.AddString( name );
    m_comboDB.SetItemData( index, dbType );

    if( dbType == selDbType )
      selDbName = name;
  }; // for i

  m_checkDB.SetCheck( !selDbName.IsEmpty() );
  if( m_comboDB.SelectString( -1, selDbName ) < 0 )
    m_comboDB.SetCurSel( 0 ); // im Zweifelsfall das erste selektieren

	// set auto anfang
  m_checkAutoFrom.SetCheck( pSollProfil->GetAutoAnfang() );

	// update static control
  CString strFrom, strTo;
	if( m_bTemplate )	// doc is a template
    strFrom = strTo = TEXT( "?" );
	else				// doc is not a template
	{
		strFrom.Format( "%.3f", rectIst.left );
		strTo.Format( "%.3f", rectIst.right );
	}
  strFrom.FormatMessage( IDS_GPAGE_RANGE_FROM_CHECK, strFrom );
  strTo.FormatMessage( IDS_GPAGE_RANGE_TO_CHECK, strTo );
  m_checkAutoFrom.SetWindowText( strFrom );
  m_checkAutoTo.SetWindowText( strTo );

	// update edit control
	if( m_checkAutoFrom.GetCheck() )
		m_anfang = rectIst.left;
	else
    m_anfang = pDataDoc->GetRangeFrom();

  if( !m_bTemplate )	// when doc is not a template ensure value lies in limits
	{
		if( m_anfang < rectIst.left || m_anfang >= rectIst.right )
      m_anfang = rectIst.left;
	}
	
  // set auto end
  m_checkAutoTo.SetCheck( pSollProfil->GetAutoEnde() );
	
	// update edit control
	if( m_checkAutoTo.GetCheck() )
		m_ende = rectIst.right;
	else
		m_ende = pDataDoc->GetRangeTo();

	if( !m_bTemplate )	// when doc is not a template ensure value lies in limits
	{
		if( m_ende <= rectIst.left ||	m_ende > rectIst.right )
      m_ende = rectIst.right;
	}

	/****** height ******/
	// set auto height
  m_checkAutoHeight.SetCheck( pSollProfil->GetAutoHeight() );
	
  // update static control
  CString strMaxHeight;
	if( m_bTemplate )	// doc is a template
    strMaxHeight = "?";
	else				// doc is not a template
    strMaxHeight.Format( "%.3f", pIstProfil->GetMaxHeight() );
  m_heightDefault.FormatMessage( IDS_GPAGE_HEIGHT_DEFAULT, strMaxHeight );

	// update edit control
	if( m_checkAutoHeight.GetCheck() )
		m_height = pIstProfil->GetHeight();
	else
		m_height = pSollProfil->GetHeight();

	/****** template ******/
	if( m_bTemplate )	// doc is a template (show Hinweis instead)
	{
    m_templCombo.ShowWindow( FALSE );
    m_comboStamps.ShowWindow( FALSE );
		GetDlgItem( IDC_GPAGE_PROPS_PROPS )->ShowWindow( FALSE );
		GetDlgItem( IDC_GPAGE_PROPS_STAMP )->ShowWindow( FALSE );
		GetDlgItem( IDC_GPAGE_PROPS )->ShowWindow( FALSE );
		
	}
	else				// doc is not a template
	{
		GetDlgItem( IDC_GPAGE_HINT )->ShowWindow( FALSE );
		if( !bApplyTemp )
		{
			// add text to controls
      CString str( MAKEINTRESOURCE( IDS_NONE ) );
			
      m_templCombo.AddString( str );
			m_comboStamps.AddString( str );
			
      CPlotterApp* pApp = GETPLOTTERAPP;
      pApp->LoadStempelFiles();
			
      int index = 0;
			CString file = m_pDoc->GetStempel()->GetFileName();
			// add stempels
			for( int i = 0; i < pApp->m_strStempelFiles.GetSize(); i++ )
			{
        CStempelDoc* pStpl = CStempelDoc::LoadStempel( pApp->m_strStempelFiles[i] );
				if( pStpl != NULL )
				{
          CString str;
					pStpl->GetName( str );
					m_comboStamps.AddString( str );
					pStpl->DeleteContents();
					delete pStpl; pStpl = NULL;

					if( file.CompareNoCase( pApp->m_strStempelFiles[i] ) == 0 )
						index = i + 1;
				}
			}
			
      int sel = -1;
			// add templates
      for( i = 0; i < pApp->m_templates.GetSize(); i++ )
			{
				CString str = pApp->m_templates[i]->GetName();
				m_templCombo.AddString( str );
				if( str == m_pDoc->m_pPData->m_eigenschaften )
					sel = i + 1;
				CString file = pApp->m_templates[i]->GetStempel()->GetFileName();
				for( int j = 0; j < pApp->m_strStempelFiles.GetSize(); j++ )
				{
					if( file.CompareNoCase( pApp->m_strStempelFiles[j] ) == 0 )
					{
						m_templatestempels.SetAt(i, j);
						if( index == 0 && str == m_pDoc->m_pPData->m_eigenschaften )
							index = j + 1;	// select this stempel instead
						break;
					}
        } // for j
      } // for i
			pApp->FlushStempelFiles();
			
      // select the correct item
			m_comboStamps.SetCurSel( index );
			if( sel != -1 )
        m_templCombo.SetCurSel( sel );
      else
        m_templCombo.SetCurSel( 0 );
    } // if bApplyTemp
  } // if m_bTemplate
	
  UpdateControls();
  UpdateData( FALSE );
	UpdateWindow();
} // Update

void CGeneralPage::OnOK() 
{
	CString str;
	CDoublePoint scale, oldScale;
	CDoublePoint minScale;
	CClientDC dc(NULL);
	int hppmm, vppmm;
	TCHAR szMin[32], szMax[32];
	CString prompt;
	int index, i;
	CTemplate *pTemp;
  CPlotterApp* plotApp = GETPLOTTERAPP;

	if( !UpdateData( TRUE ) )
		return;
	// scale
	m_comboXScale.GetWindowText( str );
	scale.x = atof( str );
	m_comboYScale.GetWindowText( str );
	scale.y = atof( str );
	hppmm = MulDiv(dc.GetDeviceCaps(HORZRES), 1, dc.GetDeviceCaps(HORZSIZE));
	vppmm = MulDiv(dc.GetDeviceCaps(VERTRES), 1, dc.GetDeviceCaps(VERTSIZE));
	minScale.x = fabs( m_pDoc->GetProfil()->GetTotalRect().Width() );
	minScale.x *= MM_FACTOR;
	minScale.x *= 1000;
	minScale.x /= hppmm;
	minScale.x /= INT_MAX;
	minScale.x = ceil(minScale.x)+1;
	minScale.y = fabs( m_pDoc->GetProfil()->GetTotalRect().Height() );
	minScale.y *= MM_FACTOR;
	minScale.y *= 1000;
	minScale.y /= vppmm;
	minScale.y /= INT_MAX;
	minScale.y = ceil(minScale.y)+1;
	if( scale.x != 0 && scale.x < minScale.x )
	{
		_stprintf(szMin, _T("%.0f"), minScale.x);
		_stprintf(szMax, _T("%.0f"), dscale[NSCALES-1]);
		AfxFormatString2(prompt, AFX_IDP_PARSE_REAL_RANGE, szMin, szMax);

		AfxMessageBox(prompt, MB_ICONEXCLAMATION, AFX_IDP_PARSE_REAL_RANGE);
		prompt.Empty(); // exception prep
		return;
	}
	if( scale.y != 0 && scale.y < minScale.y )
	{
		_stprintf(szMin, _T("%.0f"), minScale.y);
		_stprintf(szMax, _T("%.0f"), dscale[NSCALES-1]);
		AfxFormatString2(prompt, AFX_IDP_PARSE_REAL_RANGE, szMin, szMax);

		AfxMessageBox(prompt, MB_ICONEXCLAMATION, AFX_IDP_PARSE_REAL_RANGE);
		prompt.Empty(); // exception prep
		return;
	}
	m_pDoc->GetScale(oldScale);
	if (oldScale!=scale)
		m_pDoc->SetScale(scale);
	
  // range
  int dbIndex = m_comboDB.GetCurSel();
  int dbType = DST_UNKNOWN;
  if( m_checkDB.GetCheck() == FALSE || dbIndex == -1 )
    dbType = DST_UNKNOWN;
  else
    dbType = m_comboDB.GetItemData( dbIndex );
  m_pDoc->GetProfil()->SetRangeDbType( dbType );

  double minRange = m_pDoc->GetProfil()->GetTotalRect().left;
  double maxRange = m_pDoc->GetProfil()->GetTotalRect().right;
  if( dbType != DST_UNKNOWN )
  {
    m_pDoc->GetProfil()->GetDbRange( dbType, m_anfang, m_ende );
    m_anfang = max( m_anfang, minRange );
    m_ende = min( m_ende, maxRange );
  }
  else
  {
    if( m_checkAutoFrom.GetCheck() )
      m_anfang = minRange;
    if( m_checkAutoTo.GetCheck() )
      m_ende = maxRange;
    
    if( minRange != maxRange )
    {
      if( m_anfang >= m_ende && !m_bTemplate )
      {
        AfxMessageBox( IDS_INVALIDRANGE, MB_ICONEXCLAMATION );
        prompt.Empty(); // exception prep
        return;
      }
    }
  } // if tbType

	m_pDoc->GetProfil()->SetAutoAnfang( m_checkAutoFrom.GetCheck() );
	m_pDoc->SetRangeFrom( m_anfang );
	m_pDoc->GetProfil()->SetAutoEnde( m_checkAutoTo.GetCheck() );
	m_pDoc->SetRangeTo( m_ende );

	// height
  if( !m_checkAutoHeight.GetCheck() && m_pDoc->GetProfil()->GetMaxHeight() < m_height )
  {
    if( AfxMessageBox( IDS_WARN_LARGEBEZUG, MB_ICONQUESTION | MB_YESNO ) == IDNO )
      return;
  }

	if( m_checkAutoHeight.GetCheck() )
		m_height = m_pDoc->GetProfil()->GetMaxHeight();
	m_pDoc->GetProfil()->SetAutoHeight( m_checkAutoHeight.GetCheck() );
	m_pDoc->GetProfil()->SetHeight( m_height );
	// template
	
  if( m_bNewTemplate )
	{
		// disable update drawing and apply other pages
		int nFormat = m_pParent->m_nFormat;
		m_pParent->m_nFormat = 0;
		m_pParent->ApplyPages( FALSE );
		// re-enable update drawing
		m_pParent->m_nFormat = nFormat;
		// update pages
		m_pParent->m_bUpdatePagesNeeded = TRUE;
		index = m_templCombo.GetCurSel();
		if( index >= 0 )
		{
			if( index == 0 )
				m_pDoc->m_pPData->m_eigenschaften.LoadString( IDS_NONE );
			else
			{
				pTemp = plotApp->m_templates[index - 1];
				str = pTemp->GetName();
				m_pDoc->m_pPData->m_eigenschaften = str;
			}
#if _MFC_VER>=0x0421
			m_pDoc->m_pData->m_pSummInfo->SetTemplate( m_pDoc->m_pPData->m_eigenschaften );
#endif
		}
	}

	if( m_bNewStempel )
	{
    Profil* pProf;
		POSITION pos;
		CDrawObj *pObj;
		int n_SectionGetSize = m_pDoc->GetSections()->GetSize();//Dick 18.01.99
    if( n_SectionGetSize < 1 )
      n_SectionGetSize = 1;
		
    // get a profil
		if( m_pDoc->GetMSection( n_SectionGetSize - 1) == NULL )
      pProf = new Profil;
    else
    {
			m_pDoc->GetMSection( m_pDoc->GetSections()->GetSize() - 1 )->LoadProfil();
      Profil* profil = m_pDoc->GetMSection( m_pDoc->GetSections()->GetSize() - 1 )->GetProfil();
      pProf = profil->Clone( profil->GetOwner() );
		}

		// renew stempel
		pos = m_pDoc->GetStempel()->GetHeadPosition();
		while (pos!=NULL)
		{
			pObj = m_pDoc->GetStempel()->GetNextObject(pos);
      m_pDoc->RemoveObject( pObj );
			if (pObj->IsText())
			{
				((CDrawRect*)pObj)->GetText(str);
				if (!str.IsEmpty())
				{
					switch(((CDrawRect*)pObj)->GetStempelTextType())
					{
						default:
							break;
					
						case STPL_TEXT_AG1:
							pProf->SetClient(0, str);
							break;
					
						case STPL_TEXT_AG2:
							pProf->SetClient(1, str);
							break;
					
						case STPL_TEXT_PB1:
							pProf->SetProjectDesc(0, str);
							break;
					
						case STPL_TEXT_PB2:
							pProf->SetProjectDesc(1, str);
							break;
					
						case STPL_TEXT_PB3:
							pProf->SetProjectDesc(2, str);
							break;
					
						case STPL_TEXT_BB1:
							pProf->SetPageDesc(0, str);
							break;
					
						case STPL_TEXT_BB2:
							pProf->SetPageDesc(1, str);
							break;
					
						case STPL_TEXT_BB3:
							pProf->SetPageDesc(2, str);
							break;
					
						case STPL_TEXT_PN:
							pProf->SetProjectNum(str);
							break;
					
						case STPL_TEXT_DAT:
							break;
					
						case STPL_TEXT_BN:
							pProf->SetPageNum(str);
							break;
					
						case STPL_TEXT_MS:
							break;
					
						case STPL_TEXT_UD:
							break;

						case STPL_TEXT_ZU:
							break;
					}
				}
			}
			pObj->Remove();
		}
		index = m_comboStamps.GetCurSel();
		if (index>0)
		{
			BOOL bCreated = FALSE;
			i = m_templCombo.GetCurSel();
			if (i>=0)
			{
				if (i==0)
				{
					pTemp = new CTemplate;
					pTemp->CreateDrawing();
				}
				else
				{
					pTemp = plotApp->m_templates[i-1];
					str = pTemp->GetStempel()->GetFileName();
				}
				plotApp->LoadStempelFiles();
				pTemp->GetStempel()->SetFileName( plotApp->m_strStempelFiles[index - 1] );
				m_pDoc->GetStempel()->SetFileName( plotApp->m_strStempelFiles[index - 1] );
				plotApp->FlushStempelFiles();
				m_pDoc->CreateStempelFromTemplate(pTemp, pProf);
				if (i==0)
				{
					pTemp->DeleteContents();
					delete pTemp;
				}
				else
					pTemp->GetStempel()->SetFileName( str );
			}
		}
		else
		{
			str.Empty();
			m_pDoc->GetStempel()->SetFileName( str );
		}

    delete pProf;
    pProf = NULL;
      
		// update other pages to reflect new stempel
		m_pParent->m_bUpdatePagesNeeded = TRUE;
	}
	if( m_templCombo.GetCurSel() > 0 )
		m_templCombo.GetWindowText( m_pDoc->m_pPData->m_eigenschaften );
	
	bOK = TRUE;
	CPropertyPage::OnOK();
}

void CGeneralPage::UpdateControls()
// aktiviert oder deaktiviert bestimmte Fenster in abhängigkeit der anderen Daten
{
  BOOL bCheckDB = m_checkDB.GetCheck();

  m_editHeight.EnableWindow( !m_checkAutoHeight.GetCheck() );

  m_editFrom.EnableWindow( !m_checkAutoFrom.GetCheck() && !bCheckDB );
  m_editTo.EnableWindow( !m_checkAutoTo.GetCheck() && !bCheckDB );
  m_checkAutoFrom.EnableWindow( !bCheckDB );
  m_checkAutoTo.EnableWindow( !bCheckDB );
  m_comboDB.EnableWindow( bCheckDB );
} // EnableControls

void CGeneralPage::OnChange() 
{
  UpdateControls();

	SetModified( TRUE );
	m_pParent->m_nFormat = 1;
}

BOOL CGeneralPage::OnApply() 
{
	bOK = FALSE;
	CPropertyPage::OnApply();
	if( !bOK )
		return FALSE;
	
  m_bNewStempel = FALSE;
	m_pParent->AttemptUpdateDrawing();
	
  return TRUE;
}

void CGeneralPage::OnChangeTemplate() 
{
	int index = m_templCombo.GetCurSel();
	if( index >= 0 )
	{
		CTemplate* pTemp;

		int j = 0;
		if( index == 0 )
		{
			pTemp = new CTemplate;
			pTemp->CreateDrawing();
		}
		else
		{
			pTemp = GETPLOTTERAPP->m_templates[index - 1];
      int i;
			if( m_templatestempels.Lookup( index - 1, i ) )
				j = i + 1;
		}
		m_comboStamps.SetCurSel( j );
		m_bNewStempel = TRUE;
		m_bNewTemplate = TRUE;
		m_pParent->ApplyTemplate( pTemp );
		m_pParent->m_bUndo = FALSE;
		if( index == 0 )
		{
			pTemp->DeleteContents();
			delete pTemp;
		}
	}

  OnChange();
} // OnChangeTemplate

void CGeneralPage::OnChangeStempel() 
{
	m_bNewStempel = TRUE;
	m_pParent->m_bUndo = FALSE;

  OnChange();
} // OnChangeStempel

void CGeneralPage::ApplyTemplate( CTemplate *pTemp )
// füllt die Seite mit Daten aus dem Template
{
  Update( pTemp );
}

