#ifndef _ITEM_CHOOSER_H_INCLUDED_
#define _ITEM_CHOOSER_H_INCLUDED_

#include "stdafx.h"
#include "..\..\commonMfc\include\commResource.h"

template<typename T>
class CItemChooser : public CDialog
{
public:
	typedef CMap<CString, LPCTSTR, T, T> TypeMap;
	
  CItemChooser( const TypeMap& map, const CString& title, const CString& text, CWnd* pParentWnd = 0 ) : CDialog( IDD_ITEM_CHOOSER, pParentWnd )
  {
    // einfach die ganze Map kopieren
    POSITION pos = map.GetStartPosition();
    while( pos )
    {
      T item;
      CString string;
      map.GetNextAssoc( pos, string, item );

      m_map.SetAt( string, item );
    }; // while state

    m_titel = title;
    m_text = text;
  };

  virtual BOOL OnInitDialog()
  {
    CDialog::OnInitDialog();

    SetWindowText( m_titel );
    m_okButton.SetWindowText( CString( MAKEINTRESOURCE( IDS_OK ) ) );
    m_cancelButton.SetWindowText( CString( MAKEINTRESOURCE( IDS_CANCEL ) ) );
    
    // ComboBox füllen
    POSITION pos = m_map.GetStartPosition();
    while( pos )
    {
      T item;
      CString string;
      m_map.GetNextAssoc( pos, string, item );

      int index = m_comboBox.AddString( string );
      m_comboBox.SetItemDataPtr( index, new T(item) );
    }; // while state
    
    int itemCount = m_comboBox.GetCount();

    if( itemCount == 0 )
      OnCancel(); // gleich den Dialog abbrechen

    m_comboBox.SetCurSel( 0 );

    if( itemCount == 1 )
      OnOK();

    m_comboBox.SetFocus();
    
    return FALSE;  // return TRUE unless you set the focus to a control
  };

  virtual void OnOK()
  {
    int index = m_comboBox.GetCurSel();
    if( index != CB_ERR )
      m_selItem = *( (T*)m_comboBox.GetItemDataPtr( index ) );
    
    EndDialog( IDOK );
  };

  // nur überschrieben, damit die eigene EndDialog aufgerufen wird
  virtual void OnCancel()
  {
    EndDialog( IDCANCEL );
  };



  void EndDialog( int nResult )
  {
    // überschrieben, um die T's in der ComboBox zu zerstören
    for( int i = 0; i < m_comboBox.GetCount(); i++ )
    {
      delete m_comboBox.GetItemDataPtr( i );
      m_comboBox.SetItemDataPtr( i, 0 );
    }; // for i

    CDialog::EndDialog( nResult );
  }; // EndDialog

  T GetSelectedItem() const { return m_selItem; };

protected:
	virtual void DoDataExchange( CDataExchange* pDX )
  {
    CDialog::DoDataExchange( pDX );

    DDX_Control( pDX, IDOK, m_okButton );
    DDX_Control( pDX, IDCANCEL, m_cancelButton );
    DDX_Control( pDX, IDC_ITEM_CHOOSER_COMBO, m_comboBox );
    DDX_Text( pDX, IDC_ITEM_CHOOSER_TEXT, m_text );
  }; // DoDataExChange

private:
  // GUI - Elemente
  CButton	m_okButton;
  CButton	m_cancelButton;
	CComboBox	m_comboBox;
  CString m_titel;
  CString m_text;

  // Implementation
  TypeMap m_map; // die Übergebenen Daten

  T m_selItem; // das selektierte Item
}; // class CItemChooser


#endif _ITEM_CHOOSER_H_INCLUDED_