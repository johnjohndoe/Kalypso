/*! Time-stamp: <@(#)datablocktypechooserdialog.h   29.08.02 - 10:47:12   Belger>
 *********************************************************************
 *  @file   : datablocktypechooserdialog.h
 *
 *  Project : WSPWIN
 *
 *  Package : ProjektManager
 *
 *  Company : BCE
 *
 *  Author  : Belger                              Date: 29.08.02
 *
 *  Purpose : Declaration of class CDataBlockTypeChooserDialog
 *
 *********************************************************************
 */
#if !defined(AFX_DATABLOCKTYPECHOOSERDIALOG_H__5CACD843_BB21_11D6_B31A_00104BB3E525__INCLUDED_)
#define AFX_DATABLOCKTYPECHOOSERDIALOG_H__5CACD843_BB21_11D6_B31A_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// datablocktypechooserdialog.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CDataBlockTypeChooserDialog 

class CDataBlockTypeChooserDialog : public CDialog
{
  // Konstruktion
public:
  CDataBlockTypeChooserDialog(CWnd* pParent = NULL);   // Standardkonstruktor
  
  // Dialogfelddaten
  //{{AFX_DATA(CDataBlockTypeChooserDialog)
  CListCtrl	m_dbList;
  //}}AFX_DATA
  
  
  // Überschreibungen
  // Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
  //{{AFX_VIRTUAL(CDataBlockTypeChooserDialog)
protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
  //}}AFX_VIRTUAL
  
  // Implementierung
public:
  void GetDatablockTypes( CArray<int, int>& dbArray ) const;
protected:
  CArray<int, int> m_dbs;
  
  // Generierte Nachrichtenzuordnungsfunktionen
  //{{AFX_MSG(CDataBlockTypeChooserDialog)
  virtual BOOL OnInitDialog();
  virtual void OnOK();
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_DATABLOCKTYPECHOOSERDIALOG_H__5CACD843_BB21_11D6_B31A_00104BB3E525__INCLUDED_
