/*! Time-stamp: <@(#)wspinsertdialog.h   28.08.02 - 08:35:03   Belger>
*********************************************************************
*  @file   : wspinsertdialog.h
*
*  Project : WSPWIN
*
*  Package : Common-Lib
*
*  Company : BCE
*
*  Author  : Belger                              Date: 28.08.02
*
*  Purpose : Declaration of class CWSPInsertDialog
*
*********************************************************************
*/

#if !defined(AFX_WSPINSERTDIALOG_H__8F035E63_BA45_11D6_B319_00104BB3E525__INCLUDED_)
#define AFX_WSPINSERTDIALOG_H__8F035E63_BA45_11D6_B319_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CWSPInsertDialog : public CDialog
{
  // Konstruktion
public:
  CWSPInsertDialog(CWnd* pParent = NULL);   // Standardkonstruktor
  
  // Dialogfelddaten
  //{{AFX_DATA(CWSPInsertDialog)
  CStatic	m_abflussStatic;
  CEdit	m_abflussEdit;
  CButton	m_constraintCheck;
  //}}AFX_DATA
  
  
  // Überschreibungen
  // Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
  //{{AFX_VIRTUAL(CWSPInsertDialog)
protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
  //}}AFX_VIRTUAL
  
  // Implementierung
public:
  BOOL GetDurchst() const { return !m_bDurchst; };
  CString GetAbflussFormat() const { return m_strAbflussFormat; };

protected:
  BOOL m_bDurchst; // nur nach OK valid
  CString m_strAbflussFormat; // nur nach OK valid

  
  // Generierte Nachrichtenzuordnungsfunktionen
  //{{AFX_MSG(CWSPInsertDialog)
  virtual BOOL OnInitDialog();
  virtual void OnOK();
  //}}AFX_MSG
  DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_WSPINSERTDIALOG_H__8F035E63_BA45_11D6_B319_00104BB3E525__INCLUDED_
