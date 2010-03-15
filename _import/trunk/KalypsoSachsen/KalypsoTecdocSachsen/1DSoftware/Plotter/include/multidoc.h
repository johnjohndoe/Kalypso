/*! Time-stamp: <@(#)multidoc.h   02.12.02 - 17:59:46   Belger>
 *********************************************************************
 *  @file   : multidoc.h
 *
 *  Author  : Belger                              Date: 02.12.02
 *
 *  Purpose : Dokumentenklasse für die Option 'Mehrere Plots auf ein Blatt'.
 *            Solche Dokumente werden erzeugt, wenn im Datei öffnen Dialog (bei
 *            geöffneten Projekt) das Kästchen 'Mehrere Plots auf ein Blatt'
 *            gewählt wird.
 *
 *********************************************************************
 */

#ifndef _MULTI_DOC_H_INCLUDED_
#define _MULTI_DOC_H_INCLUDED_

#include "commonMfc\include\printerSettings.h"

#include "drawdoc.h"

class SectionArray;
class CPlotterDoc;
class CPlotterFrame;
class CDrawObjList;

/*!
 * @class  : CMultiDoc 
 * 
 * Die Dokumentenklasse für 'MultiPlots', d.h. mehrere Plots auf
 * ein Blatt. Per 'SetSections' wird festgelegt, welche Profile dargestellt
 * werden sollen. Ansonsten beruht diese Klasse auf CDrawDoc.
 * Das ganze beruht auf einem üblen Hack: die Profile werden als CPlotterDocs erzeugt und
 * dort für eine Zeichnung formatiert. Die dort erzeugten CDrawObj-ekte werden einfach in dieses Dokument
 * geclont und neu angeordnet (relativ dirty, funktiuoniert aber gut).
*/
class CMultiDoc : public CDrawDoc
{
  DECLARE_SERIAL( CMultiDoc );

private:
  /// Die Seitenüberschrift
  static CString DEFAULT_TITEL;

  ////////////////////////////////
  // Konsturktion / Destruktion //
  ////////////////////////////////
protected:
  /// Der Standardkonstruktor, protected, damit nur dynamisch erzeugt werden kann.
  CMultiDoc();

  /// Der Standarddesturktor.
  virtual ~CMultiDoc() {};

  ////////////////////////////////////
  // Überschreibungen von CDocument //
  ////////////////////////////////////
public:
  /// Gibt die Abstände x/y zurück
  const CSize& GetGaps() const { return m_gaps; }; 
  /// Setzt die Abstände in x/y Richtung
  void SetGaps( const CSize& gaps ) { m_gaps = gaps; };

  /// findet das Objekt, welches den Titel repräsentiert
  CDrawRect* GetTitleObject() const;

  /// Gibt die Blattüberschrift zurück
  CString GetPageTitle() const;
  
  /// Setzt die Blattüberschrift
  void SetPageTitle( const CString& pageTitle );

  /////////////////////
  // Membervariablen //
  /////////////////////
private:
  // Einstellungen

  /// Die Abstände zwischen den Profilen in x (cx) und y (cy) Richtung.
  CSize m_gaps;

  /////////////////
  // Operationen //
  /////////////////
public:
  /// Legt fest, welche Profile angezeigt werden sollen.
  void SetSections( const SectionArray& sections );
  
  /// Erneuert die ganze Zeichnung.
  virtual void UpdateDrawing();

  /// Serialisierung des gesamten Dokumentes
  virtual void Serialize( CArchive& ar );

  /// Zeigt den EigenschaftenDialog an
  afx_msg BOOL OnProperties();

private:
  /// Hilfsfunktion, um die CDrawObj's aus den CPlotterDoc's z klauen und an die richtige Stelle zu schieben.
  void MoveIntoExtent( const CDrawObjList& objList, const CIntIRect& iRect );
  void CloneObjectsToLayer( const CDrawObjList& objList, CDrawLayer* pLayer );

#ifdef _DEBUG
  static log4cpp::Category& m_logCat;
#endif _DEBUG

protected:
  DECLARE_MESSAGE_MAP()
}; // CMultiDoc


#endif  // _MULTI_DOC_H_INCLUDED_

/////////////////////////////////////////////////////////////////////////////
