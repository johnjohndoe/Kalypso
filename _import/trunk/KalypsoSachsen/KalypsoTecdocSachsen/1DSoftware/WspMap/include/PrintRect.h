////////////////////////////////////////////////////////
//  An interface for all cinds of elements in rects,  //
////////////////////////////////////////////////////////

#ifndef _PRINT_RECT_H_INCLUDED_
#define _PRINT_RECT_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#define HANDLE_WIDTH 5	// handle width (Pixel)

#include "..\..\commonMfc\commonMfc.h"

//////////////////////////////////////////////////////////////////////////////////
// Klasse CPrintRectListener
// Interface für Klassen, welche auf Update-Ereignisse der PrintRects warten
// Solche können sich bei CPrintRect durch AddListener und RemoveListener anmelden
// bzw. abmelden
// Dies ist eine Implementation des Publisher-Subscriber Patterns
//////////////////////////////////////////////////////////////////////////////////
class CPrintRectListener
{
public:
  // wird aufgerufen, wenn sich die Grösse oder Position eines PrintRects geändert hat
  virtual void Update( const CRect& ) = 0;
}; // class CPrintRectListener


//////////////////////////////////////////////////////////////////////////////////
// Klasse CPrintRect:
// Die Abstrakte Oberklasse für Druckrechtecke in der Drcukvorschau
// Jedes solche Objekt hat einen Rand und einen Eigenschaftsdialog
//////////////////////////////////////////////////////////////////////////////////
class CPrintRect : public CObject
{
public:
  CPrintRect();
  virtual ~CPrintRect() {};

  DECLARE_SERIAL( CPrintRect ); // diese Klasse ist Serialisierbar

  enum TrackerState { normal, selected, active };
  
public:
  CRect GetBounds() const { return m_bounds; }
  virtual void SetBounds( const CRect&, CDC* );

  const int GetBorderStyle(){ return m_borderStyle;}
  void SetBorderStyle( const int style ) { m_borderStyle = style; };

  COLORREF GetBorderColor() const { return m_borderColor; };
  void SetBorderColor( const COLORREF color ) { m_borderColor = color; };

  void SetBorderWidth( UINT width );
  UINT GetBorderWidth(){ return m_borderWidth; }

  virtual void SetMap( CMoMap* pMoMap );
  CMoMap* GetMap() { return m_pMoMap; };
  
  //Methods
public:
  //paint the rect and rectborder
  virtual void Paint( CDC* );
  //paint the rectborder
  void PaintTracker(CDC*, TrackerState);
  
  // Ruft den Eigenschaftsdialog auf
  int DoPropertyDialog( CWnd* pParent = NULL );

  // An- und Abmeldung der CPrintRectListener
  void AddListener( CPrintRectListener* );
  void RemoveListener( CPrintRectListener* );
  const void SetState( TrackerState state ){ m_state = state;};
  const TrackerState GetState(){return m_state;};
  CPoint GetHandle(int);
  CRect GetHandleRect(int,CDC* );

  void NotifyListeners( const CRect& ); // Benachrichtigt alle Listener

  virtual void AdjustBounds( CDC* pDC, CRect& bounds ) {}; // die Grösse des Druckrechteckes an den Inhalt anpassen

protected:
  void Serialize( CArchive& ar );
  virtual CPropertyPage* CreatePropPage( UINT captionID ) { return NULL; }; // eine Eigenschaftsseite erzeugen

  //Attribute
protected:
  CMoMap* m_pMoMap;
  	
private:
  UINT m_borderStyle;  //style of border
  UINT m_borderWidth;
  COLORREF m_borderColor;

  CRect m_bounds;

  CTypedPtrArray<CPtrArray, CPrintRectListener*> m_listeners; // die Liste der PrintRectListener
  TrackerState m_state;
};

#endif _PRINT_RECT_H_INCLUDED_