#if !defined(AFX_TRIPLE_H__815BD411_B6B4_11D7_B419_00104BB3E525__INCLUDED_)
#define AFX_TRIPLE_H__815BD411_B6B4_11D7_B419_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "../../bce/include/polyline.h"

#include <vector>

// lokale Struktur für die Funktion CutWithLine
struct Triple
{
public:
  double rw, hw, hoehe, breite;
  CStringArray attribute;
  
  Triple( const double rw, const double hw, const double hoehe, const double breite, const CStringArray& attribute )
  {
    this->rw = rw;
    this->hw = hw;
    this->hoehe = hoehe;
    this->breite = breite;
    this->attribute.Copy( attribute );
  };
}; // struct Tripple

class TripleArray : public std::vector<Triple*>, public BCE::Geometry::IPolyLine
{
public:
  void Paint( CDC* dc, CRect* extent ) const;
  int Smooth( const double param, const bool bDelete );
  void RemoveAt( const int index, const bool bDelete );

public:
  /** Implementation von Interface IPolyLine */
  virtual int pointCount() const  { return size(); }
  virtual double xAt( const int index ) const { return at( index )->breite; }
  virtual double yAt( const int index ) const { return at( index )->hoehe; }
};

#endif // !defined(AFX_TRIPLE_H__815BD411_B6B4_11D7_B419_00104BB3E525__INCLUDED_)
