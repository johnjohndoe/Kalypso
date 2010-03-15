// profilmodel.h: Schnittstelle für die Klasse CProfilModel.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PROFILMODEL_H__2FC95AF3_138B_11D8_B480_00104BB3E525__INCLUDED_)
#define AFX_PROFILMODEL_H__2FC95AF3_138B_11D8_B480_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "commonMfc/include/rect.h"

class CProfilModel  
{
public:
  enum Type { tf, db, bv };

  /** Fliesszonen des Gewässers */
  enum Zone { RL, VL, HF, VR, RR, out };

  typedef std::pair<CDoublePoint, CDoublePoint> WspEntry;

  class Grenze
  {
  public:
    Grenze() {};    
    Grenze( const double x, const Type type ) : typ( type ), x( x ) {};

  public:
    const bool operator <( const Grenze& other ) const { return x < other.x; };

  public:
    Type typ;
    double x;
  };

  class WSP
  {
  public:
     unsigned long color;
     WspEntry entry;
  };

  class ProfilPoint
  {
  public:
    ProfilPoint() : x( 0.0 ), y( 0.0 ), rw( 0.0 ), hw( 0.0 ) {};
    ProfilPoint( const double x, const double y, const double rw, const double hw) : x( x ), y( y ), rw( rw ), hw( hw ) {};

  public:
    double x;
    double y;
    double rw;
    double hw;
  };

  class InterpolException {};

  static ProfilPoint NO_POINT;

public:
  typedef std::vector<ProfilPoint> PointCollection;
  typedef PointCollection::const_iterator PointIterator;
  typedef std::list<Grenze> GrenzenCollection;
  typedef GrenzenCollection::const_iterator GrenzenIterator;

public:
	CProfilModel();

  void Clear();
  void AddPoint( const ProfilPoint& point );
  void AddPoint( const double x, const double y, const double rw, const double hw );
  const ProfilPoint GetPoint( const int index ) const { return m_points[index]; };
  PointIterator GetPointBegin() const { return m_points.begin(); };
  PointIterator GetPointEnd() const { return m_points.end(); };

  ProfilPoint GetInterpolPointAt( const double x ) const;
  
  const CDoubleRect& GetExtent() const { return m_extent; };
  
  void AddMfb( const double x ) { m_mfbTrenner.Add( x ); };
  int GetMfbCount() const { return m_mfbTrenner.GetSize(); };
  double GetMfb( const int index ) const { return m_mfbTrenner[index]; };
  
  void AddGrenze( const double x, const Type type );
  GrenzenIterator GetGrenzenBegin() const { return m_grenzen.begin(); };
  GrenzenIterator GetGrenzenEnd() const { return m_grenzen.end(); };
  
  void SetTypeColor( const Type type, const unsigned long color ) { m_typeColors.SetAt( type, color ); };
  unsigned long GetTypeColor( const Type type ) const;

  void SetStation( const double station ) { m_station = station; };
  double GetStation() const { return m_station; };

  void AddWsp( WSP wsp ) { m_wsps.Add( wsp ); };
  WSP GetWsp( const int index ) const { return m_wsps[index]; };
  int GetWspCount() const { return m_wsps.GetSize(); };

  /** Ermittelt die Fliesszone, für einen beliebiegen Punkt */
  Zone GetZone( const double x );

private:
  PointCollection m_points;
  GrenzenCollection m_grenzen;

  CArray<double, double> m_mfbTrenner;
  CMap<Type, Type, unsigned long, unsigned long> m_typeColors;
  CArray<WSP, WSP&> m_wsps;
  CDoubleRect m_extent;
  double m_station;
};

#endif // !defined(AFX_PROFILMODEL_H__2FC95AF3_138B_11D8_B480_00104BB3E525__INCLUDED_)
