/*----------------    FILE HEADER  ------------------------------------------

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Copyright (C) 2002 Wolfgang Baer - WBaer@gmx.de
 
 Adapted May 2003 by IDgis, The Netherlands - www.idgis.nl
 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.io.rtree;

/**
 * HyperBoundingBox für Objekte im mehrdimensionalen Raum.
 * 
 * @version 1.0
 * @author Wolfgang Bär
 */
public class HyperBoundingBox
{
  private HyperPoint pMax;

  private HyperPoint pMin;

  /**
   * Konstruktor HyperBoundingBox. Erzeugt eine BoundingBox im Hyperraum für die
   * übergebenen Punkte.
   * 
   * @param pMin -
   *          Minimum Punkt im Hyperraum
   * @param pMax -
   *          Maximum Punkt im Hyperraum
   */
  public HyperBoundingBox( HyperPoint pMin, HyperPoint pMax )
  {
    if( pMin.getDimension() != pMax.getDimension() )
    {
      throw new IllegalArgumentException( "HyperPoints need same Dimension" );
    }

    this.pMin = pMin;
    this.pMax = pMax;
  }

  /**
   * Erzeugt eine Null-HyperBoundingBox
   * 
   * @param dimension
   *          der BoundingBox
   * @return HyperBoundingBox Null-HyperBoundingBox
   */
  public static HyperBoundingBox getNullHyperBoundingBox( int dimension )
  {
    return new HyperBoundingBox( HyperPoint.getNullHyperPoint( dimension ), HyperPoint
        .getNullHyperPoint( dimension ) );
  }

  /**
   * Gibt den Minimum Punkt der HyperBoundingBox zurück.
   * 
   * @return HyperPoint Minimum Punkt
   */
  public HyperPoint getPMin()
  {
    return pMin;
  }

  /**
   * Gibt den Maximum Punkt der HyperBoundingBox zurück.
   * 
   * @return HyperPoint Maximum Punkt
   */
  public HyperPoint getPMax()
  {
    return pMax;
  }

  /**
   * Gibt die Anzahl der Dimensionen der HyperBoundingBox
   * 
   * @return int Anzahl der Dimensionen
   */
  public int getDimension()
  {
    return pMin.getDimension();
  }

  /**
   * Test, ob die HyperBoundingBox mit der übergebenen überlappt.
   * 
   * @param box -
   *          HyperBoundingBox zum Test
   * @return boolean true, wenn überlappt
   */
  public boolean overlaps( HyperBoundingBox box )
  {
    boolean intersect = true;

    for( int i = 0; i < getDimension(); i++ )
    {
      if( ( pMin.getCoord( i ) > box.getPMax().getCoord( i ) )
          || ( pMax.getCoord( i ) < box.getPMin().getCoord( i ) ) )
      {
        intersect = false;
        break;
      }
    }

    return intersect;
  }

  /**
   * Test, ob die HyperBoundingBox die übergebene enthaelt.
   * 
   * @param box -
   *          HyperBoundingBox zum Test
   * @return boolean true, wenn enthalten
   */
  public boolean contains( HyperBoundingBox box )
  {
    boolean contains = true;

    for( int i = 0; i < getDimension(); i++ )
    {
      if( ( pMin.getCoord( i ) > box.getPMin().getCoord( i ) )
          || ( pMax.getCoord( i ) < box.getPMax().getCoord( i ) ) )
      {
        contains = false;
        break;
      }
    }

    return contains;
  }

  /**
   * Gibt die Ausdehnung der HyperBoundingBox.
   * 
   * @return double Ausdehnung
   */
  public double getArea()
  {
    double area = 1;

    for( int i = 0; i < pMin.getDimension(); i++ )
      area = area * ( pMax.getCoord( i ) - pMin.getCoord( i ) );

    return area;
  }

  /**
   * Berechnet die Vereinigungs-HyperBoundingBox mit der übergebenen BOX.
   * 
   * @param box
   * @return HyperBoundingBox Vereinigungs-HyperBoundingBox
   */
  public HyperBoundingBox unionBoundingBox( HyperBoundingBox box )
  {
    if( this.getDimension() != box.getDimension() )
    {
      throw new IllegalArgumentException( "HyperBoundingBox need same Dimension" );
    }

    if( this.equals( HyperBoundingBox.getNullHyperBoundingBox( this.getDimension() ) ) )
    {
      return box;
    }

    if( box.equals( HyperBoundingBox.getNullHyperBoundingBox( this.getDimension() ) ) )
    {
      return this;
    }

    double[] min = new double[this.getDimension()];
    double[] max = new double[this.getDimension()];

    for( int i = 0; i < this.getDimension(); i++ )
    {
      if( this.getPMin().getCoord( i ) <= box.getPMin().getCoord( i ) )
      {
        min[i] = this.getPMin().getCoord( i );
      }
      else
      {
        min[i] = box.getPMin().getCoord( i );
      }

      if( this.getPMax().getCoord( i ) >= box.getPMax().getCoord( i ) )
      {
        max[i] = this.getPMax().getCoord( i );
      }
      else
      {
        max[i] = box.getPMax().getCoord( i );
      }
    }

    return new HyperBoundingBox( new HyperPoint( min ), new HyperPoint( max ) );
  }

  /**
   * Berechnet das minimale Abstandsquadrat der HyperBoundingBox zum HyperPoint.
   * Nach Roussopoulos Nick: Nearest Neighbor Queries - MINDIST
   * 
   * @param point
   *          für Abstandberechnung.
   * @return double minimaler Abstand.
   */
  public double minDist( HyperPoint point )
  {
    double min = 0;
    double ri = 0;

    for( int i = 0; i < point.getDimension(); i++ )
    {
      if( point.getCoord( i ) < this.pMin.getCoord( i ) )
      {
        ri = this.pMin.getCoord( i );
      }
      else
      {
        if( point.getCoord( i ) > this.pMax.getCoord( i ) )
        {
          ri = this.pMax.getCoord( i );
        }
        else
        {
          ri = point.getCoord( i );
        }
      }

      min = min + Math.pow( point.getCoord( i ) - ri, 2 );
    }

    return min;
  }

  /**
   * Erstellt eine tiefe Kopie der HyperBoundingBox. - in Verbindung mit clone()
   * bei HyperPoint
   * 
   * @return Object geklonte HyperBoundingBox
   */
  public Object clone()
  {
    return new HyperBoundingBox( (HyperPoint)pMin.clone(), (HyperPoint)pMax.clone() );
  }

  /**
   * Gibt eine Stringrepräsentation der Box.
   * 
   * @return String Stringrepräsentation der Box
   */
  public String toString()
  {
    return "BOX: P-Min (" + pMin.toString() + "), P-Max (" + pMax.toString() + ")";
  }

  /**
   * Überschreibt equals in Class Object.
   * 
   * @see java.lang.Object
   */
  public boolean equals( Object obj )
  {
    HyperBoundingBox box = (HyperBoundingBox)obj;
    return ( this.pMin.equals( box.pMin ) && this.pMax.equals( box.pMax ) );
  }
}