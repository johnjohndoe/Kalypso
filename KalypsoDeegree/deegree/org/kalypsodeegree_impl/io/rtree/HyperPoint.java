/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.io.rtree;

/**
 * Punkt im multidimensionalen Raum. Basierend auf double-Koordinatenwerten.
 * 
 * @version 1.0
 * @author Wolfgang Bär
 */
public class HyperPoint
{
  private double[] coords;

  /**
   * Konstruktor für eine Multidimensionalen Punkt.
   * 
   * @param coords
   *          double[] Array von double-Werten mit den Koordinaten für jede Dimension. Länge des Array entspricht der
   *          Dimension des Punktes
   */
  public HyperPoint( double[] coords )
  {
    this.coords = coords;
  }

  /**
   * Erzeugt einen Null-HyperPoint
   * 
   * @param dimension
   *          des Punktes
   * @return HyperPoint
   */
  public static HyperPoint getNullHyperPoint( int dimension )
  {
    double[] point = new double[dimension];

    for( int i = 0; i < dimension; i++ )
      point[i] = Double.NaN;

    return new HyperPoint( point );
  }

  /**
   * Gibt die Koordinaten des Punktes zurück.
   * 
   * @return double[] Koordinaten des Punktes
   */
  public double[] getCoords()
  {
    return coords;
  }

  /**
   * Gibt die Koordinate zur Dimension index des Punktes.
   * 
   * @param index
   * @return double Koordinate
   */
  public double getCoord( int index )
  {
    return coords[index];
  }

  /**
   * Gibt die Dimension des Punktes:
   * 
   * @return int Dimension
   */
  public int getDimension()
  {
    return coords.length;
  }

  /**
   * Gibt eine Stringrepräsentation des HyperPoints.
   * 
   * @return String Stringrepräsentation des Hyperpoints
   */
  public String toString()
  {
    String ret = "";

    for( int i = 0; i < coords.length; i++ )
    {
      ret += ( coords[i] + ", " );
    }

    return ret;
  }

  /**
   * Überschreibt equals in Class Object.
   * 
   * @see java.lang.Object
   */
  public boolean equals( Object obj )
  {
    HyperPoint point = (HyperPoint)obj;
    boolean state = true;

    for( int i = 0; i < coords.length; i++ )
      if( this.coords[i] != point.coords[i] )
      {
        state = false;
      }

    return state;
  }

  /**
   * Erstellt eine tiefe Kopie des HyperPoint.
   * 
   * @return Object geklonter HyperPoint
   */
  public Object clone()
  {
    double[] point = new double[coords.length];

    for( int i = 0; i < coords.length; i++ )
      point[i] = coords[i];

    return new HyperPoint( point );
  }
}