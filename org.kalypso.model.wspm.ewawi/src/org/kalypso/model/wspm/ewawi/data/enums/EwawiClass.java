/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.ewawi.data.enums;

/**
 * @author Holger Albert
 */
public enum EwawiClass
{
  _1( 1, "0 m - 5 m", "", "Breite < 5 m", 0.0, 5.0 ),
  _2( 2, "5 m - 10 m", "", "Breite 5-10 m", 5.0, 10.0 ),
  _3( 3, "10 m - 80 m", "", "Breite 10-80 m", 10.0, 80.0 ),
  _4( 4, "> 80 m", "", "Breite > 80 m", 80.0, Double.MAX_VALUE );

  private final int m_key;

  private final String m_label;

  private final String m_comment;

  private final String m_category;

  private final double m_min;

  private final double m_max;

  EwawiClass( final int key, final String label, final String comment, final String category, final double min, final double max )
  {
    m_key = key;
    m_label = label;
    m_comment = comment;
    m_category = category;
    m_min = min;
    m_max = max;
  }

  public static EwawiClass findEwawiClass( final String widthCategory )
  {
    final EwawiClass[] values = EwawiClass.values();
    for( final EwawiClass value : values )
    {
      final String category = value.getCategory();
      if( category.equals( widthCategory ) )
        return value;
    }

    return null;
  }

  public static EwawiClass findEwawiClass( final double width )
  {
    final EwawiClass[] values = EwawiClass.values();
    for( final EwawiClass value : values )
    {
      final double min = value.getMin();
      final double max = value.getMax();

      if( min < width && width <= max )
        return value;
    }

    return null;
  }

  public int getKey( )
  {
    return m_key;
  }

  public String getLabel( )
  {
    return m_label;
  }

  public String getComment( )
  {
    return m_comment;
  }

  public String getCategory( )
  {
    return m_category;
  }

  public double getMin( )
  {
    return m_min;
  }

  public double getMax( )
  {
    return m_max;
  }
}