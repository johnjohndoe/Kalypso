/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.diagview;

import org.kalypso.template.obsdiagview.TypeAxis;

/**
 * This class overrides equals and hashcode. Two instance of DiagramAxis are
 * considered to be equal if they have the same id.
 * 
 * @author schlienger
 */
public class DiagramAxis
{
  public final static String DIRECTION_HORIZONTAL = "horizontal";
  public final static String DIRECTION_VERTICAL = "vertical";
  
  public final static String POSITION_LEFT = "left";
  public final static String POSITION_RIGHT = "right";
  public final static String POSITION_BOTTOM = "bottom";
  public final static String POSITION_TOP = "top";
  
  private final String m_id;

  private final String m_label;

  private final String m_unit;

  private final String m_direction;

  private final String m_position;

  private final boolean m_isInverted;

  private final String m_dataType;

  private final Double m_lowerMargin;

  private final Double m_upperMargin;

  public DiagramAxis( final TypeAxis axis )
  {
    // TODO currently the binding says that lower and upper margins
    // are optional, but we could not find out any solution
    // to know if the values were specified or not (primivite types,
    // for Object types it's not a problem since value is null)
    // So as soon as solution is found, maybe replace the code
    // here that currently simply checks for == 0
    this(
        axis.getId(),
        axis.getDatatype(),
        axis.getLabel(),
        axis.getUnit(),
        axis.getDirection(),
        axis.getPosition(),
        axis.isInverted(),
        axis.getLowerMargin() == 0 ? null : new Double( axis.getLowerMargin() ),
        axis.getUpperMargin() == 0 ? null : new Double( axis.getUpperMargin() ) );
  }

  public DiagramAxis( final String id, final String dataType,
      final String label, final String unit, final String direction,
      final String position, final boolean isInverted )
  {
    this( id, dataType, label, unit, direction, position, isInverted, null,
        null );
  }

  public DiagramAxis( final String id, final String dataType,
      final String label, final String unit, final String direction,
      final String position, final boolean isInverted,
      final Double lowerMargin, final Double upperMargin )
  {
    m_id = id;
    m_dataType = dataType;
    m_label = label;
    m_unit = unit;
    m_direction = direction;
    m_position = position;
    m_isInverted = isInverted;
    m_lowerMargin = lowerMargin;
    m_upperMargin = upperMargin;
  }

  public String getLabel( )
  {
    return m_label;
  }

  public String getUnit( )
  {
    return m_unit;
  }

  public String getDirection( )
  {
    return m_direction;
  }

  public String getPosition( )
  {
    return m_position;
  }

  public boolean isInverted( )
  {
    return m_isInverted;
  }

  public String getDataType( )
  {
    return m_dataType;
  }

  public String getIdentifier( )
  {
    return m_id;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return toFullString();
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( final Object obj )
  {
    if( !(obj instanceof DiagramAxis) )
      return false;

    final DiagramAxis other = (DiagramAxis) obj;

    return m_id.equals( other.getIdentifier() );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode( )
  {
    return m_id.hashCode();
  }

  /**
   * @return complete Label of this axis (concatenates the label and the unit)
   */
  public String toFullString( )
  {
    if( m_unit.length() > 0 )
      return m_label + " [" + m_unit + "]";

    return m_label;
  }

  /**
   * The lower margin is expressed in percent of the whole axis range.
   * 
   * @return the lower margin in percent (for instance 0.07 for 7%) or null if not set
   */
  public Double getLowerMargin( )
  {
    return m_lowerMargin;
  }

  /**
   * The upper margin is expressed in percent of the whole axis range.
   * 
   * @return the upper margin in percent (for instance 0.07 for 7%) or null if not set
   */
  public Double getUpperMaring( )
  {
    return m_upperMargin;
  }
}