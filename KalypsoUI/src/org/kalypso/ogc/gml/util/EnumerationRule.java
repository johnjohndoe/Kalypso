/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.util;

/**
 * This class is a rule for the EnumerationRestriction.
 * 
 * @author albert
 */
public class EnumerationRule implements IRule
{
  /**
   * This variable stores the list of allowed values in a enumeration.
   */
  private Object[] m_enums;

  /**
   * This variable stores the list of allowed values in a enumeration.
   */
  private Object[] m_labels;

  public EnumerationRule( Object[] enums, Object[] labels )
  {
    super();
    m_enums = enums;
    m_labels = labels;
  }

  /**
   * RULE : EnumerationRestriction
   * 
   * @see org.kalypso.ogc.gml.util.IRule#isValid(java.lang.Object)
   */
  public boolean isValid( Object object )
  {
    boolean ret = false;

    /* If the object does not exist, return true. */
    if( object == null )
      return true;

    for( int i = 0; i < m_enums.length; i++ )
    {
      if( object.equals( m_enums[i] ) )
      {
        ret = true;
      }
    }

    return ret;
  }

  /**
   * This function returns the list of allowed values for a enumeration.
   * 
   * @return Allowed values for a enumeration.
   */
  public Object[] getEnums( )
  {
    return m_enums;
  }

  /**
   * This function sets the list of allowed values for a enumeration.
   * 
   * @param enums
   *          Allowed values for a enumeration.
   */
  public void setEnums( String[] enums )
  {
    m_enums = enums;
  }

  public Object[] getLabels( )
  {
    return m_labels;
  }

  public void setLabels( Object[] labels )
  {
    m_labels = labels;
  }

}
