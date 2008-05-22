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
package org.kalypsodeegree_impl.model.feature.validation.rules;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

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
  public IStatus isValid( Object object )
  {
    Status status = new Status( Status.CANCEL, Platform.PI_RUNTIME, Status.CANCEL, "Der Ausdruck entspricht nicht einem Wert aus der Enumeration.", null );

    /* If the object does not exist, return true. */
    if( object == null )
      return new Status( Status.OK, Platform.PI_RUNTIME, Status.OK, "EnumerationRule: Validation OK (null).", null );

    for( int i = 0; i < m_enums.length; i++ )
    {
      if( object.equals( m_enums[i] ) )
      {
        status = new Status( Status.OK, Platform.PI_RUNTIME, Status.OK, "EnumerationRule: Validation OK.", null );
      }
    }

    return status;
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
