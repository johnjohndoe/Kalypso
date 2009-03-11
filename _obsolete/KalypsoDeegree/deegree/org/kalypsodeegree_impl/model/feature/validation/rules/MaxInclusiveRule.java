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
 * This class is a rule for the MaxInclusiveRestriction.
 * 
 * @author albert
 */
public class MaxInclusiveRule implements IRule
{
  /**
   * This variable stores a value, that should be checked against. This is a maximum inclusive value.
   */
  public Number m_checkagainst;

  public MaxInclusiveRule( Number checkagainst )
  {
    super();
    m_checkagainst = checkagainst;
  }

  /**
   * RULE : MaxInclusiveRestriction
   * 
   * @see org.kalypso.ogc.gml.util.Rule#isValid(java.lang.Object)
   */
  public IStatus isValid( Object object )
  {
    Status status = new Status( Status.CANCEL, Platform.PI_RUNTIME, Status.CANCEL, "Wert muss kleiner oder gleich " + m_checkagainst.toString() + " sein.", null );

    /* If the object does not exist or is no number, return true. */
    if( (object == null) || (!(object instanceof Number)) )
      return new Status( Status.OK, Platform.PI_RUNTIME, Status.OK, "MaxInclusiveRule: Validation OK (null).", null );

    /* Cast in a number. It must be one, because a restriction for a maximum could only work with numbers. */
    Number number = (Number) object;

    if( number.floatValue() <= m_checkagainst.floatValue() )
    {
      status = new Status( Status.OK, Platform.PI_RUNTIME, Status.OK, "MaxInclusiveRule: Validation OK.", null );
    }

    return status;
  }

  /**
   * This function sets the parameter to check against.
   * 
   * @param checkagainst
   *          The max value.
   */
  public void setCheckParameter( Number checkagainst )
  {
    m_checkagainst = checkagainst;
  }

  /**
   * This function returns the parameter, against which is checked.
   * 
   * @return The max value.
   */
  public Number getCheckParameter( )
  {
    return m_checkagainst;
  }
}