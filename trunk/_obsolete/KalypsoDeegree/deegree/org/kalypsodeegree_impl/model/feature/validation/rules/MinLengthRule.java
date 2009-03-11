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
 * This class is a rule for the MinLengthRestriction.
 * 
 * @author albert
 */
public class MinLengthRule implements IRule
{
  /**
   * This variable stores the min-length value.
   */
  private int m_min;

  public MinLengthRule( int min )
  {
    super();
    m_min = min;
  }

  /**
   * RULE : MinLengthRestriction
   * 
   * @see org.kalypso.ogc.gml.util.IRule#isValid(java.lang.Object)
   */
  public IStatus isValid( Object object )
  {
    Status status = new Status( Status.CANCEL, Platform.PI_RUNTIME, Status.CANCEL, "Die Zeichenkette muß mindestens " + Integer.toString( m_min ) + " Zeichen lang sein.", null );

    /* If the object does not exist, return true. */
    if( object == null )
      return new Status( Status.OK, Platform.PI_RUNTIME, Status.OK, "MinLengthRule: Validation OK (null).", null );

    /* The object given should be a char[] or a String. */
    if( object instanceof char[] )
    {
      char[] text = (char[]) object;

      if( text.length >= m_min )
      {
        /* The text is greater or equal then the allowed minimum. Everything is ok. */
        status = new Status( Status.OK, Platform.PI_RUNTIME, Status.OK, "MinLengthRule: Validation OK.", null );
      }
    }
    else if( object instanceof String )
    {
      String text = (String) object;

      if( text.length() >= m_min )
      {
        /* The text is greater or equal then the allowed minimum. Everything is ok. */
        status = new Status( Status.OK, Platform.PI_RUNTIME, Status.OK, "MinLengthRule: Validation OK.", null );
      }
    }
    else
    {
      /* If it is no char[] or String, return true. */
      status = new Status( Status.OK, Platform.PI_RUNTIME, Status.OK, "MinLengthRule: Validation OK.", null );
    }

    return status;
  }

  /**
   * This function gets the minimum allowed length.
   * 
   * @return The minimum.
   */
  public int getMin( )
  {
    return m_min;
  }

  /**
   * This function sets the minimum allowed length.
   * 
   * @param min
   *          The minimum.
   */
  public void setMin( int min )
  {
    m_min = min;
  }
}
