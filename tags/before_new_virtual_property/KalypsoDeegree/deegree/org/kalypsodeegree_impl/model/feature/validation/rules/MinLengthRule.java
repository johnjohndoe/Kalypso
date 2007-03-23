/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
