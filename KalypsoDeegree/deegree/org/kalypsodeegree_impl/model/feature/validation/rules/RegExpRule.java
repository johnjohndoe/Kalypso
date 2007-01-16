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
package org.kalypsodeegree_impl.model.feature.validation.rules;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

/**
 * This class is a rule for the RegExpRestriction.
 * 
 * @author albert
 */
public class RegExpRule implements IRule
{
  /**
   * This variable stores a value, that should be checked against. This could be a minimun-, maximum-value or a string
   * pattern.
   */
  public String[] m_patterns;

  public RegExpRule( String[] patterns )
  {
    super();
    m_patterns = patterns;
  }

  /**
   * RULE : RegExpRestriction
   * 
   * @see org.kalypso.ogc.gml.util.Rule#isValid(java.lang.Object)
   */
  public IStatus isValid( Object object )
  {
    Status status = new Status( Status.OK, Platform.PI_RUNTIME, Status.OK, "RegExpRule: Validation OK.", null );

    /* If the object does not exist, return true. */
    if( object == null )
      return new Status( Status.OK, Platform.PI_RUNTIME, Status.OK, "RegExpRule: Validation OK (null).", null );

    String txt = null;

    if( object instanceof String )
    {
      /* Cast in a string. */
      txt = (String) object;
    }
    else if( object instanceof char[] )
    {
      /* Cast in a char[]. */
      char[] text = (char[]) object;

      /* ToString. */
      txt = String.copyValueOf( text );
    }
    else
    {
      /* Not a allowed class-type, return true. */
      return status;
    }

    for( int i = 0; i < m_patterns.length; i++ )
    {
      Pattern p = Pattern.compile( m_patterns[i] );
      Matcher m = p.matcher( txt );

      boolean ret = m.matches();

      /* If one pattern fails, there is no need to check the other patterns. */
      if( ret == false )
      {
        status = new Status( Status.CANCEL, Platform.PI_RUNTIME, Status.CANCEL, "Ausdruck entspricht nicht den gegebenen Pattern.", null );
        break;
      }
    }

    return status;
  }

  /**
   * This function sets the parameter to check against.
   * 
   * @param patterns
   *          The RegExp pattern.
   */
  public void setPatterns( String[] patterns )
  {
    m_patterns = patterns;
  }

  /**
   * This function returns the parameters, against which is checked.
   * 
   * @return The RegExp pattern.
   */
  public String[] getPatterns( )
  {
    return m_patterns;
  }
}