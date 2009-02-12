/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.contribs.java.lang;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * Convenient class which provides a simple facility to call the dispose() method on a list of objects.
 * <p>
 * You will typically use it the following way:
 * <p>
 * <code><pre>
 * public void dispose()
 * {
 * 	 new DisposeHelper( m_someObjectArray ).dispose();
 * }
 * </pre></code>
 * 
 * @author schlienger
 */
public final class DisposeHelper
{
  private final List m_candidates = new ArrayList( 20 );

  public DisposeHelper()
  {
  // default
  }

  public DisposeHelper( final Object[] disposeCandidates )
  {
    m_candidates.addAll( Arrays.asList( disposeCandidates ) );
  }

  public DisposeHelper( final Collection collection )
  {
    m_candidates.addAll( collection );
  }

  public void addDisposeCandidates( final Object[] disposeCandidates )
  {
    m_candidates.addAll( Arrays.asList( disposeCandidates ) );
  }

  /**
   * Adds a dispose candidate and returns this DisposeHelper to allow chaining like
   * <code>disposeHelper.addDisposeCandidate( foo ).addDisposeCandidate( bar );</code>
   */
  public DisposeHelper addDisposeCandidate( final Object disposeCandidate )
  {
    m_candidates.add( disposeCandidate );
    return this;
  }

  /**
   * Disposes all candidates by calling their dispose method (if they have one) and clears the candidates list
   */
  public void dispose()
  {
    final Object[] objects = m_candidates.toArray();
    for( int i = 0; i < objects.length; i++ )
    {
      try
      {
        if( objects[i] != null ) // ignore the null objects
        {
          final Method disposeMethod = objects[i].getClass().getMethod( "dispose", null );
          disposeMethod.invoke( objects[i], null );
        }
      }
      catch( final NoSuchMethodException ignored )
      {
        // empty
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

    m_candidates.clear();
  }
}
