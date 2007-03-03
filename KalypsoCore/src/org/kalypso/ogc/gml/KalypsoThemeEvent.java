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
package org.kalypso.ogc.gml;

/**
 * This event encapsulates information about the kind of change that has occured in an IKalypsoTheme
 * 
 * @author Stefan Kurzbach
 */
public class KalypsoThemeEvent
{
  public static final int CONTEXT_CHANGED = 1 << 1;

  public static final int CONTENT_CHANGED = 1 << 2;

  private final IKalypsoTheme m_source;

  private final int m_type;

  /**
   * Create a new KalypsoThemeEvent
   * 
   * @param source
   *          the theme that has changed
   * @param type
   *          a bitmask constant denoting the kind of change
   */
  public KalypsoThemeEvent( final IKalypsoTheme source, final int type )
  {
    m_source = source;
    m_type = type;
  }

  /**
   * returns the theme that has changed
   */
  public IKalypsoTheme getSource( )
  {
    return m_source;
  }

  /**
   * true if the event is of the given type
   */
  public boolean isType( final int type )
  {
    return (m_type & type) == type;
  }

}
