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
package org.kalypso.ogc.gml.util;

import org.kalypso.loader.IPooledObject;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * <p>
 * Dieser Thread wartet solange, bis eine Karte vollständig geladen wurde.
 * </p>
 * <p>
 * Danach macht er etwas (d.h. führt ein übergebenen Runnable aus) und beendet sich.
 * </p>
 * 
 * @author belger
 */
public class GisTemplateLoadedThread extends Thread
{
  private final IMapModell m_modell;

  public GisTemplateLoadedThread( final IMapModell modell, final Runnable runnable )
  {
    super( runnable );

    m_modell = modell;
  }

  /**
   * @see java.lang.Thread#run()
   */
  public void run()
  {
    int maxWait = 10;
    while( true )
    {
      if( isLoaded() )
        break;
      try
      {
        sleep( 500 );
      }
      catch( final InterruptedException e )
      {
        e.printStackTrace();
      }
      if( maxWait-- < 0 ) // do not wait for ever
        break;
    }
    super.run();
  }

  private boolean isLoaded()
  {
    if( m_modell == null )
      return false;
    
    final IKalypsoTheme[] themes = m_modell.getAllThemes();
    for( int i = 0; i < themes.length; i++ )
    {
      final IKalypsoTheme theme = themes[i];
      if( theme instanceof IPooledObject )
      {
        if( !( (IPooledObject)theme ).isLoaded() )
          return false;
      }
    }
    return true;
  }

}