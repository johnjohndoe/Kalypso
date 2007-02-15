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
package org.kalypso.ogc.gml;

import org.eclipse.core.runtime.PlatformObject;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;

/**
 * <p>
 * Abstract implementation of IKalypsoTheme
 * </p>
 * <p>
 * Implements common features to all KalypsoTheme's
 * </p>
 * 
 * @author Belger
 */
public abstract class AbstractKalypsoTheme extends PlatformObject implements IKalypsoTheme
{
  private String m_name;

  private String m_type = "unknown type";

  private final ModellEventProvider m_eventProvider = new ModellEventProviderAdapter();

  private final KalypsoThemeEventProviderAdapter m_themeEventProvider = new KalypsoThemeEventProviderAdapter();

  private final IMapModell m_mapModel;

  public AbstractKalypsoTheme( final String name, final IMapModell mapModel )
  {
    m_name = name;
    m_mapModel = mapModel;
  }

  public AbstractKalypsoTheme( final String name, final String type, final IMapModell mapModel )
  {
    m_name = name;
    m_type = type;
    m_mapModel = mapModel;
  }

  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setName(java.lang.String)
   */
  public void setName( final String name )
  {
    m_name = name;
    fireModellEvent( null );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    fireModellEvent( modellEvent );
  }

  public void addModellListener( ModellEventListener listener )
  {
    m_eventProvider.addModellListener( listener );
  }

  public void fireModellEvent( ModellEvent event )
  {
    m_eventProvider.fireModellEvent( event );
  }

  public void removeModellListener( ModellEventListener listener )
  {
    m_eventProvider.removeModellListener( listener );
  }

  @Override
  public String toString( )
  {
    return m_name;
  }

  public String getType( )
  {
    return m_type;
  }

  public void setType( String type )
  {
    m_type = type;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getMapModell()
   */
  public IMapModell getMapModell( )
  {
    return m_mapModel;
  }

  /**
   * Returns the type of the theme by default. Override if needed.
   * 
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getContext()
   */
  public String getContext( )
  {
    return getType();
  }

  /**
   * @param listener
   * @see org.kalypso.ogc.gml.KalypsoThemeEventProviderAdapter#addKalypsoThemeListener(org.kalypso.ogc.gml.IKalypsoThemeListener)
   */
  public void addKalypsoThemeListener( IKalypsoThemeListener listener )
  {
    m_themeEventProvider.addKalypsoThemeListener( listener );
  }

  /**
   * @param listener
   * @see org.kalypso.ogc.gml.KalypsoThemeEventProviderAdapter#removeKalypsoThemeListener(org.kalypso.ogc.gml.IKalypsoThemeListener)
   */
  public void removeKalypsoThemeListener( IKalypsoThemeListener listener )
  {
    m_themeEventProvider.removeKalypsoThemeListener( listener );
  }

  /**
   * @param event
   * @see org.kalypso.ogc.gml.KalypsoThemeEventProviderAdapter#fireKalypsoThemeEvent(org.kalypso.ogc.gml.KalypsoThemeEvent)
   */
  public void fireKalypsoThemeEvent( KalypsoThemeEvent event )
  {
    m_themeEventProvider.fireKalypsoThemeEvent( event );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose( )
  {
    m_eventProvider.dispose();
    m_themeEventProvider.dispose();
  }
}
