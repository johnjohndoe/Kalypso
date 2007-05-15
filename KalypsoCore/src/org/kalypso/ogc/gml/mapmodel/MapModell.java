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
package org.kalypso.ogc.gml.mapmodel;

import java.awt.Graphics;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.ScrabLayerFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author doemming
 */
public class MapModell implements IMapModell
{
  private final ModellEventProviderAdapter m_eventProvider = new ModellEventProviderAdapter();

  private final static Boolean THEME_ENABLED = Boolean.valueOf( true );

  private final static Boolean THEME_DISABLED = Boolean.valueOf( false );

  private final Vector<IKalypsoTheme> m_themes = new Vector<IKalypsoTheme>();

  private final Map<IKalypsoTheme, Boolean> m_enabledThemeStatus = new HashMap<IKalypsoTheme, Boolean>();

  private final CS_CoordinateSystem m_coordinatesSystem;

  private IKalypsoTheme m_activeTheme = null;

  private IProject m_project;

  private String m_name;

  public MapModell( final CS_CoordinateSystem crs, final IProject project )
  {
    m_coordinatesSystem = crs;
    m_project = project;
  }

  public void dispose( )
  {
    activateTheme( null );
    for( final IKalypsoTheme theme : m_themes )
      theme.dispose();
    m_themes.clear();
    m_enabledThemeStatus.clear();
    m_project = null;
  }

  public void activateTheme( final IKalypsoTheme theme )
  {
    m_activeTheme = theme;
    fireModellEvent( new ModellEvent( this, ModellEvent.THEME_ACTIVATED ) );
  }

  public IKalypsoTheme getActiveTheme( )
  {
    return m_activeTheme;
  }

  public void addTheme( final IKalypsoTheme theme )
  {
    m_themes.add( theme );
    m_enabledThemeStatus.put( theme, THEME_ENABLED );

    theme.addModellListener( this );

    fireModellEvent( new ModellEvent( this, ModellEvent.THEME_ADDED ) );

    if( m_activeTheme == null )
      activateTheme( theme );
  }

  public void insertTheme( final IKalypsoTheme theme, final int position )
  {
    m_themes.insertElementAt( theme, position );
    m_enabledThemeStatus.put( theme, THEME_ENABLED );

    theme.addModellListener( this );

    fireModellEvent( new ModellEvent( this, ModellEvent.THEME_ADDED ) );

    if( m_activeTheme == null )
      activateTheme( theme );
  }

  public void enableTheme( final IKalypsoTheme theme, final boolean status )
  {
    // TODO: check if theme is in this model?
    if( status )
      m_enabledThemeStatus.put( theme, THEME_ENABLED );
    else
      m_enabledThemeStatus.put( theme, THEME_DISABLED );
    fireModellEvent( null );
  }

  public IKalypsoTheme[] getAllThemes( )
  {
    return m_themes.toArray( new IKalypsoTheme[m_themes.size()] );
  }

  public CS_CoordinateSystem getCoordinatesSystem( )
  {
    return m_coordinatesSystem;
  }

  public void paint( final Graphics g, final GeoTransform p, final GM_Envelope bbox, final double scale, final boolean selected )
  {
    // directly access themes in order to avoid synchronization problems
    final IKalypsoTheme[] themes = m_themes.toArray( new IKalypsoTheme[m_themes.size()] );
    // paint themes in reverse order
    for( int i = themes.length; i > 0; i-- )
    {
      final IKalypsoTheme theme = themes[i - 1];
      if( isThemeEnabled( theme ) )
        theme.paint( g, p, scale, bbox, selected );
    }
  }

  public IKalypsoTheme getTheme( final int pos )
  {
    return m_themes.elementAt( pos );
  }

  public int getThemeSize( )
  {
    return m_themes.size();
  }

  public boolean isThemeActivated( IKalypsoTheme theme )
  {
    return m_activeTheme == theme;
  }

  public boolean isThemeEnabled( IKalypsoTheme theme )
  {
    return m_enabledThemeStatus.get( theme ) == THEME_ENABLED;
  }

  public void moveDown( IKalypsoTheme theme )
  {
    int pos = m_themes.indexOf( theme );
    if( pos > 0 )
      swapThemes( theme, getTheme( pos - 1 ) );
  }

  public void moveUp( IKalypsoTheme theme )
  {
    int pos = m_themes.indexOf( theme );
    if( pos + 1 < m_themes.size() )
      swapThemes( theme, getTheme( pos + 1 ) );
  }

  public void removeTheme( int pos )
  {
    removeTheme( m_themes.elementAt( pos ) );
  }

  public void removeTheme( final IKalypsoTheme theme )
  {
    theme.removeModellListener( this );
    m_themes.remove( theme );
    m_enabledThemeStatus.remove( theme );

    fireModellEvent( null );

    if( m_activeTheme == theme )
      activateTheme( theme );
  }

  public void setCoordinateSystem( CS_CoordinateSystem crs ) throws Exception
  {
    if( crs.equals( m_coordinatesSystem ) )
      throw new UnsupportedOperationException();
  }

  public void swapThemes( IKalypsoTheme theme1, IKalypsoTheme theme2 )
  {
    int pos1 = m_themes.indexOf( theme1 );
    int pos2 = m_themes.indexOf( theme2 );
    m_themes.set( pos1, theme2 );
    m_themes.set( pos2, theme1 );
    fireModellEvent( null );
  }

  public GM_Envelope getFullExtentBoundingBox( )
  {
    final IKalypsoTheme[] themes = getAllThemes();
    GM_Envelope result = null;
    for( int i = 0; i < themes.length; i++ )
    {
      final IKalypsoTheme kalypsoTheme = themes[i];

      if( isThemeEnabled( kalypsoTheme ) )
      {
        try
        {
          final GM_Envelope boundingBox = kalypsoTheme.getBoundingBox();

          if( result == null )
            result = boundingBox;
          else
            result = result.getMerged( boundingBox );
        }
        catch( final Exception e )
        {
          // TODO: das sollte nicht sein, exception einfach weiterwerfen
          e.printStackTrace();
        }
      }
    }

    return result;
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

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    fireModellEvent( modellEvent );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getProject()
   */
  public IProject getProject( )
  {
    return m_project;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getScrabLayer()
   */
  public IKalypsoFeatureTheme getScrabLayer( )
  {
    IKalypsoTheme[] allThemes = getAllThemes();
    for( int i = 0; i < allThemes.length; i++ )
    {
      IKalypsoTheme theme = allThemes[i];
      if( theme instanceof ScrabLayerFeatureTheme )
        return (IKalypsoFeatureTheme) theme;
    }
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#accept(org.kalypso.kalypsomodel1d2d.ui.map.channeledit.KalypsoThemeVisitor,
   *      int)
   */
  public void accept( KalypsoThemeVisitor ktv, int depth )
  {

    for( int j = 0; j < getAllThemes().length; j++ )
    {
      accept( ktv, depth, getTheme( j ) );
    }

  }

  public void accept( KalypsoThemeVisitor ktv, int depth, IKalypsoTheme theme )
  {
    final boolean recurse = ktv.visit( theme );

    if( recurse && depth != FeatureVisitor.DEPTH_ZERO )
    {
      if( theme instanceof IMapModell && depth == KalypsoThemeVisitor.DEPTH_INFINITE )
      {
        final IMapModell innerModel = (IMapModell) theme;
        innerModel.accept( ktv, depth );
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#setName(java.lang.String)
   */
  public void setName( String name )
  {
    m_name = name;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object o )
  {
    return getAllThemes();
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    return null;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  public String getLabel( final Object o )
  {
    return getName();
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
   */
  public Object getParent( final Object o )
  {
    return null;
  }
}