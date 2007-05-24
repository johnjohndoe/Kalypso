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
import java.util.Collection;
import java.util.HashSet;
import java.util.Vector;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.contribs.eclipse.core.runtime.SafeRunnable;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeListener;
import org.kalypso.ogc.gml.KalypsoThemeAdapter;
import org.kalypso.ogc.gml.ScrabLayerFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.visitor.ThemeVisiblePredicate;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Andreas von D�mming
 */
public class MapModell implements IMapModell
{
  private final Vector<IKalypsoTheme> m_themes = new Vector<IKalypsoTheme>();

  private final Collection<IMapModellListener> m_listeners = new HashSet<IMapModellListener>();

  private final CS_CoordinateSystem m_coordinatesSystem;

  private IKalypsoTheme m_activeTheme = null;

  private IProject m_project;

  private String m_name;

  private final IKalypsoThemeListener m_themeListener = new KalypsoThemeAdapter()
  {
    @Override
    public void contextChanged( final IKalypsoTheme source )
    {
      fireContextChanged( source );
    }

    /**
     * @see org.kalypso.ogc.gml.KalypsoThemeAdapter#visibilityChanged(org.kalypso.ogc.gml.IKalypsoTheme, boolean)
     */
    @Override
    public void visibilityChanged( final IKalypsoTheme source, final boolean newVisibility )
    {
      fireThemeVisibilityChanged( source, newVisibility );
    }

    /**
     * @see org.kalypso.ogc.gml.KalypsoThemeAdapter#statusChanged(org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void statusChanged( final IKalypsoTheme source )
    {
      fireThemeStatusChanged( source );
    }
  };

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
    m_project = null;
  }

  public void activateTheme( final IKalypsoTheme theme )
  {
    final IKalypsoTheme oldTheme = m_activeTheme;

    m_activeTheme = theme;

    fireThemeActivated( oldTheme, theme );
  }

  public IKalypsoTheme getActiveTheme( )
  {
    return m_activeTheme;
  }

  public void addTheme( final IKalypsoTheme theme )
  {
    m_themes.add( theme );

    theme.addKalypsoThemeListener( m_themeListener );

    fireThemeAdded( theme );

    if( m_activeTheme == null )
      activateTheme( theme );
  }

  public void insertTheme( final IKalypsoTheme theme, final int position )
  {
    m_themes.insertElementAt( theme, position );

    theme.addKalypsoThemeListener( m_themeListener );

    fireThemeAdded( theme );

    if( m_activeTheme == null )
      activateTheme( theme );
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
      if( theme.isVisible() )
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

  public boolean isThemeActivated( final IKalypsoTheme theme )
  {
    return m_activeTheme == theme;
  }

  public void moveDown( final IKalypsoTheme theme )
  {
    final int pos = m_themes.indexOf( theme );
    if( pos > 0 )
      swapThemes( theme, getTheme( pos - 1 ) );
  }

  public void moveUp( final IKalypsoTheme theme )
  {
    final int pos = m_themes.indexOf( theme );
    if( pos + 1 < m_themes.size() )
      swapThemes( theme, getTheme( pos + 1 ) );
  }

  public void removeTheme( final int pos )
  {
    removeTheme( m_themes.elementAt( pos ) );
  }

  public void removeTheme( final IKalypsoTheme theme )
  {
    theme.removeKalypsoThemeListener( m_themeListener );
    m_themes.remove( theme );

    fireThemeRemoved( theme, theme.isVisible() );

    if( m_activeTheme == theme )
      activateTheme( theme );
  }

  public void setCoordinateSystem( final CS_CoordinateSystem crs ) throws Exception
  {
    if( crs.equals( m_coordinatesSystem ) )
      throw new UnsupportedOperationException();
  }

  public void swapThemes( final IKalypsoTheme theme1, final IKalypsoTheme theme2 )
  {
    final int pos1 = m_themes.indexOf( theme1 );
    final int pos2 = m_themes.indexOf( theme2 );
    m_themes.set( pos1, theme2 );
    m_themes.set( pos2, theme1 );

    fireThemeOrderChanged();
  }

  public GM_Envelope getFullExtentBoundingBox( )
  {
    final IKalypsoTheme[] themes = getAllThemes();
    return MapModellHelper.calculateExtent( themes, new ThemeVisiblePredicate() );
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
    final IKalypsoTheme[] allThemes = getAllThemes();
    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof ScrabLayerFeatureTheme )
        return (IKalypsoFeatureTheme) theme;
    }
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#accept(org.kalypso.kalypsomodel1d2d.ui.map.channeledit.KalypsoThemeVisitor,
   *      int)
   */
  public void accept( final KalypsoThemeVisitor ktv, final int depth )
  {
    for( int j = 0; j < getAllThemes().length; j++ )
      accept( ktv, depth, getTheme( j ) );
  }

  public void accept( final KalypsoThemeVisitor ktv, final int depth, final IKalypsoTheme theme )
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
  public void setName( final String name )
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

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#addMapModelListener(org.kalypso.ogc.gml.mapmodel.IMapModellListener)
   */
  public void addMapModelListener( final IMapModellListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#removeMapModelListener(org.kalypso.ogc.gml.mapmodel.IMapModellListener)
   */
  public void removeMapModelListener( final IMapModellListener l )
  {
    m_listeners.remove( l );
  }

  private static interface IListenerRunnable
  {
    public void visit( final IMapModellListener l );
  }

  /**
   * Runns the given runnable on every listener in a safe way.
   */
  private void acceptListenersRunnable( final IListenerRunnable r )
  {
    final IMapModellListener[] listeners = m_listeners.toArray( new IMapModellListener[m_listeners.size()] );
    for( final IMapModellListener l : listeners )
    {
      final ISafeRunnable code = new SafeRunnable()
      {
        public void run( ) throws Exception
        {
          r.visit( l );
        }
      };

      SafeRunner.run( code );
    }
  }

  protected void fireThemeAdded( final IKalypsoTheme theme )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapModellListener l )
      {
        l.themeAdded( MapModell.this, theme );
      }
    } );
  }

  protected void fireThemeRemoved( final IKalypsoTheme theme, final boolean lastVisibility )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapModellListener l )
      {
        l.themeRemoved( MapModell.this, theme, lastVisibility );
      }
    } );
  }

  protected void fireThemeActivated( final IKalypsoTheme previouslyActive, final IKalypsoTheme activeTheme )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapModellListener l )
      {
        l.themeActivated( MapModell.this, previouslyActive, activeTheme );
      }
    } );
  }

  protected void fireThemeVisibilityChanged( final IKalypsoTheme theme, final boolean visibility )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapModellListener l )
      {
        l.themeVisibilityChanged( MapModell.this, theme, visibility );
      }
    } );
  }

  protected void fireThemeStatusChanged( final IKalypsoTheme theme )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapModellListener l )
      {
        l.themeStatusChanged( MapModell.this, theme );
      }
    } );
  }

  protected void fireThemeOrderChanged( )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapModellListener l )
      {
        l.themeOrderChanged( MapModell.this );
      }
    } );
  }

  protected void fireRepaintRequested( final GM_Envelope bbox )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapModellListener l )
      {
        l.repaintRequested( MapModell.this, bbox );
      }
    } );
  }

  protected void fireContextChanged( final IKalypsoTheme theme )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapModellListener l )
      {
        l.themeContextChanged( MapModell.this, theme );
      }
    } );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#invalidate(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public void invalidate( final GM_Envelope bbox )
  {
    fireRepaintRequested( bbox );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModell#getThemeParent(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public Object getThemeParent( final IKalypsoTheme abstractKalypsoTheme )
  {
    // normally, its just me
    return this;
  }
}