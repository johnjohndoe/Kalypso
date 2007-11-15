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

import java.awt.Font;
import java.awt.Image;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.SafeRunnable;
import org.kalypso.core.KalypsoCoreExtensions;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * <p>
 * Abstract implementation of IKalypsoTheme
 * </p>
 * <p>
 * Implements common features to all KalypsoTheme's
 * </p>
 * 
 * @author Gernot Belger
 */
public abstract class AbstractKalypsoTheme extends PlatformObject implements IKalypsoTheme
{
  private static interface IListenerRunnable
  {
    public void visit( final IKalypsoThemeListener l );
  }

  protected static final Object[] EMPTY_CHILDREN = new Object[] {};

  private final Collection<IKalypsoThemeListener> m_listeners = new HashSet<IKalypsoThemeListener>();

  private final Map<String, String> m_properties = Collections.synchronizedMap( new HashMap<String, String>() );

  private final IMapModell m_mapModel;

  private String m_name;

  private String m_type;

  /**
   * The status of this theme. Should be set of implementing classes whenever something unexpected occurs (e.g. error
   * while loading the theme, ...).
   */
  private IStatus m_status = Status.OK_STATUS;

  private boolean m_isVisible = true;

  public AbstractKalypsoTheme( final String name, final String type, final IMapModell mapModel )
  {
    Assert.isNotNull( mapModel );

    m_name = name;
    m_type = type;
    m_mapModel = mapModel;

    /* Initialize properties */
    // deleteable defaults to 'true', because this was the old behaviour
    m_properties.put( IKalypsoTheme.PROPERTY_DELETEABLE, Boolean.toString( true ) );
  }

  /**
   * Runns the given runnable on every listener in a safe way.
   */
  private void acceptListenersRunnable( final IListenerRunnable r )
  {
    final IKalypsoThemeListener[] listeners = m_listeners.toArray( new IKalypsoThemeListener[m_listeners.size()] );
    for( final IKalypsoThemeListener l : listeners )
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

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#addKalypsoThemeListener(org.kalypso.ogc.gml.IKalypsoThemeListener)
   */
  public void addKalypsoThemeListener( final IKalypsoThemeListener listener )
  {
    m_listeners.add( listener );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose( )
  {
    m_listeners.clear();
  }

  /**
   * Fire the given event to my registered listeners.
   */
  protected void fireContextChanged( )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IKalypsoThemeListener l )
      {
        l.contextChanged( AbstractKalypsoTheme.this );
      }
    } );
  }

  /**
   * Fire the given event to my registered listeners.
   */
  protected void fireStatusChanged( )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IKalypsoThemeListener l )
      {
        l.statusChanged( AbstractKalypsoTheme.this );
      }
    } );
  }

  /**
   * Fire the given event to my registered listeners.
   */
  protected void fireVisibilityChanged( final boolean newVisibility )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IKalypsoThemeListener l )
      {
        l.visibilityChanged( AbstractKalypsoTheme.this, newVisibility );
      }
    } );
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object o )
  {
    return AbstractKalypsoTheme.EMPTY_CHILDREN;
  }

  /**
   * Returns the type of the theme by default. Override if needed.
   * 
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getContext()
   */
  public String getTypeContext( )
  {
    return getType();
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    final IStatus status = getStatus();
    if( !status.isOK() )
    {
      final ISharedImages sharedImages = PlatformUI.getWorkbench().getSharedImages();

      switch( status.getSeverity() )
      {
        case IStatus.ERROR:
          return sharedImages.getImageDescriptor( "IMG_OBJS_ERROR_PATH" );
        case IStatus.WARNING:
          return sharedImages.getImageDescriptor( "IMG_OBJS_WARNING_PATH" );
        case IStatus.INFO:
          return sharedImages.getImageDescriptor( "IMG_OBJS_INFO_PATH" );
      }
    }

    return null;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  public String getLabel( final Object o )
  {
    final StringBuffer sb = new StringBuffer();

    // REMARK: as the type is now clear from the properties view
    // This is not needed any more
// final String type = getType();
// if( type != null && type.length() > 0 )
// {
// sb.append( "[" );
// sb.append( type );
// sb.append( "] " );
// }

    final String themeName = getName();
    sb.append( themeName );

// if( !isLoaded() )
// sb.append( " (wird geladen...)" );

    final IStatus status = getStatus();
    if( !status.isOK() )
    {
      sb.append( " - " );
      sb.append( status.getMessage() );
    }

    return sb.toString();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getLegendGraphic(java.awt.Font, java.lang.String)
   */
  @SuppressWarnings("unused")
  public Image getLegendGraphic( final Font font, final String layerName ) throws CoreException
  {
    /* Nothing to do here, childs should implement. */
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getMapModell()
   */
  public IMapModell getMapModell( )
  {
    return m_mapModel;
  }

  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
   */
  public Object getParent( final Object o )
  {
    Assert.isTrue( o == this );

    return m_mapModel.getThemeParent( this );
  }

  /**
   * @return <code>defaultValue</code>, if the requested property is not set.
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getProperty(java.lang.String, java.lang.String)
   */
  public String getProperty( final String name, final String defaultValue )
  {
    if( !m_properties.containsKey( name ) )
      return defaultValue;

    return m_properties.get( name );
  }

  /**
   * Return the names of all known properties.
   */
  public String[] getPropertyNames( )
  {
    return m_properties.keySet().toArray( new String[m_properties.keySet().size()] );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getStatus()
   */
  public IStatus getStatus( )
  {
    return m_status;
  }

  public String getType( )
  {
    return m_type;
  }

  public void invalidate( final GM_Envelope bbox )
  {
    if( isVisible() )
      m_mapModel.invalidate( bbox );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#isLoaded()
   */
  public boolean isLoaded( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#isVisible()
   */
  public boolean isVisible( )
  {
    return m_isVisible;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#removeKalypsoThemeListener(org.kalypso.ogc.gml.IKalypsoThemeListener)
   */
  public void removeKalypsoThemeListener( final IKalypsoThemeListener listener )
  {
    m_listeners.remove( listener );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setExtent(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public void setExtent( final int width, final int height, final GM_Envelope extent )
  {
    /* Nothing to do here ... */
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setName(java.lang.String)
   */
  public void setName( final String name )
  {
    m_name = name;

    fireStatusChanged();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setProperty(java.lang.String, java.lang.String)
   */
  public void setProperty( final String name, final String value )
  {
    m_properties.put( name, value );

    // REMARK: we use status changed at the moment, maybe we should fire a special event for properties?
    fireStatusChanged();
  }

  public void setStatus( final IStatus status )
  {
    m_status = status;

    fireStatusChanged();
  }

  public void setType( final String type )
  {
    m_type = type;

    fireStatusChanged();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setVisible(boolean)
   */
  public void setVisible( final boolean visible )
  {
    if( visible != m_isVisible )
    {
      m_isVisible = visible;
      fireVisibilityChanged( visible );
    }
  }

  @Override
  public String toString( )
  {
    return m_name;
  }

  /**
   * @see org.eclipse.core.runtime.PlatformObject#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IKalypsoThemeInfo.class )
    {
      /* If an explizit info is configured for this map, use it */
      final String themeInfoId = getProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, null );
      if( themeInfoId != null )
        return KalypsoCoreExtensions.createThemeInfo( themeInfoId, this );
    }

    return super.getAdapter( adapter );
  }

}