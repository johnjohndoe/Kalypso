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

import java.awt.Graphics;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.loader.IPooledObject;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.types.LayerType;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyComparator;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.graphics.sld.DefaultStyleFactory;

/**
 * <p>
 * Ein Decorator f?r ein {@link org.kalypso.ogc.gml.KalypsoFeatureTheme}, welches dieses (asynchron) ?ber den Pool aus
 * einer Source l?dt.
 * </p>
 * <p>
 * Die ganze dynamic, also die Überwachung, ob sich das Pool-Objekt geändert hat etc. findet hier statt
 * </p>
 * <p>
 * Hier findet auch die Verwaltung statt, ob sich Daten des Themas geändert haben
 * </p>
 * <p>
 * Implementiert unter anderem {@link org.kalypso.commons.command.ICommandTarget}, da sich die Daten des unterliegenden
 * Themas ändern können
 * </p>
 * 
 * @author Belger
 */
public class GisTemplateFeatureTheme extends AbstractKalypsoTheme implements IPoolListener, ICommandTarget, IKalypsoFeatureTheme, IPooledObject
{
  protected static final Logger LOGGER = Logger.getLogger( GisTemplateFeatureTheme.class.getName() );

  private JobExclusiveCommandTarget m_commandTarget;

  private boolean m_loaded = false;

  private final PoolableObjectType m_layerKey;

  private final String m_featurePath;

  // private final PoolableObjectType[] m_styleKeys;
  // private final String[] m_styleNames;
  private KalypsoFeatureTheme m_theme = null;

  private boolean m_disposed = false;

  private final IFeatureSelectionManager m_selectionManager;

  private List<GisTemplateUserStyle> m_gisTemplateUserStyles = new ArrayList<GisTemplateUserStyle>();

  public GisTemplateFeatureTheme( final LayerType layerType, final URL context, final IFeatureSelectionManager selectionManager )
  {
    super( "<no name>" );
    m_selectionManager = selectionManager;
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    final String source = layerType.getHref();
    final String type = layerType.getLinktype();
    final String featurePath = layerType.getFeaturePath();
    m_layerKey = new PoolableObjectType( type, source, context );
    m_featurePath = featurePath;
    if( layerType instanceof StyledLayerType )
    {
      final StyledLayerType mapLayerType = (StyledLayerType) layerType;
      setType( type.toUpperCase() );
      setName( mapLayerType.getName() );
      final List<Style> stylesList = mapLayerType.getStyle();
      for( int i = 0; i < stylesList.size(); i++ )
      {
        final Style styleType = stylesList.get( i );
        final GisTemplateUserStyle gisTemplateUserStyle = new GisTemplateUserStyle( new PoolableObjectType( styleType.getLinktype(), styleType.getHref(), context ), styleType.getStyle() );
        m_gisTemplateUserStyles.add( gisTemplateUserStyle );
      }
    }
    try
    {
      pool.addPoolListener( this, m_layerKey );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      // catch any exception
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose( )
  {
    m_disposed = true;
    // remove from pool
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    pool.removePoolListener( this );
    if( m_commandTarget != null )
      m_commandTarget.dispose();
    if( m_theme != null )
    {
      m_theme.removeModellListener( this );
      m_theme.dispose();
      m_theme = null;
    }
    // remove styles
    final GisTemplateUserStyle[] templateStyles = m_gisTemplateUserStyles.toArray( new GisTemplateUserStyle[m_gisTemplateUserStyles.size()] );
    for( int i = 0; i < templateStyles.length; i++ )
    {
      GisTemplateUserStyle style = templateStyles[i];
      removeStyle( style );
    }
  }

  // private void removeTemplateUserStyles()
  // {
  // for( Iterator iter = m_gisTemplateUserStyles.iterator(); iter.hasNext(); )
  // {
  // final GisTemplateUserStyle style = (GisTemplateUserStyle)iter.next();
  // removeStyle( style );
  // }
  // }
  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean)
   */
  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final boolean selected )
  {
    if( m_theme != null )
      m_theme.paint( g, p, scale, bbox, selected );
  }

  public void saveFeatures( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      if( m_theme != null )
        KalypsoGisPlugin.getDefault().getPool().saveObject( m_theme.getWorkspace(), monitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Speichern" ) );
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    if( m_theme != null )
      return m_theme.getBoundingBox();
    return null;
  }

  public void fillLayerType( final LayerType layer, final String id, final boolean isVisible )
  {
    final ObjectFactory extentFac = new ObjectFactory();
    final PoolableObjectType key = m_layerKey;
    layer.setId( id );
    layer.setHref( key.getLocation() );
    layer.setLinktype( key.getType() );
    layer.setActuate( "onRequest" );
    layer.setType( "simple" );
    layer.setFeaturePath( m_featurePath );
    if( layer instanceof StyledLayerType )
    {
      final StyledLayerType styledLayerType = (StyledLayerType) layer;
      styledLayerType.setName( getName() );
      styledLayerType.setVisible( isVisible );
      styledLayerType.getDepends();
      final List<Style> stylesList = styledLayerType.getStyle();
      for( GisTemplateUserStyle style : m_gisTemplateUserStyles )
      {
        final Style styleType = extentFac.createStyledLayerTypeStyle();
        style.fillStyleType( stylesList, styleType );
      }
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    if( m_disposed ) // if disposed, do not register any more objects at the pool
      LOGGER.info( "Theme already disposed: " + this );
    LOGGER.info( "Object loaded: " + key + "   -  Object: " + newValue );
    try
    {
      // final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
      if( KeyComparator.getInstance().compare( key, m_layerKey ) == 0 )
      {
        // clear the theme
        if( m_theme != null )
        {
          m_theme.removeModellListener( this );
          m_theme.dispose();
          m_theme = null;
        }
        if( newValue != null )
        {
          final CommandableWorkspace commandableWorkspace = (CommandableWorkspace) newValue;
          m_theme = new KalypsoFeatureTheme( commandableWorkspace, m_featurePath, getName(), m_selectionManager );
          m_theme.addModellListener( this );
          m_commandTarget = new JobExclusiveCommandTarget( m_theme.getWorkspace(), null );
          fireModellEvent( new ModellEvent( this, ModellEvent.THEME_ADDED ) );
          for( Iterator iter = m_gisTemplateUserStyles.iterator(); iter.hasNext(); )
          {
            GisTemplateUserStyle style = (GisTemplateUserStyle) iter.next();
            addStyle( style );
          }
          if( m_gisTemplateUserStyles.isEmpty() )
          {
            final DefaultStyleFactory defaultStyleFactory = KalypsoGisPlugin.getDefaultStyleFactory();
            final UserStyle style = defaultStyleFactory.createUserStyle( getFeatureType(), " - generierter Standard-Stil -" );
            // DO not use xml-tags in this names please
            final GisTemplateUserStyle kus = new GisTemplateUserStyle( style, "default" );
            addStyle( kus );
          }
          m_loaded = true;
        }
      }
    }
    catch( final Throwable e )
    {
      // alles abfangen, damit der Pool trotzdem weitermacht
      e.printStackTrace();
    }
    // ModellEvent event = new ModellEvent( this,null ) ;
    // ModellEvent event = new ModellEvent( this, ModellEvent.FULL_CHANGE ) ;
    if( m_theme != null )
      m_theme.fireModellEvent( null );
    else
      fireModellEvent( null );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    m_loaded = false;
    if( KeyComparator.getInstance().compare( key, m_layerKey ) == 0 )
    {
      // clear the theme
      m_theme.removeModellListener( this );
      m_theme.dispose();
      m_theme = null;
    }
    // schon mal mitteilen, dass sich das Thema geändert hat
    fireModellEvent( new ModellEvent( this, ModellEvent.FULL_CHANGE ) );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getWorkspace()
   */
  public CommandableWorkspace getWorkspace( )
  {
    if( m_theme != null )
      return m_theme.getWorkspace();
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    if( m_theme != null )
      return m_theme.getFeatureType();
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#addStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void addStyle( final KalypsoUserStyle style )
  {
    if( m_theme != null )
      m_theme.addStyle( style );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#removeStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void removeStyle( final KalypsoUserStyle style )
  {
    if( m_theme != null )
      m_theme.removeStyle( style );
    style.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getStyles()
   */
  public UserStyle[] getStyles( )
  {
    if( m_theme != null )
      return m_theme.getStyles();
    return new UserStyle[0];
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureList()
   */
  public FeatureList getFeatureList( )
  {
    if( m_theme != null )
      return m_theme.getFeatureList();
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getSchedulingRule()
   */
  public ISchedulingRule getSchedulingRule( )
  {
    return m_commandTarget.getSchedulingRule();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureListVisible(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public FeatureList getFeatureListVisible( final GM_Envelope env )
  {
    if( m_theme != null )
      return m_theme.getFeatureListVisible( env );
    return null;
  }

  /**
   * @see org.kalypso.loader.IPooledObject#isLoaded()
   */
  public boolean isLoaded( )
  {
    for( final Iterator iter = m_gisTemplateUserStyles.iterator(); iter.hasNext(); )
    {
      final GisTemplateUserStyle style = (GisTemplateUserStyle) iter.next();
      if( !style.isLoaded() )
        return false;
    }
    return m_loaded;
  }

  public PoolableObjectType getLayerKey( )
  {
    return m_layerKey;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#isDisposed()
   */
  public boolean isDisposed( )
  {
    return m_disposed;
  }
}