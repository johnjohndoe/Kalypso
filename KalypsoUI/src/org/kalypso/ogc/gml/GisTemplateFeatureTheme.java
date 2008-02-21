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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.catalog.CatalogSLD;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.wms.provider.legends.feature.FeatureThemeLegendProvider;
import org.kalypso.template.types.LayerType;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyComparator;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.graphics.sld.DefaultStyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;

/**
 * <p>
 * Ein Decorator für ein {@link org.kalypso.ogc.gml.KalypsoFeatureTheme}, welches dieses (asynchron) über den Pool aus
 * einer Source lädt.
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
public class GisTemplateFeatureTheme extends AbstractKalypsoTheme implements IPoolListener, ICommandTarget, IKalypsoFeatureTheme, IKalypsoSaveableTheme, IKalypsoUserStyleListener
{
  protected static final Logger LOGGER = Logger.getLogger( GisTemplateFeatureTheme.class.getName() );

  private JobExclusiveCommandTarget m_commandTarget;

  private boolean m_loaded = false;

  private final PoolableObjectType m_layerKey;

  private final String m_featurePath;

  private KalypsoFeatureTheme m_theme = null;

  private boolean m_disposed = false;

  private final IFeatureSelectionManager m_selectionManager;

  private final List<GisTemplateUserStyle> m_gisTemplateUserStyles = new ArrayList<GisTemplateUserStyle>();

  public GisTemplateFeatureTheme( final LayerType layerType, final URL context, final IFeatureSelectionManager selectionManager, final IMapModell mapModel, final String legendIcon, final boolean shouldShowChildren )
  {
    super( Messages.getString( "org.kalypso.ogc.gml.GisTemplateFeatureTheme.noname" ), layerType.getLinktype(), mapModel, legendIcon, context, shouldShowChildren ); //$NON-NLS-1$

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

      for( final Style style : stylesList )
      {
        final PoolableObjectType sldPoolableObjectType = new PoolableObjectType( style.getLinktype(), style.getHref(), context );
        final GisTemplateUserStyle gisTemplateUserStyle = new GisTemplateUserStyle( sldPoolableObjectType, style.getStyle() );
        m_gisTemplateUserStyles.add( gisTemplateUserStyle );
      }

      GisTemplateFeatureTheme.configureProperties( this, mapLayerType );
    }
    try
    {
      setStatus( StatusUtilities.createInfoStatus( "lade Daten..." ) );
      pool.addPoolListener( this, m_layerKey );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      // catch any exception
    }
  }

  public static void configureProperties( final IKalypsoTheme theme, final StyledLayerType mapLayerType )
  {
    final List<Property> propertyList = mapLayerType.getProperty();
    for( final Property property : propertyList )
      theme.setProperty( property.getName(), property.getValue() );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  @Override
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
      m_theme.dispose();
      m_theme = null;
    }
    // remove styles
    final GisTemplateUserStyle[] templateStyles = m_gisTemplateUserStyles.toArray( new GisTemplateUserStyle[m_gisTemplateUserStyles.size()] );
    for( final GisTemplateUserStyle style : templateStyles )
      removeStyle( style );

    super.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean)
   */
  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final boolean selected, final IProgressMonitor monitor ) throws CoreException
  {
    if( m_theme != null )
      m_theme.paint( g, p, scale, bbox, selected, monitor );
  }

  /**
   * @see org.kalypso.ogc.gml.ITemplateTheme#saveFeatures(org.eclipse.core.runtime.IProgressMonitor)
   */
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
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Speichern" ) ); //$NON-NLS-1$
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
  public GM_Envelope getFullExtent( )
  {
    if( m_theme != null )
      return m_theme.getFullExtent();
    return null;
  }

  public void fillLayerType( final LayerType layer, final String id, final boolean isVisible )
  {
    final ObjectFactory extentFac = new ObjectFactory();
    final PoolableObjectType key = m_layerKey;
    layer.setId( id );
    layer.setHref( key.getLocation() );
    layer.setLinktype( key.getType() );
    layer.setActuate( "onRequest" ); //$NON-NLS-1$
    layer.setType( "simple" ); //$NON-NLS-1$
    layer.setFeaturePath( m_featurePath );
    if( layer instanceof StyledLayerType )
    {
      final StyledLayerType styledLayerType = (StyledLayerType) layer;
      styledLayerType.setName( getName() );
      styledLayerType.setVisible( isVisible );
      styledLayerType.getDepends();
      final List<Style> stylesList = styledLayerType.getStyle();
      for( final GisTemplateUserStyle style : m_gisTemplateUserStyles )
      {
        final Style styleType = extentFac.createStyledLayerTypeStyle();
        style.fillStyleType( stylesList, styleType );
      }

      GisTemplateFeatureTheme.fillProperties( this, extentFac, styledLayerType );
    }
  }

  public static void fillProperties( final AbstractKalypsoTheme theme, final ObjectFactory extentFac, final StyledLayerType styledLayerType )
  {
    final List<Property> propertyList = styledLayerType.getProperty();
    final String[] propertyNames = theme.getPropertyNames();
    for( final String name : propertyNames )
    {
      final Property property = extentFac.createStyledLayerTypeProperty();
      property.setName( name );
      final String value = theme.getProperty( name, null );
      if( value != null )
      {
        property.setValue( value );
        propertyList.add( property );
      }
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    m_loaded = true;

    try
    {
      if( KeyComparator.getInstance().compare( key, m_layerKey ) == 0 )
      {
        // clear the theme
        if( m_theme != null )
        {
          m_theme.dispose();
          m_theme = null;
        }
        if( newValue != null )
        {
          final CommandableWorkspace commandableWorkspace = (CommandableWorkspace) newValue;

          /* Get current property set */
          final String[] propertyNames = getPropertyNames();
          final Map<String, String> properties = new HashMap<String, String>();
          for( final String propName : propertyNames )
          {
            final String value = getProperty( propName, null );
            properties.put( propName, value );
          }

          m_theme = new KalypsoFeatureTheme( commandableWorkspace, m_featurePath, getName(), m_selectionManager, getMapModell(), getLegendIcon(), getContext(), shouldShowChildren() );
          m_commandTarget = new JobExclusiveCommandTarget( m_theme.getWorkspace(), null );

          /* Put current property set into m_theme */
          for( final String propName : propertyNames )
            m_theme.setProperty( propName, properties.get( propName ) );

          for( final GisTemplateUserStyle style : m_gisTemplateUserStyles )
            addStyle( style );

          if( m_gisTemplateUserStyles.isEmpty() )
          {
            final DefaultStyleFactory defaultStyleFactory = KalypsoDeegreePlugin.getDefaultStyleFactory();
            final IFeatureType featureType = getFeatureType();
            if( featureType != null )
            {
              final CatalogSLD styleCatalog = KalypsoCorePlugin.getDefault().getSLDCatalog();
              final URL context = m_layerKey.getContext();
              final IUrlResolver2 resolver = new IUrlResolver2()
              {
                public URL resolveURL( final String href ) throws MalformedURLException
                {
                  return UrlResolverSingleton.resolveUrl( context, href );
                }
              };
              final FeatureTypeStyle fts = styleCatalog.getDefault( resolver, featureType );
              final UserStyle userStyle;
              if( fts == null )
              {
                System.out.println( "no default style found for " + featureType.getQName() ); //$NON-NLS-1$
                userStyle = defaultStyleFactory.createUserStyle( featureType, " - " //$NON-NLS-1$
                    + Messages.getString( "org.kalypso.ogc.gml.GisTemplateFeatureTheme.generatedstyle" ) //$NON-NLS-1$
                    + " -" ); //$NON-NLS-1$
              }
              else
              {
                /*
                 * Create a user style that wraps the catalog-based Feature Type Style. Inherit name, title and
                 * abstract.
                 */
                final String name = fts.getName();
                final String title = fts.getTitle();
// final String description = " Default Style for " + featureType.getQName().getLocalPart();
                final String description = fts.getAbstract();
                userStyle = (UserStyle_Impl) StyleFactory.createStyle( name, title, description, fts );
              }
              final GisTemplateUserStyle kus = new GisTemplateUserStyle( userStyle, userStyle.getTitle() );
              addStyle( kus );
            }
          }
        }
      }
    }
    catch( final Throwable e )
    {
      final IStatus errorStatus = StatusUtilities.createStatus( IStatus.ERROR, "Theme konnte nicht initialisiert werden: " + e.toString(), e );
      KalypsoGisPlugin.getDefault().getLog().log( errorStatus );
      setStatus( status );
      return;
    }

    // REMARK: accessing the full extent here may cause dead lock due to access to pool via x-linked features at this
    // point.
    // Also: Causes the map to hang during loading, so we don't do it, even if now the map always gets repaintet, even
    // if the theme extent does not cover the map. invalidate( getFullExtent() );
    invalidate( null );

    fireContextChanged();
    setStatus( status );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    if( KeyComparator.getInstance().compare( key, m_layerKey ) == 0 )
    {
      // clear the theme
      setStatus( StatusUtilities.createWarningStatus( "Daten wurden gelöscht" ) );
      m_theme.dispose();
      m_theme = null;
    }

    // schon mal mitteilen, dass sich das Thema geändert hat
    fireContextChanged();
    invalidate( getFullExtent() );
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
    style.addStyleListener( this );

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
  @Override
  public boolean isLoaded( )
  {
    for( final GisTemplateUserStyle style : m_gisTemplateUserStyles )
    {
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

  public IFeatureSelectionManager getSelectionManager( )
  {
    return m_selectionManager;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
   */
  public void dirtyChanged( final IPoolableObjectType key, final boolean isDirty )
  {
    // TODO Change label, showing if dirty or not
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getContext()
   */
  @Override
  public String getTypeContext( )
  {
    final IFeatureType featureType = getFeatureType();
    if( featureType != null )
      return featureType.getQName().toString();
    else
      return super.getTypeContext();
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getLabel(java.lang.Object)
   */
  @Override
  public String getLabel( final Object o )
  {
    if( m_theme != null )
      return m_theme.getLabel( m_theme );

    return super.getLabel( o );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getImageDescriptor(java.lang.Object)
   */
  @Override
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    if( m_theme != null )
      return m_theme.getImageDescriptor( object );

    return super.getImageDescriptor( object );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getDefaultIcon()
   */
  @Override
  protected ImageDescriptor getDefaultIcon( )
  {
    if( m_theme != null )
      return m_theme.getDefaultIcon();

    return KalypsoGisPlugin.getImageProvider().getImageDescriptor( ImageProvider.DESCRIPTORS.IMAGE_THEME_FEATURE );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object o )
  {
    if( m_theme == null )
      return super.getChildren( o );

    return m_theme.getChildren( m_theme );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getName()
   */
  @Override
  public String getName( )
  {
    if( m_theme != null )
      return m_theme.getName();

    return super.getName();
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#setName(java.lang.String)
   */
  @Override
  public void setName( final String name )
  {
    if( m_theme != null )
      m_theme.setName( name );

    super.setName( name );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getType()
   */
  @Override
  public String getType( )
  {
    if( m_theme != null )
      return super.getType();

    return super.getType();
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#setType(java.lang.String)
   */
  @Override
  public void setType( final String type )
  {
    if( m_theme != null )
      m_theme.setType( type );

    super.setType( type );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getStatus()
   */
  @Override
  public IStatus getStatus( )
  {
    if( m_theme != null )
      return m_theme.getStatus();

    return super.getStatus();
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#setStatus(org.eclipse.core.runtime.IStatus)
   */
  @Override
  public void setStatus( final IStatus status )
  {
    if( m_theme != null )
      ((AbstractKalypsoTheme) m_theme).setStatus( status );

    super.setStatus( status );
  }

  public void paintInternal( final IPaintInternalDelegate delegate ) throws CoreException
  {
    if( m_theme != null )
      m_theme.paintInternal( delegate );
  }

  /**
   * @see org.eclipse.core.runtime.PlatformObject#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( m_theme != null )
    {
      final Object result = m_theme.getAdapter( adapter );
      if( result != null )
        return result;
    }

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getProperty(java.lang.String, java.lang.String)
   */
  @Override
  public String getProperty( final String name, final String defaultValue )
  {
    if( m_theme == null )
      return super.getProperty( name, defaultValue );

    return m_theme.getProperty( name, defaultValue );
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#setProperty(java.lang.String, java.lang.String)
   */
  @Override
  public void setProperty( final String name, final String value )
  {
    super.setProperty( name, value );

    if( m_theme != null )
      m_theme.setProperty( name, value );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoUserStyleListener#styleChanged(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void styleChanged( final KalypsoUserStyle source )
  {
    fireStatusChanged();
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getLegendGraphic(org.eclipse.swt.graphics.Font)
   */
  @Override
  public Image getLegendGraphic( Font font ) throws CoreException
  {
    if( m_theme != null )
    {
      FeatureThemeLegendProvider provider = new FeatureThemeLegendProvider( m_theme );
      return provider.getLegendGraphic( font );
    }

    return super.getLegendGraphic( font );
  }
}