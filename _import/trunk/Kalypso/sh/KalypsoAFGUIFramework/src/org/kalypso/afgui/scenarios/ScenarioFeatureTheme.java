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
package org.kalypso.afgui.scenarios;

import java.awt.Graphics;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.catalog.CatalogSLD;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.GisTemplateFeatureTheme;
import org.kalypso.ogc.gml.GisTemplateUserStyle;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoUserStyleListener;
import org.kalypso.ogc.gml.IPaintDelegate;
import org.kalypso.ogc.gml.KalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.types.LayerType;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.graphics.sld.DefaultStyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.StyleNotDefinedException;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;

/**
 * @author kurzbach
 * 
 */
public class ScenarioFeatureTheme extends AbstractKalypsoTheme implements IKalypsoFeatureTheme, IKalypsoUserStyleListener
{
  private final String m_featurePath;

  private KalypsoFeatureTheme m_theme = null;

  private final IFeatureSelectionManager m_selectionManager;

  private final List<GisTemplateUserStyle> m_gisTemplateUserStyles = new ArrayList<GisTemplateUserStyle>();

  private final JobExclusiveCommandTarget m_commandTarget;

  public ScenarioFeatureTheme( final I10nString layerName, final LayerType layerType, final URL context, final IFeatureSelectionManager selectionManager, final IMapModell mapModell, final String legendIcon, final boolean shouldShowChildren ) throws CoreException
  {
    super( layerName, layerType.getLinktype(), mapModell, legendIcon, context, shouldShowChildren );

    m_featurePath = layerType.getFeaturePath();
    m_selectionManager = selectionManager;
    if( layerType instanceof StyledLayerType )
    {
      final StyledLayerType mapLayerType = (StyledLayerType) layerType;
      setType( layerType.getLinktype().toUpperCase() );
      final List<Style> stylesList = mapLayerType.getStyle();

      for( final Style style : stylesList )
      {
        final PoolableObjectType sldPoolableObjectType = new PoolableObjectType( style.getLinktype(), style.getHref(), context );
        final GisTemplateUserStyle gisTemplateUserStyle = new GisTemplateUserStyle( sldPoolableObjectType, style.getStyle(), false );
        m_gisTemplateUserStyles.add( gisTemplateUserStyle );
      }

      GisTemplateFeatureTheme.configureProperties( this, mapLayerType );
    }
    setStatus( StatusUtilities.createInfoStatus( "lade Daten..." ) );

    final String classKey = layerType.getHref();
    final IModel model = getModel( classKey );

    // clear the theme
    if( m_theme != null )
    {
      m_theme.dispose();
      m_theme = null;
    }

    /* Get current property set */
    final String[] propertyNames = getPropertyNames();
    final Map<String, String> properties = new HashMap<String, String>();
    for( final String propName : propertyNames )
    {
      final String value = getProperty( propName, null );
      properties.put( propName, value );
    }

    final GMLWorkspace workspace = model.getFeature().getWorkspace();
    m_theme = new KalypsoFeatureTheme( new CommandableWorkspace( workspace ), m_featurePath, getName(), m_selectionManager, mapModell, legendIcon, context, shouldShowChildren );
    m_commandTarget = new JobExclusiveCommandTarget( m_theme.getWorkspace(), null );

    /* Put current property set into m_theme */
    for( final String propName : propertyNames )
    {
      m_theme.setProperty( propName, properties.get( propName ) );
    }

    for( final GisTemplateUserStyle style : m_gisTemplateUserStyles )
    {
      addStyle( style );
    }

    if( m_gisTemplateUserStyles.isEmpty() )
    {
      final IFeatureType featureType = getFeatureType();
      if( featureType != null )
      {
        final CatalogSLD styleCatalog = KalypsoCorePlugin.getDefault().getSLDCatalog();
        final IUrlResolver2 resolver = new IUrlResolver2()
        {
          public URL resolveURL( final String href ) throws MalformedURLException
          {
            return UrlResolverSingleton.resolveUrl( context, href );
          }
        };
        final FeatureTypeStyle fts = styleCatalog.getDefault( resolver, featureType );
        final UserStyle userStyle;
        try
        {
          if( fts == null )
          {
            System.out.println( "no default style found for " + featureType.getQName() ); //$NON-NLS-1$
            userStyle = DefaultStyleFactory.createUserStyle( featureType, " - default - " ); //$NON-NLS-1$
          }
          else
          {
            /*
             * Create a user style that wraps the catalog-based Feature Type Style. Inherit name, title and abstract.
             */
            final String name = fts.getName();
            final String title = fts.getTitle();
            // final String description = " Default Style for " + featureType.getQName().getLocalPart();
            final String description = fts.getAbstract();
            userStyle = (UserStyle_Impl) StyleFactory.createStyle( name, title, description, fts );
          }

          final GisTemplateUserStyle kus = new GisTemplateUserStyle( userStyle, userStyle.getTitle(), false );
          addStyle( kus );
        }
        catch( final StyleNotDefinedException e )
        {
          e.printStackTrace();
        }
      }
    }
  }

  private IModel getModel( final String classKey ) throws CoreException
  {
    final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
    final String dataSetScope = dataProvider.getDataSetScope();
    final Map<String, IScenarioDatum> scenarioDataMap = ScenarioDataExtension.getScenarioDataMap( dataSetScope );
    final IScenarioDatum scenarioDatum = scenarioDataMap.get( classKey );
    final Class< ? extends IModel> clazz = scenarioDatum.getModelClass();
    final IModel model = dataProvider.getModel( clazz );
    return model;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_commandTarget != null )
    {
      m_commandTarget.dispose();
    }
    if( m_theme != null )
    {
      m_theme.dispose();
      m_theme = null;
    }
    // remove styles
    final GisTemplateUserStyle[] templateStyles = m_gisTemplateUserStyles.toArray( new GisTemplateUserStyle[m_gisTemplateUserStyles.size()] );
    for( final GisTemplateUserStyle style : templateStyles )
    {
      removeStyle( style );
    }

    super.dispose();
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

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, org.kalypsodeegree.model.geometry.GM_Envelope,
   *      double, java.lang.Boolean, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void paint( final Graphics g, final GeoTransform p, final GM_Envelope bbox, final double scale, final Boolean selected, final IProgressMonitor monitor ) throws CoreException
  {
    if( m_theme != null )
      m_theme.paint( g, p, bbox, scale, selected, monitor );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#paint(double, org.kalypsodeegree.model.geometry.GM_Envelope,
   *      java.lang.Boolean, org.eclipse.core.runtime.IProgressMonitor, org.kalypso.ogc.gml.IPaintDelegate)
   */
  @Override
  public void paint( final double scale, final GM_Envelope bbox, final Boolean selected, final IProgressMonitor monitor, final IPaintDelegate delegate ) throws CoreException
  {
    if( m_theme != null )
      m_theme.paint( scale, bbox, selected, monitor, delegate );
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
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeaturePath()
   */
  public String getFeaturePath( )
  {
    return m_featurePath;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#addStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void addStyle( final KalypsoUserStyle style )
  {
    style.addStyleListener( this );

    if( m_theme != null )
    {
      m_theme.addStyle( style );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#removeStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void removeStyle( final KalypsoUserStyle style )
  {
    if( m_theme != null )
    {
      m_theme.removeStyle( style );
    }
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
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getSelectionManager()
   */
  public IFeatureSelectionManager getSelectionManager( )
  {
    return m_selectionManager;
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
  public I10nString getName( )
  {
    if( m_theme != null )
      return m_theme.getName();

    return super.getName();
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#setName(org.kalypso.contribs.java.lang.I10nString)
   */
  @Override
  public void setName( final I10nString name )
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
    {
      m_theme.setType( type );
    }

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
    {
      ((AbstractKalypsoTheme) m_theme).setStatus( status );
    }

    super.setStatus( status );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoUserStyleListener#styleChanged(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void styleChanged( final KalypsoUserStyle source )
  {
    fireStatusChanged();
  }
}
