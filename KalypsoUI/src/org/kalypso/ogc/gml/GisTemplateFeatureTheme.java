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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.loader.IPooledObject;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypso.template.types.LayerType;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.StyleType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyComparator;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.visitors.QuerySelectionVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.SetSelectionVisitor;

/**
 * <p>
 * Ein Decorator f?r ein {@link org.kalypso.ogc.gml.KalypsoFeatureTheme},
 * welches dieses (asynchron) ?ber den Pool aus einer Source l?dt.
 * </p>
 * <p>
 * Die ganze dynamic, also die ?berwachung, ob sich das Pool-Objekt ge?ndert
 * etc. findet hier statt
 * </p>
 * 
 * <p>
 * Hier findet auch die Verwaltung statt, ob sich Daten des Themas ge?ndert
 * haben
 * </p>
 * <p>
 * Implementiert unter anderem {@link org.kalypso.util.command.ICommandTarget},
 * da sich die Daten des unterliegenden Themas ?ndern k?nnen
 * </p>
 * 
 * @author Belger
 */
public class GisTemplateFeatureTheme extends AbstractKalypsoTheme implements IPoolListener,
    ICommandTarget, IKalypsoFeatureTheme, IPooledObject
{
  protected static final Logger LOGGER = Logger.getLogger( GisTemplateFeatureTheme.class.getName() );
  
  private JobExclusiveCommandTarget m_commandTarget;

  private boolean m_loaded = false;

  private final PoolableObjectType m_layerKey;

  private final String m_featurePath;

  private final PoolableObjectType[] m_styleKeys;

  private final String[] m_styleNames;

  private KalypsoFeatureTheme m_theme = null;
  
  /** Um bei einem Neuladen der Daten die Selektion zu erhalten
   featureId -> selection */
  private final Map m_lastSelectedFeaturesMap = new HashMap();

  private boolean m_disposed = false;

  /**
   * @param layerType
   * @param context
   */
  public GisTemplateFeatureTheme( final LayerType layerType, final URL context )
  {
    super( "<no name>" );

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final String source = layerType.getHref();
    final String type = layerType.getLinktype();
    final String featurePath = layerType.getFeaturePath();

    m_layerKey = new PoolableObjectType( type, source, context );
    m_featurePath = featurePath;

    if( layerType instanceof Layer )
    {
      final Layer mapLayerType = (Layer)layerType;
      setType( type.toUpperCase() );
      setName( mapLayerType.getName() );

      final List stylesList = mapLayerType.getStyle();

      m_styleKeys = new PoolableObjectType[stylesList.size()];
      m_styleNames = new String[stylesList.size()];
      for( int i = 0; i < stylesList.size(); i++ )
      {
        final StyleType styleType = (StyleType)stylesList.get( i );

        m_styleKeys[i] = new PoolableObjectType( styleType.getLinktype(), styleType.getHref(),
            context );
        m_styleNames[i] = styleType.getStyle();
      }
    }
    else
    {
      m_styleKeys = new PoolableObjectType[] {};
      m_styleNames = new String[] {};
    }

    pool.addPoolListener( this, m_layerKey );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose()
  {
    m_disposed = true;
    
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
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paintSelected(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, int)
   */
  public void paintSelected( Graphics g, GeoTransform p, double scale, GM_Envelope bbox,
      int selectionId )
  {
    if( m_theme != null )
      m_theme.paintSelected( g, p, scale, bbox, selectionId );
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
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Speichern", e ) );
    }
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox()
  {
    if( m_theme != null )
      return m_theme.getBoundingBox();

    return null;
  }

  public void fillLayerType( final LayerType layer, final String id, final boolean isVisible )
      throws JAXBException
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
      final StyledLayerType styledLayerType = (StyledLayerType)layer;
      styledLayerType.setName( getName() );
      styledLayerType.setVisible( isVisible );
      styledLayerType.getDepends();

      final List stylesList = styledLayerType.getStyle();
      final IPoolableObjectType[] styleKeys = m_styleKeys;
      for( int j = 0; j < styleKeys.length; j++ )
      {
        StyleType styleType = extentFac.createStyledLayerTypeStyleType();
        IPoolableObjectType styleKey = styleKeys[j];
        styleType.setActuate( "onRequest" );
        styleType.setHref( styleKey.getLocation() );
        styleType.setLinktype( styleKey.getType() );
        styleType.setStyle( m_styleNames[j] );
        styleType.setType( "simple" );
        stylesList.add( styleType );
      }
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    if( m_disposed )
      LOGGER.info( "Theme already disposed: " + this );
    
    LOGGER.info( "Object loaded: " + key + "   -  Object: " + newValue );
    
    try
    {
      final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
      
      if( KeyComparator.getInstance().compare( key, m_layerKey ) == 0 )
      {
        if( m_theme != null )
        {
          // die alte selektion merken, falls das Thema gleich wieder geladen wird!
          final FeatureList featureList = m_theme.getFeatureList();
          m_lastSelectedFeaturesMap.clear();
          featureList.accept( new QuerySelectionVisitor( m_lastSelectedFeaturesMap ) );

          m_theme.removeModellListener( this );
          m_theme.dispose();
          m_theme = null;
        }
        
        if( newValue == null )
          return;
          
        m_theme = new KalypsoFeatureTheme( (CommandableWorkspace)newValue, m_featurePath, getName() );

        m_theme.addModellListener( this );

        m_commandTarget = new JobExclusiveCommandTarget( m_theme.getWorkspace(), null );

        fireModellEvent( new ModellEvent( this, ModellEvent.THEME_ADDED ) );

        final FeatureList featureList = m_theme.getFeatureList();
        featureList.accept( new SetSelectionVisitor( m_lastSelectedFeaturesMap ) );
        fireModellEvent( new ModellEvent( this, ModellEvent.SELECTION_CHANGED ) );

        // erst jetzt mit dem style laden anfangen!
        for( int i = 0; i < m_styleKeys.length; i++ )
          pool.addPoolListener( this, m_styleKeys[i] );
      }

      // styles
      for( int i = 0; i < m_styleKeys.length; i++ )
      {
        final IPoolableObjectType styleKey = m_styleKeys[i];
        if( KeyComparator.getInstance().compare( styleKey, key ) == 0 && newValue != null )
        {
          final StyledLayerDescriptor sld = (StyledLayerDescriptor)newValue;
          final UserStyle style = sld.findUserStyle( m_styleNames[i] );
          if( style != null && m_theme != null )
            m_theme.addStyle( new KalypsoUserStyle( style ) );
          else
          {
            // error handling?
          }

          fireModellEvent( null );

          break;
        }
      }
    }
    catch( final Throwable e )
    {
      // alles abfangen, damit der Pool trotzdem weitermacht
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    LOGGER.info( "Object invalid: " + key + "   -  Object: " + oldValue );
    
    m_loaded = false;
    
    if( m_theme == null )
      return;
    
    if( KeyComparator.getInstance().compare( key, m_layerKey ) == 0 )
    {
      // die alte selektion merken, falls das Thema gleich wieder geladen wird!
      final FeatureList featureList = m_theme.getFeatureList();
      m_lastSelectedFeaturesMap.clear();
      featureList.accept( new QuerySelectionVisitor( m_lastSelectedFeaturesMap ) );
      
      m_theme.removeModellListener( this );
      m_theme.dispose();
      m_theme = null;

      // schon mal mitteilen, dass sich das Thema geändert hat
      fireModellEvent( new ModellEvent( this, ModellEvent.FULL_CHANGE ) );
    }

    for( int i = 0; i < m_styleKeys.length; i++ )
    {
      final IPoolableObjectType styleKey = m_styleKeys[i];
      if( KeyComparator.getInstance().compare( key, styleKey ) == 0 )
        m_theme.removeStyle( (KalypsoUserStyle)oldValue );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getWorkspace()
   */
  public CommandableWorkspace getWorkspace()
  {
    if( m_theme != null )
      return m_theme.getWorkspace();

    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureType()
   */
  public FeatureType getFeatureType()
  {
    if( m_theme != null )
      return m_theme.getFeatureType();

    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#addStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void addStyle( KalypsoUserStyle style )
  {
    if( m_theme != null )
      m_theme.addStyle( style );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#removeStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void removeStyle( KalypsoUserStyle style )
  {
    if( m_theme != null )
      m_theme.removeStyle( style );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getStyles()
   */
  public UserStyle[] getStyles()
  {
    if( m_theme != null )
      return m_theme.getStyles();
    return new UserStyle[0];
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureList()
   */
  public FeatureList getFeatureList()
  {
    if( m_theme != null )
      return m_theme.getFeatureList();
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getSchedulingRule()
   */
  public ISchedulingRule getSchedulingRule()
  {
    return m_commandTarget.getSchedulingRule();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureListVisible(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public FeatureList getFeatureListVisible( GM_Envelope env )
  {
    if( m_theme != null )
      return m_theme.getFeatureListVisible( env );
    return null;
  }

  /**
   * @see org.kalypso.loader.IPooledObject#isLoaded()
   */
  public boolean isLoaded()
  {
    if( m_loaded )
      return true;
    // theme not here
    if( m_theme == null )
      return false;
    // wrong number of styles ?
    if( m_theme.getStyles().length != m_styleKeys.length )
      return false;
    m_loaded = true;
    return m_loaded;
  }

}