package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.net.URL;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypso.template.types.LayerType;
import org.kalypso.template.types.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.StyleType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * <p>
 * Ein Decorator für ein {@link org.kalypso.ogc.gml.KalypsoFeatureTheme},
 * welches dieses (asynchron) über den Pool aus einer Source lädt.
 * </p>
 * <p>
 * Die ganze dynamic, also die Überwachung, ob sich das Pool-Objekt geändert
 * etc. findet hier statt
 * </p>
 * 
 * <p>
 * Hier findet auch die Verwaltung statt, ob sich Daten des Themas geändert
 * haben
 * </p>
 * <p>
 * Implementiert unter anderem {@link org.kalypso.util.command.ICommandTarget},
 * da sich die Daten des unterliegenden Themas ändern können
 * </p>
 * 
 * @author Belger
 */
public class PoolableKalypsoFeatureTheme extends AbstractKalypsoTheme implements IPoolListener,
    ICommandTarget
{
  private final JobExclusiveCommandTarget m_commandTarget = new JobExclusiveCommandTarget( null );

  private final PoolableObjectType m_layerKey;

  private final String m_featurePath;

  private final PoolableObjectType[] m_styleKeys;

  private final String[] m_styleNames;

  private KalypsoFeatureTheme m_theme = null;

  public PoolableKalypsoFeatureTheme( final LayerType layerType, final URL context )
  {
    super( "<no name>" );

    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final String source = layerType.getHref();
    final String type = layerType.getLinktype();
    final String featurePath = layerType.getFeaturePath();

    m_layerKey = new PoolableObjectType( type, source, context );
    m_featurePath = featurePath;

    pool.addPoolListener( this, m_layerKey );

    if( layerType instanceof Layer )
    {
      final Layer mapLayerType = (Layer)layerType;

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

        pool.addPoolListener( this, m_styleKeys[i] );
      }
    }
    else
    {
      m_styleKeys = null;
      m_styleNames = null;
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose()
  {
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    pool.removePoolListener( this );

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
   *      org.deegree.graphics.transformation.GeoTransform, double,
   *      org.deegree.model.geometry.GM_Envelope, int)
   */
  public void paintSelected( Graphics g, GeoTransform p, double scale, GM_Envelope bbox,
      int selectionId )
  {
    if( m_theme != null )
      m_theme.paintSelected( g, p, scale, bbox, selectionId );
  }

  public void saveFeatures() throws FactoryException
  {
    // TODO: do it in a job
    KalypsoGisPlugin.getDefault().getPool().saveObject( m_theme.getWorkspace(), new NullProgressMonitor() );
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
    layer.setHref( key.getSourceAsString() );
    layer.setLinktype( key.getType() );
    layer.setActuate( "onRequest" );
    layer.setType( "simple" );

    if( layer instanceof StyledLayerType )
    {
      final StyledLayerType styledLayerType = (StyledLayerType)layer;
      styledLayerType.setName( getName() );
      styledLayerType.setVisible( isVisible );
      styledLayerType.getDepends();

      final List stylesList = styledLayerType.getStyle();
      IPoolableObjectType[] styleKeys = m_styleKeys;
      for( int j = 0; j < styleKeys.length; j++ )
      {
        StyleType styleType = extentFac.createStyledLayerTypeStyleType();
        IPoolableObjectType styleKey = styleKeys[j];
        styleType.setActuate( "onRequest" );
        styleType.setHref( styleKey.getSourceAsString() );
        styleType.setLinktype( styleKey.getType() );
        styleType.setType( "simple" );
        stylesList.add( styleType );
      }
    }
  }

  public KalypsoFeatureTheme getFeatureTheme()
  {
    return m_theme;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, Object newValue, IStatus status )
  {
    if( key == m_layerKey )
    {
      if( m_theme != null )
      {
        m_theme.removeModellListener( this );
        m_theme.dispose();
        m_theme = null;
      }
  
      m_theme = new KalypsoFeatureTheme( (GMLWorkspace)newValue, m_featurePath, getName() );
      m_theme.addModellListener( this );
  
      fireModellEvent( new ModellEvent( this, ModellEvent.THEME_ADDED ) );
    }
    
    // styles
    for( int i = 0; i < m_styleKeys.length; i++ )
    {
      final IPoolableObjectType styleKey = m_styleKeys[i];
      if( styleKey == key )
      {
        final StyledLayerDescriptor sld = (StyledLayerDescriptor)newValue;
        final UserStyle style = sld.findUserStyle( m_styleNames[i] );
        if( style != null && m_theme != null )
          m_theme.addStyle( new KalypsoUserStyle( style ) );
        else
        {
          // TODO error handling
        }
    
        fireModellEvent( null );
        
        break;
      }
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    if( key == m_layerKey )
    {
      m_theme.removeModellListener( this );
      m_theme.dispose();
      m_theme = null;

      // schon mal mitteilen, dass sich das Thema geändert hat
      fireModellEvent( new ModellEvent( this, ModellEvent.FULL_CHANGE ) );
    }

    for( int i = 0; i < m_styleKeys.length; i++ )
    {
      final IPoolableObjectType styleKey = m_styleKeys[i];
      if( key == styleKey )
        m_theme.removeStyle( (KalypsoUserStyle)oldValue );
    }
  }
}