package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.net.URL;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.feature.visitors.UnselectFeatureVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
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
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

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
    ICommandTarget, IKalypsoFeatureTheme
{
  private JobExclusiveCommandTarget m_commandTarget;

  private final PoolableObjectType m_layerKey;

  private final String m_featurePath;

  private final PoolableObjectType[] m_styleKeys;

  private final String[] m_styleNames;

  private IKalypsoFeatureTheme m_theme = null;

  private final int m_selectionID;

  /**
   * @param selectionID Falls ungleich -1, wird das erste feature des geladenen Themas mit dieser ID selektiert
   */
  public GisTemplateFeatureTheme( final LayerType layerType, final URL context, final int selectionID )
  {
    super( "<no name>" );

    m_selectionID = selectionID;
    
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

    final String source = layerType.getHref();
    final String type = layerType.getLinktype();
    final String featurePath = layerType.getFeaturePath();

    m_layerKey = new PoolableObjectType( type, source, context );
    m_featurePath = featurePath;

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
   *      org.deegree.graphics.transformation.GeoTransform, double,
   *      org.deegree.model.geometry.GM_Envelope, int)
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
  public void objectLoaded( final IPoolableObjectType key, Object newValue, IStatus status )
  {
    try
    {
      final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();

      if( pool.equalsKeys( key, m_layerKey ) )
      {
        if( m_theme != null )
        {
          m_theme.removeModellListener( this );
          m_theme.dispose();
          m_theme = null;
        }

        m_theme = new KalypsoFeatureTheme( (CommandableWorkspace)newValue, m_featurePath, getName() );

        if( m_selectionID != -1 )
        {
          final FeatureList featureList = m_theme.getFeatureList();
          if( featureList != null && featureList.size() > 0 )
          {
            featureList.accept( new UnselectFeatureVisitor( m_selectionID ) );
            
            ((Feature)featureList.get( 0 )).select( m_selectionID );
          }
        }
        
        m_theme.addModellListener( this );

        m_commandTarget = new JobExclusiveCommandTarget( m_theme.getWorkspace(), null );

        // jetzt immer die styles noch mal holen
        // das ist nicht ok! was ist, wenn inzwischen neue styles vom user
        // hinzugefügt wurden?
        // das ist nicht ok! was ist, wenn inzwischen neue styles vom user hinzugef?gt wurden?
        for( int i = 0; i < m_styleKeys.length; i++ )
          pool.addPoolListener( this, m_styleKeys[i] );

        fireModellEvent( new ModellEvent( this, ModellEvent.THEME_ADDED ) );
      }

      // styles
      for( int i = 0; i < m_styleKeys.length; i++ )
      {
        final IPoolableObjectType styleKey = m_styleKeys[i];
        if( pool.equalsKeys( styleKey, key ) )
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
    if( key == m_layerKey )
    {
      m_theme.removeModellListener( this );
      m_theme.dispose();
      m_theme = null;

      // schon mal mitteilen, dass sich das Thema ge?ndert hat
      fireModellEvent( new ModellEvent( this, ModellEvent.FULL_CHANGE ) );
    }

    for( int i = 0; i < m_styleKeys.length; i++ )
    {
      final IPoolableObjectType styleKey = m_styleKeys[i];
      if( key == styleKey )
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
}