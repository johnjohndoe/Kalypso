package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.util.List;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.loader.ILoader;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.gismapview.GismapviewType.LayersType.Layer;
import org.kalypso.template.types.LayerType;
import org.kalypso.template.types.StyledLayerType.StyleType;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.pool.BorrowObjectJob;
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
  protected final ResourcePool m_layerPool = KalypsoGisPlugin.getDefault().getPool(
      KalypsoFeatureLayer.class );
  
  private final ResourcePool m_stylePool = KalypsoGisPlugin.getDefault().getPool( UserStyle.class );

  private final JobExclusiveCommandTarget m_commandTarget = new JobExclusiveCommandTarget( null );

  private final PoolableObjectType m_layerKey;

  private IKalypsoTheme m_theme = null;

  private boolean m_isEditing = false;

  private PoolableObjectType[] m_styleKeys;

  private final LayerType m_layerType;

  public PoolableKalypsoFeatureTheme( final LayerType layerType, final IProject project )
  {
    super( "<no name>" );
    m_layerType=layerType;
    m_layerPool.addPoolListener( this );
    m_stylePool.addPoolListener( this );
    
    final String source = layerType.getHref();
    final String type = layerType.getLinktype();

    m_layerKey = new PoolableObjectType( type, source, project );

    if( layerType instanceof Layer )
    {
      final Layer mapLayerType = (Layer)layerType;
      
      setName( mapLayerType.getName() );
      
      final List stylesList = mapLayerType.getStyle();

      m_styleKeys = new PoolableObjectType[stylesList.size()];
      for( int i = 0; i < stylesList.size(); i++ )
      {
        final StyleType styleType = (StyleType)stylesList.get(i);
  
        m_styleKeys[i] = new PoolableObjectType( styleType.getLinktype(),
            styleType.getHref(), project );
      }
    }
    if("wms".equals(type))
      startWMSLayerLoading(m_layerKey, project);
    else
    startLayerLoading( m_layerKey );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose()
  {
    m_layerPool.removePoolListener( this );
    m_stylePool.removePoolListener( this );

    m_layerPool.releaseKey( m_layerKey );
    if( m_styleKeys != null )
    {
      for( int i = 0; i < m_styleKeys.length; i++ )
        m_stylePool.releaseKey( m_styleKeys[i] );
    }
    
    m_commandTarget.dispose();

    if( m_theme != null )
    {
      m_theme.removeModellListener( this );
      m_theme.dispose();
      m_theme = null;
    }
  }
  
  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getName()
   */
  public String getName()
  {
    return super.getName() + ( m_theme == null ? " - loading..." : "" );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(org.kalypso.util.pool.ResourcePool, org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, boolean)
   */
  public synchronized void onObjectInvalid( final ResourcePool source, final IPoolableObjectType key, final Object oldValue, final boolean bCannotReload )
      throws Exception
  {
    if( source == m_layerPool )
    {
      if( oldValue == this )
      {
        if( m_theme != null )
        {
          m_theme.removeModellListener(this);
          m_theme.dispose();
          m_theme = null;
        }
        
        // diesmal gehts vermutlich schnell, deshalb keinen job oder
        // progress-monitor
        final IKalypsoLayer layer = (IKalypsoLayer)m_layerPool.getObject( key,
            new NullProgressMonitor() );

        m_theme = new KalypsoFeatureTheme( layer, getName() );
        m_theme.addModellListener( this );

        // styles laden
        if( m_styleKeys != null )
        {
          for( int i = 0; i < m_styleKeys.length; i++ )
            startStyleLoading( m_styleKeys[i] );
        }

        fireModellEvent( new ModellEvent( this, ModellEvent.THEME_ADDED ) );
      }
      else if( m_theme != null && oldValue == m_theme.getLayer() )
      {
        // Thema neu laden lassen
        startLayerLoading( key );

        // schon mal mitteilen, dass sich das Thema geändert hat
        fireModellEvent( new ModellEvent( this, ModellEvent.FULL_CHANGE ) );
      }
    }
    
    if( source == m_stylePool )
    {
      if( oldValue == this )
      {
        // diesmal gehts vermutlich schnell, deshalb keinen job oder
        // progress-monitor
        final KalypsoUserStyle userStyle = (KalypsoUserStyle)m_stylePool.getObject( key,
            new NullProgressMonitor() );

        m_theme.addStyle( userStyle );
        
        fireModellEvent( null );
      }
      else if( oldValue instanceof KalypsoUserStyle )
      {
        m_theme.removeStyle( (KalypsoUserStyle)oldValue );
        
        startStyleLoading( key );
      }
    }
  }
  
  private void startWMSLayerLoading( final IPoolableObjectType layerKey, IProject project )
  {
      ILoaderFactory loaderFactory = KalypsoGisPlugin.getDefault().getLoaderFactory(KalypsoWMSLayer.class);

      final ILoader loaderInstance;
      try
      {
        loaderInstance = loaderFactory.getLoaderInstance(layerKey.getType());
        final KalypsoWMSLayer layer = (KalypsoWMSLayer)loaderInstance.load(layerKey.getSource(), project, null);
        m_theme=new KalypsoWMSTheme( getName(), layer );
      }
      catch( FactoryException e )
      {
        e.printStackTrace();
      }
      catch( LoaderException e )
      {
        e.printStackTrace();
      }
      
    }

  private void startLayerLoading( final IPoolableObjectType layerKey )
          {
    final String path = layerKey.getSource().getProperty( "PATH" ,"");
    final Job layerJob = new BorrowObjectJob( "Thema laden: " + path, m_layerPool, this, layerKey, this );
    final LayerRule layerRule = new LayerRule();
    layerJob.setRule( layerRule );
    layerJob.schedule();
    
    }
  
  private void startStyleLoading( final IPoolableObjectType styleKey )
  {
    final String path = styleKey.getSource().getProperty( "PATH" );
    
    final Job styleJob = new BorrowObjectJob( "Style geladen: " + path, m_stylePool, this, styleKey, this );
    styleJob.setRule( new StyleRule() );
    styleJob.schedule();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paintSelected(java.awt.Graphics, org.deegree.graphics.transformation.GeoTransform, double, org.deegree.model.geometry.GM_Envelope, int)
   */
  public void paintSelected( Graphics g, GeoTransform p, double scale, GM_Envelope bbox,
      int selectionId )
  {
    if( m_theme != null )
      m_theme.paintSelected( g, p, scale, bbox, selectionId );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getStyles()
   */
  public UserStyle[] getStyles()
  {
    if( m_theme == null )
      return null;

    return m_theme.getStyles();
  }

  public IPoolableObjectType[] getPoolableStyles()
  {
    return m_styleKeys;
  }
  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#addStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void addStyle( final KalypsoUserStyle style )
  {
    if( m_theme != null )
      m_theme.addStyle( style );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#removeStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void removeStyle( final KalypsoUserStyle style )
  {
    if( m_theme != null )
      m_theme.removeStyle( style );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getLayer()
   */
  public IKalypsoLayer getLayer()
  {
    if( m_theme == null )
      return null;

    return m_theme.getLayer();
  }

  public void saveFeatures()
  {
//    final Job saveJob = new Job( "Daten speichern" )
//    {
//      protected IStatus run( final IProgressMonitor monitor )
//      {
//        try
//        {
//          m_layerPool.saveObject( getLayer(), monitor );
//        }
//        catch( final FactoryException e )
//        {
//          return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
//              "Fehler beim Speichern der Daten", e );
//        }
//
//        return Status.OK_STATUS;
//      }
//    };

    try
    {
      m_layerPool.saveObject( getLayer(), new NullProgressMonitor() );
//    saveJob.setPriority( Job.LONG );
//    saveJob.schedule();
    }
    catch( final FactoryException e )
    {
      e.printStackTrace();
    }
  }

  public void setEditing( final boolean isEditing )
  {
    m_isEditing = isEditing;
  }

  public boolean isEditing()
  {
    return m_isEditing;
  }

  public boolean isDirty()
  {
    return m_commandTarget.isDirty();
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  private final class LayerRule implements ISchedulingRule
  {
    public boolean isConflicting( final ISchedulingRule rule )
    {
      return ( rule instanceof LayerRule );
    }

    public boolean contains( final ISchedulingRule rule )
    {
      return rule == this;
    }

    public PoolableKalypsoFeatureTheme getTheme()
    {
      return PoolableKalypsoFeatureTheme.this;
    }
  }

  private final class StyleRule implements ISchedulingRule
  {
    public boolean isConflicting( final ISchedulingRule rule )
    {
      // Styles müssen nach den jeweiligen Themen geladen werden
      // und alle Styles nacheinander, da Deegree hier nicht Thread Save ist
      return ( rule instanceof StyleRule );
    }
    
    public boolean contains( final ISchedulingRule rule )
    {
      return rule == this;
    }
  }

  public PoolableObjectType getLayerKey()
  {
    return m_layerKey;
  }
  
  public LayerType getLayertype()
  {
      return m_layerType;
  }
}