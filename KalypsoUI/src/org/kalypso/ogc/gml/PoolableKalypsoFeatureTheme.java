package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.util.List;

import org.deegree.graphics.sld.UserStyle;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.ogc.MapModell;
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

  private KalypsoFeatureTheme m_theme = null;

  private boolean m_isEditing = false;

  private MapModell m_parent = null;

  private PoolableObjectType[] m_styleKeys;

  public PoolableKalypsoFeatureTheme( final LayerType layerType, final IProject project )
  {
    super( "<no name>" );
    
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
        final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)m_layerPool.getObject( key,
            new NullProgressMonitor() );

        m_theme = new KalypsoFeatureTheme( layer, getName() );
        m_theme.setParent( m_parent );
        m_theme.addModellListener( this );

        // styles laden
        if( m_styleKeys != null )
        {
          for( int i = 0; i < m_styleKeys.length; i++ )
            startStyleLoading( m_styleKeys[i] );
        }

        fireModellEvent( new ModellEvent( this, ModellEvent.FULL_CHANGE ) );
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
        
        fireModellEvent( new ModellEvent( this, ModellEvent.FULL_CHANGE ) );
      }
      else if( oldValue instanceof KalypsoUserStyle )
      {
        m_theme.removeStyle( (KalypsoUserStyle)oldValue );
        
        startStyleLoading( key );
      }
    }
  }
  
  private void startLayerLoading( final IPoolableObjectType layerKey )
  {
    final String path = layerKey.getSource().getProperty( "PATH" );
    
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
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics)
   */
  public void paint( final Graphics g )
  {
    if( m_theme != null )
      m_theme.paint( g );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paintSelected(java.awt.Graphics,
   *      int)
   */
  public void paintSelected( final Graphics g, final int selectionId )
  {
    if( m_theme != null )
      m_theme.paintSelected( g, selectionId );
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
  public KalypsoFeatureLayer getLayer()
  {
    if( m_theme == null )
      return null;

    return m_theme.getLayer();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setParent(org.kalypso.ogc.MapModell)
   */
  public void setParent( final MapModell parent )
  {
    m_parent = parent;
    
    if( m_theme != null )
      m_theme.setParent( parent );
  }

  public void saveFeatures()
  {
    final Job saveJob = new Job( "Daten speichern" )
    {
      protected IStatus run( final IProgressMonitor monitor )
      {
        try
        {
          m_layerPool.saveObject( getLayer(), monitor );
        }
        catch( final FactoryException e )
        {
          return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
              "Fehler beim Speichern der Daten", e );
        }

        return Status.OK_STATUS;
      }
    };

    saveJob.setPriority( Job.LONG );
    saveJob.schedule();
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
}