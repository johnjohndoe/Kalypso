package org.kalypso.ogc.gml;

import java.awt.Graphics;

import org.deegree.graphics.sld.UserStyle;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.pool.BorrowObjectJob;
import org.kalypso.util.pool.IPoolListener;
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
 * 
 * @author Belger
 */
public class PoolableKalypsoFeatureTheme extends AbstractKalypsoTheme implements IPoolListener,
    ICommandTarget
{
  private static final Object DUMMY_OBJECT = new Object();

  protected final ResourcePool m_layerPool = KalypsoGisPlugin.getDefault().getPool(
      KalypsoFeatureLayer.class );

  private final JobExclusiveCommandTarget m_commandTarget = new JobExclusiveCommandTarget( null );

  private final PoolableObjectType m_key;

  private KalypsoFeatureTheme m_theme = null;

  private boolean m_isEditing = false;

  public PoolableKalypsoFeatureTheme( final String name, final String type, final String source,
      IProject project )
  {
    super( name );

    m_key = new PoolableObjectType( type, source, project );

    m_layerPool.addPoolListener( this );

    startThemeLoading();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose()
  {
    m_layerPool.removePoolListener( this );

    m_commandTarget.dispose();

    if( m_theme != null )
      m_theme.dispose();
  }

  /**
   * @throws Exception
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(java.lang.Object,
   *      boolean)
   */
  public void onObjectInvalid( final Object oldValue, final boolean bCannotReload )
      throws Exception
  {
    if( oldValue == DUMMY_OBJECT )
    {
      // theme should be null!
      // diesmal gehts vermutlich schnell, deshalb keinen job oder
      // progress-monitor
      final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)m_layerPool.getObject( m_key,
          new NullProgressMonitor() );

      m_theme = new KalypsoFeatureTheme( layer, getName() );

      fireModellEvent( new ModellEvent( this, ModellEvent.FULL_CHANGE ) );
    }

    if( m_theme != null && oldValue == m_theme.getLayer() )
    {
      // forget old theme
      m_theme.dispose();
      m_theme = null;

      // Thema neu laden lassen
      startThemeLoading();

      // trozdem mitteilen, dass das Thema sich geändert hat
      fireModellEvent( new ModellEvent( this, ModellEvent.FULL_CHANGE ) );
    }
  }

  private void startThemeLoading()
  {
    final String path = m_key.getSource().getProperty( "PATH" );
    final Job job = new BorrowObjectJob( "Thema laden: " + path, m_layerPool, this, m_key,
        DUMMY_OBJECT );
    job.schedule();
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
    if( m_theme != null )
      m_theme.setParent( parent );
  }

  public PoolableObjectType getKey()
  {
    return m_key;
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

}