package org.kalypso.util.pool;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.kalypso.eclipse.core.runtime.jobs.MutexSchedulingRule;
import org.kalypso.loader.ILoader;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.loader.ILoaderListener;
import org.kalypso.loader.LoaderException;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.factory.FactoryException;

/**
 * @author dömming,belger
 */
public class ResourcePool implements ILoaderListener
{
  private final ILoaderFactory m_factory;

  /** type -> loader */
  private final Map m_loaderCache = new HashMap();

  /**
   * für den BorrowObjecJob, damit alle Objekte eines Pools nacheinander geladen
   * werden
   */
  private final ISchedulingRule m_schedulingRule = new MutexSchedulingRule();

  private final Comparator m_keyComparator = new KeyComparator();

  /** key -> Set(IPoolListener) */
  private Map m_listeners = Collections.synchronizedMap( new TreeMap( m_keyComparator ) );

  /** key -> object */
  private Map m_objects = new TreeMap( m_keyComparator );

  /** key -> job */
  private Map m_jobs = new TreeMap( m_keyComparator );

  private Logger m_logger = Logger.getLogger( this.getClass().getName() );

  public ResourcePool( final ILoaderFactory factory )
  {
    synchronized( this )
    {
      m_factory = factory;
    }
  }

  public void dispose()
  {
    synchronized( this )
    {
      for( final Iterator iter = m_listeners.entrySet().iterator(); iter.hasNext(); )
      {
        final Map.Entry entry = (Entry)iter.next();
        ( (Set)entry.getValue() ).clear();

        releaseKey( (IPoolableObjectType)entry.getKey() );

        iter.remove();
      }

      for( Iterator iter = m_loaderCache.values().iterator(); iter.hasNext(); )
        ( (ILoader)iter.next() ).removeLoaderListener( this );
    }
  }

  /**
   * Fügt einen neuen Listener zum Pool für eine bestimmten Key hinzu Ist das
   * Objekt für den key vorhanden, wird der Listener sofort informiert
   * 
   * @param l
   * @param key
   */
  public void addPoolListener( final IPoolListener l, final IPoolableObjectType key )
  {
    synchronized( this )
    {
      Set listeners = (Set)m_listeners.get( key );

      // falls noch keine Listener da waren eine neue Liste anlegen
      if( listeners == null )
      {
        listeners = new HashSet();
        m_listeners.put( key, listeners );
      }

      listeners.add( l );

      final Object o = checkValid( key );
      if( o != null )
        l.objectLoaded( key, o, Status.OK_STATUS );
    }
  }

  public void removePoolListener( final IPoolListener l )
  {
    synchronized( m_listeners )
    {
      final Collection toRemove = new ArrayList();
      
      // von allen keys den Listener löschen
      for( final Iterator iter = m_listeners.entrySet().iterator(); iter.hasNext(); )
      {
        final Map.Entry entry = (Entry)iter.next();

        final IPoolableObjectType key = (IPoolableObjectType)entry.getKey();
        final Set listeners = (Set)entry.getValue();

        listeners.remove( l );
        if( listeners.isEmpty() )
        {
          // aktuellen Eintrag löschen!
          toRemove.add( key );

          releaseKey( key );
        }
      }
      
      for( final Iterator iter = toRemove.iterator(); iter.hasNext(); )
        m_listeners.remove( iter.next() );
    }
  }

  /**
   * Prüft, ob das Objekt für den Key vorhanden ist Falls ja wird es
   * zurückgegeben. Falls nein wird der Ladevorgang gestartet
   * 
   * @param key
   * @return object belonging to key
   */
  private Object checkValid( final IPoolableObjectType key )
  {
    // falls das objekt da ist, einfach zurückgeben
    final Object object = m_objects.get( key );
    if( object != null )
      return object;

    // falls nicht den Ladevorgng starten
    startLoading( key );

    return null;
  }

  private void startLoading( final IPoolableObjectType key )
  {
    if( m_jobs.containsKey( key ) )
      return;

    final BorrowObjectJob job = new BorrowObjectJob( key );
    m_jobs.put( key, job );
    job.schedule();
  }

  protected void onObjectLoaded( final IPoolableObjectType key, final Object object,
      final IStatus status )
  {
    if( object == null )
      return;
    
    // das Objekt eintragen
    m_objects.put( key, object );

    // alle Listener informieren
    final Set listenerset = (Set)m_listeners.get( key );
    if( listenerset != null )
    {
      final Set listeners = Collections.synchronizedSet( listenerset );
      for( final Iterator iter = listeners.iterator(); iter.hasNext(); )
      {
        final IPoolListener l = (IPoolListener)iter.next();
        l.objectLoaded( key, object, status );
      }
    }

    // die job markierung entfernen
    m_jobs.remove( key );
  }

  /**
   * @see org.kalypso.loader.ILoaderListener#onLoaderObjectInvalid(java.lang.Object,
   *      boolean)
   */
  public void onLoaderObjectInvalid( final Object oldValue, final boolean bCannotReload )
  {
    synchronized( this )
    {
      m_logger.info( "Objekt invalidated: " + oldValue );

      final IPoolableObjectType key = findKey( oldValue );
      if( key != null )
      {
        m_objects.remove( key );

        final Set listeners = (Set)m_listeners.get( key );
        if( listeners != null )
        {
          for( Iterator iter = listeners.iterator(); iter.hasNext(); )
          {
            final IPoolListener l = (IPoolListener)iter.next();
            l.objectInvalid( key, oldValue );
          }
        }

        checkValid( key );
      }
    }
  }

  private IPoolableObjectType findKey( final Object o )
  {
    for( final Iterator iter = m_objects.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();
      final Object value = entry.getValue();
      if( value == o )
        return (IPoolableObjectType)entry.getKey();
    }

    return null;
  }

  /**
   * Erzeugt ein Objekt anhand seines Typs. Benutzt den entsprechenden ILoader.
   * 
   * @param key
   * @param monitor
   * @return instance using loader
   * @throws LoaderException
   * @throws FactoryException
   */
  protected Object makeObject( final IPoolableObjectType key, final IProgressMonitor monitor )
      throws LoaderException, FactoryException
  {
    m_logger.info( "Loading objekt for key: " + key );

    final String type = key.getType();

    final ILoader loader = getLoader( type );

    final Object object = loader.load( key.getLocation(), key.getContext(), monitor );

    return object;
  }

  protected ILoader getLoader( final String type ) throws FactoryException
  {
    ILoader loader = (ILoader)m_loaderCache.get( type );
    if( loader == null )
    {
      loader = m_factory.getLoaderInstance( type );

      loader.addLoaderListener( this );

      m_loaderCache.put( type, loader );
    }

    return loader;
  }

  private void releaseKey( final IPoolableObjectType key )
  {
    synchronized( this )
    {
      m_logger.info( "Releasing key: " + key );
      
      final BorrowObjectJob job = (BorrowObjectJob)m_jobs.get( key );
      if( job != null )
        job.cancel();

      m_jobs.remove( key );

      final Set listeners = (Set)m_listeners.get( key );
      if( listeners != null )
        listeners.clear();
      
      final Object object = m_objects.get( key );
      if( object != null )
      {
        try
        {
          final ILoader loader = getLoader( key.getType() );
          loader.release( object );
        }
        catch( final FactoryException e1 )
        {
          e1.printStackTrace();
        }
      }

      m_objects.remove( key );
    }
  }

  public void saveObject( final Object object, final IProgressMonitor monitor )
      throws LoaderException, FactoryException

  {
    synchronized( this )
    {
      final IPoolableObjectType key = findKey( object );

      if( key != null )
      {
        final ILoader loader = getLoader( key.getType() );
        loader.save( key.getLocation(), key.getContext(), monitor, object );
      }
    }
  }

  protected ISchedulingRule getSchedulingRule()
  {
    return m_schedulingRule;
  }

  /**
   * BorrowObjectJob borrows the job from the pool and fires event once object
   * is loaded.
   * 
   * @author belger
   */
  private class BorrowObjectJob extends Job
  {
    private final IPoolableObjectType m_key;

    protected Object m_object = null;

    /**
     * exceptions that occur when job runs can be ignored. In that case the job
     * is canceled
     */
    private boolean m_ignoreExceptions;

    /**
     * Constructor
     * 
     * @param key
     *          the key to which this job is associated
     */
    public BorrowObjectJob( final IPoolableObjectType key )
    {
      super( "Lade Resource: " + key.toString() );

      m_key = key;
      m_ignoreExceptions = key.isIgnoreExceptions();

      setPriority( Job.LONG );

      // Jobs auf dem gleichen Pool müssen nacheinander laufen!
      setRule( getSchedulingRule() );

      addJobChangeListener( new JobChangeAdapter()
      {
        /**
         * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
         */
        public void done( final IJobChangeEvent event )
        {
          try
          {
            if( event.getResult().isOK() )
              onObjectLoaded( key, m_object, event.getResult() );
            else
              onObjectLoaded( key, null, event.getResult() );
          }
          finally
          {
            removeJobChangeListener( this );
            // means: removeJobChangeListener( JobChangeAdapter.this );
          }
        }
      } );
    }

    /**
     * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
     */
    protected IStatus run( final IProgressMonitor monitor )
    {
      try
      {
        m_object = makeObject( m_key, monitor );
      }
      catch( final Exception e )
      {
        if( m_ignoreExceptions )
          return Status.CANCEL_STATUS;

        return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
            "Fehler beim Laden einer Resource", e );
      }

      return Status.OK_STATUS;
    }
  }

  public boolean equalsKeys( final IPoolableObjectType key1, final IPoolableObjectType key2 )
  {
    return m_keyComparator.compare( key1, key2 ) == 0;
  }

  /**
   * @return Returns the keyComparator.
   */
  public Comparator getKeyComparator()
  {
    return m_keyComparator;
  }
}