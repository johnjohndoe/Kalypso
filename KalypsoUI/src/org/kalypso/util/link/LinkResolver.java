package org.kalypso.util.link;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.IdentityHashMap;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.BorrowObjectJob;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * @author schlienger
 */
public class LinkResolver implements IPoolListener
{
  private final Map m_key2link = new IdentityHashMap();
  
  private final ResourcePool m_pool;
  private final static Object DUMMY_OBJECT = new Object();

  private final ILinkResolverListener m_listener;

  /**
   * Resolves the given links.
   * 
   * @deprecated
   * 
   * @param links
   * @param resolveType
   * @param project
   * @param listener
   * 
   * @throws MalformedURLException
   */
  public LinkResolver( final ObjectLink[] links, final Class resolveType, final IProject project, final ILinkResolverListener listener ) throws MalformedURLException
  {
    this( links, resolveType, project.getFullPath().toFile().toURL(), listener );
  }

  public LinkResolver( final ObjectLink[] links, final Class resolveType, final URL context, final ILinkResolverListener listener )
  {
    m_listener = listener;
    m_pool = KalypsoGisPlugin.getDefault().getPool( resolveType );
    
    for( int i = 0; i < links.length; i++ )
    {
      final PoolableObjectType key = new PoolableObjectType( links[i].getLinkType(), links[i].getXlink().getHRef(), context );

      m_key2link.put( key, links[i] );
      
      final Job job = new BorrowObjectJob( "Link auslösen für " + resolveType.getName(), m_pool, this, key, DUMMY_OBJECT );
      job.schedule();
    }
  }

  public void dispose()
  {
    m_key2link.clear();
  }
  
  /**
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(org.kalypso.util.pool.ResourcePool, org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, boolean)
   */
  public synchronized void onObjectInvalid( ResourcePool source, IPoolableObjectType key, Object oldObject, boolean bCannotReload ) throws Exception
  {
    final ObjectLink link = (ObjectLink)m_key2link.get( key );
    
    if( oldObject == DUMMY_OBJECT || link.isResolved() && link.getLinkedObject() == oldObject )
    {
      link.linkResolved( m_pool.getObject( key, new NullProgressMonitor() ) );
      
      m_listener.onLinkResolved( new LinkEvent( this, link ) );
    }
  }
}
