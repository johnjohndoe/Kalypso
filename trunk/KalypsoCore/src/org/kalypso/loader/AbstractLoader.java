package org.kalypso.loader;

import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author belger
 */
public abstract class AbstractLoader implements ILoader, IResourceChangeListener
{
  final AbstractLoaderResourceDeltaVisitor m_visitor = new AbstractLoaderResourceDeltaVisitor( this );

  private final List m_listener = new ArrayList();

  private final List m_objectList = new ArrayList();

  public AbstractLoader()
  {
    ResourcesPlugin.getWorkspace().addResourceChangeListener( this );
  }

  public void dispose()
  {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );
  }

  /**
   * @see org.kalypso.loader.ILoader#load(java.util.Properties, java.net.URL,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public Object load( Properties source, URL context, IProgressMonitor monitor )
      throws LoaderException
  {
    final Object newObject = loadIntern( source, context, monitor );

    m_objectList.add( newObject );

    return newObject;
  }

  /**
   * This method should be overriden by clients extending this class.
   */
  protected abstract Object loadIntern( final Properties source, final URL context,
      final IProgressMonitor monitor ) throws LoaderException;

  /**
   * @see org.kalypso.loader.ILoader#addLoaderListener(org.kalypso.loader.ILoaderListener)
   */
  public final void addLoaderListener( final ILoaderListener l )
  {
    m_listener.add( l );
  }

  /**
   * @see org.kalypso.loader.ILoader#removeLoaderListener(org.kalypso.loader.ILoaderListener)
   */
  public final void removeLoaderListener( final ILoaderListener l )
  {
    m_listener.remove( l );
  }

  public final void fireLoaderObjectInvalid( final Object oldObject, final boolean bCannotReload )
      throws Exception
  {
    for( final Iterator iter = m_listener.iterator(); iter.hasNext(); )
      ( (ILoaderListener)iter.next() ).onLoaderObjectInvalid( oldObject, bCannotReload );
  }

  /**
   * @see org.kalypso.loader.ILoader#release(java.lang.Object)
   */
  public final void release( final Object object )
  {
    m_objectList.remove( object );

    m_visitor.releaseResources( object );
  }

  protected final boolean hasObject( final Object oldValue )
  {
    return m_objectList.contains( oldValue );
  }

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public final void resourceChanged( final IResourceChangeEvent event )
  {
    if( event.getType() == IResourceChangeEvent.POST_CHANGE )
    {
      final IResourceDelta delta = event.getDelta();
      try
      {
        delta.accept( m_visitor );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }
    }
  }

  public void addResource( final IResource resource, final Object object )
  {
    m_visitor.addResource( resource, object );
  }

  /**
   * @throws LoaderException
   * @see org.kalypso.loader.ILoader#save(java.util.Properties,
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( final Properties source, final IProject project,
      final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    throw new LoaderException( "Operation not supported" );
  }
  
  public void save( final Properties source, final URL context,
      final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    throw new UnsupportedOperationException();
  }
}