package org.kalypso.loader;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * @author belger
 */
public abstract class AbstractLoader implements ILoader
{
  private final List m_listener = new ArrayList();

  private final List m_objectList = new ArrayList();
  
  /**
   * @see org.kalypso.loader.ILoader#load(java.util.Properties, org.eclipse.core.resources.IProject, org.eclipse.core.runtime.IProgressMonitor)
   */
  public final Object load( final Properties source, final IProject project, final IProgressMonitor monitor ) throws LoaderException
  {
    final Object newObject = loadIntern( source, project, monitor );
    
    m_objectList.add( newObject );
    
    return newObject;
  }

  protected abstract Object loadIntern( final Properties source, final IProject project, final IProgressMonitor monitor ) throws LoaderException;

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

  public final void fireLoaderObjectInvalid( final Object oldObject, final boolean bCannotReload ) throws Exception
  {
    for( final Iterator iter = m_listener.iterator(); iter.hasNext(); )
      ( (ILoaderListener)iter.next() ).onLoaderObjectInvalid( oldObject, bCannotReload );
  }

  /**
   * @see org.kalypso.loader.ILoader#release(java.lang.Object)
   */
  public void release( final Object object )
  {
    m_objectList.remove( object );
  }

  protected final boolean hasObject( final Object oldValue )
  {
    return m_objectList.contains(oldValue);
  }
}