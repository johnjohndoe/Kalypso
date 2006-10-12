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
package org.kalypso.loader;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.util.SafeRunnable;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;

/**
 * @author belger
 */
public abstract class AbstractLoader implements ILoader, IResourceChangeListener
{
  private final AbstractLoaderResourceDeltaVisitor m_visitor = new AbstractLoaderResourceDeltaVisitor( this );

  private final List<ILoaderListener> m_listener = new ArrayList<ILoaderListener>();

  private final List<Object> m_objectList = new ArrayList<Object>();

  /** Resources in this list will be ignored at the next resource change event. */
  private final Collection<String> m_ignoreresourceList = new HashSet<String>();

  public AbstractLoader( )
  {
    ResourcesPlugin.getWorkspace().addResourceChangeListener( this, IResourceChangeEvent.POST_CHANGE );
  }

  /**
   * TODO: this will never be called. The resource pool caches the loaders and reuse them, but never disposes them.
   * Maybe it would better to use one loader per resource, in order to do so we should refaktor the loaders as well.
   * Each loader should be responsible for exakt one object.
   */
  public void dispose( )
  {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );
    m_listener.clear();
    m_objectList.clear();
  }

  public Object[] getObjects( )
  {
    return m_objectList.toArray( new Object[m_objectList.size()] );
  }

  /**
   * @see org.kalypso.loader.ILoader#load(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  public Object load( final String source, final URL context, final IProgressMonitor monitor ) throws LoaderException
  {
    final Object newObject = loadIntern( source, context, monitor );

    m_objectList.add( newObject );

    return newObject;
  }

  /**
   * This method should be overriden by clients extending this class.
   */
  protected abstract Object loadIntern( final String source, final URL context, final IProgressMonitor monitor ) throws LoaderException;

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
    if( m_ignoreresourceList.contains( m_visitor.pathForObject( oldObject ) ) )
      return;

    // protect against concurrent modification exception and broken listeners
    final ILoaderListener[] ls = m_listener.toArray( new ILoaderListener[m_listener.size()] );
    for( int i = 0; i < ls.length; i++ )
    {
      final ILoaderListener listener = ls[i];
      SafeRunnable.run( new SafeRunnable()
      {
        public void run( ) throws Exception
        {
          listener.onLoaderObjectInvalid( oldObject, bCannotReload );
        }
      } );
    }
  }

  /**
   * Always call super implementation if overwritten.
   * 
   * @see org.kalypso.loader.ILoader#release(java.lang.Object)
   */
  public void release( final Object object )
  {
    m_objectList.remove( object );

    m_visitor.releaseResources( object );
  }

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public final void resourceChanged( final IResourceChangeEvent event )
  {
    // allways true, because of the bitmask set on adding this listener
    if( event.getType() == IResourceChangeEvent.POST_CHANGE )
    {
      final IResourceDelta delta = event.getDelta();
      try
      {
        delta.accept( m_visitor );
      }
      catch( final CoreException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoCorePlugin.getDefault().getLog().log( status );
      }
    }
  }

  public void addResource( final IResource resource, final Object object )
  {
    m_visitor.addResource( resource, object );
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor,
   *      java.lang.Object)
   */
  public void save( final String source, final URL context, final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    throw new LoaderException( "Operation not supported" );
  }

  /**
   * @see org.kalypso.loader.ILoader#lockEvents(java.lang.Object, boolean)
   */
  public void lockEvents( final Object data, boolean doLock )
  {
    if( doLock )
      m_ignoreresourceList.add( m_visitor.pathForObject( data ) );
    else
      m_ignoreresourceList.remove( m_visitor.pathForObject( data ) );
  }
}