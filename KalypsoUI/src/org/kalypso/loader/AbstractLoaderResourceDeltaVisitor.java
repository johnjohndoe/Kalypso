package org.kalypso.loader;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.plugin.KalypsoGisPlugin;

/**
 * @author belger
 */
public class AbstractLoaderResourceDeltaVisitor implements IResourceDeltaVisitor
{
  /** resource -> object */
  private Map m_resourceMap = new HashMap();

  /** object -> resource */
  private final Map m_objectMap = new HashMap();

  private final AbstractLoader m_loader;

  public AbstractLoaderResourceDeltaVisitor( final AbstractLoader loader )
  {
    m_loader = loader;
  }

  public void addResource( final IResource resource, final Object o )
  {
    m_resourceMap.put( resource, o );
    m_objectMap.put( o, resource );
  }

  public void releaseResources( final Object o )
  {
    while( m_objectMap.containsKey( o ) )
    {
      final Object resource = m_objectMap.get( o );

      m_resourceMap.remove( resource );
      m_objectMap.remove( o );
    }
  }

  /**
   * @see org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse.core.resources.IResourceDelta)
   */
  public boolean visit( IResourceDelta delta ) throws CoreException
  {
    switch( delta.getKind() )
    {
    case IResourceDelta.ADDED:
      // ist mir egal! und alles was drunter ist auch!
      return false;

    case IResourceDelta.REMOVED:
    case IResourceDelta.CHANGED:
    {
      try
      {
        final Object oldValue = m_resourceMap.get( delta.getResource() );
        if( oldValue != null )
          m_loader.fireLoaderObjectInvalid( oldValue, delta.getKind() == IResourceDelta.REMOVED );
      }
      catch( final Exception e )
      {
        throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
            "Fehler beim Wiederherstellen einer Resource", e ) );
      }

      // handle changed resource
      return true;
    }
    }

    return true;
  }

}