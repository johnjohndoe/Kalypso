package org.kalypso.loader;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.core.IKalypsoCoreConstants;

/**
 * 
 * <pre>
 * Changes:
 * 2004-11-09 - schlienger - uses now the path of the resource as key in the map
 * 						     Path should be taken using the pathFor( IResource ) method
 * </pre>
 * 
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

  /**
   * TRICKY: manschmal hat der Pfad der Resource zwei slashes, manschmal nicht.
   * Deswegen dieser Methode: sie ersetzt die "//" mit "/" wenn es notwendig
   * ist. Der PFad wird letztendlich als key für der Map benutzt, um zu prüfen
   * ob eine Resource schon da ist.
   * 
   * @param resource
   * @return Pfad (ohne eventuelle "//")
   */
  private String pathFor( final IResource resource )
  {
    String path = resource.toString();

    // blöder HACK weil sonst werden die Resourcen nicht richtig verglichen
    if( path.indexOf( "//" ) != -1 )
      path = path.replaceAll( "//", "/" );

    return path;
  }

  public void addResource( final IResource resource, final Object o )
  {
    m_resourceMap.put( pathFor( resource ), o );
    m_objectMap.put( o, resource );
  }

  public void releaseResources( final Object o )
  {
    while( m_objectMap.containsKey( o ) )
    {
      final IResource resource = (IResource)m_objectMap.get( o );

      m_resourceMap.remove( pathFor( resource ) );
      m_objectMap.remove( o );
    }
  }

  /**
   * @see org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse.core.resources.IResourceDelta)
   */
  public boolean visit( IResourceDelta delta ) throws CoreException
  {
    final IResource resource = delta.getResource();
    final Object oldValue = m_resourceMap.get( pathFor( resource ) );
    if( oldValue != null )
    {
      switch( delta.getKind() )
      {
      case IResourceDelta.REMOVED:
        // todo: sollte eigentlich auch behandelt werden
        // aber so, dass och die chance auf ein add besteht
        break;
        
      case IResourceDelta.ADDED:
      case IResourceDelta.CHANGED:
      {
        try
        {
            m_loader.fireLoaderObjectInvalid( oldValue, delta.getKind() == IResourceDelta.REMOVED );
        }
        catch( final Exception e )
        {
          throw new CoreException( new Status( IStatus.ERROR, IKalypsoCoreConstants.PLUGIN_ID, 0,
              "Fehler beim Wiederherstellen einer Resource", e ) );
        }

        // handle changed resource
        return true;
      }
      }
    }

    return true;
  }
}