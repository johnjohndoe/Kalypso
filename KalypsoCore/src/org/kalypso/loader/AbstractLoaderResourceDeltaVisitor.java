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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.core.IKalypsoCoreConstants;

/**
 * <pre>
 *   Changes:
 *   2004-11-09 - schlienger - uses now the path of the resource as key in the map
 *   						     Path should be taken using the pathFor( IResource ) method
 * </pre>
 * 
 * @author belger
 */
public class AbstractLoaderResourceDeltaVisitor implements IResourceDeltaVisitor
{
  /** resource -> object */
  private final Map<String, Object> m_resourceMap = new HashMap<String, Object>();

  /** object -> resource */
  private final Map<Object, IResource> m_objectMap = new HashMap<Object, IResource>();

  /** Resources in this list will be ignored at the next resource change event. */
  private final Collection<String> m_ignoreOneTimeList = new HashSet<String>();

  private final AbstractLoader m_loader;

  public AbstractLoaderResourceDeltaVisitor( final AbstractLoader loader )
  {
    m_loader = loader;
  }

  /**
   * TRICKY: manchmal hat der Pfad der Resource zwei slashes, manchmal nicht. Deswegen dieser Methode: sie ersetzt die
   * "//" mit "/" wenn es notwendig ist. Der PFad wird letztendlich als key für der Map benutzt, um zu prüfen ob eine
   * Resource schon da ist.
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
      final IResource resource = m_objectMap.get( o );

      m_resourceMap.remove( pathFor( resource ) );
      m_objectMap.remove( o );
    }
  }

  /**
   * @see org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse.core.resources.IResourceDelta)
   */
  public boolean visit( final IResourceDelta delta ) throws CoreException
  {
    final IResource resource = delta.getResource();

    if( m_ignoreOneTimeList.contains( pathFor( resource ) ) )
    {
      m_ignoreOneTimeList.remove( resource );
      return true;
    }

    final Object oldValue = m_resourceMap.get( pathFor( resource ) );
    if( oldValue != null )
    {
      switch( delta.getKind() )
      {
        case IResourceDelta.REMOVED:
          // todo: sollte eigentlich auch behandelt werden
          // aber so, dass noch die chance auf ein add besteht
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
            throw new CoreException( new Status( IStatus.ERROR, IKalypsoCoreConstants.PLUGIN_ID, 0, "Fehler beim Wiederherstellen einer Resource", e ) );
          }

          // handle changed resource
          return true;
        }
      }
    }

    return true;
  }

  /**
   * Der nächste change event dieser Resource wird ignoriert
   */
  public void ignoreResourceOneTime( final IResource resource )
  {
    m_ignoreOneTimeList.add( pathFor( resource ) );
  }
}