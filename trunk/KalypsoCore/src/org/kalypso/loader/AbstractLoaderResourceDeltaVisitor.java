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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.core.IKalypsoCoreConstants;
import org.kalypso.core.i18n.Messages;

/**
 * <pre>
 *           Changes:
 *           2004-11-09 - schlienger - uses now the path of the resource as key in the map
 *           						     Path should be taken using the pathFor( IResource ) method
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

  private final AbstractLoader m_loader;

  public AbstractLoaderResourceDeltaVisitor( final AbstractLoader loader )
  {
    m_loader = loader;
  }

  /**
   * TRICKY: manchmal hat der Pfad der Resource zwei slashes, manchmal nicht. Deswegen dieser Methode: sie ersetzt die
   * "//" mit "/" wenn es notwendig ist. Der Pfad wird letztendlich als key für der Map benutzt, um zu prüfen ob eine
   * Resource schon da ist.
   * 
   * @param resource
   * @return Pfad (ohne eventuelle "//")
   */
  private String pathFor( final IResource resource )
  {
    String path = resource.toString();

    // blöder HACK weil sonst werden die Resourcen nicht richtig verglichen
    if( path.indexOf( "//" ) != -1 ) //$NON-NLS-1$
      path = path.replaceAll( "//", "/" ); //$NON-NLS-1$ //$NON-NLS-2$

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

      // HACK: The m_object map has the object as key.
      // A shape adds more than one resource with the same object (workspace), so only the last is stored.
      // The m_resourceMap however has the resources as key and the workspace as value.
      // This means all resources are stored.
      // This leads to a bug, where only the last (in case of the shape it is the .shx file) will be removed.
      // The other resources will remain in the map.
      // This solves it, but is an ugly hack.
      final String pathFor = pathFor( resource );
      if( pathFor.endsWith( ".shx" ) ) //$NON-NLS-1$
      {
        final String path = pathFor.substring( 0, pathFor.length() - 4 );
        final String shpPath = path + ".shp"; //$NON-NLS-1$
        final String dbfPath = path + ".dbf"; //$NON-NLS-1$

        m_resourceMap.remove( shpPath );
        m_resourceMap.remove( dbfPath );
      }

      m_objectMap.remove( o );
    }
  }

  /**
   * @see org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse.core.resources.IResourceDelta)
   */
  public boolean visit( final IResourceDelta delta ) throws CoreException
  {
    final IResource resource = delta.getResource();

    final Object oldValue = m_resourceMap.get( pathFor( resource ) );
    if( oldValue != null )
    {
      final int flags = delta.getFlags();

      if( (flags & IResourceDelta.MARKERS) != 0 )
      {
        final IMarkerDelta[] markerDeltas = delta.getMarkerDeltas();
        for( final IMarkerDelta delta2 : markerDeltas )
        {
          if( delta2.getType().equals( IKalypsoCoreConstants.RESOURCE_LOCK_MARKER_TYPE ) && (delta2.getKind() & IResourceDelta.ADDED) != 0 )
            m_loader.lockEvents( oldValue, true );
        }
      }

      try
      {
        if( (flags & (IResourceDelta.CONTENT | IResourceDelta.ENCODING)) != 0 )
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
              m_loader.fireLoaderObjectInvalid( oldValue, delta.getKind() == IResourceDelta.REMOVED );
            }
          }
        }
      }
      catch( final Exception e )
      {
        throw new CoreException( new Status( IStatus.ERROR, IKalypsoCoreConstants.PLUGIN_ID, 0, Messages.getString("org.kalypso.loader.AbstractLoaderResourceDeltaVisitor.6"), e ) ); //$NON-NLS-1$
      }
      finally
      {
        if( (flags & IResourceDelta.MARKERS) != 0 )
        {
          final IMarkerDelta[] markerDeltas = delta.getMarkerDeltas();
          for( final IMarkerDelta delta2 : markerDeltas )
          {
            if( delta2.getType().equals( IKalypsoCoreConstants.RESOURCE_LOCK_MARKER_TYPE ) && (delta2.getKind() & IResourceDelta.REMOVED) != 0 )
              m_loader.lockEvents( oldValue, false );
          }
        }
      }
    }

    return true;
  }

  /**
   * TRICKY: this method normalizes the path in order to make comparisaon robust.
   * <p>
   * This is maybe obsolete, because the bad pathes (e.g. those beginning with '//' where made by
   * {@link org.kalypso.contribs.eclipse.core.resources.ResourceUtilities#createURL(IResource)}, which is fixed now.
   */
  public String pathForObject( final Object data )
  {
    final IResource resource = m_objectMap.get( data );
    return resource == null ? null : pathFor( resource );
  }
}