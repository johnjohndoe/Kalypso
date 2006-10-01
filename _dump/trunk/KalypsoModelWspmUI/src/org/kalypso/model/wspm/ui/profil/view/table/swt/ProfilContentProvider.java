/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.ui.profil.view.table.swt;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;

/**
 * @author Gernot Belger
 */
public class ProfilContentProvider implements IStructuredContentProvider, IResourceChangeListener
{
  private Viewer m_viewer;

  private Map<IProfilPoint, Collection<IMarker>> m_markerIndex = new HashMap<IProfilPoint, Collection<IMarker>>();

  private Map<IProfilPoint, Collection<IMarker>> m_unmodmarkerIndex = Collections.unmodifiableMap( m_markerIndex );

  private IProfilEventManager m_pem;

  private final IFile m_file;

  public ProfilContentProvider( final IFile file )
  {
    m_file = file;
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    workspace.addResourceChangeListener( this, IResourceChangeEvent.POST_CHANGE );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    workspace.removeResourceChangeListener( this );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_viewer = viewer;
    m_pem = (IProfilEventManager) newInput;

    indexMarkers();
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    final IProfilEventManager manager = (IProfilEventManager) inputElement;
    if( manager == null )
      return new String[] { "<kein Profil zugeordnet>" };

    final IProfil profil = manager.getProfil();
    final List<IProfilPoint> points = profil.getPoints();
    return points.toArray( new IProfilPoint[points.size()] );
  }

  public void resourceChanged( final IResourceChangeEvent event )
  {
    if( m_viewer == null )
      return;

    final IMarkerDelta[] deltas = event.findMarkerDeltas( KalypsoModelWspmUIPlugin.MARKER_ID, true );
    boolean refresh = false;
    for( final IMarkerDelta delta : deltas )
    {
      if( delta.getResource().equals( m_file ) )
      {
        refresh = true;
        break;
      }
    }

    if( refresh )
    {
      indexMarkers();

      ViewerUtilities.refresh( m_viewer, true );
    }
  }

  private void indexMarkers( )
  {
    try
    {
      m_markerIndex.clear();

      if( m_pem == null || m_file == null )
        return;

      final IMarker[] markers = m_file.findMarkers( KalypsoModelWspmUIPlugin.MARKER_ID, true, IResource.DEPTH_ZERO );
      for( final IMarker marker : markers )
      {
        final Integer pointPos = (Integer) marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPOS );
        final IProfilPoint point = m_pem.getProfil().getPoints().get( pointPos );

        if( m_markerIndex.containsKey( pointPos ) )
        {
          final Collection<IMarker> list = m_markerIndex.get( marker );
          list.add( marker );
        }
        else
        {
          final ArrayList<IMarker> markerList = new ArrayList<IMarker>();
          markerList.add( marker );
          m_markerIndex.put( point, markerList );
        }
      }
    }
    catch( final IndexOutOfBoundsException ioobe )
    {
      ioobe.printStackTrace();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }

  public Map<IProfilPoint, Collection<IMarker>> getMarkerIndex( )
  {
    return m_unmodmarkerIndex;
  }
}
