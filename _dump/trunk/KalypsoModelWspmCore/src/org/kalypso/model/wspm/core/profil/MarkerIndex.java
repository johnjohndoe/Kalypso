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
package org.kalypso.model.wspm.core.profil;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.observation.result.IRecord;

/**
 * A helper class that helps indexing the problem markers of a profile by certain categories (by IRecord, ...).
 * 
 * @author Gernot Belger
 * @author kimwerner
 */
public class MarkerIndex
{
  private final Map<IRecord, Collection<IMarker>> m_recordIndex = new HashMap<IRecord, Collection<IMarker>>();

  private final Map<Integer, Collection<IMarker>> m_severityIndex = new HashMap<Integer, Collection<IMarker>>();

  private final IMarker[] m_markers;

  public MarkerIndex( final IProfil profil, final IMarker[] markers )
  {
    m_markers = markers;

    try
    {
      for( final IMarker marker : markers )
      {
        final Integer pointPos = (Integer) marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPOS );
        if( pointPos == -1 )
        {

        }
        else if( pointPos < profil.getResult().size() )
        {
          final IRecord record = profil.getResult().get( pointPos );
          if( !m_recordIndex.containsKey( record ) )
            m_recordIndex.put( record, new ArrayList<IMarker>() );

          final Collection<IMarker> recordMarkers = m_recordIndex.get( record );
          recordMarkers.add( marker );
        }

        final int severity = marker.getAttribute( IMarker.SEVERITY, IMarker.SEVERITY_INFO );
        if( !m_severityIndex.containsKey( severity ) )
          m_severityIndex.put( severity, new ArrayList<IMarker>() );
        final Collection<IMarker> severityMarkers = m_severityIndex.get( severity );
        severityMarkers.add( marker );

        // TODO: Can't: must be moved to ui or tracing option moved here...
        // if( "true".equals( Platform.getDebugOption( KalypsoModelWspmUIPlugin.ID + "/debug/validationMarkers/table" )
        // )
        // )
        // {
        // final String message = marker.getAttribute( IMarker.MESSAGE, null );
        //
        // final String debugMsg = String.format( "Found resource marker: message=%s, pointPos=%d", message, pointPos );
        // System.out.println( debugMsg );
        // }
      }
    }
    catch( final CoreException e )
    {
      KalypsoModelWspmCorePlugin.getDefault().getLog().log( e.getStatus() );
    }
  }

  /**
   * Returns all markers contained in this index
   */
  public IMarker[] getMarkers( )
  {
    return m_markers;
  }

  /**
   * Return all markers associated to the given record.
   */
  public IMarker[] get( final IRecord record )
  {
    final Collection<IMarker> markers = m_recordIndex.get( record );
    if( markers == null )
      return new IMarker[] {};

    return markers.toArray( new IMarker[markers.size()] );
  }

  final public boolean hasMarkers( )
  {
    return m_markers.length > 0;
  }

  /**
   * Return all markers associated to the given severity.
   */
  public IMarker[] get( final int severity )
  {
    final Collection<IMarker> markers = m_severityIndex.get( severity );
    if( markers == null )
      return new IMarker[] {};

    return markers.toArray( new IMarker[markers.size()] );
  }
}
