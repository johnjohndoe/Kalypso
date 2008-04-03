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
package org.kalypso.model.wspm.ui.profil.validation;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.ide.IDE;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.Messages;

final public class ResourceValidatorMarkerCollector implements IValidatorMarkerCollector
{
  private final IResource m_resource;

  private final static String[] USED_ATTRIBUTES = new String[] { IMarker.MESSAGE, IMarker.LOCATION, IMarker.SEVERITY, IMarker.TRANSIENT, IDE.EDITOR_ID_ATTR,
      IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPOS, IValidatorMarkerCollector.MARKER_ATTRIBUTE_POINTPROPERTY, IValidatorMarkerCollector.MARKER_ATTRIBUTE_QUICK_FIX_PLUGINID,
      IValidatorMarkerCollector.MARKER_ATTRIBUTE_QUICK_FIX_RESOLUTIONS, IValidatorMarkerCollector.MARKER_ATTRIBUTE_PROFILE_ID, IValidatorMarkerCollector.MARKER_ATTRIBUTE_STATION };

  private final String m_editorID;

  private final List<IMarker> m_markers = new ArrayList<IMarker>();

  private final IProfil m_profile;

  private final String m_profileFeatureID;

  public ResourceValidatorMarkerCollector( final IResource resource, final String editorID, final IProfil profile, String profileID )
  {
    m_resource = resource;
    m_editorID = editorID;
    m_profile = profile;
    m_profileFeatureID = profileID;
  }

  /**
   * Creates a (profile-)marker on the given resource. All validation rules should use this method, so changes in the
   * implementation (e.g. the type of the marker) are reflected on all rules.
   * 
   * @throws CoreException
   */
  public void createProfilMarker( final int severity, final String message, final String location, final int pointPos, final String pointProperty, final String resolutionPluginId, final IProfilMarkerResolution resolutionMarker ) throws CoreException
  {
    if( "true".equals( Platform.getDebugOption( KalypsoModelWspmUIPlugin.ID + "/debug/validationMarkers" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
    {
      final String debugMsg = String.format( Messages.ResourceValidatorMarkerCollector_2, severity, message, location, pointPos );
      System.out.println( debugMsg );
    }
    final IMarker marker = m_resource.createMarker( KalypsoModelWspmUIPlugin.MARKER_ID );

    final String station = String.format( "%.4f", m_profile.getStation() ); //$NON-NLS-1$

    final Object[] values = new Object[] { message, location, severity, true, m_editorID, pointPos, pointProperty, resolutionPluginId,
        resolutionMarker == null ? null : resolutionMarker.getSerializedParameter(), m_profileFeatureID, station };

    marker.setAttributes( USED_ATTRIBUTES, values );

    m_markers.add( marker );
  }

  public void reset( ) throws CoreException
  {
    m_resource.deleteMarkers( KalypsoModelWspmUIPlugin.MARKER_ID, true, IResource.DEPTH_ZERO );
  }

  public void reset( final String profilFeatureID ) throws CoreException
  {
    final IMarker[] markers = m_resource.findMarkers( KalypsoModelWspmUIPlugin.MARKER_ID, true, IResource.DEPTH_ZERO );
    final ArrayList<IMarker> toDelete = new ArrayList<IMarker>();
    for( int i = 0; i < markers.length; i++ )
    {
      if( markers[i].getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_PROFILE_ID ).equals( profilFeatureID ) )
        toDelete.add( markers[i] );
    }

    if( !toDelete.isEmpty() )
    {
      for( final IMarker marker : toDelete )
        marker.delete();
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector#getMarkers()
   */
  public IMarker[] getMarkers( )
  {
    return m_markers.toArray( new IMarker[m_markers.size()] );
  }

}