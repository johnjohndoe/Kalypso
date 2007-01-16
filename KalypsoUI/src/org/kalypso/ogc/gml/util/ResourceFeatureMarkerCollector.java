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
package org.kalypso.ogc.gml.util;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.ide.IDE;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.validation.IFeatureMarkerCollector;

public class ResourceFeatureMarkerCollector implements IFeatureMarkerCollector
{
  private final IResource m_resource;

  private final static String[] USED_ATTRIBUTES = new String[] { IMarker.MESSAGE, IMarker.LOCATION, IMarker.SEVERITY, IMarker.TRANSIENT, IDE.EDITOR_ID_ATTR };

  private final String m_editorID;

  // private XStream m_xstream;

  public ResourceFeatureMarkerCollector( final IResource resource, final String editorID )
  {
    m_resource = resource;
    m_editorID = editorID;

    // m_xstream = new XStream( new DomDriver() );
  }

  /**
   * Creates a (profile-)marker on the given resource. All validation rules should use this method, so changes in the
   * implementation (e.g. the type of the marker) are reflekted on all rules.
   * 
   * @throws CoreException
   */
  public void createMarker( final Feature feature, final IPropertyType property, final boolean isSevere, final String message, final String location, final String resolutionPluginId, final Object[] resolutionMarkers ) throws CoreException
  {
    final String debugMsg = String.format( "Creating resource marker: featureID=%s, property=%s, isSever=%b, message=%s, location=%s", feature.getId(), property == null ? "null"
        : property.getQName().toString(), isSevere, message, location );
    System.out.println( debugMsg );

    // if( "true".equals( Platform.getDebugOption( KalypsoModelWspmUIPlugin.ID + "/debug/validationMarkers" ) ) )
    // {
    // final String debugMsg = String.format( "Creating resource marker: isSever=%b, message=%s, location=%s,
    // pointPos=%d", isSevere, message, location, pointPos );
    // System.out.println( debugMsg );
    // }
    //    
    // final String resSerialised = m_xstream.toXML( resolutionMarkers );

    if( m_resource != null )
    {
      final IMarker marker = m_resource.createMarker( IMarker.PROBLEM );

      final Object[] values = new Object[] { message, location, isSevere ? IMarker.SEVERITY_ERROR : IMarker.SEVERITY_WARNING, true, m_editorID };

      marker.setAttributes( USED_ATTRIBUTES, values );
    }
  }

  public void reset( ) throws CoreException
  {
    if( m_resource != null )
      m_resource.deleteMarkers( IMarker.PROBLEM, true, IResource.DEPTH_ZERO );
  }
}