/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;
import org.kalypso.transformation.GeoTransformer;

/**
 * Gathers all wprof data for one profile.
 * 
 * @author Gernot Belger
 */
class ProfileData
{
  /** ObjectType -> all WProf Points of this type */
  private final ProfilePolygones m_profilePolygones = new ProfilePolygones();

  private final GeoTransformer m_transformer;

  private final URL m_photoContext;

  private final TuhhWspmProject m_project;

  private final ProfileMarkers m_markers;

  public ProfileData( final TuhhWspmProject project, final GeoTransformer transformer, final URL photoContext, final PunktattributMapping punktattribute )
  {
    m_project = project;
    m_transformer = transformer;
    m_photoContext = photoContext;
    m_markers = new ProfileMarkers( punktattribute );
  }

  public void addPoint( final IWProfPoint wprofPoint ) throws Exception
  {
    final String objectType = wprofPoint.getObjectType();

    final ProfilePolygon polygon = m_profilePolygones.getPolygon( objectType );
    polygon.add( wprofPoint );
    m_markers.addWProfPoint( wprofPoint );
  }

  public void addProfileToProject( ) throws CoreException
  {
    final ProfileCreator profileCreator = new ProfileCreator( m_profilePolygones, m_markers, m_transformer, m_photoContext );
    profileCreator.createProfile( m_project );
  }

}
