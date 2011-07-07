/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.gaf;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.wspm.pdb.internal.gaf.GafLine;
import org.kalypso.model.wspm.pdb.internal.gaf.GafPoint;
import org.kalypso.transformation.transformer.JTSTransformer;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Assembles all read points into different profiles.
 * 
 * @author Gernot Belger
 */
public class GafProfiles
{
  private final Set<BigDecimal> m_committedStations = new HashSet<BigDecimal>();

  private final Collection<GafProfile> m_profiles = new ArrayList<GafProfile>();

  private GafProfile m_currentProfile;

  private final GeometryFactory m_geometryFactory;

  private final GafPointCheck m_pointChecker;

  private final JTSTransformer m_transformer;

  private final LineString m_riverline;

  public GafProfiles( final GafPointCheck checker, final JTSTransformer jtsTransformer, final LineString riverline )
  {
    m_geometryFactory = riverline.getFactory();
    m_transformer = jtsTransformer;
    m_pointChecker = checker;
    m_riverline = riverline;
  }

  public void addPoint( final GafPoint point )
  {
    final BigDecimal station = point.getStation();

    final boolean duplicateStation = m_committedStations.contains( station );

    if( m_currentProfile != null && !station.equals( m_currentProfile.getStation() ) )
      commitProfile();

    if( m_currentProfile == null )
      createProfile( station );

    final GafCode code = point.getCode();
    m_currentProfile.addPoint( point, code );
    if( duplicateStation )
    {
      final String message = String.format( "duplicate station: %s", station );
      m_currentProfile.addStatus( IStatus.WARNING, message );
    }
  }

  public void stop( )
  {
    commitProfile();
  }

  private void createProfile( final BigDecimal station )
  {
    m_currentProfile = new GafProfile( station, m_geometryFactory );
  }

  private void commitProfile( )
  {
    if( m_currentProfile == null )
      return;

    final BigDecimal station = m_currentProfile.getStation();

    m_committedStations.add( station );

    m_currentProfile.check( m_riverline );
    m_profiles.add( m_currentProfile );

    m_currentProfile = null;
  }

  public GafProfile[] getProfiles( )
  {
    return m_profiles.toArray( new GafProfile[m_profiles.size()] );
  }

  public void addLines( final GafLine[] lines )
  {
    for( final GafLine line : lines )
    {
      if( line.getStatus().isOK() )
      {
        final GafPoint point = new GafPoint( line, m_pointChecker, m_geometryFactory, m_transformer );
        addPoint( point );
      }
    }
    stop();
  }
}