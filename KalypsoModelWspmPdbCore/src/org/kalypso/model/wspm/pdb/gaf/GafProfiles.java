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
package org.kalypso.model.wspm.pdb.gaf;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.wspm.pdb.internal.gaf.Coefficients;
import org.kalypso.model.wspm.pdb.internal.gaf.GafCode;
import org.kalypso.model.wspm.pdb.internal.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.internal.gaf.GafLogger;
import org.kalypso.model.wspm.pdb.internal.gaf.GafPoint;
import org.kalypso.model.wspm.pdb.internal.gaf.GafReader;

import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Assembles all read points into different profiles.
 * 
 * @author Gernot Belger
 */
public class GafProfiles
{
  private final GafCodes m_gafCodes;

  private final Set<BigDecimal> m_committedStations = new HashSet<BigDecimal>();

  private final Collection<GafProfile> m_profiles = new ArrayList<GafProfile>();

  private GafProfile m_currentProfile;

  private final GafLogger m_logger;

  private final GeometryFactory m_geometryFactory;

  private final GafPointCheck m_pointChecker;

  public GafProfiles( final GafLogger logger, final GafCodes gafCodes, final Coefficients coefficients, final GeometryFactory geometryFactory )
  {
    m_logger = logger;
    m_gafCodes = gafCodes;
    m_geometryFactory = geometryFactory;
    m_pointChecker = new GafPointCheck( gafCodes, coefficients, logger );
  }

  public void addPoint( final GafPoint point )
  {
    final BigDecimal station = point.getStation();

    m_pointChecker.check( point );

    if( m_committedStations.contains( station ) )
    {
      final String message = String.format( "duplicate station: %s", station );
      m_logger.log( IStatus.WARNING, message );
    }

    if( m_currentProfile != null && !station.equals( m_currentProfile.getStation() ) )
      commitProfile();

    if( m_currentProfile == null )
      createProfile( station );

    final String code = point.getCode();
    // FIXME: we need to real code here in order to put the point into the right part...
    // i.e. we need to rework this in order to allow the user to define other codes here
    final GafCode gafCode = m_gafCodes.getCode( code );
    if( gafCode == null )
      throw new GafReader.SkipLineException( IStatus.WARNING, "Skipping point with unknwon code" );
    m_currentProfile.addPoint( point, gafCode );
  }

  public void stop( )
  {
    commitProfile();
  }

  private void createProfile( final BigDecimal station )
  {
    m_currentProfile = new GafProfile( station, m_logger, m_geometryFactory );
  }

  private void commitProfile( )
  {
    if( m_currentProfile == null )
      return;

    final BigDecimal station = m_currentProfile.getStation();

    m_committedStations.add( station );

    m_profiles.add( m_currentProfile );

    m_currentProfile = null;
  }

  public GafProfile[] getProfiles( )
  {
    return m_profiles.toArray( new GafProfile[m_profiles.size()] );
  }

  public String translateCode( final String code )
  {
    // TODO Auto-generated method stub
    return code;
  }

  public String translateHyk( final String hyk )
  {
    final GafCode hykCode = m_gafCodes.getHykCode( hyk );
    if( hykCode != null )
      return hykCode.getHyk();

    return null;
  }
}