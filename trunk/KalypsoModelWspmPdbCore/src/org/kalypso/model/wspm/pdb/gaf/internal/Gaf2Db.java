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
package org.kalypso.model.wspm.pdb.gaf.internal;

import java.util.Date;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.gaf.GafProfile;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * Writes a gaf profile into the database.
 * 
 * @author Gernot Belger
 */
public class Gaf2Db implements IPdbOperation
{
  private final WaterBody m_waterBody;

  private final GeometryFactory m_geometryFactory;

  private int m_profileCount = 0;

  private final State m_state;

  private final GafProfile[] m_profiles;

  private final IProgressMonitor m_monitor;

  public Gaf2Db( final WaterBody waterBody, final State state, final GafProfile[] profiles, final int srid, final IProgressMonitor monitor )
  {
    m_waterBody = waterBody;
    m_state = state;
    m_profiles = profiles;
    m_monitor = monitor;
    m_geometryFactory = new GeometryFactory( new PrecisionModel(), srid );
  }

  @Override
  public String getLabel( )
  {
    return "Import gaf data";
  }

  @Override
  public void execute( final Session session )
  {
    m_monitor.beginTask( "Importing cross sections into database", m_profiles.length );

    addState( session, m_state );

    for( final GafProfile profile : m_profiles )
    {
      m_monitor.subTask( String.format( "converting cross section %s", profile.getStation() ) );
      commitProfile( session, profile );
      m_monitor.worked( 1 );
    }

    m_monitor.subTask( "writing data into database" );
  }

  private void addState( final Session session, final State state )
  {
    final Date now = new Date();
    state.setCreationDate( now );
    state.setEditingDate( now );

    session.save( m_state );
  }

  private void commitProfile( final Session session, final GafProfile profile )
  {
    final CrossSection crossSection = commitCrossSection( session, profile );

    /* add parts */
    final GafPart[] parts = profile.getParts();
    for( int i = 0; i < parts.length; i++ )
    {
      final GafPart gafPart = parts[i];
      final CrossSectionPart csPart = commitPart( session, crossSection, gafPart, i );
      final GafPoint[] points = gafPart.getPoints();
      for( int j = 0; j < points.length; j++ )
      {
        final GafPoint gafPoint = points[j];
        commitPoint( session, csPart, gafPoint, j );
      }
    }
  }

  private CrossSection commitCrossSection( final Session session, final GafProfile profile )
  {
    final CrossSection crossSection = new CrossSection();

    final String id = m_state.getName() + "_" + m_profileCount++;
    crossSection.setName( id );

    // TODO: what to set into comment?
    // Log entries of this station?
    // or comment of state

    crossSection.setState( m_state );
    crossSection.setWaterBody( m_waterBody );

    crossSection.setDescription( StringUtils.EMPTY );

    crossSection.setStation( profile.getStation() );

    /* Copy initial dates from state */
    crossSection.setCreationDate( m_state.getCreationDate() );
    crossSection.setEditingDate( m_state.getEditingDate() );
    crossSection.setEditingUser( m_state.getEditingUser() );
    crossSection.setMeasurementDate( m_state.getMeasurementDate() );

    final LineString line = profile.createLine();
    crossSection.setLine( line );

    session.save( crossSection );

    return crossSection;
  }

  private CrossSectionPart commitPart( final Session session, final CrossSection crossSection, final GafPart part, final int index )
  {
    final CrossSectionPart csPart = new CrossSectionPart();

    csPart.setCrossSection( crossSection );

    final String name = crossSection.getName() + "_" + index;
    csPart.setName( name );
    csPart.setCategory( part.getKind() );
    csPart.setLine( part.getLine() );

    session.save( csPart );

    return csPart;
  }

  private void commitPoint( final Session session, final CrossSectionPart csPart, final GafPoint gafPoint, final int index )
  {
    final Point point = new Point();

    point.setCrossSectionPart( csPart );

    final GafCode codeKZ = gafPoint.getCodeKZ();
    final String name = csPart.getName() + "_" + index;
    point.setName( name );

    point.setDescription( codeKZ.getDescription() );

    point.setConsecutiveNum( index );
    point.setHight( gafPoint.getHeight() );
    point.setWidth( gafPoint.getWidth() );
    point.setHyk( gafPoint.getHyk().getHyk() );
    point.setKz( codeKZ.getCode() );

    final Coordinate coordinate = gafPoint.getCoordinate();
    if( coordinate != null )
    {
      final com.vividsolutions.jts.geom.Point location = m_geometryFactory.createPoint( coordinate );
      point.setLocation( location );
    }

    point.setName( gafPoint.getPointId() );

    // TODO: implement roughness
    point.setRoughness( null );
    point.setRoughnessKstValue( null );
    point.setRoughnessKValue( null );
    point.setVegetationAx( null );
    point.setVegetationAy( null );
    point.setVegetationDp( null );
    point.setVegetation( null );

    session.save( point );
  }
}