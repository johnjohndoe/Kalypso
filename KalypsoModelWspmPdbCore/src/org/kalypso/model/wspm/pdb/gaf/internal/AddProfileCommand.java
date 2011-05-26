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
package org.kalypso.model.wspm.pdb.gaf.internal;

import org.apache.commons.lang.StringUtils;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Writes one new profile and all its parts and points into the database.
 * 
 * @author Gernot Belger
 */
public class AddProfileCommand implements IPdbOperation
{
  private final GafProfile m_profile;

  private final int m_profileCount;

  private final WaterBody m_waterBody;

  private final State m_state;

  private final GeometryFactory m_geometryFactory;

  public AddProfileCommand( final int number, final GafProfile profile, final WaterBody waterBody, final State state, final GeometryFactory geometryFactory )
  {
    m_profileCount = number;
    m_profile = profile;
    m_waterBody = waterBody;
    m_state = state;
    m_geometryFactory = geometryFactory;
  }

  @Override
  public String getLabel( )
  {
    return String.format( "Add Profile: %s", m_profile.getStation() );
  }

  @Override
  public void execute( final Session session ) throws HibernateException
  {
    // add cross section
    final CrossSection crossSection = commitCrossSection( session );

    // add parts
    final GafPart[] parts = m_profile.getParts();
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

  private CrossSection commitCrossSection( final Session session )
  {
    final CrossSection crossSection = new CrossSection();

    final String id = m_state.getName() + "_" + m_profileCount;
    crossSection.setName( id );

    // TODO: what to set into comment?
    // Log entries of this station?
    // or comment of state

    crossSection.setState( m_state );
    crossSection.setWaterBody( m_waterBody );

    crossSection.setDescription( StringUtils.EMPTY );

    crossSection.setStation( m_profile.getStation() );

    /* Copy initial dates from state */
    crossSection.setCreationDate( m_state.getCreationDate() );
    crossSection.setEditingDate( m_state.getEditingDate() );
    crossSection.setEditingUser( m_state.getEditingUser() );
    crossSection.setMeasurementDate( m_state.getMeasurementDate() );

    final LineString line = m_profile.createLine();
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

    final String name = csPart.getName() + "_" + index;
    point.setName( name );

    // TODO: maybe the warning for this line if it exists
    point.setDescription( null );

    point.setConsecutiveNum( index );
    point.setHight( gafPoint.getHeight() );
    point.setWidth( gafPoint.getWidth() );
    point.setHyk( gafPoint.getHyk().getHyk() );
    point.setKz( gafPoint.getCodeKZ().getCode() );

    final Coordinate coordinate = gafPoint.getCoordinate();
    if( coordinate != null )
    {
      final com.vividsolutions.jts.geom.Point location = m_geometryFactory.createPoint( coordinate );
      point.setLocation( location );
    }
    else
    {
      System.out.println( "xxx" );
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