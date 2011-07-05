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
package org.kalypso.model.wspm.pdb.internal.gaf;

import java.util.Date;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.gaf.GafProfile;
import org.kalypso.model.wspm.pdb.gaf.GafProfiles;
import org.kalypso.model.wspm.pdb.internal.utils.PDBNameGenerator;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Writes a gaf profile into the database.
 * 
 * @author Gernot Belger
 */
public class Gaf2Db implements IPdbOperation
{
  private final WaterBody m_waterBody;

  private final State m_state;

  private final GafProfiles m_profiles;

  private final IProgressMonitor m_monitor;

  private final String m_dbType;

  private final Coefficients m_coefficients;

  private final PDBNameGenerator m_sectionNameGenerator = new PDBNameGenerator();

  public Gaf2Db( final String dbType, final WaterBody waterBody, final State state, final GafProfiles profiles, final Coefficients coefficients, final IProgressMonitor monitor )
  {
    m_dbType = dbType;
    m_waterBody = waterBody;
    m_state = state;
    m_profiles = profiles;
    m_coefficients = coefficients;
    m_monitor = monitor;
  }

  @Override
  public String getLabel( )
  {
    return "Import gaf data";
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    try
    {
      final GafProfile[] profiles = m_profiles.getProfiles();
      m_monitor.beginTask( "Importing cross sections into database", profiles.length );

      addState( session, m_state );

      for( final GafProfile profile : profiles )
      {
        m_monitor.subTask( String.format( "converting cross section %s", profile.getStation() ) );
        commitProfile( session, m_dbType, profile );
        m_monitor.worked( 1 );
      }

      m_monitor.subTask( "writing data into database" );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to write data into database", e );
    }
  }

  public static void addState( final Session session, final State state )
  {
    final Date now = new Date();
    state.setCreationDate( now );
    state.setEditingDate( now );
    session.save( state );
  }

  private void commitProfile( final Session session, final String dbType, final GafProfile profile ) throws Exception
  {
    final CrossSection crossSection = commitCrossSection( session, dbType, profile );

    /* add parts */
    final GafPart[] parts = profile.getParts();
    for( final GafPart gafPart : parts )
    {
      final PDBNameGenerator partNameGenerator = new PDBNameGenerator();
      final CrossSectionPart csPart = commitPart( session, dbType, crossSection, gafPart, partNameGenerator );
      if( csPart == null )
        continue;

      final GafPoint[] points = gafPart.getPoints();
      for( int j = 0; j < points.length; j++ )
      {
        final PDBNameGenerator pointNameGenerator = new PDBNameGenerator();
        final GafPoint gafPoint = points[j];
        commitPoint( session, csPart, gafPoint, j, pointNameGenerator );
      }
    }
  }

  private CrossSection commitCrossSection( final Session session, final String dbType, final GafProfile profile ) throws Exception
  {
    final CrossSection crossSection = new CrossSection();

    final String protoName = String.format( "%s", profile.getStation() ); //$NON-NLS-1$

    final String name = m_sectionNameGenerator.createUniqueName( protoName );
    crossSection.setName( name );

    crossSection.setState( m_state );
    crossSection.setWaterBody( m_waterBody );

    crossSection.setDescription( StringUtils.EMPTY );

    crossSection.setStation( profile.getStation() );

    /* Copy initial dates from state */
    crossSection.setCreationDate( m_state.getCreationDate() );
    crossSection.setEditingDate( m_state.getEditingDate() );
    crossSection.setEditingUser( m_state.getEditingUser() );
    crossSection.setMeasurementDate( m_state.getMeasurementDate() );

    final Geometry line = profile.createLine( dbType );
    crossSection.setLine( line );

    session.save( crossSection );

    return crossSection;
  }

  private CrossSectionPart commitPart( final Session session, final String dbType, final CrossSection crossSection, final GafPart part, final PDBNameGenerator nameGenerator ) throws Exception
  {
    final CrossSectionPart csPart = new CrossSectionPart();

    final String partKind = part.getKind();

    final String name = nameGenerator.createUniqueName( partKind );

    csPart.setName( name );
    csPart.setDescription( StringUtils.EMPTY );
    csPart.setCategory( partKind );
    final Geometry line = part.getLine( dbType );
    csPart.setLine( line );

    if( line == null )
      return null;

    csPart.setCrossSection( crossSection );
    session.save( csPart );
    return csPart;
  }

  private void commitPoint( final Session session, final CrossSectionPart csPart, final GafPoint gafPoint, final int index, final PDBNameGenerator nameGenerator ) throws Exception
  {
    final Point point = new Point();

    point.setCrossSectionPart( csPart );

    final String readCode = gafPoint.getCode();
    final String realCode = m_profiles.translateCode( readCode );

    final String readHyk = gafPoint.getHyk();
    final String realHyk = m_profiles.translateHyk( readHyk );

    /* Using pointID from gaf, but force it to be unique within each part */
    final String name = nameGenerator.createUniqueName( gafPoint.getPointId() );
    point.setName( name );

    point.setDescription( StringUtils.EMPTY );

    point.setConsecutiveNum( index );
    point.setHeight( gafPoint.getHeight() );
    point.setWidth( gafPoint.getWidth() );
    point.setHyk( realHyk );
    point.setCode( realCode );

    point.setLocation( gafPoint.getPoint() );

    final Roughness roughness = m_coefficients.getRoughnessOrUnknown( gafPoint.getRoughnessClass() );
    point.setRoughness( roughness );
    point.setRoughnessKstValue( roughness.getKstValue() );
    point.setRoughnessKValue( roughness.getKValue() );

    final Vegetation vegetation = m_coefficients.getVegetationOrUnknown( gafPoint.getVegetationClass() );
    point.setVegetation( vegetation );
    point.setVegetationAx( vegetation.getAx() );
    point.setVegetationAy( vegetation.getAy() );
    point.setVegetationDp( vegetation.getDp() );

    session.save( point );
  }
}