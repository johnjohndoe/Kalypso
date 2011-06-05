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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.hibernate.Session;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.gaf.Coefficients;
import org.kalypso.model.wspm.pdb.internal.gaf.Gaf2Db;
import org.kalypso.model.wspm.pdb.internal.gaf.GafCode;
import org.kalypso.model.wspm.pdb.internal.gaf.GafCodes;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * @author Gernot Belger
 */
public class CheckinStatePdbOperation implements IPdbOperation
{
  private static final String STR_FAILED_TO_CONVERT_GEOMETRY = "Failed to convert geometry";

  private static final BigDecimal VEGETATION_0 = new BigDecimal( 0 );

  private final Map<String, WaterBody> m_waterBodies = new HashMap<String, WaterBody>();

  private final IProgressMonitor m_monitor;

  private final State m_state;

  private final IProfileFeature[] m_profiles;

  private final IGeoTransformer m_transformer;

  private final int m_crosssectionCount = 0;

  private final Coefficients m_coefficients;

  private final GafCodes m_gafCodes;

  private final GeometryFactory m_geometryFactory;

  /**
   * @param dbSrs
   *          The coordinate system of the database
   */
  public CheckinStatePdbOperation( final GafCodes gafCodes, final Coefficients coefficients, final WaterBody[] waterBodies, final State state, final IProfileFeature[] profiles, final String dbSrs, final IProgressMonitor monitor )
  {
    m_gafCodes = gafCodes;
    m_coefficients = coefficients;
    m_state = state;
    m_profiles = profiles;

    for( final WaterBody waterBody : waterBodies )
      m_waterBodies.put( waterBody.getName(), waterBody );

    m_monitor = monitor;

    final int srid = JTSAdapter.toSrid( dbSrs );
    m_geometryFactory = new GeometryFactory( new PrecisionModel(), srid );

    m_transformer = GeoTransformerFactory.getGeoTransformer( dbSrs );
  }

  @Override
  public String getLabel( )
  {
    return "Upload cross sections into database";
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    m_monitor.beginTask( "Uploading new state into database", 10 + m_profiles.length );

    m_monitor.subTask( "saving state..." );
    Gaf2Db.addState( session, m_state );
    m_monitor.worked( 10 );

    for( final IProfileFeature feature : m_profiles )
    {
      final String label = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
      m_monitor.subTask( String.format( "saving profile '%s'...", label ) );
      uploadProfile( session, feature );
      m_monitor.worked( 1 );
    }
  }

  private void uploadProfile( final Session session, final IProfileFeature feature ) throws PdbConnectException
  {
    final IProfil profil = feature.getProfil();

    final WaterBody waterBody = findWaterBody( feature );

    final CrossSection section = new CrossSection();

    /* db relevant data */
    section.setWaterBody( waterBody );
    section.setState( m_state );
    section.setCreationDate( m_state.getCreationDate() );
    section.setEditingDate( m_state.getEditingDate() );
    section.setEditingUser( m_state.getEditingUser() );
    // TODO: should we reset measurement date?
    section.setMeasurementDate( m_state.getMeasurementDate() );

    /* Data from profile */
    final BigDecimal station = getStation( feature );
    section.setStation( station );
    // FIXME: eigentlich sollte der name erhalten bleiben
    section.setName( m_state.getName() + "_" + profil.getName() + "_" + m_crosssectionCount );
    section.setDescription( profil.getComment() );

    final String srsName = feature.getSrsName();
    final CrossSectionPart pPart = createParts( section, profil, srsName );
    if( pPart != null )
      section.setLine( pPart.getLine() );

    saveSection( session, section );
  }

  private void saveSection( final Session session, final CrossSection section )
  {
    session.save( section );

    final Set<CrossSectionPart> parts = section.getCrossSectionParts();
    for( final CrossSectionPart part : parts )
    {
      session.save( part );

      final Set<Point> points = part.getPoints();
      for( final Point point : points )
        session.save( point );
    }
  }

  private BigDecimal getStation( final IProfileFeature feature )
  {
    final BigDecimal station = feature.getBigStation();
    if( station == null )
      return null;

    return station.movePointRight( 4 );
  }

  private WaterBody findWaterBody( final IProfileFeature feature )
  {
    final WspmWaterBody wspmWaterBody = feature.getWater();
    final String refNr = wspmWaterBody.getRefNr();
    return m_waterBodies.get( refNr );
  }

  protected LineString toLine( final GM_Curve curve ) throws PdbConnectException
  {
    try
    {
      final GM_Object transformedCurve = m_transformer.transform( curve );
      return (LineString) JTSAdapter.export( transformedCurve );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( STR_FAILED_TO_CONVERT_GEOMETRY, e );
    }
  }

  protected com.vividsolutions.jts.geom.Point toPoint( final GM_Point point ) throws PdbConnectException
  {
    try
    {
      final GM_Object transformedCurve = m_transformer.transform( point );
      return (com.vividsolutions.jts.geom.Point) JTSAdapter.export( transformedCurve );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( STR_FAILED_TO_CONVERT_GEOMETRY, e );
    }
  }

  private CrossSectionPart createParts( final CrossSection section, final IProfil profil, final String profilSRS ) throws PdbConnectException
  {
    final Set<CrossSectionPart> parts = new HashSet<CrossSectionPart>();

    /* Extract profile line */
    final CrossSectionPart pPart = createProfilePart( profil, profilSRS );
    if( pPart != null )
      parts.add( pPart );
    // section.setLine( toLine( feature ) );

    /* Extract building parts */
    final CrossSectionPart[] buildingParts = createBuildingParts( profil );
    for( final CrossSectionPart buildingPart : buildingParts )
      parts.add( buildingPart );

    /* extract extra parts */
    final CrossSectionPart[] additionalParts = createAdditionalParts( profil );
    for( final CrossSectionPart additionalPart : additionalParts )
      parts.add( additionalPart );

    int partCount = 0;
    for( final CrossSectionPart part : parts )
    {
      part.setName( section.getName() + "_" + partCount++ );
      part.setCrossSection( section );
      // FIXME: no directly, add
      section.getCrossSectionParts().add( part );
    }

    return pPart;
  }

  private CrossSectionPart createProfilePart( final IProfil profil, final String profilSRS ) throws PdbConnectException
  {
    final CrossSectionPart part = new CrossSectionPart();
    part.setCategory( IGafConstants.KZ_CATEGORY_PROFILE );

    final IRecord[] records = profil.getPoints();
    final List<Coordinate> lineCrds = new ArrayList<Coordinate>(records.length);
    for( int i = 0; i < records.length; i++ )
    {
      final IRecord record = records[i];

      final String name = getStringValue( record, IWspmConstants.POINT_PROPERTY_ID, StringUtils.EMPTY );
      final String comment = getStringValue( record, IWspmConstants.POINT_PROPERTY_COMMENT, StringUtils.EMPTY );
      final BigDecimal width = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_BREITE, null );
      final BigDecimal height = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_HOEHE, null );
      final String code = getStringValue( record, IWspmConstants.POINT_PROPERTY_COMMENT, IGafConstants.CODE_PP );
      final String hyk = toHyk( code );

      final GM_Point curve = WspmGeometryUtilities.createLocation( profil, record, profilSRS );
      final com.vividsolutions.jts.geom.Point location = toPoint( curve );

      // FIXME: use real roughness
      final Roughness roughness = m_coefficients.getRoughnessOrUnknown( null );
      final BigDecimal kstValue = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST, null );
      final BigDecimal kValue = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_RAUHEIT_KS, null );

      // FIXME: use real vegetation
      final Vegetation vegetation = m_coefficients.getVegetationOrUnknown( null );
      final BigDecimal axValue = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, VEGETATION_0 );
      final BigDecimal ayValue = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, VEGETATION_0 );
      final BigDecimal dpValue = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, VEGETATION_0 );

      /* Not all point may pass */
      // TODO: allow point wo geometry?
      if( height == null )
        continue;

      final Point point = new Point( null, part, name, i );

      point.setDescription( comment );
      point.setWidth( width );
      point.setHeight( height );
      point.setCode( code );
      point.setHyk( hyk );
      point.setLocation( location );

      point.setRoughness( roughness );
      point.setRoughnessKstValue( kstValue );
      point.setRoughnessKValue( kValue );

      point.setVegetation( vegetation );
      point.setVegetationAx( axValue );
      point.setVegetationAy( ayValue );
      point.setVegetationDp( dpValue );

      if( location != null )
        lineCrds.add( location.getCoordinate() );

      part.getPoints().add( point );
    }


    final LineString line = m_geometryFactory.createLineString( lineCrds.toArray( new Coordinate[lineCrds.size()] ) );
    part.setLine( line );

    return part;
  }

  private String toHyk( final String code )
  {
    /* Just check if it is an existing code */
    final GafCode hykCode = m_gafCodes.getHykCode( code );
    if( hykCode == null )
      return null;

    /* The hyk is always the same as the code itself */
    return code;
  }

  private String getStringValue( final IRecord record, final String componentId, final String defaultValue )
  {
    final TupleResult owner = record.getOwner();
    final int index = owner.indexOfComponent( componentId );
    if( index == -1 )
      return defaultValue;

    final Object value = record.getValue( index );
    if( value == null )
      return defaultValue;

    return value.toString();
  }

  private BigDecimal getDecimalValue( final IRecord record, final String componentId, final BigDecimal defaultValue )
  {
    final TupleResult owner = record.getOwner();
    final int index = owner.indexOfComponent( componentId );
    if( index == -1 )
      return defaultValue;

    final Object value = record.getValue( index );
    if( value == null )
      return defaultValue;

    if( value instanceof BigDecimal )
      return (BigDecimal) value;

    if( value instanceof Number )
      return new BigDecimal( ((Number) value).doubleValue() );

    return defaultValue;
  }

  private CrossSectionPart[] createBuildingParts( final IProfil profil )
  {
    // TODO
    return new CrossSectionPart[0];
  }

  private CrossSectionPart[] createAdditionalParts( final IProfil profil )
  {
    // TODO
    return new CrossSectionPart[0];
  }
}