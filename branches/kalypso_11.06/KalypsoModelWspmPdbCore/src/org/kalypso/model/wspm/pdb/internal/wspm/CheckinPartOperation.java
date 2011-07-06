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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.gaf.GafCode;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.gaf.Coefficients;
import org.kalypso.model.wspm.pdb.internal.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.internal.utils.PDBNameGenerator;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Translates a {@link org.kalypso.model.wspm.core.profil.IProfil} to a
 * {@link org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart}.
 * 
 * @author Gernot Belger
 */
public class CheckinPartOperation
{
  private static final BigDecimal VEGETATION_0 = new BigDecimal( 0 );

  private final CrossSectionPart m_part = new CrossSectionPart();

  private final IProfil m_profil;

  private final String m_profilSRS;

  private final String m_heightComponentID;

  private final Coefficients m_coefficients;

  private final GafCodes m_gafCodes;

  private final GeometryFactory m_geometryFactory;

  private final IGeoTransformer m_transformer;

  public CheckinPartOperation( final CheckinStatePdbOperation stateOperation, final IProfil profil, final String profilSRS, final String mainComponentID )
  {
    m_coefficients = stateOperation.getCoefficients();
    m_gafCodes = stateOperation.getGafCodes();
    m_geometryFactory = stateOperation.getGeometryFactory();
    m_transformer = stateOperation.getTransformer();
    m_profil = profil;
    m_profilSRS = profilSRS;
    m_heightComponentID = mainComponentID;
  }

  public CrossSectionPart getPart( )
  {
    return m_part;
  }

  public void execute( ) throws PdbConnectException
  {
    // Name must be unique within each part
    final PDBNameGenerator nameGenerator = new PDBNameGenerator();

    final IRecord[] records = m_profil.getPoints();
    final List<Coordinate> lineCrds = new ArrayList<Coordinate>( records.length );
    for( int i = 0; i < records.length; i++ )
    {
      final IRecord record = records[i];

      final String name = getStringValue( record, IWspmConstants.POINT_PROPERTY_ID, StringUtils.EMPTY );
      final String comment = getStringValue( record, IWspmConstants.POINT_PROPERTY_COMMENT, StringUtils.EMPTY );
      final BigDecimal width = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_BREITE, null );
      final BigDecimal height = getDecimalValue( record, m_heightComponentID, null );
      final String code = getStringValue( record, IWspmConstants.POINT_PROPERTY_CODE, IGafConstants.CODE_PP );
      final String hyk = toHyk( code );

      final GM_Point loc = WspmGeometryUtilities.createLocation( m_profil, record, m_profilSRS, m_heightComponentID );
      final com.vividsolutions.jts.geom.Point location = toPoint( loc );

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
      // TODO: allow point without geometry?
      if( height == null )
        continue;

      /* Keep old point name if possible, else create a new unique one */
      final String uniquePointName = nameGenerator.createUniqueName( name );
      final Point point = new Point( null, m_part, uniquePointName, i );

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

      m_part.getPoints().add( point );
    }

    final int size = lineCrds.size();
    if( size == 1 )
    {
      System.out.println( "oups" );
    }
    else
    {
      final LineString line = m_geometryFactory.createLineString( lineCrds.toArray( new Coordinate[size] ) );
      m_part.setLine( line );
    }
  }

  private String toHyk( final String code )
  {
    /* Just check if it is an existing code */
    final GafCode hykCode = m_gafCodes.getHykCode( code );
    if( hykCode == null )
      return null;

    /* The hyk is always the same as the code itself */
    return hykCode.getHyk();
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

  private com.vividsolutions.jts.geom.Point toPoint( final GM_Point point ) throws PdbConnectException
  {
    try
    {
      final GM_Object transformedCurve = m_transformer.transform( point );
      return (com.vividsolutions.jts.geom.Point) JTSAdapter.export( transformedCurve );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( CheckinStatePdbOperation.STR_FAILED_TO_CONVERT_GEOMETRY, e );
    }
  }

}
