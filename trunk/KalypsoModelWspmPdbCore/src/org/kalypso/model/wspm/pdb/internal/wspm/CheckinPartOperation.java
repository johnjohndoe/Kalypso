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
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.gaf.GafCode;
import org.kalypso.model.wspm.pdb.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.gaf.GafKind;
import org.kalypso.model.wspm.pdb.gaf.GafPointCode;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.internal.utils.PDBNameGenerator;
import org.kalypso.model.wspm.pdb.wspm.CheckinHelper;
import org.kalypso.model.wspm.pdb.wspm.CheckinStateOperationData;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.geometry.GM_Point;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * Translates a {@link org.kalypso.model.wspm.core.profil.IProfil} to a {@link org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart}.
 *
 * @author Gernot Belger
 */
public class CheckinPartOperation
{
  // private static final BigDecimal VEGETATION_0 = new BigDecimal( 0 );

  private final IStatusCollector m_stati = new StatusCollector( WspmPdbCorePlugin.PLUGIN_ID );

  private final CrossSectionPart m_part = new CrossSectionPart();

  private final IProfile m_profile;

  private final String m_profilSRS;

  private final CheckinStateOperationData m_data;

  public CheckinPartOperation( final CheckinStateOperationData data, final IProfile profil, final String profilSRS )
  {
    m_data = data;
    m_profile = profil;
    m_profilSRS = profilSRS;
  }

  public CrossSectionPart getPart( )
  {
    return m_part;
  }

  public IStatus execute( ) throws PdbConnectException
  {
    /* Names must be unique within each part. */
    final PDBNameGenerator nameGenerator = new PDBNameGenerator();

    final String heightComponentID = IWspmPointProperties.POINT_PROPERTY_HOEHE;

    final IProfileRecord[] records = m_profile.getPoints();
    final List<Coordinate> lineCrds = new ArrayList<>( records.length );
    for( int i = 0; i < records.length; i++ )
    {
      final IProfileRecord record = records[i];

      final String name = getStringValue( record, IWspmConstants.POINT_PROPERTY_ID, StringUtils.EMPTY );
      final String comment = getStringValue( record, IWspmConstants.POINT_PROPERTY_COMMENT, StringUtils.EMPTY );
      final BigDecimal width = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_BREITE, null );
      final BigDecimal height = getDecimalValue( record, heightComponentID, null );

      final String profileCode = getStringValue( record, IWspmConstants.POINT_PROPERTY_CODE, null );

      final String pdbCode = toGafCode( profileCode );

      /* hyk is determined completely independent of code; just use position of markers in profile */
      final String hyk = getHykCode( record );

      final GM_Point loc = WspmGeometryUtilities.createLocation( m_profile, record, m_profilSRS, heightComponentID );
      final com.vividsolutions.jts.geom.Point location = CheckinHelper.toPoint( loc, m_data.getTransformer() );

      // FIMXE: check class
      final String roughnessClassId = getStringValue( record, IWspmConstants.POINT_PROPERTY_ROUGHNESS_CLASS, null );
      final BigDecimal kstValue = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST, null );
      final BigDecimal kValue = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_RAUHEIT_KS, null );

      // FIMXE: check class
      final String vegetationClassId = getStringValue( record, IWspmConstants.POINT_PROPERTY_BEWUCHS_CLASS, null );
      final BigDecimal axValue = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, null );
      final BigDecimal ayValue = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, null );
      final BigDecimal dpValue = getDecimalValue( record, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, null );

      /* Not all point may pass */
      // TODO: allow point without geometry?
      if( height == null )
        continue;

      final Roughness roughness = toRoughness( roughnessClassId );
      final Vegetation vegetation = toVegetation( vegetationClassId );

      /* Keep old point name if possible, else create a new unique one */
      final String uniquePointName = nameGenerator.createUniqueName( name );
      final Point point = new Point( null, m_part, uniquePointName, i );

      point.setDescription( comment );
      point.setWidth( width );
      point.setHeight( height );
      point.setCode( pdbCode );
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
    if( size != 1 )
    {
      final LineString line = m_data.getGeometryFactory().createLineString( lineCrds.toArray( new Coordinate[size] ) );
      m_part.setLine( line );
    }

    final double station = m_profile.getStation();
    final String warning = String.format( Messages.getString( "CheckinPartOperation_0" ), station, GafKind.P ); //$NON-NLS-1$
    return m_stati.asMultiStatusOrOK( warning );
  }

  private Vegetation toVegetation( final String vegetationClassId )
  {
    final ICoefficients coefficients = m_data.getCoefficients();

    if( StringUtils.isBlank( vegetationClassId ) )
      return coefficients.getUnknownVegetation();

    final Vegetation vegetation = coefficients.getVegetation( vegetationClassId );
    if( vegetation == null )
    {
      final Vegetation unknownVegetation = coefficients.getUnknownVegetation();
      m_stati.add( IStatus.WARNING, Messages.getString( "CheckinPartOperation_1" ), null, vegetationClassId, unknownVegetation.getLabel() ); //$NON-NLS-1$
      return unknownVegetation;
    }

    m_data.getClassChecker().addVegetation( vegetation );

    return vegetation;
  }

  private Roughness toRoughness( final String roughnessClassId )
  {
    final ICoefficients coefficients = m_data.getCoefficients();

    if( StringUtils.isBlank( roughnessClassId ) )
      return coefficients.getUnknownRoughness();

    final Roughness roughness = coefficients.getRoughness( roughnessClassId );
    if( roughness == null )
    {
      final Roughness unknownRoughness = coefficients.getUnknownRoughness();
      m_stati.add( IStatus.WARNING, Messages.getString( "CheckinPartOperation_2" ), null, roughnessClassId, unknownRoughness.getLabel() ); //$NON-NLS-1$
      return unknownRoughness;
    }

    m_data.getClassChecker().addRoughness( roughness );

    return roughness;
  }

  private String toGafCode( final String code )
  {
    final GafKind kind = GafKind.P;
    final GafCodes codes = m_data.getGafCodes();

    final GafCode gafCode = codes.getCode( code );
    if( gafCode != null && gafCode.getKind() == kind )
      return gafCode.getCode();

    return GafPointCode.PP.getKey();
  }

  public String getHykCode( final IProfileRecord record )
  {
    final Collection<String> codes = new ArrayList<>( 3 );

    // IMPORTANT: This order is important for gaf file export, do not change.

    final IProfilePointMarker[] dbMarkers = m_profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    final String codeBD = checkMarker( record, dbMarkers, GafPointCode.PA.getKey(), GafPointCode.PE.getKey() );
    if( codeBD != null )
      codes.add( codeBD );

    final IProfilePointMarker[] tfMarkers = m_profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final String codeTF = checkMarker( record, tfMarkers, GafPointCode.LU.getKey(), GafPointCode.RU.getKey() );
    if( codeTF != null )
      codes.add( codeTF );

    final IProfilePointMarker[] bvMarkers = m_profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    final String codeBV = checkMarker( record, bvMarkers, GafPointCode.LBOK.getKey(), GafPointCode.RBOK.getKey() );
    if( codeBV != null )
      codes.add( codeBV );

    if( codes.isEmpty() )
      return null;

    return StringUtils.join( codes, IGafConstants.HYK_CODE_SEPARATOR );
  }

  private String checkMarker( final IProfileRecord record, final IProfilePointMarker[] markers, final String codeStart, final String codeEnd )
  {
    for( final IProfilePointMarker marker : markers )
    {
      if( marker.getPoint() == record )
      {
        if( marker == markers[0] )
          return codeStart;
        else
          return codeEnd;
      }
    }

    return null;
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
      return (BigDecimal)value;

    if( value instanceof Number )
      return new BigDecimal( ((Number)value).doubleValue() );

    return defaultValue;
  }
}