/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.math.BigDecimal;

import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekFrictionDat;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekFrictionDat.FrictionType;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekFrictionDatCRFRSection;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.FlowZone;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.IFlowZoneType;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IRecord;

/**
 * @author Gernot Belger
 */
class SobekFrictionDatExporter
{
  private final IFlowZoneType[] m_zoneTypes;

  private final String m_roughnessId;

  private final boolean m_preferRoughnessClasses;

  public SobekFrictionDatExporter( final IFlowZoneType[] zoneTypes, final String roughnessId, final boolean preferRoughnessClasses )
  {
    m_zoneTypes = zoneTypes;
    m_roughnessId = roughnessId;
    m_preferRoughnessClasses = preferRoughnessClasses;
  }

  public SobekFrictionDat export( final String id, final String name, final IProfile profile )
  {
    final String fricId = String.format( "Rau_%s", id ); //$NON-NLS-1$
    final String csID = id;

    final SobekFrictionDatCRFRSection[] sections = createZones( profile );

    return new SobekFrictionDat( fricId, name, csID, sections );
  }

  private SobekFrictionDatCRFRSection[] createZones( final IProfile profile )
  {
    final SobekFrictionDatCRFRSection[] zones = new SobekFrictionDatCRFRSection[m_zoneTypes.length];

    for( int i = 0; i < zones.length; i++ )
      zones[i] = createZone( profile, m_zoneTypes[i] );

    return zones;
  }

  private SobekFrictionDatCRFRSection createZone( final IProfile profile, final IFlowZoneType zoneType )
  {
    final BigDecimal missingWidth = new BigDecimal( "0.0000" ); //$NON-NLS-1$

    final FlowZone flowZone = zoneType.createFlowZone( profile );
    if( flowZone == null )
    {
      final String comment = String.format( Messages.getString( "SobekFricExportOperation_14" ), zoneType.getLabel() ); //$NON-NLS-1$
      final SobekFrictionDatCRFRSection missingZone = new SobekFrictionDatCRFRSection( missingWidth, missingWidth );
      missingZone.setPositiveValue( FrictionType.Chezy, new BigDecimal( 0.0 ) );
      missingZone.setNegativeValue( FrictionType.Chezy, new BigDecimal( 0.0 ) );
      missingZone.setComment( comment );
      return missingZone;
    }

    final BigDecimal start = new BigDecimal( flowZone.getFrom() ).setScale( 4, BigDecimal.ROUND_HALF_UP );
    final BigDecimal end = new BigDecimal( flowZone.getTo() ).setScale( 4, BigDecimal.ROUND_HALF_UP );

    final String label = flowZone.getLabel();

    final FrictionType frictionType = getFrictionType();
    final double friction = calculateFriction( profile, flowZone.getFrom(), flowZone.getTo() );

    if( Double.isNaN( friction ) )
    {
      final String comment = String.format( "Zone '%s' has no value", zoneType.getLabel() ); //$NON-NLS-1$
      final SobekFrictionDatCRFRSection missingZone = new SobekFrictionDatCRFRSection( missingWidth, missingWidth );
      missingZone.setPositiveValue( FrictionType.Chezy, new BigDecimal( 0.0 ) );
      missingZone.setNegativeValue( FrictionType.Chezy, new BigDecimal( 0.0 ) );
      missingZone.setComment( comment );
      return missingZone;
    }

    final SobekFrictionDatCRFRSection zone = new SobekFrictionDatCRFRSection( start, end );

    zone.setPositiveValue( frictionType, new BigDecimal( friction ) );
    zone.setNegativeValue( frictionType, new BigDecimal( friction ) );
    zone.setComment( label );

    return zone;
  }

  /**
   * Calculates the mean roughness between from and to.
   */
  private double calculateFriction( final IProfile profil, final double from, final double to )
  {
    final int widthIndex = profil.indexOfProperty( IWspmPointProperties.POINT_PROPERTY_BREITE );

    // FIXME: check if there is any roughness defined, and give warning if this is not the case (neither classes nor

    double totalLength = 0.0;
    double totalRoughness = 0.0;

    final IProfileRecord[] points = profil.getPoints();
    for( int i = 0; i < points.length - 1; i++ )
    {
      final IProfileRecord point1 = points[i];
      final IProfileRecord point2 = points[i + 1];

      final double width1 = getRecordValue( widthIndex, point1 );
      final double width2 = getRecordValue( widthIndex, point2 );

      final BigDecimal roughness = WspmClassifications.getRoughnessValue( point1, m_roughnessId, m_preferRoughnessClasses );

      if( from <= width1 && from <= width2 && width1 <= to && width2 <= to )
      {
        final double segmentLength = Math.abs( width2 - width1 );
        totalLength += segmentLength;

        // REMARK: if no roughness set, we assume value of 0.0 (i.e. 0.0. goes into total)
        if( roughness != null )
          totalRoughness += roughness.doubleValue() * segmentLength;
      }
    }

    final double friction = totalRoughness / totalLength;
    if( Double.isNaN( friction ) || Double.isInfinite( friction ) )
      return 0.0;

    return friction;
  }

  private double getRecordValue( final int componentIndex, final IRecord point )
  {
    final Object value = point.getValue( componentIndex );
    if( value instanceof Number )
      return ((Number)value).doubleValue();

    return 0.0;
  }

  private FrictionType getFrictionType( )
  {
    if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS.equals( m_roughnessId ) )
      return FrictionType.White_Colebrook;

    if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST.equals( m_roughnessId ) )
      return FrictionType.Strickler_Ks;

    return FrictionType.unknown1;
  }
}