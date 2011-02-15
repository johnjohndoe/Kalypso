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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.util.Formatter;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekFrictionZone.FrictionType;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.FlowZone;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.IFlowZoneType;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.IRecord;

/**
 * @author Gernot Belger
 */
public class SobekFrictionDatExportOperation extends AbstractSobekFileExportOperation
{
  public static final String FRICTION_DAT = "friction.dat"; //$NON-NLS-1$

  public SobekFrictionDatExportOperation( final SobekExportInfo info )
  {
    super( info, FRICTION_DAT );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.sobek.ISobekProfileExportOperation#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return FRICTION_DAT;
  }

  @Override
  protected void writeProfile( final IProfileFeature profileFeature )
  {
    final IProfil profil = profileFeature.getProfil();
    final SobekFrictionZone[] frictionZones = findZones( profil );

    final SobekExportInfo info = getInfo();

    final String id = info.getID( profileFeature );
    final String name = info.getName( profileFeature );

    final String fricid = String.format( "Rau_%s", id ); //$NON-NLS-1$

    final Formatter formatter = getFormatter();
    formatter.format( "CRFR id '%s' nm '%s' cs '%s'%n", fricid, name, id ); //$NON-NLS-1$
    writeTables( formatter, frictionZones );
    formatter.format( "crfr%n" ); //$NON-NLS-1$
  }

  private void writeTables( final Formatter formatter, final SobekFrictionZone[] frictionZones )
  {
    writeZoneTable( formatter, frictionZones );

    writeFrictionTable( formatter, frictionZones, "ft" ); //$NON-NLS-1$
    writeFrictionTable( formatter, frictionZones, "fr" ); //$NON-NLS-1$
  }

  private void writeZoneTable( final Formatter formatter, final SobekFrictionZone[] frictionZones )
  {
    formatter.format( "lt ys%n" ); //$NON-NLS-1$
    formatter.format( "TBLE%n" ); //$NON-NLS-1$

    for( final SobekFrictionZone zone : frictionZones )
      formatter.format( "%.4f %.4f <%n", zone.getFrom(), zone.getTo() ); //$NON-NLS-1$

    formatter.format( "tble%n" ); //$NON-NLS-1$
  }

  private void writeFrictionTable( final Formatter formatter, final SobekFrictionZone[] frictionZones, final String tableKey )
  {
    formatter.format( "%s ys%n", tableKey ); //$NON-NLS-1$
    formatter.format( "TBLE%n" ); //$NON-NLS-1$

    for( final SobekFrictionZone zone : frictionZones )
      formatter.format( "%d %s <%n", zone.getFrictionType(), zone.getFrictionString() ); //$NON-NLS-1$

    formatter.format( "tble%n" ); //$NON-NLS-1$
  }

  private SobekFrictionZone[] findZones( final IProfil profil )
  {
    final IFlowZoneType[] zoneTypes = getInfo().getRoughnessZoneTypes();

    final SobekFrictionZone[] zones = new SobekFrictionZone[zoneTypes.length];
    for( int i = 0; i < zones.length; i++ )
      zones[i] = createZone( profil, zoneTypes[i] );
    return zones;
  }

  private SobekFrictionZone createZone( final IProfil profil, final IFlowZoneType zoneType )
  {
    final FlowZone flowZone = zoneType.createFlowZone( profil );
    if( flowZone == null )
    {
      final String comment = String.format( Messages.getString("SobekFricExportOperation_14"), zoneType.getLabel() ); //$NON-NLS-1$
      return new SobekFrictionZone( 0.0, 0.0, FrictionType.Chezy, 0.0, comment );
    }

    final double from = flowZone.getFrom();
    final double to = flowZone.getTo();
    final String label = flowZone.getLabel();

    final FrictionType frictionType = getFrictionType();
    final double friction = calculateFriction( profil, from, to );

    if( Double.isNaN( friction ) )
      System.out.println( "Oups" );

    return new SobekFrictionZone( from, to, frictionType, friction, label );
  }

  /**
   * Calculates the mean roughness between from and to.
   */
  private double calculateFriction( final IProfil profil, final double from, final double to )
  {
    final String roughnessId = getInfo().getRoughnessID();

    final int widthIndex = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int roughnessIndex = profil.indexOfProperty( roughnessId );
    // FIXME: throw exception instead and collect stati and show them to user
    if( roughnessIndex == -1 )
      return -1.0;

    double totalLength = 0.0;
    double totalRoughness = 0.0;

    final IRecord[] points = profil.getPoints();
    for( int i = 0; i < points.length - 1; i++ )
    {
      final IRecord point1 = points[i];
      final IRecord point2 = points[i + 1];

      final double width1 = getRecordValue( widthIndex, point1 );
      final double width2 = getRecordValue( widthIndex, point2 );
      final double roughness = getRecordValue( roughnessIndex, point1 );

      if( from <= width1 && from <= width2 && width1 <= to && width2 <= to )
      {
        final double segmentLength = Math.abs( width2 - width1 );
        totalLength += segmentLength;
        totalRoughness += roughness * segmentLength;
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
      return ((Number) value).doubleValue();

    return 0.0;
  }

  private FrictionType getFrictionType( )
  {
    final String roughnessId = getInfo().getRoughnessID();
    if( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS.equals( roughnessId ) )
      return FrictionType.White_Colebrook;

    if( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST.equals( roughnessId ) )
      return FrictionType.Strickler_Ks;

    return FrictionType.unknown1;
  }
}