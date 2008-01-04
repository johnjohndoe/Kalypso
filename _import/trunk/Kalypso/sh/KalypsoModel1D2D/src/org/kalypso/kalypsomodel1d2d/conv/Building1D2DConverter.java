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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Formatter;
import java.util.Locale;
import java.util.Map;

import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.BuildingParameters;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;

/**
 * Helper class to write the building file of a RMA10S calculation.
 * 
 * @author Gernot Belger
 * 
 */
public class Building1D2DConverter
{
  private final BuildingIDProvider m_buildingProvider;

  public Building1D2DConverter( final BuildingIDProvider buildingProvider )
  {
    m_buildingProvider = buildingProvider;
  }

  public void writeBuildingFile( final File outputFile ) throws IOException
  {
    Formatter formatter = null;
    try
    {
      // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
      // so no locale parameter for each format is needed any more .
      formatter = new Formatter( outputFile, Charset.defaultCharset().name(), Locale.US );
      writeBuildingFile( formatter );
      FormatterUtils.checkIoException( formatter );
    }
    finally
    {
      if( formatter != null )
      {
        // REMARK: do not check io-exception here, else other exception would be hidden by this on
        formatter.close();
      }
    }

  }

  public void writeBuildingFile( final Formatter formatter ) throws IOException
  {
    formatter.format( "TI      %s%n", "Bauwerksdaten" );

    for( final Map.Entry<Integer, IBuildingFlowRelation> buildingEntry : m_buildingProvider.getBuildingData().entrySet() )
    {
      final IBuildingFlowRelation building = buildingEntry.getValue();
      final Integer buildingID = buildingEntry.getKey();
      final BuildingParameters buildingParameters = building.getBuildingParameters();

      writeBuildingBlock( formatter, buildingID, buildingParameters );
    }

    formatter.format( "ENDDATA" );

    FormatterUtils.checkIoException( formatter );
  }

  private void writeBuildingBlock( final Formatter formatter, final Integer buildingID, final BuildingParameters buildingParameters ) throws IOException
  {
    final BigDecimal[] upstreamWaterlevels = buildingParameters.getUpstreamWaterlevels();
    final BigDecimal[] downstreamWaterlevels = buildingParameters.getDownstreamWaterlevels();

    formatter.format( "IDC     %8d%8d%8d%n", buildingID, upstreamWaterlevels.length, downstreamWaterlevels.length );

    formatBlock( formatter, "HRW", upstreamWaterlevels );
    formatBlock( formatter, "HCL", downstreamWaterlevels );

    formatDischarges( formatter, upstreamWaterlevels, downstreamWaterlevels, buildingParameters );
  }

  private void formatBlock( final Formatter formatter, final String name, final BigDecimal[] values ) throws IOException
  {
    for( int i = 0; i < values.length; )
    {
      formatter.format( "%3s     ", name );

      for( int j = 0; j < 9; j++, i++ )
      {
        if( i < values.length )
          formatter.format( Locale.US, "%8.3f", values[i] ); // write decimals with '.'
      }
      formatter.format( "%n" );

      FormatterUtils.checkIoException( formatter );
    }
  }

  private void formatDischarges( final Formatter formatter, final BigDecimal[] upstreamWaterlevels, final BigDecimal[] downstreamWaterlevels, final BuildingParameters buildingParameters ) throws IOException
  {
    for( final BigDecimal upstreamWaterlevel : upstreamWaterlevels )
    {
      final BigDecimal[] discharges = new BigDecimal[downstreamWaterlevels.length];
      for( int i = 0; i < downstreamWaterlevels.length; i++ )
      {
        final BigDecimal downstreamWaterlevel = downstreamWaterlevels[i];
        discharges[i] = buildingParameters.interpolateDischarge( upstreamWaterlevel, downstreamWaterlevel );
      }

      formatBlock( formatter, "FLW", discharges );

      FormatterUtils.checkIoException( formatter );
    }
  }

}
