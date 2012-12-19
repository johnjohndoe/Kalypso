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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Formatter;
import java.util.Locale;
import java.util.Map;

import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.BuildingParameters;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation2D;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;

/**
 * Helper class to write the building file of a RMA∑Kalypso calculation.
 * 
 * @author Gernot Belger
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
    try( OutputStream outputStream = new BufferedOutputStream( new FileOutputStream( outputFile ) ) )
    {
      writeBuildingFile( outputStream );
    }
  }

  private void writeBuildingFile( final OutputStream outputStream ) throws IOException
  {
    // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
    // so no locale parameter for each format is needed any more .
    try( Formatter formatter = new Formatter( outputStream, Charset.defaultCharset().name(), Locale.US ) )
    {
      writeBuildingFile( formatter );
      FormatterUtils.checkIoException( formatter );
    }
  }

  private void writeBuildingFile( final Formatter formatter ) throws IOException
  {
    formatter.format( "TI      %s%n", "Bauwerksdaten" ); //$NON-NLS-1$ //$NON-NLS-2$

    for( final Map.Entry<Integer, IFlowRelationship> buildingEntry : m_buildingProvider.getBuildingData().entrySet() )
    {
      final IFlowRelationship building = buildingEntry.getValue();
      boolean qSymmetry = false;
      final Integer buildingID = buildingEntry.getKey();
      BuildingParameters buildingParameters = null;
      if( building instanceof IBuildingFlowRelation )
      {
        buildingParameters = ((IBuildingFlowRelation)building).getBuildingParameters();
      }
      else if( building instanceof IBuildingFlowRelation2D )
      {
        buildingParameters = ((IBuildingFlowRelation2D)building).getBuildingParameters();
        qSymmetry = ((IBuildingFlowRelation2D)building).getQSymmetry();
      }

      // writeBuildingBlock( formatter, buildingID, buildingParameters );
      writeNewBuildingBlock( formatter, buildingID, buildingParameters, qSymmetry );
    }

    formatter.format( "ENDDATA" ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

  private void writeNewBuildingBlock( final Formatter formatter, final Integer buildingID, final BuildingParameters buildingParameters, final boolean qSymmetry ) throws IOException
  {
    final TupleResult values = buildingParameters.getValues();

    // final int qCount = buildingParameters.getDischargeCount();
    // final int totalCount = values.size();

    //    formatter.format( "DLI      %7d% 7d %7d%n", buildingID, qCount, totalCount ); //$NON-NLS-1$
    formatter.format( "DLI      %7d %s%n", buildingID, qSymmetry == true ? "t" : "f" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final int qComp = TupleResultUtilities.indexOfComponent( values, BuildingParameters.COMPONENT_DISCHARGE );
    final int howComp = TupleResultUtilities.indexOfComponent( values, BuildingParameters.COMPONENT_WATERLEVEL_UPSTREAM );
    final int huwComp = TupleResultUtilities.indexOfComponent( values, BuildingParameters.COMPONENT_WATERLEVEL_DOWNSTREAM );

    for( final IRecord record : values )
    {
      final BigDecimal q = (BigDecimal)record.getValue( qComp );
      final BigDecimal huw = (BigDecimal)record.getValue( huwComp );
      final BigDecimal how = (BigDecimal)record.getValue( howComp );

      formatter.format( "CST     %.3f %.5f %.5f%n", q, huw, how ); //$NON-NLS-1$
    }

    formatter.format( "ENDBLOC  %8d%n", buildingID ); //$NON-NLS-1$

    FormatterUtils.checkIoException( formatter );
  }

//  private void formatBlock( final Formatter formatter, final String name, final BigDecimal[] values ) throws IOException
//  {
//    for( int i = 0; i < values.length; )
//    {
//      formatter.format( "%3s     ", name ); //$NON-NLS-1$
//
//      for( int j = 0; j < 9; j++, i++ )
//      {
//        if( i < values.length )
//          formatter.format( Locale.US, "%8.3f", values[i] ); // write decimals with '.' //$NON-NLS-1$
//      }
//      formatter.format( "%n" ); //$NON-NLS-1$
//
//      FormatterUtils.checkIoException( formatter );
//    }
//  }

//  private void formatDischarges( final Formatter formatter, final BigDecimal[] upstreamWaterlevels, final BigDecimal[] downstreamWaterlevels, final BuildingParameters buildingParameters ) throws IOException
//  {
//    for( final BigDecimal upstreamWaterlevel : upstreamWaterlevels )
//    {
//      final BigDecimal[] discharges = new BigDecimal[downstreamWaterlevels.length];
//      for( int i = 0; i < downstreamWaterlevels.length; i++ )
//      {
//        final BigDecimal downstreamWaterlevel = downstreamWaterlevels[i];
//        discharges[i] = buildingParameters.interpolateDischarge( upstreamWaterlevel, downstreamWaterlevel );
//      }
//
//      formatBlock( formatter, "FLW", discharges ); //$NON-NLS-1$
//
//      FormatterUtils.checkIoException( formatter );
//    }
//  }
}
