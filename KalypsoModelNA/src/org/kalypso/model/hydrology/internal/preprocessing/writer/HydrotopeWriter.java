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

package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.parameter.ISoilType;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.CatchmentInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydrotopeInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.NaCatchmentData;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.Sealing;
import org.kalypso.model.hydrology.internal.preprocessing.util.CatchmentByAsciiIdSorter;

/**
 * @author Dejan Antanaskovic
 */
class HydrotopeWriter extends AbstractCoreFileWriter
{
  private final IDManager m_idManager;

  private final NaCatchmentData m_catchmentData;

  public HydrotopeWriter( final IDManager idManager, final NaCatchmentData catchmentData, final Logger logger )
  {
    super( logger );

    m_idManager = idManager;
    m_catchmentData = catchmentData;
  }

  @Override
  protected void writeContent( final PrintWriter writer )
  {
    final String hydrotopeFileTile = Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.2" ); //$NON-NLS-1$
    writer.append( hydrotopeFileTile ).append( '\n' );

    final Catchment[] catchments = m_catchmentData.getCatchmentsAndSubCatchments();
    // REMARK: sort by id, so we can compare ascii files more easily
    Arrays.sort( catchments, new CatchmentByAsciiIdSorter( m_idManager ) );

    for( final Catchment catchment : catchments )
    {
      final CatchmentInfo catchmentInfo = m_catchmentData.getInfo( catchment );
      final String checkMsg = catchmentInfo.checkArea();
      if( checkMsg != null )
        getLogger().warning( checkMsg );

      writeCatchment( writer, catchmentInfo );
    }
  }

  private void writeCatchment( final PrintWriter writer, final CatchmentInfo info )
  {
    final Catchment catchment = info.getCatchment();
    final int catchmentAsciiID = m_idManager.getAsciiID( catchment );

    final Sealing totalSealing = info.getTotalSealing();

    final Collection<HydrotopeInfo> hydrotops = info.getHydrotops();

    writer.print( catchmentAsciiID );
    writer.append( ' ' );
    writer.print( hydrotops.size() );
    writer.append( ' ' );
    writer.format( Locale.US, "%.3f %.3f %.3f", totalSealing.getSealedArea(), totalSealing.getNaturalArea(), totalSealing.getArea() ); //$NON-NLS-1$

    writer.append( '\n' );

    for( final HydrotopeInfo hydrotopInfo : hydrotops )
      writeHydrotope( writer, catchment, hydrotopInfo );
  }

  private void writeHydrotope( final PrintWriter writer, final Catchment catchment, final HydrotopeInfo hydrotopInfo )
  {
    final String landuseShortName = hydrotopInfo.getLanduseShortName();
    final double maxPerkolationRate = hydrotopInfo.getMaxPercolationRate();
    final double gwFactor = hydrotopInfo.getGwFactor();
    final int hydrotopID = hydrotopInfo.getLocalID();

    final double totalSealingRate = hydrotopInfo.getTotalSealingRate();
    final double naturalArea = hydrotopInfo.getNaturalArea();

    final String soiltypeName = getSoilTypeName( catchment, hydrotopInfo );

    /* Write hydrotope line */
    /* REMARK: last 0 is for the old calculation core - otherwise hydrotope file can't be read */
    writer.format( Locale.US, "%-10.10g %-10s %-10s %-10.3g %-10.3g %-10d %-10.3f 0%n", naturalArea, landuseShortName, soiltypeName, maxPerkolationRate, gwFactor, hydrotopID, totalSealingRate ); //$NON-NLS-1$
  }

  private String getSoilTypeName( final Catchment catchment, final HydrotopeInfo hydrotopInfo )
  {
    final ISoilType soiltype = hydrotopInfo.getSoilType();

    final String soiltypeName = soiltype.getName();

    /* Check if number of soil correction factors equals number of soil layers in each hydrotope */
    final int countSoilFactors = catchment.getBodenKorrekturCollection().size();
    final int countSoilLayers = soiltype.getParameters().size();

    if( countSoilFactors != countSoilLayers )
    {
      final String hydrotopeName = hydrotopInfo.getName();
      final String catchmentName = catchment.getName();
      final String message = String.format( Messages.getString( "HydrotopeWriter.1" ), countSoilLayers, hydrotopeName, countSoilFactors, catchmentName ); //$NON-NLS-1$
      getLogger().log( Level.WARNING, message );
    }
    return soiltypeName;
  }

//  public void writeMapping( final File outputFile ) throws FileNotFoundException
//  {
//    PrintWriter writer = null;
//    try
//    {
//      writer = new PrintWriter( outputFile );
//
//      final Collection<Catchment> catchments = m_hydroHash.getCatchments();
//      for( final Catchment catchment : catchments )
//      {
//        final int catchmentAsciiID = m_idManager.getAsciiID( catchment );
//
//        final CatchmentInfo catchmentInfo = m_hydroHash.getHydrotopInfo( catchment );
//        final Collection<HydrotopeInfo> hydrotops = catchmentInfo.getHydrotops();
//        for( final HydrotopeInfo hydrotopInfo : hydrotops )
//        {
//          final int hydrotopID = hydrotopInfo.getLocalID();
//          final String featureId = hydrotopInfo.getFeatureId();
//          final String hydrotopName = hydrotopInfo.getName();
//
//          writer.format( Locale.US, "%6d %6d   --->   [%s] \t%s\n", catchmentAsciiID, hydrotopID, featureId, hydrotopName ); //$NON-NLS-1$
//        }
//      }
//
//      writer.close();
//    }
//    finally
//    {
//      IOUtils.closeQuietly( writer );
//    }
//  }
}
