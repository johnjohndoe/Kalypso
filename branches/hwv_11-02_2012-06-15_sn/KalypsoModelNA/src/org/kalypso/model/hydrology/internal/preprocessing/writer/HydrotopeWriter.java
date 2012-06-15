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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.binding.parameter.Soiltype;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.CatchmentInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydrotopeInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.Sealing;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Dejan Antanaskovic
 */
public class HydrotopeWriter extends AbstractCoreFileWriter
{
  private final HydroHash m_hydroHash;

  private final IDManager m_idManager;

  private final Parameter m_parameter;

  private final Map<String, Soiltype> m_soilTypeNameHash;

  public HydrotopeWriter( final Parameter parameter, final IDManager idManager, final HydroHash hydroHash, final Logger logger )
  {
    super( logger );

    m_hydroHash = hydroHash;
    m_idManager = idManager;
    m_parameter = parameter;

    /* Build soiltype hash for faster lookup later */
    m_soilTypeNameHash = new HashMap<String, Soiltype>();
    final IFeatureBindingCollection<Soiltype> soiltypes = m_parameter.getSoiltypes();
    for( final Soiltype soiltype : soiltypes )
      m_soilTypeNameHash.put( soiltype.getName(), soiltype );
  }

  /**
   * @see org.kalypso.model.hydrology.internal.preprocessing.writer.AbstractCoreFileWriter#writeContent(java.io.PrintWriter)
   */
  @Override
  protected void writeContent( final PrintWriter writer ) throws Exception
  {
    final String hydrotopeFileTile = Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.2" ); //$NON-NLS-1$ 
    writer.append( hydrotopeFileTile ).append( '\n' );

    final Collection<Catchment> catchments = m_hydroHash.getCatchments();
    for( final Catchment catchment : catchments )
    {
      final CatchmentInfo catchmentInfo = m_hydroHash.getHydrotopInfo( catchment );
      final String checkMsg = catchmentInfo.checkArea();
      if( checkMsg != null )
        getLogger().warning( checkMsg );

      writeCatchment( writer, catchmentInfo );
    }
  }

  private void writeCatchment( final PrintWriter writer, final CatchmentInfo info ) throws SimulationException
  {
    final Catchment catchment = info.getCatchment();
    final List<HydrotopeInfo> hydrotops = info.getHydrotops();
    final Sealing totalSealing = info.getTotalSealing();
    final Sealing totalSudsSealing = info.getTotalSudsSealing();

    final int catchmentAsciiID = m_idManager.getAsciiID( catchment );

    writer.print( catchmentAsciiID );
    writer.append( ' ' );
    writer.print( hydrotops.size() );
    writer.append( ' ' );
    writer.format( Locale.US, "%.3f %.3f %.3f", totalSealing.getSealedArea(), totalSealing.getNaturalArea(), totalSealing.getArea() ); //$NON-NLS-1$

    final double sudsSealedArea = totalSudsSealing.getSealedArea();
    final double sudsNaturalArea = totalSudsSealing.getNaturalArea();
    if( sudsSealedArea > 0.0 || sudsNaturalArea > 0.0 )
      writer.format( Locale.US, " 1 %.3f %.3f %.3f", sudsSealedArea, sudsNaturalArea, totalSudsSealing.getArea() ); //$NON-NLS-1$

    writer.append( '\n' );

    for( final HydrotopeInfo hydrotopInfo : hydrotops )
      writeHydrotope( writer, hydrotopInfo );
  }

  private void writeHydrotope( final PrintWriter writer, final HydrotopeInfo hydrotopInfo ) throws SimulationException
  {
    final IHydrotope hydrotop = hydrotopInfo.getHydrotop();

    final String landuseShortName = m_hydroHash.getLanduseHash().getLanduseFeatureShortedName( hydrotop.getLanduse() );
    final double maxPerkolationRate = hydrotop.getMaxPerkolationRate();
    final double gwFactor = hydrotop.getGWFactor();
    final int hydrotopID = hydrotopInfo.getLocalID();

    final Sealing hydrotopeSealing = hydrotopInfo.getHydrotopeSealingMinusSuds();
    final Sealing hydrotopSealingAfterUnsealing = hydrotopInfo.getHydrotopSealingAfterUnsealing();
    final double totalSealingRate = hydrotopSealingAfterUnsealing.getSealingRate();
    final String soiltypeName = findSoiltypeName( hydrotop );

    writer.format( Locale.US, "%-10.3f %-10s %-10s %-10.3g %-10.3g %-10d %-10.3f", hydrotopeSealing.getNaturalArea(), landuseShortName, soiltypeName, maxPerkolationRate, gwFactor, hydrotopID, totalSealingRate ); //$NON-NLS-1$

    writer.append( ' ' );
    if( hydrotopInfo.hasSudsWithElementType() )
      writer.append( '1' );
    else
      writer.append( '0' );
    writer.append( ' ' );
    writer.append( formatSudsSealing( hydrotopInfo ) );
    writer.append( '\n' );
  }

  private String findSoiltypeName( final IHydrotope hydrotop ) throws SimulationException
  {
    final String soilTypeID = hydrotop.getSoilType();
    final Soiltype soiltype = m_parameter.findSoiltypeByID( soilTypeID );
    if( soiltype != null )
      return soiltype.getName();

    // Feature could not be found by id. For backwards compability: search by name as well:
    final Soiltype soiltypeByName = m_soilTypeNameHash.get( soilTypeID );
    if( soiltypeByName != null )
      return soilTypeID;

    final String msg = String.format( "Unknown soiltype '%s' in hydrotope '%s'.", soilTypeID, hydrotop.getId() );
    throw new SimulationException( msg );
  }

  private static String formatSudsSealing( final HydrotopeInfo info )
  {
    final String _10 = formatSudsSealing( info.getSudsSealingByType( "10" ) ); //$NON-NLS-1$ 
    final String _11 = formatSudsSealing( info.getSudsSealingByType( "11" ) ); //$NON-NLS-1$ 
    final String _12 = formatSudsSealing( info.getSudsSealingByType( "12" ) ); //$NON-NLS-1$ 
    final String _13 = formatSudsSealing( info.getSudsSealingByType( "13" ) ); //$NON-NLS-1$ 
    final String _20 = formatSudsSealing( info.getSudsSealingByType( "20" ) ); //$NON-NLS-1$ 
    final String _21 = formatSudsSealing( info.getSudsSealingByType( "21" ) ); //$NON-NLS-1$ 
    final String _30 = formatSudsSealing( info.getSudsSealingByType( "30" ) ); //$NON-NLS-1$ 
    final String _31 = formatSudsSealing( info.getSudsSealingByType( "31" ) ); //$NON-NLS-1$ 
    final String _40 = formatSudsSealing( info.getSudsSealingByType( "40" ) ); //$NON-NLS-1$ 
    final String _41 = formatSudsSealing( info.getSudsSealingByType( "41" ) ); //$NON-NLS-1$ 

    return String.format( Locale.US, "%s %s %s %s %s %s %s %s %s %s", _10, _11, _12, _13, _20, _21, _30, _31, _40, _41 );
  }

  private static String formatSudsSealing( final Sealing info )
  {
    if( Double.isNaN( info.getNaturalArea() ) )
      return String.format( Locale.US, "%.3f", info.getSealedArea() ); //$NON-NLS-1$

    return String.format( Locale.US, "%.3f %.3f", info.getNaturalArea(), info.getSealedArea() ); //$NON-NLS-1$
  }

  public void writeMapping( final File outputFile ) throws FileNotFoundException
  {
    PrintWriter writer = null;
    try
    {
      writer = new PrintWriter( outputFile );

      final Collection<Catchment> catchments = m_hydroHash.getCatchments();
      for( final Catchment catchment : catchments )
      {
        final int catchmentAsciiID = m_idManager.getAsciiID( catchment );

        final CatchmentInfo hydrotopInfo = m_hydroHash.getHydrotopInfo( catchment );
        final List<HydrotopeInfo> hydrotops = hydrotopInfo.getHydrotops();
        for( final HydrotopeInfo hydrotopWithSudInfo : hydrotops )
        {
          final IHydrotope hydrotop = hydrotopWithSudInfo.getHydrotop();
          int hydrotopID = hydrotopWithSudInfo.getLocalID();
          writer.format( Locale.US, "%6d %6d   --->   [%s] \t%s\n", catchmentAsciiID, hydrotopWithSudInfo.getLocalID(), hydrotop.getId(), hydrotop.getName() ); //$NON-NLS-1$
          hydrotopID++;
        }
      }

      writer.close();
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }

  }

}
