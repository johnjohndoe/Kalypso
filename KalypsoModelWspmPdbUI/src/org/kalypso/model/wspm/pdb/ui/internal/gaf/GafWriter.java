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
package org.kalypso.model.wspm.pdb.ui.internal.gaf;

import java.io.File;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.utils.ByCategoryComparator;
import org.kalypso.model.wspm.pdb.db.utils.ByStationComparator;
import org.kalypso.model.wspm.pdb.db.utils.ConsecutiveNumComparator;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

/**
 * This writer serializes profile data into the gaf format.
 * 
 * @author Holger Albert
 */
public class GafWriter
{
  /**
   * The constructor.
   */
  public GafWriter( )
  {
  }

  /**
   * This function writes the cross sections to the file.
   * 
   * @param crossSections
   *          The cross sections.
   * @param file
   *          The file.
   * @param monitor
   *          A progress monitor.
   * @return A status, indicating the result of the operation.
   */
  public IStatus write( final Set<CrossSection> crossSections, final File file, IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* The writer. */
    PrintWriter writer = null;

    try
    {
      /* Monitor. */
      monitor.beginTask( "Exporting profiles to GAF file", 100 * crossSections.size() );
      monitor.subTask( "Writing profiles..." );

      /* Create the writer. */
      writer = new PrintWriter( file, "UTF-8" );

      /* Sort the cross sections. */
      final CrossSection[] sortedCrossSections = crossSections.toArray( new CrossSection[] {} );
      Arrays.sort( sortedCrossSections, new ByStationComparator() );

      /* Loop the cross sections. */
      for( final CrossSection crossSection : sortedCrossSections )
      {
        /* Write the cross section. */
        writeCrossSection( crossSection, writer );

        /* Monitor. */
        monitor.worked( 100 );
      }

      return new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, "The GAF export was successfull." );
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, String.format( "The GAF export has failed: %s", ex.getLocalizedMessage() ), ex );
    }
    finally
    {
      /* Close the writer. */
      IOUtils.closeQuietly( writer );

      /* Monitor. */
      monitor.done();
    }
  }

  private void writeCrossSection( final CrossSection crossSection, final PrintWriter writer )
  {
    /* Get the cross section parts. */
    final Set<CrossSectionPart> parts = crossSection.getCrossSectionParts();

    /* Sort the cross sections. */
    final CrossSectionPart[] sortedParts = parts.toArray( new CrossSectionPart[] {} );
    Arrays.sort( sortedParts, new ByCategoryComparator() );

    /* Loop the cross section parts. */
    for( final CrossSectionPart part : sortedParts )
    {
      /* Write the cross section part. */
      writeCrossSectionPart( part, writer );
    }
  }

  private void writeCrossSectionPart( final CrossSectionPart part, final PrintWriter writer )
  {
    /* Get the points. */
    final Set<Point> points = part.getPoints();

    /* Sort the points. */
    final Point[] sortedPoints = points.toArray( new Point[] {} );
    Arrays.sort( sortedPoints, new ConsecutiveNumComparator() );

    /* Loop the points. */
    for( final Point point : sortedPoints )
    {
      /* Write the point. */
      writePoint( point, writer );
    }
  }

  private void writePoint( final Point point, final PrintWriter writer )
  {
    /* Create the line. */
    final StringBuilder builder = new StringBuilder();
    builder.append( "%.3f\t" );
    builder.append( "%s\t" );
    builder.append( "%.3f\t" );
    builder.append( "%.3f\t" );
    builder.append( "%s\t" );
    builder.append( "%d\t" );
    builder.append( "%d\t" );
    builder.append( "%.4f\t" );
    builder.append( "%.4f\t" );
    builder.append( "%s" );

    /* Get the data to write. */
    final CrossSection crossSection = point.getCrossSectionPart().getCrossSection();
    final double station = crossSection.getStation().doubleValue();
    final String id = point.getName();
    final double y = point.getWidth().doubleValue();
    final double z = point.getHeight().doubleValue();
    final String kz = point.getCode();
    final int rk = Integer.parseInt( point.getRoughness().getId().getName() );
    final int bk = Integer.parseInt( point.getVegetation().getId().getName() );
    final double hw = point.getLocation().getY();
    final double rw = point.getLocation().getX();
    final String hyk = point.getHyk() == null ? "x" : point.getHyk();

    /* Write the line. */
    writer.format( Locale.PRC, builder.toString(), station, id, y, z, kz, rk, bk, hw, rw, hyk );
    writer.println();
  }
}