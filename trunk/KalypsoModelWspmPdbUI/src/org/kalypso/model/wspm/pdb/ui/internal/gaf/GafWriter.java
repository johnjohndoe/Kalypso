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
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartType;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.utils.ByCategoryComparator;
import org.kalypso.model.wspm.pdb.db.utils.ByStationComparator;
import org.kalypso.model.wspm.pdb.db.utils.ConsecutiveNumComparator;
import org.kalypso.model.wspm.pdb.gaf.GafCode;
import org.kalypso.model.wspm.pdb.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.gaf.GafKind;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

/**
 * This writer serializes profile data into the gaf format.
 * 
 * @author Holger Albert
 */
public class GafWriter
{
  private final String GAF_LINE = "%.3f\t%s\t%s\t%s\t%s\t%d\t%d\t%.4f\t%.4f\t%s"; //$NON-NLS-1$

  // TODO: maybe get as option
  private static final String EMPTY_HYK_CODE = "x"; //$NON-NLS-1$

  private final HykExportMode m_hykExportMode;

  private final GafCodes m_codes;

  /** Stores the lines with multiple hyk entries */
  private final List<String> m_additionalLines = new ArrayList<>();

  public GafWriter( final GafCodes codes, final HykExportMode hykExportMode )
  {
    m_codes = codes;
    m_hykExportMode = hykExportMode;
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
  public IStatus write( final Set<CrossSection> crossSections, final File file, final IProgressMonitor monitor )
  {
    PrintWriter writer = null;

    try
    {
      writer = new PrintWriter( file );

      /* Monitor. */
      monitor.beginTask( "Exporting profiles to GAF file", 100 * crossSections.size() );
      monitor.subTask( "Writing profiles..." );

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

      /* write additional lines in separate file */
      writeAdditionalLines( file );

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

  private void writeAdditionalLines( final File gafFile ) throws IOException
  {
    if( m_additionalLines.isEmpty() )
      return;

    final String hykFilePath = gafFile.getAbsolutePath() + ".hyk"; //$NON-NLS-1$
    final File hykFile = new File( hykFilePath );

    FileUtils.writeLines( hykFile, m_additionalLines );
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

    final CrossSectionPartType partType = part.getCrossSectionPartType();
    final GafKind kind = findKind( partType );

    /* Loop the points. */
    for( final Point point : sortedPoints )
    {
      /* Write the point. */
      writePoint( kind, point, writer );
    }
  }

  private GafKind findKind( final CrossSectionPartType partType )
  {
    if( partType == null )
      return GafKind.P;

    try
    {
      final String category = partType.getCategory();
      return GafKind.valueOf( category );
    }
    catch( final Exception e )
    {
      // ignore
      return GafKind.P;
    }
  }

  private void writePoint( final GafKind kind, final Point point, final PrintWriter writer )
  {
    /* Get the data to write. */
    final CrossSection crossSection = point.getCrossSectionPart().getCrossSection();
    final double station = crossSection.getStation().doubleValue();
    final String id = point.getName();

    String y = "-1";
    final BigDecimal width = point.getWidth();
    if( width != null )
      y = String.format( "%.4f", width.doubleValue() );

    String z = "-1";
    final BigDecimal height = point.getHeight();
    if( height != null )
      z = String.format( "%.4f", height.doubleValue() );

    final int rk = Integer.parseInt( point.getRoughness().getId().getName() );
    final int bk = Integer.parseInt( point.getVegetation().getId().getName() );
    final double hw = point.getLocation().getY();
    final double rw = point.getLocation().getX();

    final String code = point.getCode();
    final String hyk = StringUtils.isBlank( point.getHyk() ) ? EMPTY_HYK_CODE : point.getHyk();

    /* the gaf writer might produce multiple hyk codes, we need to write one line per single code */
    final String[] hyks = StringUtils.split( hyk, IGafConstants.HYK_CODE_SEPARATOR );

    /* depending on the export mode, the code/hyk gets tweaked; first tweak than count lines, because tweak may reduce codes to normal lines */
    final Pair<String, String>[] tweakedCodes = tweakHyks( kind, code, hyks );

    int count = 0;
    for( final Pair<String, String> tweakedPair : tweakedCodes )
    {
      final String tweakedCode = tweakedPair.getLeft();
      final String tweakedHyk = tweakedPair.getRight();

      final String line = String.format( Locale.PRC, GAF_LINE, station, id, y, z, tweakedCode, rk, bk, hw, rw, tweakedHyk );

      // REMARK: store first line in file, additional hyk lines are stored in separate file
      // IMPORTANT: order of lines is given by order of hyk codes made by the PPPartBuilder (PA,PE, LU,RU, LBOK,RBOK)
      if( count++ == 0 )
        writer.append( line ).println();
      else
        m_additionalLines.add( line );
    }
  }

  private Pair<String, String>[] tweakHyks( final GafKind kind, final String code, final String[] hyks )
  {
    /* depending on mode, the tweaked codes can be mapped to repeatedly non-hyk-codes; so we use a set to eleminate duplicates */
    final Collection<Pair<String, String>> pairs = new LinkedHashSet<>();

    for( final String hyk : hyks )
    {
      final String tweakedCode = tweakCode( kind, code, hyk );
      final String tweakedHyk = tweakHyk( code, hyk );

      pairs.add( Pair.of( tweakedCode, tweakedHyk ) );
    }

    return pairs.toArray( new Pair[pairs.size()] );
  }

  private String tweakCode( final GafKind kind, final String code, final String hyk )
  {
    switch( m_hykExportMode )
    {
      case CODE:
        return code;

      case MARKER:
        /* use markers code if set, else delete existing hyk codes from code and fall back to default */
        if( hyk != EMPTY_HYK_CODE )
          return hyk;
        else if( isHykCode( code ) )
          return m_codes.getDefaultCode( kind ).getCode();
        else
          return code;

      default:
        throw new IllegalStateException();
    }
  }

  private String tweakHyk( final String code, final String hyk )
  {
    switch( m_hykExportMode )
    {
      case CODE:
        if( isHykCode( code ) )
          return code;
        else
          return EMPTY_HYK_CODE;

      case MARKER:
        return hyk;

      default:
        throw new IllegalStateException();
    }
  }

  private boolean isHykCode( final String code )
  {
    final GafCode gafCode = m_codes.getCode( code );
    if( gafCode == null )
      return false;

    final String hyk = gafCode.getHyk();
    if( StringUtils.isEmpty( hyk ) )
      return false;

    return true;
  }
}