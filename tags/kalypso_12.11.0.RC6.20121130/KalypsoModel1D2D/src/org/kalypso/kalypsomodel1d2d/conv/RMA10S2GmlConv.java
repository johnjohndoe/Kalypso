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

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.StrTokenizer;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DHelper;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsosimulationmodel.core.Assert;

/**
 * Provides algorithm to convert from a bce2d model to a 1d2d discretisation model
 *
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Patrice Congo
 * @author ig
 */
public class RMA10S2GmlConv
{
  private IRMA10SModelElementHandler m_handler;

  public static boolean VERBOSE_MODE = false;

  public static enum RESULTLINES
  {
    LINE_VA,
    LINE_VO,
    LINE_GA,
    LINE_GO
  }

  private final IProgressMonitor m_monitor;

  public RMA10S2GmlConv( final IProgressMonitor monitor )
  {
    m_monitor = monitor;
  }

  public void setRMA10SModelElementHandler( final IRMA10SModelElementHandler handler ) throws IllegalArgumentException
  {
    m_handler = handler;
  }

  public void parse( final InputStream inputStream ) throws IOException
  {
    Assert.throwIAEOnNullParam( inputStream, "inputStream" ); //$NON-NLS-1$
    parse( new InputStreamReader( inputStream ) );
  }

  private void parse( final Reader reader ) throws IOException
  {
    Assert.throwIAEOnNullParam( reader, "inputStreamReader" ); //$NON-NLS-1$

    m_handler.start();

    final LineNumberReader lnReader = new LineNumberReader( reader );

    for( String line = lnReader.readLine(); line != null; line = lnReader.readLine() )
    {
      if( m_monitor != null && m_monitor.isCanceled() )
        throw new OperationCanceledException();

      if( line.length() < 2 )
        continue;

      if( line.startsWith( "FP" ) ) //$NON-NLS-1$
        interpreteNodeLine( line );
      else if( line.startsWith( "FE" ) ) //$NON-NLS-1$
        interpreteElementLine( line );
      else if( line.startsWith( "AR" ) ) //$NON-NLS-1$
        interpreteArcLine( line );
      else if( line.startsWith( "RK" ) ) //$NON-NLS-1$
        interpreteRoughnessClassLine( line ); //$NON-NLS-1$
      else if( line.startsWith( "VA" ) ) //$NON-NLS-1$
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_VA );
      else if( line.startsWith( "VO" ) ) //$NON-NLS-1$
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_VO );
      else if( line.startsWith( "GA" ) ) //$NON-NLS-1$
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_GA );
      else if( line.startsWith( "GO" ) ) //$NON-NLS-1$
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_GO );
      else if( line.startsWith( "DA" ) ) //$NON-NLS-1$
        interpreteTimeLine( line );
      else if( line.startsWith( "TL" ) ) //$NON-NLS-1$
        interprete1d2dJunctionElement( line );
      else if( line.startsWith( "ZU" ) ) //$NON-NLS-1$
        interpreteNodeInformationLine( line );
      else if( line.startsWith( "JE" ) ) //$NON-NLS-1$
        interpreteJunctionInformationLine( line );
      else if( line.startsWith( "MM" ) ) //$NON-NLS-1$
        interpretePolynomeMinMaxLine( line );
      else if( line.startsWith( "PRA" ) ) //$NON-NLS-1$
        interpretePolynomialRangesLine( line );
      else if( line.startsWith( "AP" ) ) //$NON-NLS-1$
        interpreteSplittedPolynomialsLine( line );
      else if( line.startsWith( "PRQ" ) ) //$NON-NLS-1$
        interpretePolynomialRangesLine( line );
      else if( line.startsWith( "QP" ) ) //$NON-NLS-1$
        interpreteSplittedPolynomialsLine( line );
      else if( line.startsWith( "PRB" ) ) //$NON-NLS-1$
        interpretePolynomialRangesLine( line );
      else if( line.startsWith( "ALP" ) ) //$NON-NLS-1$
        interpreteSplittedPolynomialsLine( line );
      else if( VERBOSE_MODE )
        System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.1" ) + line ); //$NON-NLS-1$
    }

    // signal parsing stop
    m_handler.end();
  }

  private void interpreteSplittedPolynomialsLine( final String line )
  {
    final RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, ' ' );

    try
    {
      int lIntStartMaxes = 3;
      final String lStrPolyKind = lStringParser.getSub( 0 );
      final int lIntNodeId = Integer.parseInt( lStringParser.getSub( 1 ) );
      final int lIntAmountRanges = Integer.parseInt( lStringParser.getSub( 2 ) );
      final List<Double> lListPolyAreaMaxRanges = new ArrayList<>();
      Double lDoubleSlope = null;
      if( lStrPolyKind.equalsIgnoreCase( "QP" ) ) { //$NON-NLS-1$
        try
        {
          lDoubleSlope = Double.parseDouble( lStringParser.getSub( 3 ) );
          lIntStartMaxes++;
        }
        catch( final Exception e )
        {
        }
      }
      for( int i = lIntStartMaxes; i < lStringParser.getIntSepCounter(); ++i )
      {
        lListPolyAreaMaxRanges.add( Double.parseDouble( lStringParser.getSub( i ) ) );
      }
      m_handler.handle1dSplittedPolynomialsInformation( line, lStrPolyKind, lIntNodeId, lIntAmountRanges, lListPolyAreaMaxRanges, lDoubleSlope );
    }
    catch( final NumberFormatException e )
    {
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
    }

  }

  private void interpretePolynomialRangesLine( final String line )
  {
    final RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, ' ' );

    try
    {
      final String lStrPolyKind = lStringParser.getSub( 0 );
      final int lIntNodeId = Integer.parseInt( lStringParser.getSub( 1 ) );
      final int lIntAmountRanges = Integer.parseInt( lStringParser.getSub( 2 ) );
      final List<Double> lListPolyAreaMaxRanges = new ArrayList<>();

      for( int i = 3; i < lStringParser.getIntSepCounter(); ++i )
      {
        lListPolyAreaMaxRanges.add( Double.parseDouble( lStringParser.getSub( i ) ) );
      }
      m_handler.handle1dPolynomialRangesInformation( line, lStrPolyKind, lIntNodeId, lIntAmountRanges, lListPolyAreaMaxRanges );
    }
    catch( final NumberFormatException e )
    {
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
    }

  }

  private void interpretePolynomeMinMaxLine( final String line )
  {
    final RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, " \t" ); //$NON-NLS-1$
    if( lStringParser.getIntSepCounter() == 4 )
    {
      try
      {
        lStringParser.getFirstSub();
        final int id = Integer.parseInt( lStringParser.getNextSub() );
        final double min = Double.parseDouble( lStringParser.getNextSub() );
        final double max = Double.parseDouble( lStringParser.getNextSub() );
        m_handler.handle1dPolynomeMinMax( line, id, min, max );
      }
      catch( final NumberFormatException e )
      {
        m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );

  }

  // this form of parsing was used on all types of lines from the result line
  // the compiling of pattern on the fly for each call is very expensive operation,
  // so it was replaced by simple string parsing
  // with the implementation placed in private parsing class(RMA10ResultsLineSplitter)
  private void interprete1d2dJunctionElement( final String line )
  {
    // TL JunctionID, 1d-ElementID, BoundaryLineID, 1d-NodeID
    final Pattern linePattern = Pattern.compile( "TL\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)" ); //$NON-NLS-1$
    final Matcher matcher = linePattern.matcher( line );
    if( matcher.matches() )
    {
      try
      {
        final int junctionID = Integer.parseInt( matcher.group( 1 ) );
        final int element1dID = Integer.parseInt( matcher.group( 2 ) );
        final int boundaryLine2dID = Integer.parseInt( matcher.group( 3 ) );
        final int mode1dID = Integer.parseInt( matcher.group( 4 ) );

        m_handler.handleJunction( line, junctionID, element1dID, boundaryLine2dID, mode1dID );
      }
      catch( final NumberFormatException e )
      {
        m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      System.out.println( "Line will be not interpreted: " + line ); //$NON-NLS-1$
    // m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
  }

  private void interpreteTimeLine( final String line )
  {
    // TODO implement Pattern-like parsing, instead of getting line parts via string index
    if( line.length() >= 32 )
    {
      try
      {
        m_handler.handleTime( line, ResultMeta1d2dHelper.interpreteRMA10TimeLine( line ) );
      }
      catch( final NumberFormatException e )
      {
        m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );

  }

  private void interpreteNodeLine( final String line )
  {
    final RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, ' ' );

    try
    {
      final int id = Integer.parseInt( lStringParser.getSub( 1 ) );
      final double easting = Double.parseDouble( lStringParser.getSub( 2 ) );
      final double northing = Double.parseDouble( lStringParser.getSub( 3 ) );
      double elevation = NumberUtils.parseQuietDouble( lStringParser.getSub( 4 ) );

      if( elevation == KalypsoModel1D2DHelper.DOUBLE_IGNORE_VALUE )
        elevation = Double.NaN;

      if( lStringParser.getIntSepCounter() == 6 )
      {
        try
        {
          m_handler.handleNode( line, id, easting, northing, elevation, Double.parseDouble( lStringParser.getSub( 5 ) ) );
        }
        catch( final Exception e )
        {
          // FIXME???!!!
        }
      }
      else
      {
        m_handler.handleNode( line, id, easting, northing, elevation );
      }
    }
    catch( final NumberFormatException e )
    {
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
    }
  }

  private void interpreteResultLine( final String line, final RESULTLINES resultlines )
  {
    final RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, " \t" ); //$NON-NLS-1$
    if( lStringParser.getIntSepCounter() == 6 )
    {
      try
      {
        lStringParser.getFirstSub();
        final int id = Integer.parseInt( lStringParser.getNextSub() );
        final double vx = Double.parseDouble( lStringParser.getNextSub() );
        final double vy = Double.parseDouble( lStringParser.getNextSub() );
        final double depth = Double.parseDouble( lStringParser.getNextSub() );
        final double waterlevel = Double.parseDouble( lStringParser.getNextSub() );
        m_handler.handleResult( line, id, vx, vy, depth, waterlevel );
      }
      catch( final NumberFormatException e )
      {
        m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else if( lStringParser.getIntSepCounter() == 5 )
    {
      try
      {
        lStringParser.getFirstSub();
        final int id = Integer.parseInt( lStringParser.getNextSub() );
        final double veloXComponent = Double.parseDouble( lStringParser.getNextSub() );
        final double veloYComponent = Double.parseDouble( lStringParser.getNextSub() );
        final double depthComponent = Double.parseDouble( lStringParser.getNextSub() );
        m_handler.handleTimeDependentAdditionalResult( line, id, veloXComponent, veloYComponent, depthComponent, resultlines );
      }
      catch( final NumberFormatException e )
      {
        m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
  }

  private void interpreteArcLine( final String line )
  {
    final RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, " \t" ); //$NON-NLS-1$
    try
    {
      if( lStringParser.getIntSepCounter() == 7 )
      {
        lStringParser.getFirstSub();
        final int id = Integer.parseInt( lStringParser.getNextSub() );
        final int node1ID = Integer.parseInt( lStringParser.getNextSub() );
        final int node2ID = Integer.parseInt( lStringParser.getNextSub() );
        final int elementLeftID = Integer.parseInt( lStringParser.getNextSub() );
        final int elementRightID = Integer.parseInt( lStringParser.getNextSub() );
        final int middleNodeID = Integer.parseInt( lStringParser.getNextSub() );
        m_handler.handleArc( line, id, node1ID, node2ID, elementLeftID, elementRightID, middleNodeID );
      }
      else if( lStringParser.getIntSepCounter() == 6 )
      {
        {
          lStringParser.getFirstSub();
          final int id = Integer.parseInt( lStringParser.getNextSub() );
          final int node1ID = Integer.parseInt( lStringParser.getNextSub() );
          final int node2ID = Integer.parseInt( lStringParser.getNextSub() );
          final int elementLeftID = Integer.parseInt( lStringParser.getNextSub() );
          final int elementRightID = Integer.parseInt( lStringParser.getNextSub() );
          m_handler.handleArc( line, id, node1ID, node2ID, elementLeftID, elementRightID, -1 );
        }
      }
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
    }
  }

  private void interpreteNodeInformationLine( final String line )
  {
    final RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, " " ); //$NON-NLS-1$
    try
    {
      lStringParser.getFirstSub();
      final int id = Integer.parseInt( lStringParser.getNextSub() );
      final int dry = Integer.parseInt( lStringParser.getNextSub() );
      final double value1 = Double.parseDouble( lStringParser.getNextSub() );
      final double value2 = Double.parseDouble( lStringParser.getNextSub() );
      final double value3 = Double.parseDouble( lStringParser.getNextSub() );
      final double value4 = Double.parseDouble( lStringParser.getNextSub() );
      m_handler.handleNodeInformation( line, id, dry, value1, value2, value3, value4 );
    }
    catch( final NumberFormatException e )
    {
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
    }
  }

  private void interpreteJunctionInformationLine( final String line )
  {
    // JE 9*I10
    // JE 1 2 333 444 555 666 777 4 3 2
    // JE
    // -number of junction element
    // - nodeID1
    // - nodeID2
    // - nodeID3
    // - nodeID4 (opt.)
    // - nodeID5 (opt.)
    // - nodeID6 (opt.)
    // - nodeID7 (opt.)
    // - nodeID8 (opt.)

    final RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, ' ' );

    try
    {
      final int junctionId = Integer.parseInt( lStringParser.getSub( 1 ) );
      final List<Integer> junctionNodeIDList = new ArrayList<>();

      for( int i = 2; i < lStringParser.getIntSepCounter(); ++i )
      {
        junctionNodeIDList.add( Integer.parseInt( lStringParser.getSub( i ) ) );
      }
      m_handler.handle1dJunctionInformation( line, junctionId, junctionNodeIDList );
    }
    catch( final NumberFormatException e )
    {
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
    }
  }

  private void interpreteElementLine( final String line )
  {
    final RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, " " ); //$NON-NLS-1$
    final int tokenCount = lStringParser.getIntSepCounter();

    if( tokenCount < 2 )
    {
      m_handler.handleError( line, EReadError.LINE_TOO_SHORT );
      return;
    }

    /* skip element name */
    lStringParser.getFirstSub();

    try
    {
      if( tokenCount >= 5 )
      {
        final int id = Integer.parseInt( lStringParser.getNextSub() );
        final int currentRougthnessClassID = Integer.parseInt( lStringParser.getNextSub() );
        final int previousRoughnessClassID = Integer.parseInt( lStringParser.getNextSub() );
        final int eleminationNumber = Integer.parseInt( lStringParser.getNextSub() );

        m_handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, eleminationNumber );
      }
      else if( tokenCount == 4 )
      {
        final int id = Integer.parseInt( lStringParser.getNextSub() );
        final int currentRougthnessClassID = Integer.parseInt( lStringParser.getNextSub() );
        final int previousRoughnessClassID = Integer.parseInt( lStringParser.getNextSub() );

        m_handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, -1 );
      }
      else if( tokenCount == 3 )
      {
        final int id = Integer.parseInt( lStringParser.getNextSub() );
        final int currentRougthnessClassID = Integer.parseInt( lStringParser.getNextSub() );

        m_handler.handleElement( line, id, currentRougthnessClassID, -1, -1 );
      }
      else if( tokenCount == 2 )
      {
        final int id = Integer.parseInt( lStringParser.getNextSub() );

        m_handler.handleElement( line, id, -1, -1, -1 );
      }
    }
    catch( final NumberFormatException e )
    {
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
    }
  }

  private void interpreteRoughnessClassLine( final String line )
  {
    if( line.length() < 8 )
      // TODO: error handling
      return;

    final String id = line.substring( 2, 7 ).trim();
    if( StringUtils.isBlank( id ) )
      // TODO: error handling
      return;

    final StrTokenizer tokenizer = new StrTokenizer( line.substring( 7 ) );

    final String[] tokenArray = tokenizer.getTokenArray();
    if( tokenArray.length != 6 )
      // TODO: error handling
      return;

    final String label = tokenArray[0];
    // FIXME: what is the meaning of the other parameters?
    m_handler.handleRoughness( id, label );
  }

}
