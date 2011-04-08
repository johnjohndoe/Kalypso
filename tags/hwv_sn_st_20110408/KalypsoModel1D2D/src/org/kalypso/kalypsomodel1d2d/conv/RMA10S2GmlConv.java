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
import java.util.Arrays;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
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

  private final int m_monitorStep;

  // all of the patterns are left here as comments to show the format of parsed strings.

  //  private static final Pattern NODE_LINE_PATTERN = Pattern.compile( "FP\\s*([0-9]+)\\s+([0-9]+\\.[0-9]*)\\s+([0-9]+\\.[0-9]*)\\s+([\\+\\-]?[0-9]+\\.[0-9]*).*" ); //$NON-NLS-1$
  //
  //  private static final Pattern ARC_LINE_PATTERN = Pattern.compile( "AR\\s*([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+).*" ); //$NON-NLS-1$
  //
  //  private static final Pattern ARC_LINE_PATTERN2 = Pattern.compile( "AR\\s*([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+).*" ); //$NON-NLS-1$
  //
  //  private static final Pattern ELEMENT_LINE_PATTERN_4 = Pattern.compile( "FE\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([0-9]+).*" ); //$NON-NLS-1$
  //
  //  private static final Pattern ELEMENT_LINE_PATTERN_3 = Pattern.compile( "FE\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+).*" ); //$NON-NLS-1$
  //
  //  private static final Pattern ELEMENT_LINE_PATTERN_2 = Pattern.compile( "FE\\s*([0-9]+)\\s+([0-9]+).*" ); //$NON-NLS-1$
  //
  //  private static final Pattern ELEMENT_LINE_PATTERN_1 = Pattern.compile( "FE\\s*([0-9]+).*" ); //$NON-NLS-1$
  //
  //  private static final Pattern ZU_LINE_PATTERN = Pattern.compile( "ZU\\s*([0-9]+)\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+\\.[0-9]*[E]?[\\+\\-]?[0-9]*)\\s+([\\+\\-]?[0-9]+\\.[0-9]*[E]?[\\+\\-]?[0-9]*)\\s+([\\+\\-]?[0-9]+\\.[0-9]*[E]?[\\+\\-]?[0-9]*)\\s+([\\+\\-]?[0-9]+\\.[0-9]*[E]?[\\+\\-]?[0-9]*)\\s*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineFP = Pattern.compile( "FP.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineFE = Pattern.compile( "FE.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineAR = Pattern.compile( "AR.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineRK = Pattern.compile( "RK.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineVA = Pattern.compile( "VA.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineVO = Pattern.compile( "VO.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineGA = Pattern.compile( "GA.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineGO = Pattern.compile( "GO.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineDA = Pattern.compile( "DA.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineTL = Pattern.compile( "TL.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineZU = Pattern.compile( "ZU.*" ); //$NON-NLS-1$
  //
  //  private static final Pattern lineJE = Pattern.compile( "JE.*" ); //$NON-NLS-1$

  public RMA10S2GmlConv( final IProgressMonitor monitor, final int numberOfLinesToProcess )
  {
    m_monitorStep = numberOfLinesToProcess / 100;
    if( monitor == null )
      m_monitor = new NullProgressMonitor();
    else
      m_monitor = monitor;
  }

  public RMA10S2GmlConv( )
  {
    m_monitor = new NullProgressMonitor();
    m_monitorStep = -1;
  }

  public void parse( final InputStream inputStream ) throws IllegalStateException, IOException
  {
    Assert.throwIAEOnNullParam( inputStream, "inputStream" ); //$NON-NLS-1$
    parse( new InputStreamReader( inputStream ) );
  }

  public void parse( final Reader reader ) throws IllegalStateException, IOException
  {
    Assert.throwIAEOnNullParam( reader, "inputStreamReader" ); //$NON-NLS-1$

    m_monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.0" ), 100 );//$NON-NLS-1$
    m_handler.start();

    int numberOfLinesProcessed = 0;
    final boolean traceProgress = !(m_monitor instanceof NullProgressMonitor) && (m_monitorStep > 0);
    final LineNumberReader lnReader = new LineNumberReader( reader );
    for( String line = lnReader.readLine(); line != null; line = lnReader.readLine() )
    {
      if( m_monitor.isCanceled() )
        return;
      if( traceProgress && (++numberOfLinesProcessed == m_monitorStep) )
      {
        numberOfLinesProcessed = 0;
        m_monitor.worked( 1 );
      }
      if( line.length() < 2 )
        continue;
      if( line.startsWith( "FP" ) )
        interpreteNodeLine( line );
      if( line.startsWith( "FE" ) )
        interpreteElementLine( line );
      if( line.startsWith( "AR" ) )
        interpreteArcLine( line );
      if( line.startsWith( "RK" ) )
      {
        // still not implemented
      }
      if( line.startsWith( "VA" ) )
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_VA );
      if( line.startsWith( "VO" ) )
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_VO );
      if( line.startsWith( "GA" ) )
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_GA );
      if( line.startsWith( "GO" ) )
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_GO );
      if( line.startsWith( "DA" ) )
        interpreteTimeLine( line );
      if( line.startsWith( "TL" ) )
        interprete1d2dJunctionElement( line );
      if( line.startsWith( "ZU" ) )
        interpreteNodeInformationLine( line );
      if( line.startsWith( "JE" ) )
        interpreteJunctionInformationLine( line );
      
      else if( VERBOSE_MODE )
        System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.1" ) + line ); //$NON-NLS-1$
    }

    // signal parsing stop
    m_handler.end();

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
      System.out.println( "Line will be not interpreted: " + line );
    // m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
  }

  private void interpreteTimeLine( final String line )
  {
    // TODO implement Pattern-like parsing, instead of getting line parts via string index
    if( line.length() >= 32 )
    {
      try
      {
//        final String yearString = line.substring( 6, 13 ).trim();
//        final String hourString = line.substring( 18, 32 ).trim();
//
//        final int year = Integer.parseInt( yearString );
//        final BigDecimal hours = new BigDecimal( hourString );
//
//        // REMARK: we read the calculation core time with the time zone, as defined in Kalypso Preferences
//        final Calendar calendar = Calendar.getInstance( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
//        calendar.clear();
//        calendar.set( year, 0, 1 );
//
//        final BigDecimal wholeHours = hours.setScale( 0, BigDecimal.ROUND_DOWN );
//        final BigDecimal wholeMinutes = hours.subtract( wholeHours ).multiply( new BigDecimal( "60" ) ); //$NON-NLS-1$
//
//        calendar.add( Calendar.HOUR, wholeHours.intValue() );
//        calendar.add( Calendar.MINUTE, wholeMinutes.intValue() );

//        m_handler.handleTime( line, calendar.getTime() );
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

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#setRMA10SModelElementHandler(org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler)
   */
  public void setRMA10SModelElementHandler( final IRMA10SModelElementHandler handler ) throws IllegalArgumentException
  {
    m_handler = handler;
  }

  private void interpreteNodeLine( final String line )
  {
    RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, ' ' );
    {
      try
      {
        //other way to use this parser to get the custom substrings  
        // lStringParser.getFirstSub();
        // final int id = Integer.parseInt( lStringParser.getNextSub() );
        // final double easting = Double.parseDouble( lStringParser.getNextSub() );
        // final double northing = Double.parseDouble( lStringParser.getNextSub() );
        // double elevation = Double.parseDouble( lStringParser.getNextSub() );
        final int id = Integer.parseInt( lStringParser.getSub( 1 ) );
        final double easting = Double.parseDouble( lStringParser.getSub( 2 ) );
        final double northing = Double.parseDouble( lStringParser.getSub( 3 ) );
        double elevation = Double.NaN;
        try{
          elevation = Double.parseDouble( lStringParser.getSub( 4 ) );
        }
        catch (Exception e) {
          // also allow the NaN in result file
        }
        // TODO: the value '-9999' represents the NODATA-value, should be discussed
        if( elevation == -9999 )
          elevation = Double.NaN;
        m_handler.handleNode( line, id, easting, northing, elevation );
      }
      catch( final NumberFormatException e )
      {
        m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
      }
    }
  }

  private void interpreteResultLine( final String line, final RESULTLINES resultlines )
  {
    RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, " \t" );
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
    RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, " \t" );
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
    RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, " " );
    try
    {
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

    RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, ' ' );

    try
    {
      final int junctionId = Integer.parseInt( lStringParser.getSub( 1 ) );
      final List<Integer> junctionNodeIDList = new ArrayList<Integer>();

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

  // private void interpreteFlowResistanceLine( final String line )
  // {
  // Matcher matcher = null;
  // final Pattern fourParamLinePattern = Pattern.compile(
  // "FR\\s*([0-9]+)\\s+([0-9]+\\.[0-9]*)\\s+([0-9]+\\.[0-9]*)\\s+([0-9]+\\.[0-9]*)\\s+([0-9]+\\.[0-9]*)\\s*" );
  // //$NON-NLS-1$
  // matcher = fourParamLinePattern.matcher( line );
  // try
  // {
  // if( matcher.matches() )
  // {
  // final int id = Integer.parseInt( matcher.group( 1 ) );
  // final double combinedLambda = Double.parseDouble( matcher.group( 2 ) );
  // final double soilLambda = Double.parseDouble( matcher.group( 3 ) );
  // final double vegetationLambda = Double.parseDouble( matcher.group( 4 ) );
  // m_handler.handleFlowResitance( line, id, combinedLambda, soilLambda, vegetationLambda );
  // }
  // else
  // m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
  // }
  // catch( final NumberFormatException e )
  // {
  // m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
  // }
  // }

  private void interpreteElementLine( final String line )
  {
    RMA10ResultsLineSplitter lStringParser = new RMA10ResultsLineSplitter( line, " " );
    try
    {
      if( lStringParser.getIntSepCounter() >= 5 )
      {
        lStringParser.getFirstSub();
        final int id = Integer.parseInt( lStringParser.getNextSub() );
        final int currentRougthnessClassID = Integer.parseInt( lStringParser.getNextSub() );
        final int previousRoughnessClassID = Integer.parseInt( lStringParser.getNextSub() );
        final int eleminationNumber = Integer.parseInt( lStringParser.getNextSub() );
        // final int weirUpStreamNode = Integer.parseInt( lStringParser.getNextSub() ); // opt.
        // final int calcId = Integer.parseInt( lStringParser.getNextSub() );

        m_handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, eleminationNumber );
      }
      else
      {
        if( lStringParser.getIntSepCounter() == 4 )
        {
          lStringParser.getFirstSub();
          final int id = Integer.parseInt( lStringParser.getNextSub() );
          final int currentRougthnessClassID = Integer.parseInt( lStringParser.getNextSub() );
          final int previousRoughnessClassID = Integer.parseInt( lStringParser.getNextSub() );
          m_handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, -1 );
        }
        else
        {
          if( lStringParser.getIntSepCounter() == 3 )
          {
            lStringParser.getFirstSub();
            final int id = Integer.parseInt( lStringParser.getNextSub() );
            final int currentRougthnessClassID = Integer.parseInt( lStringParser.getNextSub() );
            m_handler.handleElement( line, id, currentRougthnessClassID, -1, -1 );
          }
          else
          {
            if( lStringParser.getIntSepCounter() == 2 )
            {
              lStringParser.getFirstSub();
              final int id = Integer.parseInt( lStringParser.getNextSub() );
              m_handler.handleElement( line, id, -1, -1, -1 );
            }
            else
              m_handler.handleError( line, EReadError.LINE_TOO_SHORT );
          }
        }
      }
    }
    catch( final NumberFormatException e )
    {
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
    }
  }

  /**
   * simple implementation of string splitter based on {@link java.util.StringTokenizer} or on very simple parser of
   * string without strong validation and error handling.
   * 
   * the goal of this class is to out source the string handling and to implement some faster result parsing.
   * 
   */
  class RMA10ResultsLineSplitter
  {

    private boolean m_boolUseSimpleMethod = true;

    private final String m_strFullString;

    private final char m_charSeparator;

    private boolean m_boolSeparator = false;

    private int m_intStrLen = 0;

    private int m_intSepCounter = 0;

    private char[] m_charStringAsArray;

    private int[] m_intPositionsAsArray;

    StringTokenizer m_stringTokenizer;

    RMA10ResultsLineSplitter( final String pStrFull, final char pCharSeparator )
    {
      m_strFullString = pStrFull;
      init();
      m_charSeparator = pCharSeparator;
      m_intPositionsAsArray = new int[m_intStrLen];
      Arrays.fill( m_intPositionsAsArray, m_intStrLen );
      parseLine();
    }

    RMA10ResultsLineSplitter( final String pStrFull, final String pStrSeparators )
    {
      m_strFullString = pStrFull;
      init();
      m_charSeparator = ' ';
      m_stringTokenizer = new StringTokenizer( pStrFull, pStrSeparators );
      m_boolUseSimpleMethod = false;
    }

    private void init( )
    {
      m_intStrLen = m_strFullString.length();
      m_charStringAsArray = m_strFullString.toCharArray();
    }

    private void parseLine( )
    {
      m_boolSeparator = true;
      int j = 0;
      for( int i = 0; i < m_intStrLen; ++i )
      {
        if( m_charStringAsArray[i] == m_charSeparator )
        {
          if( !m_boolSeparator )
          {
            m_boolSeparator = true;
          }
        }
        else
        {
          if( m_boolSeparator )
          {
            m_boolSeparator = false;
            m_intPositionsAsArray[j++] = i;
            m_intSepCounter++;
          }
        }
      }
    }

    public final int getIntSepCounter( )
    {
      if( m_boolUseSimpleMethod )
        return m_intSepCounter;
      return m_stringTokenizer.countTokens();
    }

    public String getSub( final int pIntPos )
    {
      return m_strFullString.substring( m_intPositionsAsArray[pIntPos], m_intPositionsAsArray[pIntPos + 1] ).trim();
    }

    public String getFirstSub( )
    {
      return m_stringTokenizer.nextToken();
    }

    public String getNextSub( )
    {
      if( m_stringTokenizer.hasMoreTokens() )
      {
        String lStrRes = m_stringTokenizer.nextToken();
        return lStrRes;
      }
      return "";
    }

  }
}
