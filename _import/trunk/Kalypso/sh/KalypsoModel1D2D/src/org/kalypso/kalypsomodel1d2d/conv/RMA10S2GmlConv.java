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
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Provides algorithm to convert from a bce2d model to a 1d2d discretisation model
 *
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Patrice Congo
 */
public class RMA10S2GmlConv
{
  private IRMA10SModelElementHandler m_handler;

  public static boolean VERBOSE_MODE = false;

  public static enum RESULTLINES  {    LINE_VA,    LINE_VO,    LINE_GA,    LINE_GO  }

  private final IProgressMonitor m_monitor;

  private final int m_monitorStep;

  //  private static final Pattern NODE_LINE_PATTERN = Pattern.compile( "FP\\s*([0-9]+)\\s+([0-9]+\\.[0-9]*)\\s+([0-9]+\\.[0-9]*)\\s+([\\+\\-]?[0-9]+\\.[0-9]*).*" ); //$NON-NLS-1$

  private static final Pattern ARC_LINE_PATTERN = Pattern.compile( "AR\\s*([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+).*" ); //$NON-NLS-1$

  private static final Pattern ARC_LINE_PATTERN2 = Pattern.compile( "AR\\s*([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+).*" ); //$NON-NLS-1$

  private static final Pattern ELEMENT_LINE_PATTERN_4 = Pattern.compile( "FE\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([0-9]+).*" ); //$NON-NLS-1$

  private static final Pattern ELEMENT_LINE_PATTERN_3 = Pattern.compile( "FE\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+).*" ); //$NON-NLS-1$

  private static final Pattern ELEMENT_LINE_PATTERN_2 = Pattern.compile( "FE\\s*([0-9]+)\\s+([0-9]+).*" ); //$NON-NLS-1$

  private static final Pattern ELEMENT_LINE_PATTERN_1 = Pattern.compile( "FE\\s*([0-9]+).*" ); //$NON-NLS-1$

  private static final Pattern ZU_LINE_PATTERN = Pattern.compile( "ZU\\s*([0-9]+)\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+\\.[0-9]*[E]?[\\+\\-]?[0-9]*)\\s+([\\+\\-]?[0-9]+\\.[0-9]*[E]?[\\+\\-]?[0-9]*)\\s+([\\+\\-]?[0-9]+\\.[0-9]*[E]?[\\+\\-]?[0-9]*)\\s+([\\+\\-]?[0-9]+\\.[0-9]*[E]?[\\+\\-]?[0-9]*)\\s*" ); //$NON-NLS-1$

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
    final Pattern lineFP = Pattern.compile( "FP.*" ); //$NON-NLS-1$
    final Pattern lineFE = Pattern.compile( "FE.*" ); //$NON-NLS-1$
    final Pattern lineAR = Pattern.compile( "AR.*" ); //$NON-NLS-1$
    final Pattern lineRK = Pattern.compile( "RK.*" ); //$NON-NLS-1$
    final Pattern lineVA = Pattern.compile( "VA.*" ); //$NON-NLS-1$
    final Pattern lineVO = Pattern.compile( "VO.*" ); //$NON-NLS-1$
    final Pattern lineGA = Pattern.compile( "GA.*" ); //$NON-NLS-1$
    final Pattern lineGO = Pattern.compile( "GO.*" ); //$NON-NLS-1$
    final Pattern lineDA = Pattern.compile( "DA.*" ); //$NON-NLS-1$
    final Pattern lineTL = Pattern.compile( "TL.*" ); //$NON-NLS-1$
    final Pattern lineZU = Pattern.compile( "ZU.*" ); //$NON-NLS-1$
    final Pattern lineJE = Pattern.compile( "JE.*" ); //$NON-NLS-1$

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
      if( lineFP.matcher( line ).matches() )
        interpreteNodeLine( line );
      else if( lineFE.matcher( line ).matches() )
        interpreteElementLine( line );
      else if( lineAR.matcher( line ).matches() )
        interpreteArcLine( line );
      else if( lineRK.matcher( line ).matches() )
      {
        // still not implemented
      }
      else if( lineVA.matcher( line ).matches() )
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_VA );
      else if( lineVO.matcher( line ).matches() )
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_VO );
      else if( lineGA.matcher( line ).matches() )
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_GA );
      else if( lineGO.matcher( line ).matches() )
        interpreteResultLine( line, RMA10S2GmlConv.RESULTLINES.LINE_GO );
      else if( lineDA.matcher( line ).matches() )
        interpreteTimeLine( line );
      else if( lineTL.matcher( line ).matches() )
        interprete1d2dJunctionElement( line );
      else if( lineZU.matcher( line ).matches() )
        interpreteNodeInformationLine( line );
      else if( lineJE.matcher( line ).matches() )
        interpreteJunctionInformationLine( line );
      else if( VERBOSE_MODE )
        System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.1" ) + line ); //$NON-NLS-1$
    }

    // signal parsing stop
    m_handler.end();

  }

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
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
  }

  private void interpreteTimeLine( final String line )
  {
    // TODO implement Pattern-like parsing, instead of getting line parts via string index
    if( line.length() >= 32 )
    {
      try
      {
        final String yearString = line.substring( 6, 13 ).trim();
        final String hourString = line.substring( 18, 32 ).trim();

        final int year = Integer.parseInt( yearString );
        final BigDecimal hours = new BigDecimal( hourString );

        // REMARK: we read the calculation core time with the time zone, as defined in Kalypso Preferences
        final Calendar calendar = Calendar.getInstance( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
        calendar.clear();
        calendar.set( year, 0, 1 );

        final BigDecimal wholeHours = hours.setScale( 0, BigDecimal.ROUND_DOWN );
        final BigDecimal wholeMinutes = hours.subtract( wholeHours ).multiply( new BigDecimal( "60" ) ); //$NON-NLS-1$

        calendar.add( Calendar.HOUR, wholeHours.intValue() );
        calendar.add( Calendar.MINUTE, wholeMinutes.intValue() );

        m_handler.handleTime( line, calendar.getTime() );
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
    final String[] nodeLineStrings = line.split( "[ ]+" ); //$NON-NLS-1$

    if( nodeLineStrings[0].equals( "FP" ) ) //$NON-NLS-1$
    {
      try
      {
        final int id = Integer.parseInt( nodeLineStrings[1] );
        final double easting = Double.parseDouble( nodeLineStrings[2] );
        final double northing = Double.parseDouble( nodeLineStrings[3] );
        double elevation = Double.parseDouble( nodeLineStrings[4] );
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
    else
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
  }

  private void interpreteResultLine( final String line, final RESULTLINES resultlines )
  {
    final String[] strings = line.split( "\\s+" ); //$NON-NLS-1$
    if( strings.length == 6 )
    {
      try
      {
        final int id = Integer.parseInt( strings[1] );
        final double vx = Double.parseDouble( strings[2] );
        final double vy = Double.parseDouble( strings[3] );
        final double depth = Double.parseDouble( strings[4] );
        final double waterlevel = Double.parseDouble( strings[5] );
        m_handler.handleResult( line, id, vx, vy, depth, waterlevel );
      }
      catch( final NumberFormatException e )
      {
        m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else if( strings.length == 5 )
    {
      try
      {
        final int id = Integer.parseInt( strings[1] );
        final double veloXComponent = Double.parseDouble( strings[2] );
        final double veloYComponent = Double.parseDouble( strings[3] );
        final double depthComponent = Double.parseDouble( strings[4] );
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
    Matcher matcher = ARC_LINE_PATTERN.matcher( line );
    try
    {
      if( matcher.matches() )
      {
        final int id = Integer.parseInt( matcher.group( 1 ) );
        final int node1ID = Integer.parseInt( matcher.group( 2 ) );
        final int node2ID = Integer.parseInt( matcher.group( 3 ) );
        final int elementLeftID = Integer.parseInt( matcher.group( 4 ) );
        final int elementRightID = Integer.parseInt( matcher.group( 5 ) );
        final int middleNodeID = Integer.parseInt( matcher.group( 6 ) );
        m_handler.handleArc( line, id, node1ID, node2ID, elementLeftID, elementRightID, middleNodeID );
      }
      else
      {
        matcher = ARC_LINE_PATTERN2.matcher( line );
        if( matcher.matches() )
        {
          final int id = Integer.parseInt( matcher.group( 1 ) );
          final int node1ID = Integer.parseInt( matcher.group( 2 ) );
          final int node2ID = Integer.parseInt( matcher.group( 3 ) );
          final int elementLeftID = Integer.parseInt( matcher.group( 4 ) );
          final int elementRightID = Integer.parseInt( matcher.group( 5 ) );
          m_handler.handleArc( line, id, node1ID, node2ID, elementLeftID, elementRightID, -1 );
        }
        else
          m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    catch( final NumberFormatException e )
    {
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
    }
  }

  private void interpreteNodeInformationLine( final String line )
  {
    final Matcher matcher = ZU_LINE_PATTERN.matcher( line );
    try
    {
      if( matcher.matches() )
      {
        final int id = Integer.parseInt( matcher.group( 1 ) );
        final int dry = Integer.parseInt( matcher.group( 2 ) );
        final double value1 = Double.parseDouble( matcher.group( 3 ) );
        final double value2 = Double.parseDouble( matcher.group( 4 ) );
        final double value3 = Double.parseDouble( matcher.group( 5 ) );
        final double value4 = Double.parseDouble( matcher.group( 6 ) );
        m_handler.handleNodeInformation( line, id, dry, value1, value2, value3, value4 );
      }
      else
        m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
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

    // TODO: implementation not very nice. right now it only works with junctions up to 5 nodes!

    final Pattern junctionLinePattern3 = Pattern.compile( "JE\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*" ); //$NON-NLS-1$
    final Pattern junctionLinePattern4 = Pattern.compile( "JE\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*" ); //$NON-NLS-1$
    final Pattern junctionLinePattern5 = Pattern.compile( "JE\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*" ); //$NON-NLS-1$

    try
    {
      Matcher matcher = junctionLinePattern3.matcher( line );
      if( matcher.matches() )
      {
        final int junctionId = Integer.parseInt( matcher.group( 1 ) );
        final List<Integer> junctionNodeIDList = new ArrayList<Integer>();

        final Integer nodeID1 = Integer.parseInt( matcher.group( 2 ) );
        final Integer nodeID2 = Integer.parseInt( matcher.group( 3 ) );
        final Integer nodeID3 = Integer.parseInt( matcher.group( 4 ) );

        junctionNodeIDList.add( nodeID1 );
        junctionNodeIDList.add( nodeID2 );
        junctionNodeIDList.add( nodeID3 );

        m_handler.handle1dJunctionInformation( line, junctionId, junctionNodeIDList );
      }
      else
      {
        matcher = junctionLinePattern4.matcher( line );

        if( matcher.matches() )
        {
          final int junctionId = Integer.parseInt( matcher.group( 1 ) );
          final List<Integer> junctionNodeIDList = new ArrayList<Integer>();

          final Integer nodeID1 = Integer.parseInt( matcher.group( 2 ) );
          final Integer nodeID2 = Integer.parseInt( matcher.group( 3 ) );
          final Integer nodeID3 = Integer.parseInt( matcher.group( 4 ) );
          final Integer nodeID4 = Integer.parseInt( matcher.group( 5 ) );

          junctionNodeIDList.add( nodeID1 );
          junctionNodeIDList.add( nodeID2 );
          junctionNodeIDList.add( nodeID3 );
          junctionNodeIDList.add( nodeID4 );

          m_handler.handle1dJunctionInformation( line, junctionId, junctionNodeIDList );
        }
        else
        {
          matcher = junctionLinePattern5.matcher( line );
          if( matcher.matches() )
          {
            final int junctionId = Integer.parseInt( matcher.group( 1 ) );
            final List<Integer> junctionNodeIDList = new ArrayList<Integer>();

            final Integer nodeID1 = Integer.parseInt( matcher.group( 2 ) );
            final Integer nodeID2 = Integer.parseInt( matcher.group( 3 ) );
            final Integer nodeID3 = Integer.parseInt( matcher.group( 4 ) );
            final Integer nodeID4 = Integer.parseInt( matcher.group( 5 ) );
            final Integer nodeID5 = Integer.parseInt( matcher.group( 6 ) );

            junctionNodeIDList.add( nodeID1 );
            junctionNodeIDList.add( nodeID2 );
            junctionNodeIDList.add( nodeID3 );
            junctionNodeIDList.add( nodeID4 );
            junctionNodeIDList.add( nodeID5 );

            m_handler.handle1dJunctionInformation( line, junctionId, junctionNodeIDList );
          }
        }
      }
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
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
    Matcher matcher = null;
    matcher = ELEMENT_LINE_PATTERN_4.matcher( line );
    try
    {
      if( matcher.matches() )
      {
        final int id = Integer.parseInt( matcher.group( 1 ) );
        final int currentRougthnessClassID = Integer.parseInt( matcher.group( 2 ) );
        final int previousRoughnessClassID = Integer.parseInt( matcher.group( 3 ) );
        final int eleminationNumber = Integer.parseInt( matcher.group( 4 ) );
        // final int weirUpStreamNode = Integer.parseInt( matcher.group( 5 ) ); // opt.
        // final int calcId = Integer.parseInt( matcher.group( 6 ) );

        m_handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, eleminationNumber );
      }
      else
      {
        matcher = ELEMENT_LINE_PATTERN_3.matcher( line );
        if( matcher.matches() )
        {
          final int id = Integer.parseInt( matcher.group( 1 ) );
          final int currentRougthnessClassID = Integer.parseInt( matcher.group( 2 ) );
          final int previousRoughnessClassID = Integer.parseInt( matcher.group( 3 ) );
          m_handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, -1 );
        }
        else
        {
          matcher = ELEMENT_LINE_PATTERN_2.matcher( line );
          if( matcher.matches() )
          {
            final int id = Integer.parseInt( matcher.group( 1 ) );
            final int currentRougthnessClassID = Integer.parseInt( matcher.group( 2 ) );
            m_handler.handleElement( line, id, currentRougthnessClassID, -1, -1 );
          }
          else
          {
            matcher = ELEMENT_LINE_PATTERN_1.matcher( line );
            if( matcher.matches() )
            {
              final int id = Integer.parseInt( matcher.group( 1 ) );
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

}
