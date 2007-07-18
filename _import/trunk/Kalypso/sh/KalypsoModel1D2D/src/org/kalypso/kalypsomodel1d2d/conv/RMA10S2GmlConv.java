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
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;

/**
 * Provides algorithm to convert from a bce2d model to a 1d2d discretisation model
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Patrice Congo
 */
public class RMA10S2GmlConv implements IRMA10SModelReader
{
  private IRMA10SModelElementHandler m_handler;

  public static boolean verboseMode = false;

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#parse(java.io.InputStream)
   */
  public void parse( final InputStream inputStream ) throws IllegalStateException, IOException
  {
    Assert.throwIAEOnNullParam( inputStream, "inputStream" );
    this.parse( new InputStreamReader( inputStream ) );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#parse(java.io.InputStreamReader)
   */
  public void parse( final Reader reader ) throws IllegalStateException, IOException
  {
    Assert.throwIAEOnNullParam( reader, "inputStreamReader" );
    final LineNumberReader lnr = new LineNumberReader( reader );
    final Pattern lineFP = Pattern.compile( "FP.*" );
    final Pattern lineFE = Pattern.compile( "FE.*" );
    final Pattern lineAR = Pattern.compile( "AR.*" );
    final Pattern lineRK = Pattern.compile( "RK.*" );
    final Pattern lineVA = Pattern.compile( "VA.*" );
    final Pattern lineDA = Pattern.compile( "DA.*" );
    final Pattern lineTL = Pattern.compile( "TL.*" );

    // signal parsing start
    m_handler.start();

    for( String line = lnr.readLine(); line != null; line = lnr.readLine() )
    {
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
        interpreteResultLine( line );
      else if( lineDA.matcher( line ).matches() )
        interpreteTimeLine( line );
      else if( lineTL.matcher( line ).matches() )
        interprete1d2dJunctionElement( line );
      else
        if( verboseMode )
          System.out.println( "Unsupported section:" + line );
    }

    // signal parsing stop
    m_handler.end();

  }

  private void interprete1d2dJunctionElement( final String line )
  {
 // TL JunctionID, 1d-ElementID, BoundaryLineID, 1d-NodeID
    final Pattern linePattern = Pattern.compile( "TL\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)" );
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
        m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );
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
        final double hours = Double.parseDouble( hourString );

        final Calendar calendar = new GregorianCalendar( TimeZone.getTimeZone( "UTM" ) );
        calendar.clear();
        calendar.set( year, 0, 1 );

        final int wholeHours = (int) Math.floor( hours );
        final int minutesInHours = (int) (hours - wholeHours);
        final int wholeMinutes = (int) Math.floor( minutesInHours * 60 );

        calendar.add( Calendar.HOUR, wholeHours );
        calendar.add( Calendar.MINUTE, wholeMinutes );

        m_handler.handleTime( line, calendar.getTime() );
      }
      catch( final NumberFormatException e )
      {
        m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#setModelElementIDProvider(org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider)
   */
  @Deprecated
  public void setModelElementIDProvider( final IModelElementIDProvider idProvider ) throws IllegalArgumentException
  {
    // this.idProvider=idProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#setRMA10SModelElementHandler(org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler)
   */
  public void setRMA10SModelElementHandler( final IRMA10SModelElementHandler handler ) throws IllegalArgumentException
  {
    m_handler = handler;
  }

  static public void toDiscretisationModel( final InputStream rma10sModelInput, final IFEDiscretisationModel1d2d targetModel, final IPositionProvider positionProvider, final IModelElementIDProvider idProvider ) throws IllegalStateException, IOException
  {
    final IRMA10SModelReader reader = new RMA10S2GmlConv();
    final IRMA10SModelElementHandler handler = new DiscretisationModel1d2dHandler( targetModel, positionProvider, idProvider );
//    reader.setModelElementIDProvider( idProvider );
    reader.setRMA10SModelElementHandler( handler );
    reader.parse( rma10sModelInput );
  }

  private void interpreteNodeLine( final String line )
  {
    final Pattern linePattern = Pattern.compile( "FP\\s*([0-9]+)\\s+([0-9]+\\.[0-9]*)\\s+([0-9]+\\.[0-9]*)\\s+([\\+\\-]?[0-9]+\\.[0-9]*).*" );
    final Matcher matcher = linePattern.matcher( line );
    if( matcher.matches() )
    {
      try
      {
        final int id = Integer.parseInt( matcher.group( 1 ) );
        final double easting = Double.parseDouble( matcher.group( 2 ) );
        final double northing = Double.parseDouble( matcher.group( 3 ) );
        double elevation = Double.parseDouble( matcher.group( 4 ) );
        // TODO: the value '-9999' represents the NODATA-value, should be discussed
        if( elevation == -9999 )
          elevation = Double.NaN;
        m_handler.handleNode( line, id, easting, northing, elevation );
      }
      catch( final NumberFormatException e )
      {
        m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );
  }

  private void interpreteResultLine( final String line )
  {
    final String[] strings = line.split( "\\s+" );
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
        m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );
  }

  private void interpreteArcLine( final String line )
  {
    final Pattern middleNodePattern = Pattern.compile( "AR\\s*([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+).*" );
    Matcher matcher = middleNodePattern.matcher( line );
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
        final Pattern noMiddleNodePattern = Pattern.compile( "AR\\s*([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+).*" );
        matcher = noMiddleNodePattern.matcher( line );
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
          m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    catch( final NumberFormatException e )
    {
      m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );
    }
  }

  private void interpreteElementLine( final String line )
  {
    Matcher matcher = null;
    final Pattern fourParamLinePattern = Pattern.compile( "FE\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([0-9]+).*" );
    matcher = fourParamLinePattern.matcher( line );
    try
    {
      if( matcher.matches() )
      {
        final int id = Integer.parseInt( matcher.group( 1 ) );
        final int currentRougthnessClassID = Integer.parseInt( matcher.group( 2 ) );
        final int previousRoughnessClassID = Integer.parseInt( matcher.group( 3 ) );
        final int eleminationNumber = Integer.parseInt( matcher.group( 4 ) );
        m_handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, eleminationNumber );
      }
      else
      {
        final Pattern threeParamLinePattern = Pattern.compile( "FE\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+).*" );
        matcher = threeParamLinePattern.matcher( line );
        if( matcher.matches() )
        {
          final int id = Integer.parseInt( matcher.group( 1 ) );
          final int currentRougthnessClassID = Integer.parseInt( matcher.group( 2 ) );
          final int previousRoughnessClassID = Integer.parseInt( matcher.group( 3 ) );
          m_handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, -1 );
        }
        else
        {
          final Pattern twoParamLinePattern = Pattern.compile( "FE\\s*([0-9]+)\\s+([0-9]+).*" );
          matcher = twoParamLinePattern.matcher( line );
          if( matcher.matches() )
          {
            final int id = Integer.parseInt( matcher.group( 1 ) );
            final int currentRougthnessClassID = Integer.parseInt( matcher.group( 2 ) );
            m_handler.handleElement( line, id, currentRougthnessClassID, -1, -1 );
          }
          else
          {
            final Pattern oneParamLinePattern = Pattern.compile( "FE\\s*([0-9]+).*" );
            matcher = oneParamLinePattern.matcher( line );
            if( matcher.matches() )
            {
              final int id = Integer.parseInt( matcher.group( 1 ) );
              m_handler.handleElement( line, id, -1, -1, -1 );
            }
            else
              m_handler.handlerError( line, EReadError.LINE_TOO_SHORT );
          }
        }
      }
    }
    catch( final NumberFormatException e )
    {
      m_handler.handlerError( line, EReadError.ILLEGAL_SECTION );
    }
  }

}
