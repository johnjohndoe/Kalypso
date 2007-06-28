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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;

/**
 * Provides algorithm to convert from a bce2d model to a 1d2d discretisation model
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
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
    LineNumberReader lnr = new LineNumberReader( reader );
    char char0, char1;

    // signal parsing start
    m_handler.start();

    for( String line = lnr.readLine(); line != null; line = lnr.readLine() )
    {
      if( line.length() < 2 )
      {
        continue;
      }

      char0 = line.charAt( 0 );
      char1 = line.charAt( 1 );

      if( char0 == 'F' && char1 == 'P' )
      {
        interpreteNodeLine( line, m_handler );

      }
      else if( char0 == 'F' && char1 == 'E' )
      {
        // LineID, ID
        interpreteElementLine( line, m_handler );
      }
      else if( char0 == 'A' && char1 == 'R' )
      {
        // edge LINEID, ID, node1, node2, ellinks, elrechts, mid-side node (after calculation)
        interpreteArcLine( line, m_handler );
      }
      else if( char0 == 'R' && char1 == 'K' )
      {

      }
      else if( char0 == 'V' && char1 == 'A' )
      {
        // result LINEID, ID, vx, vy, depth, waterlevel
        interpreteResultLine( line, m_handler );
      }
      else if( char0 == 'T' && char1 == 'I' )
      {
        // result TIMESTEP
//        TI          18.0000000         3
        interpreteTimeLine( line, m_handler );
      }
      else if( char0 == 'T' && char1 == 'L' )
      {
        //TL JunctionID, 1d-ElementID, BoundaryLineID, 1d-NodeID
        interprete1d2dJunctionElement( line, m_handler );
      }
      else
      {
        if( verboseMode )
          System.out.println( "Unsupported section:" + line );
      }
    }

    // signal parsing stop
    m_handler.end();

  }

  private void interprete1d2dJunctionElement( String line, IRMA10SModelElementHandler handler )
  {
    final Pattern linePattern = Pattern.compile( "TL\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)\\s*([0-9]+)" );
    final Matcher matcher = linePattern.matcher( line );
    if( matcher.matches() )
    {
      try
      {
        int junctionID = Integer.parseInt( matcher.group( 1 ) );
        int element1dID = Integer.parseInt( matcher.group( 2 ) );
        int boundaryLine2dID = Integer.parseInt( matcher.group( 3 ) );
        int mode1dID = Integer.parseInt( matcher.group( 4 ) );
        
        handler.handleJunction( line, junctionID, element1dID, boundaryLine2dID, mode1dID);
      }
      catch( NumberFormatException e )
      {
        handler.handlerError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      handler.handlerError( line, EReadError.ILLEGAL_SECTION );
  }

  private void interpreteTimeLine( String line, IRMA10SModelElementHandler handler )
  {
    final Pattern linePattern = Pattern.compile( "TI\\s+([0-9]+\\.[0-9]*)\\s*([0-9]+)" );
    final Matcher matcher = linePattern.matcher( line );
    if( matcher.matches() )
    {
      try
      {
        double time = Double.parseDouble( matcher.group( 1 ) );
        int timestep = Integer.parseInt( matcher.group( 2 ) );
        
        handler.handleTime( line, time, timestep );
      }
      catch( NumberFormatException e )
      {
        handler.handlerError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      handler.handlerError( line, EReadError.ILLEGAL_SECTION );
    
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#setModelElementIDProvider(org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider)
   */
  public void setModelElementIDProvider( IModelElementIDProvider idProvider ) throws IllegalArgumentException
  {
    // this.idProvider=idProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#setRMA10SModelElementHandler(org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler)
   */
  public void setRMA10SModelElementHandler( IRMA10SModelElementHandler handler ) throws IllegalArgumentException
  {
    m_handler = handler;
  }

  static public void toDiscretisationModel( InputStream rma10sModelInput, IFEDiscretisationModel1d2d targetModel, IPositionProvider positionProvider, IModelElementIDProvider idProvider ) throws IllegalStateException, IOException
  {
    IRMA10SModelReader reader = new RMA10S2GmlConv();

    IRMA10SModelElementHandler handler = new DiscretisationModel1d2dHandler( targetModel, positionProvider, idProvider );
    reader.setModelElementIDProvider( idProvider );
    reader.setRMA10SModelElementHandler( handler );
    reader.parse( rma10sModelInput );

    return;
  }

  private static final void interpreteNodeLine( final String line, final IRMA10SModelElementHandler handler )
  {
    final Pattern linePattern = Pattern.compile( "FP\\s*([0-9]+)\\s+([0-9]+\\.[0-9]*)\\s+([0-9]+\\.[0-9]*)\\s+([\\+\\-]?[0-9]+\\.[0-9]*).*" );
    final Matcher matcher = linePattern.matcher( line );
    if( matcher.matches() )
    {
      try
      {
        int id = Integer.parseInt( matcher.group( 1 ) );
        double easting = Double.parseDouble( matcher.group( 2 ) );
        double northing = Double.parseDouble( matcher.group( 3 ) );    
        double elevation = Double.parseDouble( matcher.group( 4 ) );
        //TODO: the value '-9999' represents the NODATA-value, should be discussed
        if (elevation == -9999) 
          elevation = Double.NaN;
        handler.handleNode( line, id, easting, northing, elevation );
      }
      catch( NumberFormatException e )
      {
        handler.handlerError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      handler.handlerError( line, EReadError.ILLEGAL_SECTION );
  }

  private static final void interpreteResultLine( final String line, final IRMA10SModelElementHandler handler ) {
    final String[] strings = line.split( "\\s+" );
    if( strings.length == 6 )
    {
      try
      {
        int id = Integer.parseInt( strings[1] );
        double vx = Double.parseDouble( strings[2] );
        double vy = Double.parseDouble( strings[3] );
        double depth = Double.parseDouble( strings[4] );
        double waterlevel = Double.parseDouble( strings[5] );
        handler.handleResult( line, id, vx, vy, depth, waterlevel );
      }
      catch( NumberFormatException e )
      {
        handler.handlerError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    else
      handler.handlerError( line, EReadError.ILLEGAL_SECTION );
  }

  private static final void interpreteArcLine( final String line, final IRMA10SModelElementHandler handler )
  {
    final Pattern middleNodePattern = Pattern.compile( "AR\\s*([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+).*" );
    Matcher matcher = middleNodePattern.matcher( line );
    try
    {
      if( matcher.matches() )
      {
        int id = Integer.parseInt( matcher.group( 1 ) );
        int node1ID = Integer.parseInt( matcher.group( 2 ) );
        int node2ID = Integer.parseInt( matcher.group( 3 ) );
        int elementLeftID = Integer.parseInt( matcher.group( 4 ) );
        int elementRightID = Integer.parseInt( matcher.group( 5 ) );
        int middleNodeID = Integer.parseInt( matcher.group( 6 ) );
        handler.handleArc( line, id, node1ID, node2ID, elementLeftID, elementRightID, middleNodeID );
      }
      else
      {
        final Pattern noMiddleNodePattern = Pattern.compile( "AR\\s*([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+).*" );
        matcher = noMiddleNodePattern.matcher( line );
        if( matcher.matches() )
        {
          int id = Integer.parseInt( matcher.group( 1 ) );
          int node1ID = Integer.parseInt( matcher.group( 2 ) );
          int node2ID = Integer.parseInt( matcher.group( 3 ) );
          int elementLeftID = Integer.parseInt( matcher.group( 4 ) );
          int elementRightID = Integer.parseInt( matcher.group( 5 ) );
          handler.handleArc( line, id, node1ID, node2ID, elementLeftID, elementRightID, -1 );
        }
        else
          handler.handlerError( line, EReadError.ILLEGAL_SECTION );
      }
    }
    catch( NumberFormatException e )
    {
      handler.handlerError( line, EReadError.ILLEGAL_SECTION );
    }
  }

  private static final void interpreteElementLine( final String line, final IRMA10SModelElementHandler handler )
  {
    Matcher matcher = null;
    final Pattern fourParamLinePattern = Pattern.compile( "FE\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([0-9]+).*" );
    matcher = fourParamLinePattern.matcher( line );
    try
    {
      if( matcher.matches() )
      {
        int id = Integer.parseInt( matcher.group( 1 ) );
        int currentRougthnessClassID = Integer.parseInt( matcher.group( 2 ) );
        int previousRoughnessClassID = Integer.parseInt( matcher.group( 3 ) );
        int eleminationNumber = Integer.parseInt( matcher.group( 4 ) );
        handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, eleminationNumber );
      }
      else
      {
        final Pattern threeParamLinePattern = Pattern.compile( "FE\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+).*" );
        matcher = threeParamLinePattern.matcher( line );
        if( matcher.matches() )
        {
          int id = Integer.parseInt( matcher.group( 1 ) );
          int currentRougthnessClassID = Integer.parseInt( matcher.group( 2 ) );
          int previousRoughnessClassID = Integer.parseInt( matcher.group( 3 ) );
          handler.handleElement( line, id, currentRougthnessClassID, previousRoughnessClassID, -1 );
        }
        else
        {
          final Pattern twoParamLinePattern = Pattern.compile( "FE\\s*([0-9]+)\\s+([0-9]+).*" );
          matcher = twoParamLinePattern.matcher( line );
          if( matcher.matches() )
          {
            int id = Integer.parseInt( matcher.group( 1 ) );
            int currentRougthnessClassID = Integer.parseInt( matcher.group( 2 ) );
            handler.handleElement( line, id, currentRougthnessClassID, -1, -1 );
          }
          else
          {
            final Pattern oneParamLinePattern = Pattern.compile( "FE\\s*([0-9]+).*" );
            matcher = oneParamLinePattern.matcher( line );
            if( matcher.matches() )
            {
              int id = Integer.parseInt( matcher.group( 1 ) );
              handler.handleElement( line, id, -1, -1, -1 );
            }
            else
              handler.handlerError( line, EReadError.LINE_TOO_SHORT );
          }
        }
      }
    }
    catch( NumberFormatException e )
    {
      handler.handlerError( line, EReadError.ILLEGAL_SECTION );
    }
  }

}
