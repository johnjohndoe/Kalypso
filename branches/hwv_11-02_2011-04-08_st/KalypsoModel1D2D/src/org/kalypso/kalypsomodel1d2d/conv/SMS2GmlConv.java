/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.Assert;

/**
 * Provides algorithm to convert from a SMS model to a 1d2d discretisation model
 * 
 * @author Thomas Jung
 */
public class SMS2GmlConv
{
  private IFEModelElementHandler m_handler;

  public static boolean VERBOSE_MODE = false;

  private final IProgressMonitor m_monitor;

  private final int m_monitorStep;

  private static final Pattern ELEMENT_LINE_PATTERN_E3T = Pattern.compile( "E3T\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+).*" ); //$NON-NLS-1$

  private static final Pattern ELEMENT_LINE_PATTERN_E4Q = Pattern.compile( "E4Q\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+).*" ); //$NON-NLS-1$

  public SMS2GmlConv( final IProgressMonitor monitor, final int numberOfLinesToProcess )
  {
    m_monitorStep = numberOfLinesToProcess / 100;
    if( monitor == null )
      m_monitor = new NullProgressMonitor();
    else
      m_monitor = monitor;
  }

  public SMS2GmlConv( )
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
    final Pattern lineND = Pattern.compile( "ND.*" ); //$NON-NLS-1$
    final Pattern lineE3T = Pattern.compile( "E3T.*" ); //$NON-NLS-1$
    final Pattern lineE4Q = Pattern.compile( "E4Q.*" ); //$NON-NLS-1$

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
      if( lineND.matcher( line ).matches() )
        interpreteNodeLine( line );
      else if( lineE3T.matcher( line ).matches() )
        interpreteElementLine( line );
      else if( lineE4Q.matcher( line ).matches() )
        interpreteElementLine( line );
      else if( VERBOSE_MODE )
        System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.1" ) + line ); //$NON-NLS-1$
    }

    // signal parsing stop

    m_handler.end();

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelReader#setRMA10SModelElementHandler(org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler)
   */
  public void setModelElementHandler( final IFEModelElementHandler handler ) throws IllegalArgumentException
  {
    m_handler = handler;
  }

  private void interpreteNodeLine( final String line )
  {
    final String[] nodeLineStrings = line.split( "[ ]+" ); //$NON-NLS-1$

    if( nodeLineStrings[0].equals( "ND" ) ) //$NON-NLS-1$
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

  private void interpreteElementLine( final String line )
  {
    Matcher matcher = null;
    matcher = ELEMENT_LINE_PATTERN_E3T.matcher( line );
    try
    {

      if( matcher.matches() )
      {
        final List<Integer> nodeIdList = new ArrayList<Integer>();

        final int id = Integer.parseInt( matcher.group( 1 ) );

        nodeIdList.add( Integer.parseInt( matcher.group( 2 ) ) );
        nodeIdList.add( Integer.parseInt( matcher.group( 3 ) ) );
        nodeIdList.add( Integer.parseInt( matcher.group( 4 ) ) );

        final Integer[] nodes = nodeIdList.toArray( new Integer[nodeIdList.size()] );

        final int roughnessClassID = Integer.parseInt( matcher.group( 5 ) );
        m_handler.handleElement( line, id, nodes, roughnessClassID );
      }
      else
      {
        matcher = ELEMENT_LINE_PATTERN_E4Q.matcher( line );
        if( matcher.matches() )
        {
          final List<Integer> nodeIdList = new ArrayList<Integer>();

          final int id = Integer.parseInt( matcher.group( 1 ) );

          nodeIdList.add( Integer.parseInt( matcher.group( 2 ) ) );
          nodeIdList.add( Integer.parseInt( matcher.group( 3 ) ) );
          nodeIdList.add( Integer.parseInt( matcher.group( 4 ) ) );
          nodeIdList.add( Integer.parseInt( matcher.group( 5 ) ) );

          final Integer[] nodes = nodeIdList.toArray( new Integer[nodeIdList.size()] );

          final int roughnessClassID = Integer.parseInt( matcher.group( 6 ) );
          m_handler.handleElement( line, id, nodes, roughnessClassID );
        }
        else
          m_handler.handleError( line, EReadError.LINE_TOO_SHORT );
      }
    }
    catch( final NumberFormatException e )
    {
      m_handler.handleError( line, EReadError.ILLEGAL_SECTION );
    }
  }
}
