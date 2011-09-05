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
package org.kalypso.kalypsomodel1d2d.internal.import2dm;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.ProgressInputStream;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;

/**
 * Provides algorithm to convert from a SMS model to a 1d2d discretisation model
 * 
 * @author Thomas Jung
 */
class SMSParser
{
  private final IStatusCollector m_stati = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

  private final ISMSModel m_model;

  private static final Pattern ELEMENT_LINE_PATTERN_E3T = Pattern.compile( "E3T\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+).*" ); //$NON-NLS-1$

  private static final Pattern ELEMENT_LINE_PATTERN_E4Q = Pattern.compile( "E4Q\\s*([0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+)\\s+([\\+\\-]?[0-9]+).*" ); //$NON-NLS-1$

  public SMSParser( final ISMSModel model2dm )
  {
    m_model = model2dm;
  }

  public IStatus parse( final URL url, final IProgressMonitor monitor ) throws IOException
  {
    InputStream is = null;
    try
    {
      final long contentLength = UrlUtilities.getContentLength( url );
      is = new ProgressInputStream( new BufferedInputStream( url.openStream() ), contentLength, monitor );

      parse( is );

      is.close();

      final String okMessage = String.format( "Sucessfully read %s", url.toExternalForm() );
      final String problemMessage = String.format( "Problem(s) while reading %s", url.toExternalForm() );
      return m_stati.asMultiStatusOrOK( problemMessage, okMessage );
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
  }

  private void parse( final InputStream inputStream ) throws IOException
  {
    Assert.isNotNull( inputStream );
    parse( new InputStreamReader( inputStream ) );
  }

  private void parse( final Reader reader ) throws IOException
  {
    Assert.isNotNull( reader );

    final LineNumberReader lnReader = new LineNumberReader( reader );
    for( String line = lnReader.readLine(); line != null; line = lnReader.readLine() )
    {
      try
      {
        if( line.startsWith( "ND" ) ) //$NON-NLS-1$
          interpreteNodeLine( line, lnReader );
        else if( line.startsWith( "E3T" ) ) //$NON-NLS-1$
          interpreteE3TLine( line, lnReader );
        else if( line.startsWith( "E4Q" ) ) //$NON-NLS-1$
          interpreteE4QLine( line, lnReader );
      }
      catch( final NumberFormatException e )
      {
        addStatus( lnReader, IStatus.ERROR, "Format error: '%s'", e.getLocalizedMessage() );
      }

      /* Abort after too many problems, */
      final int maxProblemCount = 100;
      if( m_stati.size() > maxProblemCount )
      {
        m_stati.add( IStatus.ERROR, "Too many problems encountered, aborting..." );
        return;
      }
    }
  }

  private void interpreteNodeLine( final String line, final LineNumberReader lnReader )
  {
    final String[] nodeLineStrings = StringUtils.split( line );

    if( !"ND".equals( nodeLineStrings[0] ) )
    {
      addStatus( lnReader, IStatus.WARNING, "Corrupt 'ND' line" );
      return;
    }

    final int id = Integer.parseInt( nodeLineStrings[1] );
    final double easting = Double.parseDouble( nodeLineStrings[2] );
    final double northing = Double.parseDouble( nodeLineStrings[3] );
    double elevation = Double.parseDouble( nodeLineStrings[4] );
    // TODO: the value '-9999' represents the NODATA-value, should be discussed
    if( elevation == -9999 )
      elevation = Double.NaN;

    m_model.addNode( line, id, easting, northing, elevation );
  }

  private void interpreteE3TLine( final String line, final LineNumberReader lnReader )
  {
    final Matcher lineMatcher = ELEMENT_LINE_PATTERN_E3T.matcher( line );
    if( !lineMatcher.matches() )
    {
      addStatus( lnReader, IStatus.WARNING, "Illegal E3T format" );
      return;
    }

    final List<Integer> nodeIdList = new ArrayList<Integer>();

    final int id = Integer.parseInt( lineMatcher.group( 1 ) );

    nodeIdList.add( Integer.parseInt( lineMatcher.group( 2 ) ) );
    nodeIdList.add( Integer.parseInt( lineMatcher.group( 3 ) ) );
    nodeIdList.add( Integer.parseInt( lineMatcher.group( 4 ) ) );

    final Integer[] nodes = nodeIdList.toArray( new Integer[nodeIdList.size()] );

    final int roughnessClassID = Integer.parseInt( lineMatcher.group( 5 ) );
    m_model.addElement( line, id, nodes, roughnessClassID );
  }

  private void interpreteE4QLine( final String line, final LineNumberReader lnReader )
  {
    final Matcher elementMatcher = ELEMENT_LINE_PATTERN_E4Q.matcher( line );
    if( !elementMatcher.matches() )
    {
      addStatus( lnReader, IStatus.WARNING, "Illegal E3T format" );
      return;
    }

    final List<Integer> nodeIdList = new ArrayList<Integer>();

    final int id = Integer.parseInt( elementMatcher.group( 1 ) );

    nodeIdList.add( Integer.parseInt( elementMatcher.group( 2 ) ) );
    nodeIdList.add( Integer.parseInt( elementMatcher.group( 3 ) ) );
    nodeIdList.add( Integer.parseInt( elementMatcher.group( 4 ) ) );
    nodeIdList.add( Integer.parseInt( elementMatcher.group( 5 ) ) );

    final Integer[] nodes = nodeIdList.toArray( new Integer[nodeIdList.size()] );

    final int roughnessClassID = Integer.parseInt( elementMatcher.group( 6 ) );
    m_model.addElement( line, id, nodes, roughnessClassID );
  }

  private void addStatus( final LineNumberReader lnReader, final int severity, final String message, final Object... args )
  {
    final String format = "Line %5d: " + message;

    final Object[] arguments = ArrayUtils.addAll( new Object[] { lnReader.getLineNumber() }, args );

    final String msg = String.format( format, arguments );

    m_stati.add( severity, msg );
  }

}
