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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments;

import java.io.File;
import java.math.BigDecimal;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

/**
 * @author Gernot Belger
 */
public class SearchDocumentsOperation implements ICoreRunnableWithProgress
{
  final AttachmentStationContext[] m_searchContexts = new AttachmentStationContext[] {
      //
      new AttachmentStationContext( null, '+', 3 ), //
      new AttachmentStationContext( null, '-', 3 ), //
      new AttachmentStationContext( null, '.', 0 ), //
      new AttachmentStationContext( null, ',', 0 ) //
  };

  private final ImportAttachmentsData m_data;

  private final ImportAttachmentsDocumentsData m_documentData;

  public SearchDocumentsOperation( final ImportAttachmentsData data, final ImportAttachmentsDocumentsData documentData )
  {
    m_data = data;
    m_documentData = documentData;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    m_documentData.clear();

    final File importDir = m_data.getImportDir();
    if( !importDir.isDirectory() )
    {
      final String msg = String.format( "Import directory does not exist: %s", importDir );
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, msg );
    }

    final File[] allFiles = importDir.listFiles();
    if( allFiles == null )
    {
      final String msg = String.format( "Failed to access directory: %s", importDir );
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, msg );
    }

    final String searchRegex = getSearchRegex();
    final Pattern[] stationPatterns = preparePatterns( searchRegex );
    final Pattern searchPattern = asSearchPattern( searchRegex );

    for( final File file : allFiles )
      readFile( file, searchPattern, stationPatterns );

    return Status.OK_STATUS;
  }

  /*
   * Replace the <station> token with a general search pattern, so we only consider any file that matches the basic
   * pattern.
   */
  private Pattern asSearchPattern( final String searchPattern )
  {
    final String generalPattern = searchPattern.replaceAll( "\\Q<station>\\E", ".*" ); //$NON-NLS-1$
    return Pattern.compile( generalPattern, Pattern.CASE_INSENSITIVE );
  }

  private Pattern[] preparePatterns( final String searchPattern )
  {
    final Pattern[] stationPatterns = new Pattern[m_searchContexts.length];

    final AttachmentPatternReplacer patternReplacer = new AttachmentPatternReplacer();
    for( int i = 0; i < m_searchContexts.length; i++ )
    {
      final String current = patternReplacer.replaceTokens( searchPattern, m_searchContexts[i] );
      stationPatterns[i] = Pattern.compile( current, Pattern.CASE_INSENSITIVE );
    }

    return stationPatterns;
  }

  private String getSearchRegex( ) throws CoreException
  {
    final String importPattern = m_data.getImportPattern();
    final String token = String.format( "<%s>", AttachmentStationPattern.TOKEN );

    final int countMatches = StringUtils.countMatches( importPattern, token );
    if( countMatches != 1 )
    {
      final String msg = String.format( "Invalid pattern: %s", importPattern );
      throw new CoreException( new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, msg ) );
    }

    if( importPattern.endsWith( token ) )
    {
      // REMARK: in this case, we automatically extend the pattern with a '*',
      // as we assume that we at least have an extension
      return asRegex( importPattern.substring( 0, importPattern.length() - token.length() ) ) + token + ".*"; //$NON-NLS-1$
    }

    if( importPattern.startsWith( token ) )
      return token + asRegex( importPattern.substring( token.length() ) );

    final String[] split = StringUtils.split( importPattern, token );
    return asRegex( split[0] ) + token + asRegex( split[1] );
  }

  private String asRegex( final String pattern )
  {
    final StringBuilder builder = new StringBuilder();

    for( int i = 0; i < pattern.length(); i++ )
    {
      final int index = pattern.indexOf( '*', i );
      if( index == -1 )
      {
        appendEscaped( builder, pattern.substring( i ) );
        i = pattern.length();
      }
      else
      {
        appendEscaped( builder, pattern.substring( i, index ) );
        builder.append( ".*?" );
        i = index + 1;
      }
    }

    return builder.toString();
  }

  private void appendEscaped( final StringBuilder builder, final String text )
  {
    if( text.isEmpty() )
      return;

    builder.append( "\\Q" ).append( text ).append( "\\E" );
  }

  private void readFile( final File file, final Pattern searchPattern, final Pattern[] stationPatterns )
  {
    /* Skip all files that do not match the general search pattern */
    final Matcher matcher = searchPattern.matcher( file.getName() );
    if( !matcher.matches() )
      return;

    /* Read station */
    final BigDecimal station = findStation( file, stationPatterns );

    m_documentData.addDocument( station, file );
  }

  private BigDecimal findStation( final File file, final Pattern[] searchPatterns )
  {
    final String filename = file.getName();

    for( int i = 0; i < searchPatterns.length; i++ )
    {
      final Matcher matcher = searchPatterns[i].matcher( filename );
      if( matcher.matches() )
      {
        final String stationString = matcher.group( 1 );

        final BigDecimal station = m_searchContexts[i].parseStation( stationString );
        if( station == null )
          return null;

        // Set scale to 1, as it is in the database, else we get problems to compare
        // with existing cross sections
        return station.setScale( 1, BigDecimal.ROUND_HALF_UP );
      }
    }

    return null;
  }
}