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
package org.kalypso.model.wspm.tuhh.ui.utils;

import java.math.BigDecimal;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.patternreplace.PatternInputReplacer;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class GuessStationPatternReplacer extends PatternInputReplacer<GuessStationContext>
{
  public GuessStationPatternReplacer( )
  {
    addReplacer( new GuessStationPattern() );
  }

  public static Pattern[] preparePatterns( final GuessStationContext[] searchContexts, final String searchPattern )
  {
    final Pattern[] stationPatterns = new Pattern[searchContexts.length];

    final GuessStationPatternReplacer patternReplacer = new GuessStationPatternReplacer();
    for( int i = 0; i < searchContexts.length; i++ )
    {
      final String current = patternReplacer.replaceTokens( searchPattern, searchContexts[i] );
      stationPatterns[i] = Pattern.compile( current, Pattern.CASE_INSENSITIVE );
    }

    return stationPatterns;
  }

  public static BigDecimal findStation( final String searchString, final GuessStationContext[] searchContexts, final Pattern[] searchPatterns )
  {
    for( int i = 0; i < searchPatterns.length; i++ )
    {
      final Matcher matcher = searchPatterns[i].matcher( searchString );
      if( matcher.matches() )
      {
        final String stationString = matcher.group( 1 );

        final BigDecimal station = searchContexts[i].parseStation( stationString );
        if( station == null )
          return null;

        // Set scale to 1, as it is in the database, else we get problems to compare
        // with existing cross sections
        return station.setScale( 1, BigDecimal.ROUND_HALF_UP );
      }
    }

    return null;
  }

  /*
   * Replace the <station> token with a general search pattern, so we only consider any file that matches the basic
   * pattern.
   */
  public static Pattern asSearchPattern( final String searchPattern )
  {
    final String generalPattern = searchPattern.replaceAll( "\\Q<station>\\E", ".*" ); //$NON-NLS-1$ //$NON-NLS-2$
    return Pattern.compile( generalPattern, Pattern.CASE_INSENSITIVE );
  }

  public static String getSearchRegex( final String stationPattern ) throws CoreException
  {
    final String token = String.format( "<%s>", GuessStationPattern.TOKEN ); //$NON-NLS-1$

    final int countMatches = StringUtils.countMatches( stationPattern, token );
    if( countMatches != 1 )
    {
      final String msg = String.format( Messages.getString("GuessStationPatternReplacer.2"), stationPattern ); //$NON-NLS-1$
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), msg ) );
    }

    if( stationPattern.endsWith( token ) )
    {
      // REMARK: in this case, we automatically extend the pattern with a '*',
      // as we assume that we at least have an extension
      return asRegex( stationPattern.substring( 0, stationPattern.length() - token.length() ) ) + token + ".*"; //$NON-NLS-1$
    }

    if( stationPattern.startsWith( token ) )
      return token + asRegex( stationPattern.substring( token.length() ) );

    final String[] split = stationPattern.split( "\\Q" + token + "\\E" ); //$NON-NLS-1$ //$NON-NLS-2$
    return asRegex( split[0] ) + token + asRegex( split[1] );
  }

  private static String asRegex( final String pattern )
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
        builder.append( ".*?" ); //$NON-NLS-1$
        i = index;
      }
    }

    return builder.toString();
  }

  private static void appendEscaped( final StringBuilder builder, final String text )
  {
    if( text.isEmpty() )
      return;

    builder.append( "\\Q" ).append( text ).append( "\\E" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

}