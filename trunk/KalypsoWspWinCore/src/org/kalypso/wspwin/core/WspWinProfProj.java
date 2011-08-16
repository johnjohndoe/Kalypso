/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.wspwin.core;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.SystemUtils;
import org.kalypso.wspwin.core.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class WspWinProfProj
{
  public static final String PROFPROJ_FILENAME = "PROFPROJ.TXT"; //$NON-NLS-1$

  private final Collection<ProfileBean> m_profiles = new ArrayList<ProfileBean>();

  public void add( final ProfileBean profileBean )
  {
    m_profiles.add( profileBean );
  }

  public void write( final File wspwinDir, final WspWinZustand[] zustaende ) throws IOException
  {
    final File profprojFile = new File( WspWinHelper.getProfDir( wspwinDir ), WspWinFiles.PROFPROJ_TXT );

    final Map<String, String> profileStateHash = new HashMap<String, String>();
    for( final ProfileBean profile : m_profiles )
    {
      final String prfFilename = profile.getFileName();
      final String stateName = profile.getStateName();
      final String stateFilename = findStateFilename( stateName, zustaende );
      if( stateFilename != null )
        profileStateHash.put( prfFilename, stateFilename );
    }

    final BufferedWriter pw = new BufferedWriter( new FileWriter( profprojFile ) );

    pw.append( String.format( "%d %d%n", m_profiles.size(), profileStateHash.size() ) );

    for( final ProfileBean profile : m_profiles )
      pw.append( profile.formatLine() ).append( SystemUtils.LINE_SEPARATOR );
    pw.append( SystemUtils.LINE_SEPARATOR );

    for( final Entry<String, String> entry : profileStateHash.entrySet() )
    {
      final String prfFilename = entry.getKey();
      final String stateFilename = entry.getValue();
      pw.append( String.format( "%s %s%n", prfFilename, stateFilename ) );
    }

    pw.close();
  }

  private String findStateFilename( final String stateName, final WspWinZustand[] zustaende )
  {
    for( final WspWinZustand zustand : zustaende )
    {
      final ZustandBean bean = zustand.getBean();
      if( stateName.equals( bean.getName() ) )
        return bean.getFileName();
    }
    return null;
  }

  public ProfileBean[] getProfiles( )
  {
    return m_profiles.toArray( new ProfileBean[m_profiles.size()] );
  }

  /**
   * Reads the file profproj.txt
   */
  public void read( final File wspwinDir ) throws IOException, ParseException
  {
    final File profprojFile = new File( WspWinHelper.getProfDir( wspwinDir ), WspWinFiles.PROFPROJ_TXT );

    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( profprojFile ) );

      final int[] counts = readStrHeader( reader );
      final int profilCount = counts[0];
      final int relationCount = counts[1];

      if( relationCount == 0 )
      {
        // ignore for now; later we may do sanity checks, if there are unused profiles
      }

      final ProfileBean[] profiles = ProfileBean.readProfiles( reader, profilCount );
      m_profiles.addAll( Arrays.asList( profiles ) );
    }
    catch( final ParseException pe )
    {
      final String msg = Messages.getString( "org.kalypso.wspwin.core.WspCfg.6" ) + profprojFile.getAbsolutePath() + " \n" + pe.getLocalizedMessage(); //$NON-NLS-1$ //$NON-NLS-2$
      final ParseException newPe = new ParseException( msg, pe.getErrorOffset() );
      newPe.setStackTrace( pe.getStackTrace() );
      throw newPe;
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  /** Reads the first line of a profproj.txt or .str file. Returns 2 ints: profile count + second count. */
  public static int[] readStrHeader( final LineNumberReader reader ) throws IOException, ParseException
  {
    final String firstLine = reader.readLine();
    if( firstLine == null || firstLine.length() == 0 )
      throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.WspCfg.8" ), reader.getLineNumber() ); //$NON-NLS-1$

    // ignore the values, we read the count from the linecount
    // just parse the type
    final StringTokenizer firstLineTokenizer = new StringTokenizer( firstLine );
    if( firstLineTokenizer.countTokens() < 2 )
      throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.WspCfg.9" ), reader.getLineNumber() ); //$NON-NLS-1$

    final int[] counts = new int[2];
    counts[0] = Integer.parseInt( firstLineTokenizer.nextToken() );
    counts[1] = Integer.parseInt( firstLineTokenizer.nextToken() );

    // if it is a .str file, we ignore the following name and waterName

    return counts;
  }
}
