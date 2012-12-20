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
package org.kalypso.wspwin.core;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Locale;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.wspwin.core.i18n.Messages;

/**
 * Represents the contents of an wsp.cfg file
 *
 * @author belger
 */
public class WspCfg
{
  public enum TYPE
  {
    PASCHE('b', "BCE (Pasche)"), //$NON-NLS-1$
    KNAUF('l', "LWA (Knauf)"); //$NON-NLS-1$

    private final char m_code;

    private final String m_label;

    private TYPE( final char code, final String label )
    {
      m_code = code;
      m_label = label;
    }

    public char getCode( )
    {
      return m_code;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  private final Collection<WspWinZustand> m_zustaende = new ArrayList<>();

  private final WspWinProfProj m_profProj = new WspWinProfProj();

  /** model type: 'b' is Pasche-TUHH, 'l' is PSW-Knauf */
  private TYPE m_type;

  private String m_projectName = StringUtils.EMPTY;

  private final WspWinProject m_project;

  /**
   * Creates an empty {@link WspCfg}. Should only be used in order to call {@link #read()} afterwards.
   */
  public WspCfg( final WspWinProject project )
  {
    this( project, StringUtils.EMPTY );
  }

  public WspCfg( final WspWinProject project, final String projectName )
  {
    this( project, TYPE.PASCHE, projectName );
  }

  public WspCfg( final WspWinProject project, final TYPE type, final String projectName )
  {
    m_project = project;
    m_type = type;
    m_projectName = projectName;
  }

  public void setProjectName( final String name )
  {
    m_projectName = name;
  }

  public WspWinProject getProject( )
  {
    return m_project;
  }

  public TYPE getType( )
  {
    return m_type;
  }

  private void setType( final char code )
  {
    for( final TYPE type : TYPE.values() )
    {
      if( code == type.getCode() )
      {
        setType( type );
        return;
      }
    }

    // default to pasche
    setType( TYPE.PASCHE );
  }

  private void setType( final TYPE type )
  {
    m_type = type;
  }

  public WspWinZustand[] getZustaende( )
  {
    return m_zustaende.toArray( new WspWinZustand[m_zustaende.size()] );
  }

  public IStatus read( )
  {
    final IStatusCollector log = new StatusCollector( KalypsoWspWinCorePlugin.PLUGIN_ID );

    final File profDir = m_project.getProfDir();

    final Collection<ZustandBean> zustandBeans = new ArrayList<>();

    final IStatus wspCfgStatus = readWspCfg( profDir, zustandBeans );
    log.add( wspCfgStatus );

    final IStatus probezStatus = readProbez( profDir );
    log.add( probezStatus );

    /* Read zusteande */
    for( final ZustandBean zustandBean : zustandBeans )
    {
      try
      {
        // TODO: improve error handling: import completely fails if we get an error here
        final WspWinZustand wspwinZustand = zustandBean.readZustand( m_type, profDir );
        m_zustaende.add( wspwinZustand );
      }
      catch( ParseException | IOException e )
      {
        final String msg = String.format( Messages.getString("WspCfg.0"), zustandBean.getFileName() ); //$NON-NLS-1$
        log.add( IStatus.WARNING, msg, e );
      }
    }

    try
    {
      /* Read profproj */
      m_profProj.read( m_project.getProjectDir() );
    }
    catch( final ParseException | IOException e )
    {
      final String msg = String.format( Messages.getString("WspCfg.1"), WspWinFiles.PROFPROJ_TXT ); //$NON-NLS-1$
      log.add( IStatus.WARNING, msg, e );
    }

    return log.asMultiStatus( String.format( Messages.getString( "WspCfg.2" ), WspWinFiles.WSP_CFG ) ); //$NON-NLS-1$
  }

  private IStatus readProbez( final File profDir )
  {
    try
    {
      final File probezFile = new File( profDir, WspWinFiles.PROBEZ_TXT );
      if( probezFile.isFile() )
        m_projectName = FileUtils.readFileToString( probezFile );

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      final String message = String.format( Messages.getString("WspCfg.3"), WspWinFiles.PROBEZ_TXT ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, KalypsoWspWinCorePlugin.PLUGIN_ID, message, e );
    }
  }

  private IStatus readWspCfg( final File profDir, final Collection<ZustandBean> zustandBeans )
  {
    final File wspCfgFile = new File( profDir, WspWinFiles.WSP_CFG );

    try (LineNumberReader reader = new LineNumberReader( new FileReader( wspCfgFile ) ))
    {
      final String firstLine = reader.readLine();
      if( firstLine == null || firstLine.length() == 0 )
        return new Status( IStatus.ERROR, KalypsoWspWinCorePlugin.PLUGIN_ID, Messages.getString( "org.kalypso.wspwin.core.WspCfg.1" ) ); //$NON-NLS-1$

      // ignore the values, we read the count from the linecount
      // just parse the type
      final char type = firstLine.charAt( firstLine.length() - 1 );
      setType( type );

      while( reader.ready() )
      {
        final String line = reader.readLine();
        if( line == null )
          break;

        final String trimmedLine = line.trim();
        if( trimmedLine.length() == 0 || trimmedLine.length() < 85 )
          continue;

        try
        {
          final String waterName = trimmedLine.substring( 0, 15 ).trim();
          final String name = trimmedLine.substring( 15, 30 ).trim();
          // normally it should always be german, but it depends on the wspwin installation
          final DateFormat dateInstance = SimpleDateFormat.getDateInstance( SimpleDateFormat.SHORT, Locale.GERMAN );
          final String dateString = trimmedLine.substring( 30, 41 ).trim();
          final Date date = dateInstance.parse( dateString );
          final BigDecimal start = new BigDecimal( trimmedLine.substring( 41, 56 ).trim() );
          final BigDecimal end = new BigDecimal( trimmedLine.substring( 56, 71 ).trim() );
          final String fileName = trimmedLine.substring( 71 ).trim();

          final ZustandBean zustandBean = new ZustandBean( name, waterName, fileName, start, end, date );
          zustandBeans.add( zustandBean );
        }
        catch( final NumberFormatException e )
        {
          e.printStackTrace();
          throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.WspCfg.3", reader.getLineNumber() ), reader.getLineNumber() ); //$NON-NLS-1$
        }
      }

      return Status.OK_STATUS;
    }
    catch( final ParseException | IOException e )
    {
      return new Status( IStatus.ERROR, KalypsoWspWinCorePlugin.PLUGIN_ID, e.getLocalizedMessage(), e );
    }
  }

  public WspWinZustand createZustand( final String name, final String filename, final String waterName, final Date creationDate )
  {
    return createZustand( name, filename, waterName, creationDate, null, null );
  }

  public WspWinZustand createZustand( final String name, final String filename, final String waterName, final Date creationDate, final BigDecimal startStation, final BigDecimal endStation )
  {
    final ZustandBean bean = new ZustandBean( name, waterName, filename, startStation, endStation, creationDate );
    final WspWinZustand zustand = new WspWinZustand( bean );
    m_zustaende.add( zustand );
    return zustand;
  }

  public ProfileBean[] getProfiles( )
  {
    return m_profProj.getProfiles();
  }

  public void write( ) throws IOException
  {
    final File profDir = m_project.getProfDir();
    profDir.mkdirs();

    /* probez.txt */
    final File probezFile = new File( profDir, WspWinFiles.PROBEZ_TXT );
    FileUtils.writeStringToFile( probezFile, m_projectName );

    /* wsp.cfg */
    writeContent();

    /* ProfProj */
    final WspWinZustand[] zustaende = getZustaende();
    final File projectDir = m_project.getProjectDir();
    m_profProj.write( projectDir, zustaende );

    /* Zustaende */
    for( final WspWinZustand zustand : zustaende )
    {
      final ZustandBean bean = zustand.getBean();
      bean.writeZustand( projectDir, zustand );
    }
  }

  private void writeContent( ) throws IOException
  {
    final File wspCfgFile = m_project.getWspCfgFile();

    final BufferedWriter pw = new BufferedWriter( new FileWriter( wspCfgFile ) );

    final int numProfiles = m_profProj.getProfiles().length;
    final int yyyy = 0;
    pw.append( String.format( "%5d %4d %4d %s%n", numProfiles, m_zustaende.size(), yyyy, getType().getCode() ) ); //$NON-NLS-1$

    for( final WspWinZustand zustand : m_zustaende )
    {
      final ZustandBean zustandBean = zustand.getBean();
      pw.append( zustandBean.formatLine() ).append( SystemUtils.LINE_SEPARATOR );
    }

    pw.close();
  }

  /**
   * Recalculates the segments (i.e. profile from-to and distances) for the reach.
   */
  public void updateSegmentInfo( )
  {
    for( final WspWinZustand zustand : m_zustaende )
      zustand.updateSegmentInfo();
  }

  /**
   * Creates a profile ands it to the global definition (profproj)
   */
  public ProfileBean createProfile( final String waterName, final String stateName, final BigDecimal station, final String fileName, final String mehrfeldCode, final int vzk )
  {
    final ProfileBean bean = new ProfileBean( waterName, stateName, station, fileName, mehrfeldCode, vzk );
    m_profProj.add( bean );
    return bean;
  }

  /**
   * Returns the profile with the given name.
   */
  public ProfileBean findProfile( final String fileName )
  {
    final ProfileBean[] profileBeans = m_profProj.getProfiles();
    for( final ProfileBean profileBean : profileBeans )
    {
      if( fileName.equals( profileBean.getFileName() ) )
        return profileBean;
    }

    return null;
  }

  public String getProjectName( )
  {
    return m_projectName;
  }
}