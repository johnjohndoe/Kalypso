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
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Locale;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.SystemUtils;
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

  private final Collection<WspWinZustand> m_zustaende = new ArrayList<WspWinZustand>();

  private final WspWinProfProj m_profProj = new WspWinProfProj();

  /** model type: 'b' is Pasche-TUHH, 'l' is PSW-Knauf */
  private TYPE m_type;

  private File m_projectDir;

  private String m_projectName = StringUtils.EMPTY;

  public WspCfg( final TYPE type )
  {
    m_type = type;
  }

  public WspCfg( )
  {
    this( TYPE.PASCHE );
  }

  public void setProjectName( final String name )
  {
    m_projectName = name;
  }

  public File getProjectDir( )
  {
    return m_projectDir;
  }

  public void setProjectDir( final File projectDir )
  {
    m_projectDir = projectDir;
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

  public void read( final File wspwinDir ) throws IOException, ParseException
  {
    final File profDir = WspWinHelper.getProfDir( wspwinDir );
    final File wspCfgFile = new File( profDir, WspWinFiles.WSP_CFG );

    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( wspCfgFile ) );

      final WspCfg bean = new WspCfg();
      bean.setProjectDir( wspwinDir );

      final String firstLine = reader.readLine();
      if( firstLine == null || firstLine.length() == 0 )
        throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.WspCfg.1" ), reader.getLineNumber() ); //$NON-NLS-1$

      // ignore the values, we read the count from the linecount
      // just parse the type
      final char type = firstLine.charAt( firstLine.length() - 1 );
      bean.setType( type );

      final Collection<ZustandBean> zustandBeans = new ArrayList<ZustandBean>();
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
          final Double start = new Double( trimmedLine.substring( 41, 56 ) );
          final Double end = new Double( trimmedLine.substring( 56, 71 ) );
          final String fileName = trimmedLine.substring( 71 ).trim();

          final ZustandBean zustandBean = new ZustandBean( name, waterName, fileName, start, end, date );
          zustandBeans.add( zustandBean );
        }
        catch( final NumberFormatException e )
        {
          e.printStackTrace();
          throw new ParseException( Messages.getString( "org.kalypso.wspwin.core.WspCfg.3" ) + reader.getLineNumber(), reader.getLineNumber() ); //$NON-NLS-1$
        }
      }

      /* Read zusteande */
      for( final ZustandBean zustandBean : zustandBeans )
      {
        final WspWinZustand wspwinZustand = zustandBean.readZustand( profDir, m_profProj );
        m_zustaende.add( wspwinZustand );
      }

      /* Read profproj */
      m_profProj.read( wspwinDir );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  public WspWinZustand createZustand( final String name, final String filename, final String waterName, final Date creationDate )
  {
    final ZustandBean bean = new ZustandBean( name, waterName, filename, Double.NaN, Double.NaN, creationDate );
    final WspWinZustand zustand = new WspWinZustand( bean, m_profProj );
    m_zustaende.add( zustand );
    return zustand;
  }

  public ProfileBean[] getProfiles( )
  {
    return m_profProj.getProfiles();
  }

  public void write( final File wspwinDir ) throws IOException
  {
    updateSegmentInfo();

    final File profDir = WspWinHelper.getProfDir( wspwinDir );
    profDir.mkdirs();

    /* probez.txt */
    final File probezFile = new File( profDir, WspWinFiles.PROBEZ_TXT );
    FileUtils.writeStringToFile( probezFile, m_projectName );

    /* wsp.cfg */
    writeContent( wspwinDir );

    /* ProfProj */
    final WspWinZustand[] zustaende = getZustaende();
    m_profProj.write( wspwinDir, zustaende );

    /* Zustaende */
    for( final WspWinZustand zustand : zustaende )
    {
      final ZustandBean bean = zustand.getBean();
      bean.writeZustand( wspwinDir, zustand );
    }
  }

  private void writeContent( final File wspwinDir ) throws IOException
  {
    final File wspCfgFile = new File( WspWinHelper.getProfDir( wspwinDir ), WspWinFiles.WSP_CFG );

    final BufferedWriter pw = new BufferedWriter( new FileWriter( wspCfgFile ) );

    // TODO
    final int xxx = 0;
    final int yyyy = 0;
    pw.append( String.format( "%5d %4d %4d %s%n", xxx, m_zustaende.size(), yyyy, getType().getCode() ) );

    for( final WspWinZustand zustand : m_zustaende )
    {
      final ZustandBean zustandBean = zustand.getBean();
      pw.append( zustandBean.formatLine() ).append( SystemUtils.LINE_SEPARATOR );
    }

    pw.close();
  }

  private void updateSegmentInfo( )
  {
    for( final WspWinZustand zustand : m_zustaende )
      zustand.updateSegmentInfo();
  }
}