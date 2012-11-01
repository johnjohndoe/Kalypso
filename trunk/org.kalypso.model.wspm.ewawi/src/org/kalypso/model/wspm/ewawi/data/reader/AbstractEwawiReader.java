/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.ewawi.data.reader;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiHorizont;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiObjectart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiProfilart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;

/**
 * Encapsulates some common reading code.
 * 
 * @author Gernot Belger
 */
abstract class AbstractEwawiReader
{
  private final DateFormat m_ewawiDF = new SimpleDateFormat( "dd.MM.yyyy" ); //$NON-NLS-1$

  private final EwawiPlus m_data;

  public AbstractEwawiReader( final EwawiPlus data )
  {
    m_data = data;
  }

  protected EwawiPlus getData( )
  {
    return m_data;
  }

  public void read( final File file ) throws IOException, ParseException
  {
    try( LineNumberReader reader = new LineNumberReader( new FileReader( file ) ) )
    {
      m_data.setSourceFile( file );
      read( reader );
    }
  }

  private void read( final LineNumberReader reader ) throws IOException, ParseException
  {
    while( reader.ready() )
    {
      final String line = reader.readLine().trim();
      readLine( line );
    }
  }

  private void readLine( final String line ) throws ParseException
  {
    try
    {
      final String[] tabs = StringUtils.split( line, '\t' ); //$NON-NLS-1$
      readTabs( tabs );
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
      throw new ParseException( line, 0 );
    }
  }

  protected abstract void readTabs( final String[] tabs ) throws ParseException;

  protected static EwawiObjectart asObjectArt( final String text )
  {
    // REMARK: first as int, because we get '1' or '01'
    final Integer asInt = Integer.valueOf( text );

    return EwawiObjectart.valueOf( "_" + asInt ); //$NON-NLS-1$
  }

  protected static Long asLong( final String text )
  {
    if( "-".equals( text ) ) //$NON-NLS-1$
      return null;

    return new Long( text );
  }

  protected static EwawiPunktart asPunktart( final String text )
  {
    // REMARK: first as int, because we get '1' or '01'
    final Integer asInt = Integer.valueOf( text );

    return EwawiPunktart.valueOf( "_" + asInt ); //$NON-NLS-1$
  }

  protected static BigDecimal asDecimal( final String text )
  {
    if( "-".equals( text ) ) //$NON-NLS-1$
      return null;

    return new BigDecimal( text );
  }

  protected Date asDate( final String text ) throws ParseException
  {
    if( "-".equals( text ) ) //$NON-NLS-1$
      return null;

    return m_ewawiDF.parse( text );
  }

  protected static String asString( final String text )
  {
    return text;
  }

  protected static EwawiProfilart asProfilart( final String text )
  {
    // REMARK: first as int, because we get '1' or '01'
    final Integer asInt = Integer.valueOf( text );

    return EwawiProfilart.valueOf( "_" + asInt ); //$NON-NLS-1$
  }

  protected static Short asShort( final String text )
  {
    if( "-".equals( text ) ) //$NON-NLS-1$
      return null;

    return new Short( text );
  }

  protected static String[] asStringArray( final String text, final char separator )
  {
    if( "-".equals( text ) ) //$NON-NLS-1$
      return new String[0];

    return StringUtils.split( text, separator );
  }

  protected static EwawiHorizont asHorizont( final String text )
  {
    // REMARK: first as int, because we get '1' or '01'
    final Integer asInt = Integer.valueOf( text );

    return EwawiHorizont.valueOf( "_" + asInt ); //$NON-NLS-1$
  }
}