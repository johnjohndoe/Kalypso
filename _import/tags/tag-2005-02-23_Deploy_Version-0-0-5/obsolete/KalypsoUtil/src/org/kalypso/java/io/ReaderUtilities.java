/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.java.io;

import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.apache.tools.ant.filters.ReplaceTokens;
import org.apache.tools.ant.filters.ReplaceTokens.Token;

/**
 * @author belger
 */
public class ReaderUtilities
{
  /** do not instantiate this class */
  private ReaderUtilities( )
  {
    //
  }

  public final static String readStringFromReader( final Reader r )
      throws IOException
  {
    final StringWriter sw = new StringWriter();

    readerCopy( r, sw );

    return sw.toString();
  }

  /**
   * Kopiert den Inhalt eines Readers in einen Writer. Beide werden nach Ende
   * der Operation geschlossen.
   * 
   * @param r
   * @param w
   * @throws IOException
   */
  public static final void readerCopy( final Reader r, final Writer w )
      throws IOException
  {
    final char[] buffer = new char[1024 * 16];
    while( true )
    {
      final int i = r.read( buffer );
      if( i == -1 )
        break;

      w.write( buffer, 0, i );
    }

    r.close();
    w.close();
  }

  /**
   * <p>
   * Führt ein Pattern-Ersetzen durch, bevor die Gistableview geparst wird Jeder
   * key der Properties wird durch seinen value ersetzt. Funktioniert nur
   * zeilenweise, d.h.
   * </p>
   * <p>
   * Performance schlecht: nur für Reader mit wenig Inhalt verwenden
   * </p>
   * 
   * @param r
   * @param replaceProps
   * @return string with replaced substrings
   * 
   * @throws IOException
   */
  public static final String readAndReplace( final Reader r,
      final Properties replaceProps ) throws IOException
  {
    String content = ReaderUtilities.readStringFromReader( r );

    for( final Iterator iter = replaceProps.entrySet().iterator(); iter
        .hasNext(); )
    {
      final Map.Entry entry = (Entry) iter.next();
      final String key = entry.getKey().toString();
      final String value = entry.getValue().toString();

      content = content.replaceAll( key, value );
    }

    return content;
  }

  /**
   * Creates a Reader which replaces all occurences of the given tokens with the
   * associated values.
   * <p>
   * Each token has the following form:
   * &lt;beginToken&gt;&lt;tokenName&gt;&lt;endToken&gt;
   * <p>
   * <ul>
   * <li>beginToken: the char denoting how token start
   * <li>tokenName: the name of the token
   * <li>endToken: the char denoting how token end
   * </ul>
   * <p>
   * A token example: %1% (begin and end token are the same i.e. %, the name is 1)
   */
  public static Reader createTokenReplaceReader( Reader reader,
      Properties token2value, char beginToken, char endToken )
  {
    final ReplaceTokens rtr = new ReplaceTokens( reader );

    rtr.setBeginToken( beginToken );
    rtr.setEndToken( endToken );

    for( final Iterator iter = token2value.entrySet().iterator(); iter
        .hasNext(); )
    {
      final Map.Entry entry = (Entry) iter.next();
      final String key = entry.getKey().toString();
      final String value = entry.getValue().toString();

      final Token token = new ReplaceTokens.Token();
      token.setKey( key );
      token.setValue( value );

      rtr.addConfiguredToken( token );
    }

    return rtr;
  }

  public static void dumpAllAvailable( final Reader reader ) throws IOException
  {
    while( reader.ready() )
    {
      final char c = (char) reader.read();
      System.out.print( c );
    }
  }
}