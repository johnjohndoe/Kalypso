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
package org.kalypso.contribs.java.net;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;

/**
 * @author belger
 */
public interface IUrlResolver
{
  public URL resolveURL( final URL base, final String relative ) throws MalformedURLException;

  /**
   * An iterator over entries of a map (Map.Entry) Each entry represant a token , wich can be replaced
   */
  public Iterator getReplaceEntries();

  /**
   * add a Replace token to the map, which can be accessed via getReplaceEntries()
   */
  public void addReplaceToken( final String key, final String value );

  /**
   * Erzeugt einen BufferedWriter, der an den Ort der URL schreibt. Der Writer muss vom Aufrufenden geschlossen werden.
   * 
   * @throws IOException
   */
  public OutputStreamWriter createWriter( final URL url ) throws IOException;

  /**
   * Erzeugt einen (ungepufferten) Reader anhand einer URL. Insbesondere wird das Encoding des Reader nach bestem Wissen
   * und Gewissen gesetzt.
   */
  public InputStreamReader createReader( final URL url ) throws IOException;
}
