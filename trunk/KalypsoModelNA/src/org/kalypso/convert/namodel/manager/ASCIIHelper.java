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
package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author doemming
 */
public class ASCIIHelper
{
  private final String[] m_asciiFormat;

  public ASCIIHelper( final URL parseDefinition )
  {
    InputStream is = null;
    List< ? > lines = new ArrayList<String>();
    try
    {
      is = parseDefinition.openStream();
      lines = IOUtils.readLines( is );
      is.close();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }

    m_asciiFormat = lines.toArray( new String[lines.size()] );
  }

  public String toAscii( final Feature feature, final int formatLineIndex )
  {
    return ASCIIHelper.toAsciiLine( feature, m_asciiFormat[formatLineIndex] );
  }

  private static String toAsciiLine( final Feature feature, final String formatLine )
  {
    final StringBuffer result = new StringBuffer( "" ); //$NON-NLS-1$
    final String[] formats = FortranFormatHelper.patternBrackets.split( formatLine );
    for( final String format : formats )
    {
      Matcher m = FortranFormatHelper.pPairFormat.matcher( format );
      if( m.matches() )
        result.append( ASCIIHelper.toAsciiValue( feature, format ) );
      m = FortranFormatHelper.pSpaceFormat.matcher( format );
      if( m.matches() )
        result.append( format.replace( '_', ' ' ) );
    }

    return result.toString();
  }

  private static String toAsciiValue( final Feature feature, final String pairFormat )
  {
    if( "".equals( pairFormat ) ) //$NON-NLS-1$
      return ""; //$NON-NLS-1$
    final String[] s = pairFormat.split( "," ); //$NON-NLS-1$
    if( "todo".equals( s[0] ) ) //$NON-NLS-1$
      return "(TODO:" + s[0] + ")"; //$NON-NLS-1$ //$NON-NLS-2$
    if( "IGNORE".equals( s[0] ) ) //$NON-NLS-1$
      return ""; //$NON-NLS-1$
    //        System.out.println(s[0]);
    final Object property = feature.getProperty( s[0] );

    if( property == null )
      return "(" + s[0] + "==NULL ?)"; //$NON-NLS-1$ //$NON-NLS-2$
    final String value = property.toString(); // PropertyName

    final String format = s[1];
    return FortranFormatHelper.printf( value, format );
  }

}
