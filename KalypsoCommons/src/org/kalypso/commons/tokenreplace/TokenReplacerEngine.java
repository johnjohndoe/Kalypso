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
package org.kalypso.commons.tokenreplace;

import java.util.HashMap;
import java.util.Map;

/**
 * Replacer engine to support string replacements of kind ${token:argument}.
 * 
 * @author belger
 */
public class TokenReplacerEngine
{
  /** String with which the tokens start. */
  public static final String TOKEN_START = "${";

  /** String with which the tokens end. */
  public static final String TOKEN_END = "}";

  private Map<String, ITokenReplacer> m_trMap = new HashMap<String, ITokenReplacer>();

  public TokenReplacerEngine( final ITokenReplacer[] trs )
  {
    for( final ITokenReplacer replace : trs )
      m_trMap.put( replace.getToken(), replace );
  }

  public String replaceTokens( final Object valueObject, final String tokenString )
  {
    if( tokenString == null )
      return null;
    final StringBuffer buffer = new StringBuffer( tokenString );

    int pos = 0;
    while( pos < buffer.length() )
    {
      final int start = buffer.indexOf( TOKEN_START, pos );
      if( start == -1 || start == buffer.length() - 1 )
        break;

      final int stop = buffer.indexOf( TOKEN_END, start + 1 );
      if( stop == -1 )
        break;

      final String currentTokenString = buffer.substring( start + 2, stop );
      if( currentTokenString.length() == 0 )
        buffer.replace( start, stop, "" );
      else
      {
        final int colon = currentTokenString.indexOf( ':' );
        final String currentToken;
        final String currentArgument;
        if( colon == -1 )
        {
          currentToken = currentTokenString;
          currentArgument = null;
        }
        else
        {
          currentToken = currentTokenString.substring( 0, colon );
          currentArgument = currentTokenString.substring( colon + 1 );
        }

        if( !m_trMap.containsKey( currentToken ) )
          pos = stop;
        else
        {
          final String replaced = replaceSafely( valueObject, currentToken, currentArgument );
          buffer.replace( start, stop + 1, replaced );
          pos += replaced.length();
        }
      }

    }

    return buffer.toString();
  }

  private String replaceSafely( final Object valueObject, final String currentToken, final String currentArgument )
  {
    try
    {
      return m_trMap.get( currentToken ).replaceToken( valueObject, currentArgument );
    }
    catch( final Throwable e )
    {
      return e.getLocalizedMessage();
    }
  }

}
