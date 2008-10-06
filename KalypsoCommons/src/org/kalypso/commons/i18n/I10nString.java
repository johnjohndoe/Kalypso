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
package org.kalypso.commons.i18n;

import java.util.Locale;

/**
 * An international string.
 * 
 * @author Gernot Belger
 */
public class I10nString
{
  private final String m_key;

  private final ITranslator m_translator;

  /**
   * Convenience constructor, same as {@link #I10nString(String, null)}. If this constructor is used, no translation
   * takes place, the key itself will be returned as value.
   */
  public I10nString( final String key )
  {
    this( key, null );
  }

  public I10nString( final String key, final ITranslator translator )
  {
    m_key = key;
    m_translator = translator;
  }

  public String getKey( )
  {
    return m_key;
  }

  public String getValue( )
  {
    return getValue( Locale.getDefault() );
  }

  public String getValue( final Locale locale )
  {
    return getValue( locale, new Object[] {} );
  }

  public String getValue( final Object... context )
  {
    return getValue( Locale.getDefault(), context );
  }

  public String getValue( final Locale locale, final Object... context )
  {
    if( m_key == null || m_key.length() == 0 || m_key.charAt( 0 ) != '%' )
      return m_key;

    if( m_translator == null )
      return m_key;

    return m_translator.get( m_key.substring( 1 ), locale, context );
  }

  public ITranslator getTranslator( )
  {
    return m_translator;
  }

}
