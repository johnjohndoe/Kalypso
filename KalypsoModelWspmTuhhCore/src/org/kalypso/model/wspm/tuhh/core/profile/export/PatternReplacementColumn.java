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
package org.kalypso.model.wspm.tuhh.core.profile.export;

import java.math.BigDecimal;

import org.apache.commons.lang.StringUtils;
import org.kalypso.commons.pair.IKeyValue;
import org.kalypso.commons.patternreplace.IPatternInput;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.IProfilePatternData;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.IValueWithFormat;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfilePatternInputReplacer;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfileResultPattern;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;

/**
 * @author Gernot Belger
 */
public class PatternReplacementColumn implements IProfileExportColumn
{
  public static final int NOT_SET = -1;

  private final String m_header;

  private final String m_pattern;

  private final int m_formatWidth;

  private final int m_formatPrecision;

  public PatternReplacementColumn( final String header, final String pattern )
  {
    this( header, pattern, NOT_SET, NOT_SET );
  }

  public PatternReplacementColumn( final String header, final String pattern, final int formatWidth, final int formatPrecision )
  {
    m_header = header;
    m_pattern = pattern;
    m_formatWidth = formatWidth;
    m_formatPrecision = formatPrecision;
  }

  @Override
  public String getHeader( )
  {
    return m_header;
  }

  public String getPattern( )
  {
    return m_pattern;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.export.IProfileExportColumn#getValue(org.kalypso.model.wspm.tuhh.core.profile.pattern.IProfilePatternData)
   */
  @Override
  public String formatValue( final IProfilePatternData data )
  {
    final ProfilePatternInputReplacer replacer = ProfilePatternInputReplacer.getINSTANCE();

    final IKeyValue<IPatternInput<IProfilePatternData>, String> pair = replacer.getSinglePatternValue( m_pattern );
    if( pair != null )
    {
      final IPatternInput<IProfilePatternData> token = pair.getKey();
      if( token instanceof IValueWithFormat )
      {
        final String params = pair.getValue();
        final IValueWithFormat< ? > valueWithFormat = (IValueWithFormat< ? >) token;
        final Object value = valueWithFormat.getValue( data, params );
        if( value == null )
          return StringUtils.EMPTY;

        return formatValue( value );
      }
    }

    return replacer.replaceTokens( m_pattern, data );
  }

  private String formatValue( final Object value )
  {
    final int precision = getFormatPrecision();
    final int width = getFormatWidth();

    final Class< ? > type = value.getClass();

    final String formatString = getFormatString( type, width, precision );
    return String.format( formatString, value );
  }

  private String getFormatString( final Class< ? > type, final int width, final int precision )
  {
    if( type == Double.class || type == Float.class || type == BigDecimal.class )
      return formatFormatString( 'f', width, precision );

    if( Number.class.isAssignableFrom( type ) )
      return formatFormatString( 'd', width, -1 );

    // Using width as precision -> we want to shorten the string to this width
    return formatFormatString( 's', -1, width );
  }

  protected String formatFormatString( final char formatType, final int width, final int precision )
  {
    if( width != NOT_SET && precision != NOT_SET )
      return String.format( "%%%d.%d%s", width, precision, formatType );

    if( width != NOT_SET )
      return String.format( "%%%d%s", width, formatType );

    if( precision != NOT_SET )
      return String.format( "%%.%d%s", precision, formatType );

    return String.format( "%%%s", formatType );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.export.IProfileExportColumn#getValue(org.kalypso.model.wspm.tuhh.core.profile.pattern.IProfilePatternData)
   */
  @Override
  public Object getValue( final IProfilePatternData data )
  {
    final ProfilePatternInputReplacer replacer = ProfilePatternInputReplacer.getINSTANCE();

    final IKeyValue<IPatternInput<IProfilePatternData>, String> pair = replacer.getSinglePatternValue( m_pattern );
    if( pair == null )
      return null;

    final IPatternInput<IProfilePatternData> token = pair.getKey();
    if( token instanceof IValueWithFormat )
    {
      final String params = pair.getValue();
      final IValueWithFormat< ? > valueWithFormat = (IValueWithFormat< ? >) token;
      return valueWithFormat.getValue( data, params );
    }

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.export.IProfileExportColumn#getFormatWidth()
   */
  @Override
  public int getFormatWidth( )
  {
    return m_formatWidth;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.export.IProfileExportColumn#getFormatPrecision()
   */
  @Override
  public int getFormatPrecision( )
  {
    return m_formatPrecision;
  }

  public Class< ? > getType( )
  {
    final ProfilePatternInputReplacer replacer = ProfilePatternInputReplacer.getINSTANCE();

    final IKeyValue<IPatternInput<IProfilePatternData>, String> pair = replacer.getSinglePatternValue( m_pattern );
    if( pair == null )
      return null;

    final IPatternInput<IProfilePatternData> token = pair.getKey();
    if( token instanceof IValueWithFormat )
    {
      final String params = pair.getValue();
      final IValueWithFormat< ? > valueWithFormat = (IValueWithFormat< ? >) token;
      return valueWithFormat.getType( params );
    }

    return null;
  }

  public int getDefaultWidth( )
  {
    final ProfilePatternInputReplacer replacer = ProfilePatternInputReplacer.getINSTANCE();

    final IKeyValue<IPatternInput<IProfilePatternData>, String> pair = replacer.getSinglePatternValue( m_pattern );
    if( pair == null )
      return 10;

    final IPatternInput<IProfilePatternData> token = pair.getKey();
    if( token instanceof IValueWithFormat )
    {
      final String params = pair.getValue();
      final IValueWithFormat< ? > valueWithFormat = (IValueWithFormat< ? >) token;
      return valueWithFormat.getDefaultWidth( params );
    }

    return 10;
  }

  public int getDefaultPrecision( )
  {
    final ProfilePatternInputReplacer replacer = ProfilePatternInputReplacer.getINSTANCE();

    final IKeyValue<IPatternInput<IProfilePatternData>, String> pair = replacer.getSinglePatternValue( m_pattern );
    if( pair == null )
      return 3;

    final IPatternInput<IProfilePatternData> token = pair.getKey();
    if( token instanceof IValueWithFormat )
    {
      final String params = pair.getValue();
      final IValueWithFormat< ? > valueWithFormat = (IValueWithFormat< ? >) token;
      return valueWithFormat.getDefaultPrecision( params );
    }

    return 3;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.export.IProfileExportColumn#getResult()
   */
  @Override
  public IWspmResult getResult( final IProfileFeature[] profiles )
  {
    final ProfilePatternInputReplacer replacer = ProfilePatternInputReplacer.getINSTANCE();

    final IKeyValue<IPatternInput<IProfilePatternData>, String> pair = replacer.getSinglePatternValue( m_pattern );
    if( pair == null )
      return null;

    final IPatternInput<IProfilePatternData> token = pair.getKey();
    if( token instanceof ProfileResultPattern )
    {
      final String params = pair.getValue();
      final ProfileResultPattern resultPattern = (ProfileResultPattern) token;
      return resultPattern.getResult( profiles, params );
    }

    return null;
  }
}
