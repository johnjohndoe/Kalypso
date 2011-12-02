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
package org.kalypso.model.wspm.tuhh.core.profile.pattern;

import java.math.BigDecimal;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.lang.StringUtils;
import org.kalypso.commons.pair.IKeyValue;
import org.kalypso.commons.pair.KeyValueFactory;
import org.kalypso.commons.patternreplace.AbstractPatternInput;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

import com.google.common.collect.ComputationException;
import com.google.common.collect.MapMaker;

public final class ProfileResultPattern extends AbstractPatternInput<IProfilePatternData> implements IValueWithFormat<Object>
{
  private final ConcurrentMap<IKeyValue<IProfileFeature, String>, WspmResultLengthSection> m_lengthSectionCache = new MapMaker().expiration( 5, TimeUnit.SECONDS ).makeComputingMap( new ResultFinder() );

  public ProfileResultPattern( )
  {
    super( "Result", "Result" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.pattern.IValueWithFormat#getType(java.lang.String)
   */
  @Override
  public Class< ? extends Object> getType( final String params )
  {
    final IComponent component = findComponent( params );
    if( component == null )
      return String.class;

    final QName valueTypeName = component.getValueTypeName();
    return XmlTypes.toJavaClass( valueTypeName );
  }

  protected IComponent findComponent( final String params )
  {
    final IKeyValue<String, String> parsedParams = parseParams( params );
    if( parsedParams == null )
      return null;

    final String componentID = parsedParams.getValue();
    final IComponent component = ComponentUtilities.getFeatureComponent( componentID );
    return component;
  }

  @Override
  public Object getValue( final IProfilePatternData data, final String param )
  {
    final IProfileFeature profileFeature = data.getProfileFeature();
    if( profileFeature == null )
      return null;

    final IKeyValue<String, String> parsedParams = parseParams( param );
    if( parsedParams == null )
      return null;

    final String nodeID = parsedParams.getKey();
    final String component = parsedParams.getValue();

    try
    {
      final IKeyValue<IProfileFeature, String> pair = KeyValueFactory.createPairEqualsValue( profileFeature, nodeID );
      final WspmResultLengthSection lengthSection = m_lengthSectionCache.get( pair );
      final BigDecimal station = profileFeature.getBigStation();
      return lengthSection.getValue( station, component );
    }
    catch( final NullPointerException e )
    {
      // happens, if cache has no value
      return StringUtils.EMPTY;
    }
    catch( final ComputationException e )
    {
      // happens, if compution functin throws an exception
      return StringUtils.EMPTY;
    }
  }

  public IWspmResult getResult( final IProfileFeature[] profiles, final String param )
  {
    final IKeyValue<String, String> parsedParams = parseParams( param );
    if( parsedParams == null )
      return null;

    final String nodeID = parsedParams.getKey();
    if( StringUtils.isBlank( nodeID ) )
      return null;

    if( profiles.length == 0 )
      return null;

    // CHECK: we assume, that all profiles live in the same container -> we can use the first profile to determine the
    // results... is this always correct?
    return ResultFinder.findResult( profiles[0], nodeID );
  }

  private IKeyValue<String, String> parseParams( final String param )
  {
    final String[] params = param.split( "\\:", 2 );
    if( params.length != 2 )
      return null;

    final String nodeID = params[0];
    final String component = guessComponent( params[1] );

    if( StringUtils.isBlank( nodeID ) )
      return null;
    if( StringUtils.isBlank( component ) )
      return null;

    return KeyValueFactory.createPairEqualsBoth( nodeID, component );
  }

  /**
   * @see org.kalypso.commons.patternreplace.IPatternInput#getReplacement(java.lang.Object, java.lang.String)
   */
  @Override
  public String getReplacement( final IProfilePatternData data, final String param )
  {
    final Object value = getValue( data, param );
    if( value == null )
      return StringUtils.EMPTY;

    return formatValue( value );
  }

  private String guessComponent( final String component )
  {
    if( component.contains( ":" ) )
      return component;

    if( component.startsWith( "#" ))
      return IWspmConstants.URN_OGC_GML_DICT_KALYPSO_MODEL_WSPM_COMPONENTS + component;

    return IWspmConstants.LENGTH_SECTION_PROPERTY + component;
  }

  private String formatValue( final Object value )
  {
    if( value == null )
      return StringUtils.EMPTY;

    // TODO: we need a more sophisticated handling of types here...
    if( value instanceof Number )
      return String.format( "%.4f", value ); //$NON-NLS-1$

    return ObjectUtils.toString( value );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.pattern.IValueWithFormat#getDefaultWidth(java.lang.String)
   */
  @Override
  public int getDefaultWidth( final String params )
  {
    final Class< ? extends Object> type = getType( params );
    if( type == Double.class || type == Float.class || type == BigDecimal.class )
      return 30;

    if( type == Float.class )
      return 15;

    return 10;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.pattern.IValueWithFormat#getDefaultPrecision(java.lang.String)
   */
  @Override
  public int getDefaultPrecision( final String params )
  {
    final Class< ? extends Object> type = getType( params );
    if( type == Double.class || type == Float.class || type == BigDecimal.class )
      return 10;

    if( type == Float.class )
      return 5;

    return 0;
  }

}