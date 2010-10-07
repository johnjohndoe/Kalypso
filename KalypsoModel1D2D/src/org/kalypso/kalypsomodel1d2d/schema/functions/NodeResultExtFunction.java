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
package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypsodeegree.graphics.sld.SldHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * @author ig
 */
public class NodeResultExtFunction extends FeaturePropertyFunction
{
  private static final String LAST_VALID_COLOR = "lastValidColor";

  private static final String LAST_VALID_VALUE = "lastValidValue";

  private static final String WAVEDIR_TYPE = "wavedir"; //$NON-NLS-1$

  private static final String DEFAULT_COLOR = "#ffffff"; //$NON-NLS-1$

  private static final String VALUE_TYPE = "value"; //$NON-NLS-1$

  private static final String DEPTH_TYPE = "depth"; //$NON-NLS-1$

  private QName m_resultTypeProperty;

  private QName m_locationProperty;

  private String m_kind;

  private Color m_maxColor = new Color( 0xCBFDFE );

  private Color m_minColor = new Color( 0x22375C );

  private int m_intClasses = 100;

  private String m_typeName;

  private Map<String, Object> m_mapSldSettingsIntern;

  private Color m_toColor;

  private Color m_fromColor;

  private Double m_fromValue;

  private Double m_toValue;

  private Integer m_amountClasses;

  private Map<Integer, String> m_mapActualColorsCache;

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( final Map<String, String> properties )
  {
    m_locationProperty = QName.valueOf( properties.get( "location" ) ); //$NON-NLS-1$
    m_typeName = properties.get( "resultType" );
    m_resultTypeProperty = QName.valueOf( m_typeName ); //$NON-NLS-1$
    m_kind = properties.get( "resultKind" ); //$NON-NLS-1$
    m_fromColor = m_minColor;
    m_toColor = m_maxColor;
    m_amountClasses = m_intClasses;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  @SuppressWarnings("unchecked")
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    m_typeName = pt.getQName().getLocalPart().toLowerCase().replace( m_kind, "" ); //$NON-NLS-1$
    String context = feature.getWorkspace().getContext().toExternalForm();
    int beginIndex = context.indexOf( ResultMeta1d2dHelper.TIME_STEP_PREFIX ) + ResultMeta1d2dHelper.TIME_STEP_PREFIX.length();
    String stepName = context.substring( beginIndex, beginIndex + 16 );
    m_mapSldSettingsIntern = NodeResultHelper.getSldSettingsMapForStep( stepName );
    final Double resultValue = (Double) feature.getProperty( m_resultTypeProperty );

    final GM_Point point = (GM_Point) feature.getProperty( m_locationProperty );
    if( VALUE_TYPE.equals( m_kind ) )
    {
      if( resultValue == null )
      {
        Object lLastValidResValue = m_mapSldSettingsIntern.get( LAST_VALID_VALUE );
        if( lLastValidResValue != null ){
          return lLastValidResValue;
        }
        if( m_resultTypeProperty.getLocalPart().toLowerCase().contains( WAVEDIR_TYPE ) )
        {
          m_mapSldSettingsIntern.put( LAST_VALID_VALUE, null );
          return null;
        }
        return Double.NaN;
      }
      else if( resultValue == 0.0 ){
        //filter evaluation of size awaits some value bigger then 0.0 :) also for direction
          double lPseudoZeroValue = 0.000001;
          m_mapSldSettingsIntern.put( LAST_VALID_VALUE, lPseudoZeroValue );
          return lPseudoZeroValue;
      }
      else
      {
        if( m_resultTypeProperty.getLocalPart().toLowerCase().contains( WAVEDIR_TYPE ) && resultValue.equals( Double.NaN ) )
        {
          m_mapSldSettingsIntern.put( LAST_VALID_VALUE, null );
          return null;
        }
        if( m_resultTypeProperty.getLocalPart().toLowerCase().contains( DEPTH_TYPE ) )
        {
          double depth = resultValue.doubleValue() - point.getZ();
          m_mapSldSettingsIntern.put( LAST_VALID_VALUE, depth );
          return depth;
        }
        else
        {
          m_mapSldSettingsIntern.put( LAST_VALID_VALUE, resultValue.doubleValue() );
          return resultValue.doubleValue();
        }
      }
    }
    else
    {
      if( resultValue == null )
      {
        //we set the last value for valid color based on the assumption that we are painting on the screen in sequential way: e.g. from left to right 
        String lNullRes = (String) m_mapSldSettingsIntern.get( LAST_VALID_COLOR );
        if( lNullRes == null )
        {
          return DEFAULT_COLOR;
        }
        return lNullRes;
      }
      else
      {
        try
        {
          m_fromColor = (Color) m_mapSldSettingsIntern.get( NodeResultHelper.COLOR_MIN_PREFIX + m_typeName );
          m_toColor = (Color) m_mapSldSettingsIntern.get( NodeResultHelper.COLOR_MAX_PREFIX + m_typeName );
          m_fromValue = (Double) m_mapSldSettingsIntern.get( NodeResultHelper.VALUE_MIN_PREFIX + m_typeName );
          m_toValue = (Double) m_mapSldSettingsIntern.get( NodeResultHelper.VALUE_MAX_PREFIX + m_typeName );
          m_amountClasses = ((Double) m_mapSldSettingsIntern.get( NodeResultHelper.AMOUNT_OF_CLASSES_PREFIX + m_typeName )).intValue();
          m_mapActualColorsCache = (Map<Integer, String>) m_mapSldSettingsIntern.get( NodeResultHelper.COLOR_MAP_PREFIX + m_typeName );
          if( m_mapActualColorsCache == null )
          {
            m_mapActualColorsCache = new HashMap<Integer, String>();
            m_mapSldSettingsIntern.put( NodeResultHelper.COLOR_MAP_PREFIX + m_typeName, m_mapActualColorsCache );
          }
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
        double localValue = 0;
        if( m_typeName.contains( DEPTH_TYPE ) )
        {
          double depth = resultValue.doubleValue() - point.getZ();
          localValue = depth;
        }
        else
        {
          localValue = resultValue.doubleValue();
        }

        localValue = (int) (((localValue - m_fromValue) * ( m_amountClasses ) / (m_toValue - m_fromValue) ) );
        String lColorFormatedStringCached = m_mapActualColorsCache.get( localValue );
        if( lColorFormatedStringCached == null )
        {
          Color lActColor = SldHelper.interpolateColor( m_fromColor, m_toColor, (int) localValue, m_amountClasses );
          lColorFormatedStringCached = formatColor( lActColor );
          m_mapActualColorsCache.put( (int) localValue, lColorFormatedStringCached );
        }
        m_mapSldSettingsIntern.put( LAST_VALID_COLOR, lColorFormatedStringCached );

        return lColorFormatedStringCached;
      }
    }
  }

  private String formatColor( Color c )
  {
    try
    {
      String r = (c.getRed() < 16) ? "0" + Integer.toHexString( c.getRed() ) : Integer.toHexString( c.getRed() ); //$NON-NLS-1$
      String g = (c.getGreen() < 16) ? "0" + Integer.toHexString( c.getGreen() ) : Integer.toHexString( c.getGreen() ); //$NON-NLS-1$
      String b = (c.getBlue() < 16) ? "0" + Integer.toHexString( c.getBlue() ) : Integer.toHexString( c.getBlue() ); //$NON-NLS-1$
      return "#" + r + g + b; //$NON-NLS-1$
    }
    catch( Exception e )
    {
      return DEFAULT_COLOR;
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    return valueToSet;
  }

}
