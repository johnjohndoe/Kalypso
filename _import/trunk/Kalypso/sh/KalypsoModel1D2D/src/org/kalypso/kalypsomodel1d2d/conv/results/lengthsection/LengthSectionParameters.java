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
package org.kalypso.kalypsomodel1d2d.conv.results.lengthsection;

import java.math.BigDecimal;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * @author Thomas Jung
 * 
 */
public class LengthSectionParameters
{
  private final static Pattern m_patternDouble = Pattern.compile( "[0-9]+[\\.\\,]?[0-9]*?" ); //$NON-NLS-1$

  private final FeatureList m_riverFeatures;

  private final IPropertyType m_riverNamePropertyType;

  private final IPropertyType m_fromStationPropertyType;

  private final IPropertyType m_toStationPropertyType;

  private BigDecimal[] m_stationList;

  private final BigDecimal m_stationWidth;

  private final SortedSet<Object> m_fromValueSet = new TreeSet<Object>();

  private final SortedSet<Object> m_toValueSet = new TreeSet<Object>();

  private final String m_selectedRiverName;

  public LengthSectionParameters( FeatureList riverFeatures, IPropertyType riverNamePropertyType, IPropertyType fromStationPropertyType, IPropertyType toStationPropertyType, final String selectedRiverName, final BigDecimal stationWidth )
  {
    m_riverFeatures = riverFeatures;
    m_riverNamePropertyType = riverNamePropertyType;
    m_fromStationPropertyType = fromStationPropertyType;
    m_toStationPropertyType = toStationPropertyType;
    m_selectedRiverName = selectedRiverName;
    m_stationWidth = stationWidth;

    if( m_riverFeatures != null )
      calculatePointList();
  }

  public FeatureList getRiverFeatures( )
  {
    return m_riverFeatures;
  }

  public IPropertyType getRiverNamePropertyType( )
  {
    return m_riverNamePropertyType;
  }

  public IPropertyType getFromStationPropertyType( )
  {
    return m_fromStationPropertyType;
  }

  public IPropertyType getToStationPropertyType( )
  {
    return m_toStationPropertyType;
  }

  private void calculatePointList( )
  {
    m_fromValueSet.clear();
    m_toValueSet.clear();

    for( Object o : m_riverFeatures )
    {
      Feature riverFeature = (Feature) o;

      if( m_fromStationPropertyType instanceof IValuePropertyType )
      {
        IValuePropertyType vpt = (IValuePropertyType) m_fromStationPropertyType;
        if( vpt.getValueClass() == String.class || vpt.getValueClass() == Double.class || vpt.getValueClass() == Integer.class || vpt.getValueClass() == Long.class )
        {
          final String riverName = (String) riverFeature.getProperty( m_riverNamePropertyType );
          if( riverName.equals( m_selectedRiverName ) )
            m_fromValueSet.add( riverFeature.getProperty( m_fromStationPropertyType ) );
        }
      }
      if( m_toStationPropertyType instanceof IValuePropertyType )
      {
        IValuePropertyType vpt = (IValuePropertyType) m_toStationPropertyType;
        if( vpt.getValueClass() == String.class || vpt.getValueClass() == Double.class || vpt.getValueClass() == Integer.class || vpt.getValueClass() == Long.class )
        {
          final String riverName = (String) riverFeature.getProperty( m_riverNamePropertyType );
          if( riverName.equals( m_selectedRiverName ) )
            m_toValueSet.add( riverFeature.getProperty( m_toStationPropertyType ) );
        }
      }
    }

    if( m_fromValueSet.size() == 0 || m_toValueSet.size() == 0 )
      return;

    BigDecimal min = null;
    BigDecimal max = null;

    Object first = m_fromValueSet.first();
    min = getValue( first );

    Object last = m_toValueSet.last();
    max = getValue( last );

    double mod = min.doubleValue() % m_stationWidth.doubleValue();

    double firstStep = min.doubleValue() - mod;

    BigDecimal minDecimal = new BigDecimal( firstStep ).setScale( 1, BigDecimal.ROUND_FLOOR );

    // final BigDecimal minDecimal = min.setScale( 1, BigDecimal.ROUND_FLOOR );
    final BigDecimal maxDecimal = max.setScale( 1, BigDecimal.ROUND_CEILING );

    final BigDecimal subtract = maxDecimal.subtract( minDecimal );
    final Double divide = subtract.doubleValue() / m_stationWidth.doubleValue();
    final BigDecimal value = new BigDecimal( divide ).setScale( 5, BigDecimal.ROUND_HALF_UP );
    final int numOfClasses = (value).intValue() + 1;

    m_stationList = new BigDecimal[numOfClasses];
    for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
    {
      final double currentValue = minDecimal.doubleValue() + currentClass * m_stationWidth.doubleValue();

      BigDecimal station = new BigDecimal( currentValue );

      m_stationList[currentClass] = station;
    }
  }

  private BigDecimal getValue( Object o )
  {
    if( o instanceof String )
    {
      String minString = (String) o;
      minString = minString.replace( "+", "." ); //$NON-NLS-1$ //$NON-NLS-2$
      minString = minString.replace( ",", "." ); //$NON-NLS-1$ //$NON-NLS-2$
      return checkDoubleTextValue( minString );
    }
    else if( o instanceof Double )
    {
      Double doubleValue = (Double) o;
      return new BigDecimal( doubleValue );
    }
    else if( o instanceof Integer )
    {
      Integer intValue = (Integer) o;
      return new BigDecimal( intValue );
    }
    else if( o instanceof Long )
    {
      Long longValue = (Long) o;
      return new BigDecimal( longValue );
    }
    return null;
  }

  public BigDecimal[] getStationList( )
  {
    return m_stationList;
  }

  private static BigDecimal checkDoubleTextValue( String tempText )
  {
    final Matcher m = m_patternDouble.matcher( tempText );

    if( m.matches() )
    {
      tempText = tempText.replaceAll( ",", "." ); //$NON-NLS-1$ //$NON-NLS-2$

      BigDecimal db = new BigDecimal( tempText );
      if( db.doubleValue() > 0 )
        return db;

    }
    return null;
  }

  public String getSelectedRiverName( )
  {
    return m_selectedRiverName;
  }

}
