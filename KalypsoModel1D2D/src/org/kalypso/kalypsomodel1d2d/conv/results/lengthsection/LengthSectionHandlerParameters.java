/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * @author Gernot Belger
 *
 */
public class LengthSectionHandlerParameters
{
  private final Pattern m_patternDouble = Pattern.compile( "[0-9]+[\\.\\,]?[0-9]*?" ); //$NON-NLS-1$

  private final Feature[] m_selectedRivers;

  private final String m_selectedRiverName;

  private final IValuePropertyType m_stationFromProperty;

  private final IValuePropertyType m_stationToProperty;

  private final BigDecimal m_samplingDistance;

  private final boolean m_useKmValues;

  public LengthSectionHandlerParameters( final Feature[] selectedRivers, final String selectedRiverName, final IValuePropertyType stationFromProperty, final IValuePropertyType stationToProperty, final BigDecimal samplingDistance, final boolean useKmValues )
  {
    m_selectedRivers = selectedRivers;
    m_selectedRiverName = selectedRiverName;
    m_stationFromProperty = stationFromProperty;
    m_stationToProperty = stationToProperty;
    m_samplingDistance = samplingDistance;
    m_useKmValues = useKmValues;
  }

  public Feature[] getSelectedRivers( )
  {
    return m_selectedRivers;
  }

  public String getSelectedRiverName( )
  {
    return m_selectedRiverName;
  }

  public IValuePropertyType getStationFromProperty( )
  {
    return m_stationFromProperty;
  }

  public IValuePropertyType getStationToProperty( )
  {
    return m_stationToProperty;
  }

  public BigDecimal getSamplingDistance( )
  {
    return m_samplingDistance;
  }

  public boolean getUseKmValues( )
  {
    return m_useKmValues;
  }

  /**
   * Moved into parameter class, so everyone accesses the station attributes in the same way.
   */
  public BigDecimal getNumberProperty( final Feature feature, final IValuePropertyType property, final double defaultValue )
  {
    if( property == null )
      return new BigDecimal( defaultValue );

    final Object value = feature.getProperty( property );
    if( value == null )
      return new BigDecimal( defaultValue );

    // TODO: a bit dubious, we shouldn't allow too much nonsensical input data...
    if( value instanceof String )
    {
      String minString = (String) value;
      minString = minString.replace( "+", "." ); //$NON-NLS-1$ //$NON-NLS-2$
      minString = minString.replace( ",", "." ); //$NON-NLS-1$ //$NON-NLS-2$
      return checkDoubleTextValue( minString );
    }

    if( value instanceof Number )
    {
      final Number numberValue = (Number) value;
      return new BigDecimal( numberValue.doubleValue() );
    }

    throw new IllegalArgumentException();
  }

  private BigDecimal checkDoubleTextValue( String tempText )
  {
    final Matcher m = m_patternDouble.matcher( tempText );

    if( m.matches() )
    {
      tempText = tempText.replaceAll( ",", "." ); //$NON-NLS-1$ //$NON-NLS-2$

      final BigDecimal db = new BigDecimal( tempText );
      if( db.doubleValue() > 0 )
        return db;

    }
    return null;
  }

  public GM_Curve getGeometry( final Feature feature )
  {
    // TODO: might be another geometry!
    final GM_Object geom = feature.getDefaultGeometryPropertyValue();
    if( geom == null )
      return null;

    final GM_Curve[] allCurves = (GM_Curve[]) geom.getAdapter( GM_Curve[].class );
    // TODO: message to user?
    if( allCurves.length > 1 )
      return null;

    return allCurves[0];
  }
}