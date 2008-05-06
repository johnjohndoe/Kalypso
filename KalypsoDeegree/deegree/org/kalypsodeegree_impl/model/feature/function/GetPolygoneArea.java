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
package org.kalypsodeegree_impl.model.feature.function;

import java.math.BigDecimal;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * @author thuel2
 */
public class GetPolygoneArea extends FeaturePropertyFunction
{
  private QName m_polygoneName;

  private String[] m_fractionDigitsXPathSegs;

  /**
   * @param fractionDigitsProperty
   *            determines the count of decimal places (optional) <br>
   *            accepts GMLXPathes <br>
   *            and constant values (double) for this property
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
    final String polyProp = properties.get( "polygoneProperty" );
    final String fracDigitsProp = properties.get( "fractionDigitsProperty" );

    try
    {
      m_polygoneName = polyProp == null ? null : QName.valueOf( polyProp );
    }
    catch( final IllegalArgumentException e )
    {
      e.printStackTrace();
    }

    if( fracDigitsProp == null )
      m_fractionDigitsXPathSegs = null;
    else
      m_fractionDigitsXPathSegs = fracDigitsProp.split( "/" );

  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( Feature feature, IPropertyType pt, Object currentValue )
  {
    if( m_polygoneName == null )
      return null;

    final IFeatureType featureType = feature.getFeatureType();
    final IPropertyType polyProperty = featureType.getProperty( m_polygoneName );

    if( polyProperty == null )
      return null;

    final Object objGeometry = feature.getProperty( polyProperty );
    if( !(objGeometry instanceof GM_Surface< ? >) )
      return null;

    final GM_Surface< ? > polygon = (GM_Surface< ? >) objGeometry;

    final double polygonArea = polygon.getArea();

    if( m_fractionDigitsXPathSegs == null )
      return polygonArea;
    else
    {

      int fractionDigitsCount;
      // read as property (xPath)
      try
      {
        GMLXPath path = new GMLXPath( feature );
        for( final String element : m_fractionDigitsXPathSegs )
        {
          if( element != null )
            path = new GMLXPath( path, QName.valueOf( element ) );
        }
        final GMLWorkspace workspace = feature.getWorkspace();
        final Object obj = GMLXPathUtilities.query( path, workspace );

        if( !(obj instanceof Number) )
        {
          try
          {
            // read as constant
            fractionDigitsCount = Integer.parseInt( m_fractionDigitsXPathSegs[0] );
          }
          catch( final NumberFormatException e )
          {
            fractionDigitsCount = 0;
          }
        }
        else
        {
          final Number fractionDigitNumber = (Number) obj;
          if( fractionDigitNumber == null )
            return null;
          fractionDigitsCount = fractionDigitNumber.intValue();
        }
      }
      catch( final GMLXPathException e )
      {
        e.printStackTrace();
        return null;
      }

      BigDecimal bd = new BigDecimal( polygonArea );
      bd = bd.setScale( fractionDigitsCount, BigDecimal.ROUND_UP );

      return bd.doubleValue();
    }

  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( Feature feature, IPropertyType pt, Object valueToSet )
  {
    // value can't be set by the user
    return null;
  }

}
