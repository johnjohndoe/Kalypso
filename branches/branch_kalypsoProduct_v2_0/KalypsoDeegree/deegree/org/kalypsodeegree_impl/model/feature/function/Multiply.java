/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.feature.function;

import java.math.BigDecimal;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * multiplies (double) all values whose propertyNames are handed over <br>
 * except for values of the property fractionDigitsProperty. This property determines the count of decimal places. <br>
 * accepts GMLXPathes <br>
 * and constant values (double)
 * 
 * @author thuel2
 */
public class Multiply extends FeaturePropertyFunction
{

  private Map<String, String> m_properties;
  private String[] m_fractionDigitsXPathSegs;

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
    m_properties = properties;
    final String fracDigitsProp = properties.get( "fractionDigitsProperty" );
    m_properties.remove( "fractionDigitsProperty" );
    
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

    final GMLWorkspace workspace = feature.getWorkspace();

    double multResult = 1.0;
    Object objNumber = null;
    for( String property : m_properties.values() )
    {
      if( property == null )
        return null;
      // decide if property is a constant (double)
      try
      {
        multResult = multResult * Double.parseDouble( property );
      }
      catch( NumberFormatException numForE )
      {
        try
        {
          GMLXPath path = new GMLXPath( feature );
          final String[] xPathSegs = property.split( "/" );
          for( int i = 0; i < xPathSegs.length; i++ )
          {
            if( xPathSegs[i] != null )
            {
              path = new GMLXPath( path, QName.valueOf( xPathSegs[i] ) );
            }
          }

          objNumber = GMLXPathUtilities.query( path, workspace );
          if( !(objNumber instanceof Number) )
            return null;

          multResult = multResult * ((Number) objNumber).doubleValue();
        }
        catch( GMLXPathException e )
        {
          e.printStackTrace();
          return null;
        }
      }
    }
  
    

    if( m_fractionDigitsXPathSegs == null )
      return multResult;
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

      BigDecimal bd = new BigDecimal(multResult);
      bd = bd.setScale(fractionDigitsCount,BigDecimal.ROUND_UP);

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
