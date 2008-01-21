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

import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;

/**
 * buffers line and returns a polygon <br>
 * accepts GMLXPathes <br>
 * 
 * @author thuel2
 * @param lineProperty
 * @param widthProperty
 *            can also be a constant double value
 * @param widthFactorProperty
 *            can also be a constant double value, defaults to 1
 * @param offsetProperty
 *            can also be a constant double value, defaults to 0
 * @param endCapStyleProperty
 *            can also be a constant integer value, defaults to 2 (CAP_BUTT)
 *            <ul>
 *            <li>1 (CAP_ROUND)</li>
 *            <li>2 (CAP_BUTT)</li>
 *            <li>3 (CAP_SQUARE)</li>
 *            </ul>
 * @param quadrantSegmentsProperty
 *            the number of line segments used to represent a quadrant of a circle; can also be a constant integer
 *            value, defaults to 5
 * @param userCanSetPolyIfLineIsNullProperty
 *            can also be a constant boolean, defaults to false <br>
 *            setValue(...) will return
 *            <ul>
 *            <li>valueToSet if true and lineProperty is null</li>
 *            <li>null else</li>
 *            </ul>
 */
public class LineBufferAsPolygon extends FeaturePropertyFunction
{
  private String[] m_lineXPathSegs;

  private String[] m_widthXPathSegs;

  private String[] m_widthFactorXPathSegs;

  private String[] m_offsetXPathSegs;

  private String[] m_endCapStyleXPathSegs;

  private String[] m_quadrantSegmentsXPathSegs;

  private String[] m_userCanSetPolyIfLineIsNullXPathSegs;

  // default values

  private final static double cDefaultWidthFactor = 1.0;

  private final static double cDefaultOffset = 0.0;

  private final static int cDefaultEndCapStyle = 2;

  private final static int cDefaultQuadrantSegments = 5;

  private final static boolean cDefaultUserCanSetPolyIfLineIsNull = false;

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final GMLWorkspace workspace = feature.getWorkspace();

    double widthFactor = cDefaultWidthFactor;
    double offset = cDefaultOffset;
    int endCapStyle = cDefaultEndCapStyle;
    int quadrantSegments = cDefaultQuadrantSegments;

    if( m_lineXPathSegs == null || m_widthXPathSegs == null )
      return null;

    // get userCanSetPolyIfLineIsNull (can be constant boolean value, has default)
    boolean userCanSetPolyIfLineIsNull;
    // set default
    if( m_userCanSetPolyIfLineIsNullXPathSegs == null )
      userCanSetPolyIfLineIsNull = cDefaultUserCanSetPolyIfLineIsNull;
    else
    {
      // read as property (xPath)
      try
      {
        GMLXPath path = new GMLXPath( feature );
        for( int ii = 0; ii < m_userCanSetPolyIfLineIsNullXPathSegs.length; ii++ )
        {
          if( m_userCanSetPolyIfLineIsNullXPathSegs[ii] != null )
            path = new GMLXPath( path, QName.valueOf( m_userCanSetPolyIfLineIsNullXPathSegs[ii] ) );
        }
        final Object obj = GMLXPathUtilities.query( path, workspace );

        if( !(obj instanceof Boolean) )
          // read as constant
          userCanSetPolyIfLineIsNull = Boolean.parseBoolean( m_userCanSetPolyIfLineIsNullXPathSegs[0] );
        else
          userCanSetPolyIfLineIsNull = ((Boolean) obj).booleanValue();
      }
      catch( GMLXPathException e )
      {
        e.printStackTrace();
        return null;
      }
    }
    // get line: read as property (xPath)
    GM_Curve lineValue;
    try
    {
      GMLXPath path = new GMLXPath( feature );
      for( int ii = 0; ii < m_lineXPathSegs.length; ii++ )
      {
        if( m_lineXPathSegs[ii] != null )
          path = new GMLXPath( path, QName.valueOf( m_lineXPathSegs[ii] ) );
      }
      final Object objGeometry = GMLXPathUtilities.query( path, workspace );

      if( userCanSetPolyIfLineIsNull && objGeometry == null )
        return currentValue;
      if( objGeometry == null )
        return null;
      if( !(objGeometry instanceof GM_Curve) )
        return null;
      lineValue = (GM_Curve) objGeometry;
    }
    catch( GMLXPathException e )
    {
      e.printStackTrace();
      return null;
    }

    // get width (has no default, can be constant value)
    double widthValue;
    // read as property (xPath)
    try
    {
      GMLXPath path = new GMLXPath( feature );
      for( int ii = 0; ii < m_widthXPathSegs.length; ii++ )
      {
        if( m_widthXPathSegs[ii] != null )
          path = new GMLXPath( path, QName.valueOf( m_widthXPathSegs[ii] ) );
      }
      final Object obj = GMLXPathUtilities.query( path, workspace );

      if( !(obj instanceof Number) )
        // read as constant
        widthValue = Double.parseDouble( m_widthXPathSegs[0] );
      else
      {
        final Number widthNumber = (Number) obj;
        if( widthNumber == null )
          return null;
        widthValue = widthNumber.doubleValue();
      }
    }
    catch( GMLXPathException e )
    {
      e.printStackTrace();
      return null;
    }

    // get widthFactor (has default, can be constant value)
    // read as property (xPath)
    if( m_widthFactorXPathSegs != null )
    {
      try
      {
        GMLXPath path = new GMLXPath( feature );
        for( int ii = 0; ii < m_widthFactorXPathSegs.length; ii++ )
        {
          if( m_widthFactorXPathSegs[ii] != null )
            path = new GMLXPath( path, QName.valueOf( m_widthFactorXPathSegs[ii] ) );
        }
        final Object obj = GMLXPathUtilities.query( path, workspace );
        if( !(obj instanceof Number) )
          // read as constant
          widthFactor = Double.parseDouble( m_widthFactorXPathSegs[0] );
        else
        {
          final Number widthNumber = (Number) obj;
          if( widthNumber != null )
            widthFactor = widthNumber.doubleValue();
        }
      }
      catch( GMLXPathException e )
      {
        e.printStackTrace();
        return null;
      }
    }
    // get offset (has default, can be constant value)
    // read as property (xPath)
    if( m_offsetXPathSegs != null )
    {
      try
      {
        GMLXPath path = new GMLXPath( feature );
        for( int ii = 0; ii < m_offsetXPathSegs.length; ii++ )
        {
          if( m_offsetXPathSegs[ii] != null )
            path = new GMLXPath( path, QName.valueOf( m_offsetXPathSegs[ii] ) );
        }
        final Object obj = GMLXPathUtilities.query( path, workspace );
        if( !(obj instanceof Number) )
          // read as constant
          offset = Double.parseDouble( m_offsetXPathSegs[0] );
        else
        {
          final Number offsetNumber = (Number) obj;
          if( offsetNumber != null )
            offset = offsetNumber.doubleValue();
        }
      }
      catch( GMLXPathException e )
      {
        e.printStackTrace();
        return null;
      }
    }
    // get endCapStyle (has default, can be constant value)
    // read as property (xPath)
    if( m_endCapStyleXPathSegs != null )
    {
      try
      {
        GMLXPath path = new GMLXPath( feature );
        for( int ii = 0; ii < m_endCapStyleXPathSegs.length; ii++ )
        {
          if( m_endCapStyleXPathSegs[ii] != null )
            path = new GMLXPath( path, QName.valueOf( m_endCapStyleXPathSegs[ii] ) );
        }
        final Object obj = GMLXPathUtilities.query( path, workspace );
        if( !(obj instanceof Number) )
          // read as constant
          endCapStyle = Integer.parseInt( m_endCapStyleXPathSegs[0] );
        else
        {
          final Number endCapNumber = (Number) obj;
          if( endCapNumber != null )
            offset = endCapNumber.intValue();
        }
      }
      catch( GMLXPathException e )
      {
        e.printStackTrace();
        return null;
      }
    }
    // get quadrantSegments (has default, can be constant value)
    // read as property (xPath)
    if( m_quadrantSegmentsXPathSegs != null )
    {
      try
      {
        GMLXPath path = new GMLXPath( feature );
        for( int ii = 0; ii < m_quadrantSegmentsXPathSegs.length; ii++ )
        {
          if( m_quadrantSegmentsXPathSegs[ii] != null )
            path = new GMLXPath( path, QName.valueOf( m_quadrantSegmentsXPathSegs[ii] ) );
        }
        final Object obj = GMLXPathUtilities.query( path, workspace );
        if( !(obj instanceof Number) )
          // read as constant
          quadrantSegments = Integer.parseInt( m_quadrantSegmentsXPathSegs[0] );
        else
        {
          final Number quadrantSegmentsNumber = (Number) obj;
          if( quadrantSegmentsNumber != null )
            quadrantSegments = quadrantSegmentsNumber.intValue();
        }
      }
      catch( GMLXPathException e )
      {
        e.printStackTrace();
        return null;
      }
    }
    // return buffered line as polygon
    try
    {
      final LineString lineString = (LineString) JTSAdapter.export( lineValue );
      final Geometry geometry = lineString.buffer( widthValue * widthFactor + offset, quadrantSegments, endCapStyle );
      final GM_Surface< ? > polygon = (GM_Surface< ? >) JTSAdapter.wrap( geometry );
      return polygon;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( final Map<String, String> properties )
  {
    final String lineProperty = properties.get( "lineProperty" );
    final String widthProperty = properties.get( "widthProperty" );
    final String widthFactorProperty = properties.get( "widthFactorProperty" );
    final String offsetProperty = properties.get( "offsetProperty" );
    final String endCapStyleProperty = properties.get( "endCapStyleProperty" );
    final String quadrantSegmentsProperty = properties.get( "quadrantSegmentsProperty" );
    final String userCanSetPolyIfLineIsNullProperty = properties.get( "userCanSetPolyIfLineIsNullProperty" );

    try
    {

      if( lineProperty == null )
        m_lineXPathSegs = null;
      else
        m_lineXPathSegs = lineProperty.split( "/" );

      if( widthProperty == null )
        m_widthXPathSegs = null;
      else
        m_widthXPathSegs = widthProperty.split( "/" );

      if( widthFactorProperty == null )
        m_widthFactorXPathSegs = null;
      else
        m_widthFactorXPathSegs = widthFactorProperty.split( "/" );

      if( offsetProperty == null )
        m_offsetXPathSegs = null;
      else
        m_offsetXPathSegs = offsetProperty.split( "/" );

      if( endCapStyleProperty == null )
        m_endCapStyleXPathSegs = null;
      else
        m_endCapStyleXPathSegs = endCapStyleProperty.split( "/" );

      if( quadrantSegmentsProperty == null )
        m_quadrantSegmentsXPathSegs = null;
      else
        m_quadrantSegmentsXPathSegs = quadrantSegmentsProperty.split( "/" );

      if( userCanSetPolyIfLineIsNullProperty == null )
        m_userCanSetPolyIfLineIsNullXPathSegs = null;
      else
        m_userCanSetPolyIfLineIsNullXPathSegs = userCanSetPolyIfLineIsNullProperty.split( "/" );

    }
    catch( final IllegalArgumentException e )
    {
      // ignore?
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    final GMLWorkspace workspace = feature.getWorkspace();
    Object obj = null;

    // get userCanSetPolyIfLineIsNull
    // set default
    boolean userCanSetPolyIfLineIsNull;
    if( m_userCanSetPolyIfLineIsNullXPathSegs == null )
      userCanSetPolyIfLineIsNull = cDefaultUserCanSetPolyIfLineIsNull;
    else
    {
      // try to read as property (xPath)
      try
      {
        GMLXPath path = new GMLXPath( feature );
        for( int ii = 0; ii < m_userCanSetPolyIfLineIsNullXPathSegs.length; ii++ )
        {
          if( m_userCanSetPolyIfLineIsNullXPathSegs[ii] != null )
          {
            path = new GMLXPath( path, QName.valueOf( m_userCanSetPolyIfLineIsNullXPathSegs[ii] ) );
          }
        }
        obj = GMLXPathUtilities.query( path, workspace );

        if( !(obj instanceof Boolean) )
          // read as constant
          userCanSetPolyIfLineIsNull = Boolean.parseBoolean( m_userCanSetPolyIfLineIsNullXPathSegs[0] );
        else
          userCanSetPolyIfLineIsNull = ((Boolean) obj).booleanValue();
      }
      catch( GMLXPathException e )
      {
        e.printStackTrace();
        return null;
      }
    }

    // get polyline (read as property - xPath, no default or constant value possible)
    GM_Curve polyline = null;
    try
    {
      GMLXPath path = new GMLXPath( feature );
      for( int ii = 0; ii < m_lineXPathSegs.length; ii++ )
      {
        if( m_lineXPathSegs[ii] != null )
        {
          path = new GMLXPath( path, QName.valueOf( m_lineXPathSegs[ii] ) );
        }
      }
      obj = GMLXPathUtilities.query( path, workspace );

      if( (obj instanceof GM_Curve) )
        polyline = (GM_Curve) obj;
    }
    catch( GMLXPathException e )
    {
      e.printStackTrace();
      return null;
    }

    if( userCanSetPolyIfLineIsNull && polyline == null )
      return valueToSet;
    else
      return null;
  }
}
