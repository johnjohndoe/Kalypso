/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.graphics.displayelements;

import java.util.ArrayList;

import org.deegree.graphics.displayelements.DisplayElement;
import org.deegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.deegree.graphics.displayelements.LabelDisplayElement;
import org.deegree.graphics.displayelements.LineStringDisplayElement;
import org.deegree.graphics.displayelements.PointDisplayElement;
import org.deegree.graphics.displayelements.PolygonDisplayElement;
import org.deegree.graphics.displayelements.RasterDisplayElement;
import org.deegree.graphics.sld.FeatureTypeStyle;
import org.deegree.graphics.sld.Geometry;
import org.deegree.graphics.sld.LineSymbolizer;
import org.deegree.graphics.sld.PointSymbolizer;
import org.deegree.graphics.sld.PolygonSymbolizer;
import org.deegree.graphics.sld.RasterSymbolizer;
import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.sld.TextSymbolizer;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiPrimitive;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Primitive;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.deegree_impl.graphics.sld.PointSymbolizer_Impl;
import org.deegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.deegree_impl.graphics.sld.RasterSymbolizer_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.opengis.gc.GC_GridCoverage;

/**
 * Factory class for the different kinds of <tt>DisplayElement</tt>s.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class DisplayElementFactory
{

  /**
   * returns the display elements associated to a feature
   */
  public static DisplayElement[] createDisplayElement( Object o, UserStyle[] styles )
      throws IncompatibleGeometryTypeException
  {
    Debug.debugMethodBegin( "DisplayElementFactory", "getDisplayElement" );

    ArrayList list = new ArrayList( 20 );

    if( o instanceof Feature )
    {
      Feature feature = (Feature)o;

      try
      {
        String featureTypeName = feature.getFeatureType().getName();

        for( int i = 0; i < styles.length; i++ )
        {

          if( styles[i] == null )
          {
            // create display element from default style
            DisplayElement de = buildDisplayElement( feature );
            if( de != null )
            {
              list.add( de );
            }
          }
          else
          {
            FeatureTypeStyle[] fts = styles[i].getFeatureTypeStyles();

            for( int k = 0; k < fts.length; k++ )
            {
              if( fts[k].getFeatureTypeName() == null
                  || featureTypeName.equals( fts[k].getFeatureTypeName() ) )
              {
                Rule[] rules = fts[k].getRules();

                for( int n = 0; n < rules.length; n++ )
                {

                  // does the filter rule apply?
                  Filter filter = rules[n].getFilter();

                  if( filter != null )
                  {
                    try
                    {
                      if( !filter.evaluate( feature ) )
                      {
                        Object[] props = feature.getProperties();

                        for( int m = 0; m < props.length; m++ )
                        {
                          if( !( props[m] instanceof String ) )
                          {
                            continue;
                          }
                        }
                        continue;
                      }

                      Object[] props = feature.getProperties();

                      for( int m = 0; m < props.length; m++ )
                      {
                        if( !( props[m] instanceof String ) )
                        {
                          continue;
                        }
                      }
                    }
                    catch( FilterEvaluationException e )
                    {
                      System.out.println( "Error evaluating filter: " + e );

                      continue;
                    }
                  }

                  // Filter expression is true for this feature, so a
                  // corresponding DisplayElement has to be added to the
                  // list
                  Symbolizer[] symbolizers = rules[n].getSymbolizers();

                  for( int u = 0; u < symbolizers.length; u++ )
                  {
                    DisplayElement displayElement = DisplayElementFactory.buildDisplayElement(
                        feature, symbolizers[u] );

                    if( displayElement != null )
                    {
                      list.add( displayElement );
                    }
                  }
                }
              }
            }
          }
        }

        Debug.debugMethodEnd();
      }
      catch( IncompatibleGeometryTypeException e )
      {
        System.out.println( e );
        e.printStackTrace();
      }
    }
    else
    {
      RasterSymbolizer symbolizer = new RasterSymbolizer_Impl();
      list.add( buildRasterDisplayElement( (GC_GridCoverage)o, symbolizer ) );
    }

    DisplayElement[] de = new DisplayElement[list.size()];
    return (DisplayElement[])list.toArray( de );
  }

  /**
   * Builds a <tt>DisplayElement</tt> using the given <tt>Feature</tt> or
   * raster and <tt>Symbolizer</tt>.
   * <p>
   * 
   * @param o
   *          contains the geometry or raster information (Feature or raster)
   * @param symbolizer
   *          contains the drawing (style) information and selects the geometry
   *          property of the <tt>Feature</tt> to be drawn
   * @throws IncompatibleGeometryTypeException
   *           if the selected geometry of the <tt>Feature</tt> is not
   *           compatible with the <tt>Symbolizer</tt>
   * @return constructed <tt>DisplayElement</tt>
   */
  public static DisplayElement buildDisplayElement( Object o, Symbolizer symbolizer )
      throws IncompatibleGeometryTypeException
  {
    DisplayElement displayElement = null;

    if( o instanceof Feature )
    {
      Feature feature = (Feature)o;

      // determine the geometry property to be used
      GM_Object geoProperty = null;
      Geometry geometry = symbolizer.getGeometry();

      if( geometry != null )
      {
        geoProperty = (GM_Object)feature.getProperty( geometry.getPropertyName() );
      }
      else
      {
        geoProperty = feature.getDefaultGeometryProperty();
      }

      // if the geometry property is null, do not build a DisplayElement
      if( geoProperty == null )
      {
        return null;
      }

      // PointSymbolizer
      if( symbolizer instanceof PointSymbolizer )
      {
        displayElement = buildPointDisplayElement( feature, geoProperty,
            (PointSymbolizer)symbolizer );
      } // LineSymbolizer
      else if( symbolizer instanceof LineSymbolizer )
      {
        displayElement = buildLineStringDisplayElement( feature, geoProperty,
            (LineSymbolizer)symbolizer );
      } // PolygonSymbolizer
      else if( symbolizer instanceof PolygonSymbolizer )
      {
        displayElement = buildPolygonDisplayElement( feature, geoProperty,
            (PolygonSymbolizer)symbolizer );
      }
      else if( symbolizer instanceof TextSymbolizer )
      {
        displayElement = buildLabelDisplayElement( feature, geoProperty, (TextSymbolizer)symbolizer );
      }
    }
    else
    {
      if( symbolizer instanceof RasterSymbolizer )
      {
        displayElement = buildRasterDisplayElement( (GC_GridCoverage)o,
            (RasterSymbolizer)symbolizer );
      }
    }

    return displayElement;
  }

  /**
   * Builds a <tt>DisplayElement</tt> using the given <tt>Feature</tt> or
   * Raster and a default <tt>Symbolizer</tt>.
   * <p>
   * 
   * @param o
   *          contains the geometry or raster information (Feature or raster)
   * @throws IncompatibleGeometryTypeException
   *           if the selected geometry of the <tt>Feature</tt> is not
   *           compatible with the <tt>Symbolizer</tt>
   * @return constructed <tt>DisplayElement</tt>
   */
  public static DisplayElement buildDisplayElement( Object o )
      throws IncompatibleGeometryTypeException
  {
    Debug.debugMethodBegin( "DisplayElementFactory", "buildDisplayElement(Object)" );

    DisplayElement displayElement = null;

    if( o instanceof GC_GridCoverage )
    {
      RasterSymbolizer symbolizer = new RasterSymbolizer_Impl();
      displayElement = buildRasterDisplayElement( (GC_GridCoverage)o, symbolizer );
    }
    else
    {
      Feature feature = (Feature)o;
      // determine the geometry property to be used
      GM_Object geoProperty = feature.getDefaultGeometryProperty();

      // if the geometry property is null, do not build a DisplayElement
      if( geoProperty == null )
      {
        System.out.println( "no displayelement, cause geoProperty of feature is null" );
        return null;
      }

      // PointSymbolizer
      if( geoProperty instanceof GM_Point || geoProperty instanceof GM_MultiPoint )
      {
        PointSymbolizer symbolizer = new PointSymbolizer_Impl();
        displayElement = buildPointDisplayElement( feature, geoProperty, symbolizer );
      } // LineSymbolizer
      else if( geoProperty instanceof GM_Curve || geoProperty instanceof GM_MultiCurve )
      {
        LineSymbolizer symbolizer = new LineSymbolizer_Impl();
        displayElement = buildLineStringDisplayElement( feature, geoProperty, symbolizer );
      } // PolygonSymbolizer
      else if( geoProperty instanceof GM_Surface || geoProperty instanceof GM_MultiSurface )
      {
        PolygonSymbolizer symbolizer = new PolygonSymbolizer_Impl();
        displayElement = buildPolygonDisplayElement( feature, geoProperty, symbolizer );
      }
      else
      {
        throw new IncompatibleGeometryTypeException( "not a valid geometry type" );
      }
    }

    Debug.debugMethodEnd();
    return displayElement;
  }

  /**
   * Creates a <tt>PointDisplayElement</tt> using the given geometry and style
   * information.
   * <p>
   * 
   * @param feature
   *          associated <tt>Feature<tt>
   * @param geom geometry information
   * @param sym style information
   * @return constructed <tt>PointDisplayElement</tt>
   */
  public static PointDisplayElement buildPointDisplayElement( Feature feature, GM_Object geom,
      PointSymbolizer sym )
  {

    // if the geometry is not a point geometry, the centroid(s) of the
    // geometry will be used
    PointDisplayElement displayElement = null;

    if( geom instanceof GM_Point )
    {
      displayElement = new PointDisplayElement_Impl( feature, (GM_Point)geom, sym );
    }
    else if( geom instanceof GM_MultiPoint )
    {
      displayElement = new PointDisplayElement_Impl( feature, (GM_MultiPoint)geom, sym );
    }
    else if( geom instanceof GM_MultiPrimitive )
    {
      GM_Primitive[] primitives = ( (GM_MultiPrimitive)geom ).getAllPrimitives();
      GM_Point[] centroids = new GM_Point[primitives.length];

      for( int i = 0; i < primitives.length; i++ )
      {
        centroids[i] = primitives[i].getCentroid();
      }

      try
      {
        displayElement = new PointDisplayElement_Impl( feature, GeometryFactory
            .createGM_MultiPoint( centroids ), sym );
      }
      catch( GM_Exception e )
      {
        System.out
            .println( "GM_Exception caught in DisplayElementFactory.buildPointDisplayElement: " + e );
        e.printStackTrace();
      }
    }
    else
    {
      displayElement = new PointDisplayElement_Impl( feature, geom.getCentroid(), sym );
    }

    return displayElement;
  }

  /**
   * Creates a <tt>LineStringDisplayElement</tt> using the given geometry and
   * style information.
   * <p>
   * 
   * @param feature
   *          associated <tt>Feature<tt>
   * @param geom geometry information
   * @param sym style information
   * @throws IncompatibleGeometryTypeException if the geometry property is not
   *         a <tt>GM_Curve</tt> or a <tt>GM_MultiCurve</tt>
   * @return constructed <tt>LineStringDisplayElement</tt>
   */
  public static LineStringDisplayElement buildLineStringDisplayElement( Feature feature,
      GM_Object geom, LineSymbolizer sym ) throws IncompatibleGeometryTypeException
  {
    LineStringDisplayElement displayElement = null;

    if( geom instanceof GM_Curve )
    {
      displayElement = new LineStringDisplayElement_Impl( feature, (GM_Curve)geom, sym );
    }
    else if( geom instanceof GM_MultiCurve )
    {
      displayElement = new LineStringDisplayElement_Impl( feature, (GM_MultiCurve)geom, sym );
    }
    else
    {
      throw new IncompatibleGeometryTypeException(
          "Tried to create a LineStringDisplayElement from a geometry with "
              + "an incompatible / unsupported type: '" + geom.getClass().getName() + "'!" );
    }

    return displayElement;
  }

  /**
   * Creates a <tt>PolygonDisplayElement</tt> using the given geometry and
   * style information.
   * <p>
   * 
   * @param feature
   *          associated <tt>Feature<tt>
   * @param gmObject geometry information
   * @param sym style information
   * @throws IncompatibleGeometryTypeException if the geometry property is not
   *         a <tt>GM_Surface</tt> or a <tt>GM_MultiSurface</tt>
   * @return constructed <tt>PolygonDisplayElement</tt>
   */
  public static PolygonDisplayElement buildPolygonDisplayElement( Feature feature,
      GM_Object gmObject, PolygonSymbolizer sym ) throws IncompatibleGeometryTypeException
  {
    PolygonDisplayElement displayElement = null;

    if( gmObject instanceof GM_Surface )
    {
      displayElement = new PolygonDisplayElement_Impl( feature, (GM_Surface)gmObject, sym );
    }
    else if( gmObject instanceof GM_MultiSurface )
    {
      displayElement = new PolygonDisplayElement_Impl( feature, (GM_MultiSurface)gmObject, sym );
    }
    else
    {
      throw new IncompatibleGeometryTypeException(
          "Tried to create a LineStringDisplayElement from a geometry with "
              + "an incompatible / unsupported type: '" + gmObject.getClass().getName() + "'!" );
    }

    return displayElement;
  }

  /**
   * Creates a <tt>LabelDisplayElement</tt> using the given geometry and style
   * information.
   * <p>
   * 
   * @param feature
   *          <tt>Feature</tt> to be used (necessary for evaluation of the
   *          label expression)
   * @param gmObject
   *          geometry information
   * @param sym
   *          style information
   * @throws IncompatibleGeometryTypeException
   *           if the geometry property is not a <tt>GM_Point</tt>, a
   *           <tt>GM_Surface</tt> or <tt>GM_MultiSurface</tt>
   * @return constructed <tt>PolygonDisplayElement</tt>
   */
  public static LabelDisplayElement buildLabelDisplayElement( Feature feature, GM_Object gmObject,
      TextSymbolizer sym ) throws IncompatibleGeometryTypeException
  {

    LabelDisplayElement displayElement = null;

    if( gmObject instanceof GM_Point || gmObject instanceof GM_Surface
        || gmObject instanceof GM_MultiSurface || gmObject instanceof GM_Curve
        || gmObject instanceof GM_MultiCurve )
    {
      displayElement = new LabelDisplayElement_Impl( feature, gmObject, sym );
    }
    else
    {
      throw new IncompatibleGeometryTypeException(
          "Tried to create a LabelDisplayElement from a geometry with "
              + "an incompatible / unsupported type: '" + gmObject.getClass().getName() + "'!" );
    }

    return displayElement;
  }

  /**
   * Creates a <tt>RasterDisplayElement</tt> from the submitted image. The
   * submitted <tt>GM_Envelope</tt> holds the bounding box of the imgae/raster
   * data.
   * 
   * @param gc
   *          grid coverage
   * @param sym
   *          raster symbolizer
   * 
   * @return RasterDisplayElement
   */
  public static RasterDisplayElement buildRasterDisplayElement( GC_GridCoverage gc,
      RasterSymbolizer sym )
  {
    return new RasterDisplayElement_Impl( gc, sym );
  }
}