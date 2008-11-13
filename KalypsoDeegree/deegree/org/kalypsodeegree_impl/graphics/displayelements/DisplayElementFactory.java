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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.kalypsodeegree.graphics.displayelements.LabelDisplayElement;
import org.kalypsodeegree.graphics.displayelements.LineStringDisplayElement;
import org.kalypsodeegree.graphics.displayelements.PointDisplayElement;
import org.kalypsodeegree.graphics.displayelements.PolygonDisplayElement;
import org.kalypsodeegree.graphics.displayelements.RasterDisplayElement;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.SurfaceLineSymbolizer;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.displayelements.SurfacePatchVisitableDisplayElement.IVisitorFactory;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PointSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.kalypsodeegree_impl.tools.Debug;

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
  private static final GM_Object[] EMPTY_GEOMS = new GM_Object[0];

  private static final GM_Point[] EMPTY_POINTS = new GM_Point[0];

  private static final GM_Curve[] EMPTY_CURVES = new GM_Curve[0];

  private static final GM_Surface< ? >[] EMPTY_SURFACES = new GM_Surface[0];

  /**
   * returns the display elements associated to a feature
   */
  public static DisplayElement[] createDisplayElement( final Feature feature, final UserStyle style )
  {
    final ArrayList<DisplayElement> list = new ArrayList<DisplayElement>();

    try
    {
      final IFeatureType featureType = feature.getFeatureType();
      final QName featureTypeQName = featureType.getQName();

      if( style == null )
      {
        // create display element from default style
        final DisplayElement de = buildDisplayElement( feature );
        if( de != null )
          list.add( de );
      }
      else
      {
        final FeatureTypeStyle[] fts = style.getFeatureTypeStyles();

        for( final FeatureTypeStyle element : fts )
        {
          final QName styleFTQName = element.getFeatureTypeName();
          if( styleFTQName == null //
              // || featureTypeQName.equals( styleFTQName ) //
              || GMLSchemaUtilities.substitutes( featureType, styleFTQName ) //
              || featureTypeQName.getLocalPart().equals( styleFTQName.getLocalPart() ) )
          {
            final Rule[] rules = element.getRules();

            for( final Rule element2 : rules )
            {
              // does the filter rule apply?
              final Filter filter = element2.getFilter();

              if( filter != null )
              {
                try
                {
                  if( !filter.evaluate( feature ) )
                    continue;
                }
                catch( final FilterEvaluationException e )
                {
                  System.out.println( "Error evaluating filter: " + e );

                  continue;
                }
              }

              // Filter expression is true for this
              // feature, so a
              // corresponding DisplayElement has to be
              // added to the
              // list
              final Symbolizer[] symbolizers = element2.getSymbolizers();

              for( final Symbolizer symbolizer : symbolizers )
              {
                final DisplayElement displayElement = DisplayElementFactory.buildDisplayElement( feature, symbolizer );
                if( displayElement != null )
                  list.add( displayElement );
              }
            }
          }
        }
      }
    }
    catch( final IncompatibleGeometryTypeException e )
    {
      System.out.println( "wrong style ?:" + e.getLocalizedMessage() );
      e.printStackTrace();
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }

    return list.toArray( new DisplayElement[list.size()] );
  }

  /**
   * Builds a <tt>DisplayElement</tt> using the given <tt>Feature</tt> or raster and <tt>Symbolizer</tt>.
   * <p>
   *
   * @param o
   *          contains the geometry or raster information (Feature or raster)
   * @param symbolizer
   *          contains the drawing (style) information and selects the geometry property of the <tt>Feature</tt> to be
   *          drawn
   * @throws IncompatibleGeometryTypeException
   *           if the selected geometry of the <tt>Feature</tt> is not compatible with the <tt>Symbolizer</tt>
   * @return constructed <tt>DisplayElement</tt>
   */
  public static DisplayElement buildDisplayElement( final Feature feature, final Symbolizer symbolizer ) throws IncompatibleGeometryTypeException, FilterEvaluationException
  {
    // determine the geometry property to be used
    final Object geoObject = findGeometryObject( feature, symbolizer );

    // if the geometry property is null, do not build a DisplayElement
    // Only for RasterSymbolizer a null geoObject is allowed, as it only depends on the feature
    if( geoObject == null && !(symbolizer instanceof RasterSymbolizer) )
        return null;

    final DisplayElement displayElement = buildDisplayElement( feature, symbolizer, geoObject );
    if( displayElement == null )
      return null;

    // TODO Patrice Check changes
    // decorate the display with another get through adapation
    // TODO: next line is strange: if we adapt to DisplayElementDecorator, of course
    // DisplayElementDecorator shall be returned, not only a DisplayElement....
    final DisplayElement displayElementDecorator = (DisplayElement) feature.getAdapter( DisplayElementDecorator.class );
    if( displayElementDecorator instanceof DisplayElementDecorator )
    {
      ((DisplayElementDecorator) displayElementDecorator).setDecorated( displayElement );
      return displayElementDecorator;
    }

    return displayElement;
  }

  /**
   * Internally build the display element, without decoration and other stuff.
   */
  private static DisplayElement buildDisplayElement( final Feature feature, final Symbolizer symbolizer, final Object geoObject ) throws IncompatibleGeometryTypeException
  {
    if( symbolizer instanceof PointSymbolizer )
      return buildPointDisplayElement( feature, geoObject, (PointSymbolizer) symbolizer );

    if( symbolizer instanceof LineSymbolizer )
      return buildLineStringDisplayElement( feature, geoObject, (LineSymbolizer) symbolizer );

    if( symbolizer instanceof PolygonSymbolizer )
      return buildPolygonDisplayElement( feature, geoObject, (PolygonSymbolizer) symbolizer );

    if( symbolizer instanceof TextSymbolizer )
      return buildLabelDisplayElement( feature, geoObject, (TextSymbolizer) symbolizer );

    if( symbolizer instanceof RasterSymbolizer )
      return buildRasterDisplayElement( feature, geoObject, (RasterSymbolizer) symbolizer );

    if( symbolizer instanceof SurfacePolygonSymbolizer )
      return buildSurfacePolygonDisplayElement( feature, geoObject, (SurfacePolygonSymbolizer) symbolizer );

    if( symbolizer instanceof SurfaceLineSymbolizer )
      return buildSurfaceLineDisplayElement( feature, geoObject, (SurfaceLineSymbolizer) symbolizer );

    System.out.println( "symbolizer...?: " + symbolizer );
    return null;
  }

  /**
   * Finds the geometry object for the given symbolizer and feature.
   *
   * @return Either a {@link GM_Object} or a {@link List} of {@link GM_Object}'s.
   */
  private static Object findGeometryObject( final Feature feature, final Symbolizer symbolizer ) throws FilterEvaluationException, IncompatibleGeometryTypeException
  {
    final Geometry geometry = symbolizer == null ? null : symbolizer.getGeometry();
    if( geometry == null )
      return feature.getDefaultGeometryProperty();

    final PropertyName propertyName = geometry.getPropertyName();
    final Object value = propertyName.evaluate( feature );
    if( value == null || value instanceof GM_Object || value instanceof List )
      return value;

    final String msg = String.format( "PropertyName '%s' does not evaluate to a geometry: %s", propertyName, value );
    throw new IncompatibleGeometryTypeException( msg );
  }

  public static DisplayElement buildSurfaceLineDisplayElement( final Feature feature, final Object geoProperty, final SurfaceLineSymbolizer symbolizer ) throws IncompatibleGeometryTypeException
  {
    if( !(geoProperty instanceof GM_TriangulatedSurface) )
      throw new IncompatibleGeometryTypeException( "Tried to create a SurfaceDisplayElement from a geometry with an incompatible / unsupported type: '" + geoProperty.getClass().getName() + "'!" );

    final LineColorMap colorMap = symbolizer.getColorMap();
    final GM_TriangulatedSurface tin = (GM_TriangulatedSurface) geoProperty;

    final IVisitorFactory<GM_Triangle> visitorFactory = new SurfacePatchVisitableDisplayElement.IVisitorFactory<GM_Triangle>()
    {
      public ISurfacePatchVisitor<GM_Triangle> createVisitor( final Graphics g, final GeoTransform projection, final IElevationColorModel model )
      {
        final UOM uom = symbolizer.getUom();
        return new SurfacePaintIsolinesVisitor( g, projection, new ColorMapConverter( colorMap, feature, uom, projection ) );
      }
    };

    return new SurfacePatchVisitableDisplayElement<GM_Triangle>( feature, tin, null, visitorFactory );
  }

  public static DisplayElement buildSurfacePolygonDisplayElement( final Feature feature, final Object geoProperty, final SurfacePolygonSymbolizer symbolizer ) throws IncompatibleGeometryTypeException
  {
    if( !(geoProperty instanceof GM_TriangulatedSurface) )
      throw new IncompatibleGeometryTypeException( "Tried to create a SurfaceDisplayElement from a geometry with an incompatible / unsupported type: '" + geoProperty.getClass().getName() + "'!" );

    final PolygonColorMap colorMap = symbolizer.getColorMap();
    final GM_TriangulatedSurface tin = (GM_TriangulatedSurface) geoProperty;
    final IVisitorFactory<GM_Triangle> visitorFactory = new SurfacePatchVisitableDisplayElement.IVisitorFactory<GM_Triangle>()
    {
      public ISurfacePatchVisitor<GM_Triangle> createVisitor( final Graphics g, final GeoTransform projection, final IElevationColorModel model )
      {
        final UOM uom = symbolizer.getUom();
        return new SurfacePaintPolygonVisitor( g, new ColorMapConverter( colorMap, feature, uom, projection ) );
      }
    };

    return new SurfacePatchVisitableDisplayElement<GM_Triangle>( feature, tin, null, visitorFactory );
  }

  /**
   * Builds a <tt>DisplayElement</tt> using the given <tt>Feature</tt> or Raster and a default <tt>Symbolizer</tt>.
   * <p>
   *
   * @param o
   *          contains the geometry or raster information (Feature or raster)
   * @throws IncompatibleGeometryTypeException
   *           if the selected geometry of the <tt>Feature</tt> is not compatible with the <tt>Symbolizer</tt>
   * @return constructed <tt>DisplayElement</tt>
   */
  public static DisplayElement buildDisplayElement( final Object o ) throws IncompatibleGeometryTypeException
  {
    Debug.debugMethodBegin( "DisplayElementFactory", "buildDisplayElement(Object)" );

    DisplayElement displayElement = null;

    final Feature feature = (Feature) o;
    // determine the geometry property to be used
    final GM_Object geoProperty = feature.getDefaultGeometryProperty();

    // if the geometry property is null, do not build a DisplayElement
    if( geoProperty == null )
    {
      return null;
    }

    // PointSymbolizer
    if( geoProperty instanceof GM_Point || geoProperty instanceof GM_MultiPoint )
    {
      final PointSymbolizer symbolizer = new PointSymbolizer_Impl();
      displayElement = buildPointDisplayElement( feature, geoProperty, symbolizer );
    } // LineSymbolizer
    else if( geoProperty instanceof GM_Curve || geoProperty instanceof GM_MultiCurve )
    {
      final LineSymbolizer symbolizer = new LineSymbolizer_Impl();
      displayElement = buildLineStringDisplayElement( feature, geoProperty, symbolizer );
    } // PolygonSymbolizer
    else if( geoProperty instanceof GM_Surface || geoProperty instanceof GM_MultiSurface )
    {
      final PolygonSymbolizer symbolizer = new PolygonSymbolizer_Impl();
      displayElement = buildPolygonDisplayElement( feature, geoProperty, symbolizer );
    }
    else
    {
      throw new IncompatibleGeometryTypeException( "not a valid geometry type" );
    }

    Debug.debugMethodEnd();
    return displayElement;
  }

  /**
   * Creates a <tt>PointDisplayElement</tt> using the given geometry and style information.
   * <p>
   *
   * @param feature
   *          associated <tt>Feature<tt>
   * @param geom
   *          geometry object or list of geometries
   * @param sym
   *          style information
   * @return constructed <tt>PointDisplayElement</tt>
   */
  public static PointDisplayElement buildPointDisplayElement( final Feature feature, final Object geomOrList, final PointSymbolizer sym )
  {
    final GM_Point[] points = findPoints( geomOrList, EMPTY_POINTS );
    if( points == null )
      return null;
    return new PointDisplayElement_Impl( feature, points, sym );
  }

  @SuppressWarnings("unchecked")
  private static <T> T[] findPoints( final Object geomOrList, final T[] typedList )
  {
    if( geomOrList == null )
      return null;

    if( geomOrList instanceof GM_Object )
      return (T[]) ((GM_Object) geomOrList).getAdapter( typedList.getClass() );

    if( geomOrList instanceof List )
      return findGeometries( (List< ? >) geomOrList, typedList );

    return null;
  }

  @SuppressWarnings("unchecked")
  private static <T> T[] findGeometries( final List< ? > geomList, final T[] typedList )
  {
    final List<T> result = new ArrayList<T>();
    for( final Object geom : geomList )
    {
      if( geom instanceof GM_Object )
      {
        final T[] geometries = (T[]) ((GM_Object) geom).getAdapter( typedList.getClass() );
        result.addAll( Arrays.asList( geometries ) );
      }
    }

    return result.toArray( typedList );
  }

  /**
   * Creates a <tt>LineStringDisplayElement</tt> using the given geometry and style information.
   * <p>
   *
   * @param feature
   *          associated <tt>Feature<tt>
   * @param geom
   *          geometry information
   * @param sym
   *          style information
   * @return constructed <tt>LineStringDisplayElement</tt>
   */
  public static LineStringDisplayElement buildLineStringDisplayElement( final Feature feature, final Object geomOrList, final LineSymbolizer sym )
  {
    final GM_Curve[] curves = findPoints( geomOrList, EMPTY_CURVES );
    if( curves == null )
      return null;
    return new LineStringDisplayElement_Impl( feature, curves, sym );
  }

  /**
   * Creates a <tt>PolygonDisplayElement</tt> using the given geometry and style information.
   * <p>
   *
   * @param feature
   *          associated <tt>Feature<tt>
   * @param gmObject
   *          geometry information
   * @param sym
   *          style information
   * @return constructed <tt>PolygonDisplayElement</tt>
   */
  public static PolygonDisplayElement buildPolygonDisplayElement( final Feature feature, final Object geomOrList, final PolygonSymbolizer sym )
  {
    final GM_Surface< ? >[] surfaces = findPoints( geomOrList, EMPTY_SURFACES );
    if( surfaces == null )
      return null;
    return new PolygonDisplayElement_Impl( feature, surfaces, sym );
  }

  /**
   * Creates a <tt>LabelDisplayElement</tt> using the given geometry and style information.
   * <p>
   *
   * @param feature
   *          <tt>Feature</tt> to be used (necessary for evaluation of the label expression)
   * @param gmObject
   *          geometry information
   * @param sym
   *          style information
   * @throws IncompatibleGeometryTypeException
   *           if the geometry property is not a <tt>GM_Point</tt>, a <tt>GM_Surface</tt> or <tt>GM_MultiSurface</tt>
   * @return constructed <tt>PolygonDisplayElement</tt>
   */
  public static LabelDisplayElement buildLabelDisplayElement( final Feature feature, final Object geomOrList, final TextSymbolizer sym )
  {
    final GM_Object[] objects = findPoints( geomOrList, EMPTY_SURFACES );
    if( objects == null )
      return null;

    return new LabelDisplayElement_Impl( feature, objects, sym );
  }

  /**
   * Creates a <tt>RasterDisplayElement</tt> from the submitted image. The submitted <tt>GM_Envelope</tt> holds the
   * bounding box of the imgae/raster data.
   *
   * @param feature
   *          grid coverage as feature
   * @param sym
   *          raster symbolizer
   * @return RasterDisplayElement
   */
  public static RasterDisplayElement buildRasterDisplayElement( final Feature feature, final Object geomOrList, final RasterSymbolizer sym )
  {
    // REMARK: not really necessary at the moment, as the raster symbolizer does nothing with its geometries
    // maybe it would be better to always reference the gridDomain property and give it to the symbolizer?
    final GM_Object[] objects = findPoints( geomOrList, EMPTY_GEOMS );
    if( objects == null )
      return null;

    return new RasterDisplayElement_Impl( feature, objects, sym );
  }
}