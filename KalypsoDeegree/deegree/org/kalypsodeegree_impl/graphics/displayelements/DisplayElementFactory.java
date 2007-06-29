/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;

import javax.xml.namespace.QName;

import org.eclipse.swt.graphics.RGB;
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
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PointSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
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
  /**
   * returns the display elements associated to a feature
   */
  public static DisplayElement[] createDisplayElement( final Feature feature, final UserStyle[] styles )
  {
    final ArrayList<DisplayElement> list = new ArrayList<DisplayElement>( styles.length );

    try
    {
      final IFeatureType featureType = feature.getFeatureType();
      final QName featureTypeQName = featureType.getQName();

      for( final UserStyle userStyle : styles )
      {
        if( userStyle == null )
        {
          // create display element from default style
          final DisplayElement de = buildDisplayElement( feature );
          if( de != null )
            list.add( de );
        }
        else
        {
          final FeatureTypeStyle[] fts = userStyle.getFeatureTypeStyles();

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

                // TODO: test, remove
                if( symbolizers.length == 0 )
                  list.add( DisplayElementFactory.buildDisplayElement( feature, null ) );
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
   *            contains the geometry or raster information (Feature or raster)
   * @param symbolizer
   *            contains the drawing (style) information and selects the geometry property of the <tt>Feature</tt> to
   *            be drawn
   * @throws IncompatibleGeometryTypeException
   *             if the selected geometry of the <tt>Feature</tt> is not compatible with the <tt>Symbolizer</tt>
   * @return constructed <tt>DisplayElement</tt>
   */
  public static DisplayElement buildDisplayElement( final Feature feature, final Symbolizer symbolizer ) throws IncompatibleGeometryTypeException, FilterEvaluationException
  {
    // determine the geometry property to be used
    GM_Object geoProperty = null;
    final Geometry geometry = symbolizer == null ? null : symbolizer.getGeometry();

    if( geometry != null )
    {
      final PropertyName propertyName = geometry.getPropertyName();
      final Object value = propertyName.evaluate( feature );
      if( value == null )
        return null;
      else if( value instanceof GM_Object )
        geoProperty = (GM_Object) value;
      else
      {
        final String msg = String.format( "PropertyName '%s' does not evaluate to a geometry: %s", propertyName, value );
        throw new IncompatibleGeometryTypeException( msg );
      }
    }
    else
    {
      geoProperty = feature.getDefaultGeometryProperty();
    }

    // if the geometry property is null, do not build a DisplayElement
    if( geoProperty == null && !(symbolizer instanceof RasterSymbolizer) )
    {
      return null;
    }

    DisplayElement displayElement = null;
    // PointSymbolizer
    if( symbolizer instanceof PointSymbolizer )
    {
      displayElement = buildPointDisplayElement( feature, geoProperty, (PointSymbolizer) symbolizer );
    } // LineSymbolizer
    else if( symbolizer instanceof LineSymbolizer )
    {
      displayElement = buildLineStringDisplayElement( feature, geoProperty, (LineSymbolizer) symbolizer );
    } // PolygonSymbolizer
    else if( symbolizer instanceof PolygonSymbolizer )
    {
      displayElement = buildPolygonDisplayElement( feature, geoProperty, (PolygonSymbolizer) symbolizer );
    }
    else if( symbolizer instanceof TextSymbolizer )
    {
      displayElement = buildLabelDisplayElement( feature, geoProperty, (TextSymbolizer) symbolizer );
    } // RasterSymbolizer
    else if( symbolizer instanceof RasterSymbolizer )
    {
      displayElement = buildRasterDisplayElement( feature, (RasterSymbolizer) symbolizer );
    }
    // TODO: replace with symbolizer read from sld
    
    else if( symbolizer == null && feature.getDefaultGeometryProperty() instanceof GM_TriangulatedSurface )
    {
      final IElevationColorModel colorModel = new IElevationColorModel()
      {
        public Color getColor( double elevation )
        {
          final int hue = (int)( Math.random() * 255 );
//          System.out.println( hue );
          final RGB rgb = new RGB( hue, 128, 128 );
          
          return new Color( rgb.red, rgb.green, rgb.blue );
        }

        public double getDiscretisationInterval( )
        {
          return 0;
        }

        public double[] getElevationMinMax( )
        {
          return null;
        }

        public void setElevationMinMax( double min, double max )
        {
        }
      };

      final GM_TriangulatedSurface tin = (GM_TriangulatedSurface) feature.getDefaultGeometryProperty();
      final IVisitorFactory<GM_Triangle> visitorFactory = new SurfacePatchVisitableDisplayElement.IVisitorFactory<GM_Triangle>()
      {
        public ISurfacePatchVisitor<GM_Triangle> createVisitor( final Graphics g, final GeoTransform projection, final IElevationColorModel model )
        {
//          return new SurfacePaintIsolinesVisitor<GM_Triangle>( g, projection, colorModel );
          return new SurfacePaintPlainTriangleVisitor<GM_Triangle>( g, projection, colorModel );
        }
      };

      return new SurfacePatchVisitableDisplayElement<GM_Triangle>( feature, tin, colorModel, visitorFactory );
    }
    else
    {
      System.out.println( "symbolizer...?" );
    }

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
   * Builds a <tt>DisplayElement</tt> using the given <tt>Feature</tt> or Raster and a default <tt>Symbolizer</tt>.
   * <p>
   * 
   * @param o
   *            contains the geometry or raster information (Feature or raster)
   * @throws IncompatibleGeometryTypeException
   *             if the selected geometry of the <tt>Feature</tt> is not compatible with the <tt>Symbolizer</tt>
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
   *            associated <tt>Feature<tt>
   * @param geom geometry information
   * @param sym style information
   * @return constructed <tt>PointDisplayElement</tt>
   */
  public static PointDisplayElement buildPointDisplayElement( final Feature feature, final GM_Object geom, final PointSymbolizer sym ) throws IncompatibleGeometryTypeException
  {
    if( geom == null )
      return null;

    final GM_Point[] points = (GM_Point[]) geom.getAdapter( GM_Point[].class );

    if( points == null )
      throw new IncompatibleGeometryTypeException( "Could not create PointDisplayElement from geometry: " + geom.getClass().getName() );

    return new PointDisplayElement_Impl( feature, points, sym );
  }

  /**
   * Creates a <tt>LineStringDisplayElement</tt> using the given geometry and style information.
   * <p>
   * 
   * @param feature
   *            associated <tt>Feature<tt>
   * @param geom geometry information
   * @param sym style information
   * @return constructed <tt>LineStringDisplayElement</tt>
   */
  public static LineStringDisplayElement buildLineStringDisplayElement( final Feature feature, final GM_Object geom, final LineSymbolizer sym ) throws IncompatibleGeometryTypeException
  {
    if( geom == null )
      return null;

    final GM_Curve[] curves = (GM_Curve[]) geom.getAdapter( GM_Curve[].class );
    if( curves == null )
      throw new IncompatibleGeometryTypeException( "Could not create LineStringDisplayElement from geometry: " + geom.getClass().getName() );
    return new LineStringDisplayElement_Impl( feature, curves, sym );
  }

  /**
   * Creates a <tt>PolygonDisplayElement</tt> using the given geometry and style information.
   * <p>
   * 
   * @param feature
   *            associated <tt>Feature<tt>
   * @param gmObject geometry information
   * @param sym style information
   * @throws IncompatibleGeometryTypeException if the geometry property is not
   *         a <tt>GM_Surface</tt> or a <tt>GM_MultiSurface</tt>
   * @return constructed <tt>PolygonDisplayElement</tt>
   */
  public static PolygonDisplayElement buildPolygonDisplayElement( final Feature feature, final GM_Object geom, final PolygonSymbolizer sym ) throws IncompatibleGeometryTypeException
  {
    if( geom == null )
      return null;

    final GM_Surface< ? >[] surfaces = (GM_Surface[]) geom.getAdapter( GM_Surface[].class );
    if( surfaces == null )
      throw new IncompatibleGeometryTypeException( "Could not create PolygonDisplayElement from geometry: " + geom.getClass().getName() );

    return new PolygonDisplayElement_Impl( feature, surfaces, sym );
  }

  /**
   * Creates a <tt>LabelDisplayElement</tt> using the given geometry and style information.
   * <p>
   * 
   * @param feature
   *            <tt>Feature</tt> to be used (necessary for evaluation of the label expression)
   * @param gmObject
   *            geometry information
   * @param sym
   *            style information
   * @throws IncompatibleGeometryTypeException
   *             if the geometry property is not a <tt>GM_Point</tt>, a <tt>GM_Surface</tt> or
   *             <tt>GM_MultiSurface</tt>
   * @return constructed <tt>PolygonDisplayElement</tt>
   */
  public static LabelDisplayElement buildLabelDisplayElement( final Feature feature, final GM_Object gmObject, final TextSymbolizer sym ) throws IncompatibleGeometryTypeException
  {
    if( gmObject instanceof GM_Point || gmObject instanceof GM_Surface || gmObject instanceof GM_MultiSurface || gmObject instanceof GM_Curve || gmObject instanceof GM_MultiCurve )
      return new LabelDisplayElement_Impl( feature, new GM_Object[] { gmObject }, sym );

    throw new IncompatibleGeometryTypeException( "Tried to create a LabelDisplayElement from a geometry with " + "an incompatible / unsupported type: '" + gmObject.getClass().getName() + "'!" );
  }

  /**
   * Creates a <tt>RasterDisplayElement</tt> from the submitted image. The submitted <tt>GM_Envelope</tt> holds the
   * bounding box of the imgae/raster data.
   * 
   * @param feature
   *            grid coverage as feature
   * @param sym
   *            raster symbolizer
   * @return RasterDisplayElement
   */
  public static RasterDisplayElement buildRasterDisplayElement( final Feature feature, final RasterSymbolizer sym )
  {
    // TODO: instead of giving a null geometry, retrieve a raster class from the feature and give it to the raste
    // display element.
    return new RasterDisplayElement_Impl( feature, null, sym );
  }
}