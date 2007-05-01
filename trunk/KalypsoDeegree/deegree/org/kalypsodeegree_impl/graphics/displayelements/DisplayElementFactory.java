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

import java.util.ArrayList;

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
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiPrimitive;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Primitive;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualPropertyUtilities;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PointSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
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
  public static DisplayElement[] createDisplayElement( final Feature feature, final UserStyle[] styles, final GMLWorkspace workspace )
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

          for( int k = 0; k < fts.length; k++ )
          {
            final QName styleFTQName = fts[k].getFeatureTypeName();
            if( styleFTQName == null //
                // || featureTypeQName.equals( styleFTQName ) //
                || GMLSchemaUtilities.substitutes( featureType, styleFTQName ) //
                || featureTypeQName.getLocalPart().equals( styleFTQName.getLocalPart() ) )
            {
              final Rule[] rules = fts[k].getRules();

              for( int n = 0; n < rules.length; n++ )
              {

                // does the filter rule apply?
                Filter filter = rules[n].getFilter();

                if( filter != null )
                {
                  try
                  {
                    if( !filter.evaluate( feature ) )
                      continue;
                  }
                  catch( FilterEvaluationException e )
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
                Symbolizer[] symbolizers = rules[n].getSymbolizers();

                for( int u = 0; u < symbolizers.length; u++ )
                {
                  final DisplayElement displayElement = DisplayElementFactory.buildDisplayElement( feature, symbolizers[u], workspace );
                  if( displayElement != null )
                    list.add( displayElement );
                }
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
  public static DisplayElement buildDisplayElement( Feature feature, Symbolizer symbolizer, GMLWorkspace workspace ) throws IncompatibleGeometryTypeException
  {
    // determine the geometry property to be used
    GM_Object geoProperty = null;
    final Geometry geometry = symbolizer.getGeometry();

    if( geometry != null )
    {
      final IFeatureType featureType = feature.getFeatureType();
      final String propertyName = geometry.getPropertyName();
      final VirtualFeatureTypeProperty vpt = VirtualPropertyUtilities.getPropertyType( featureType, propertyName );
      if( vpt != null )
        geoProperty = (GM_Object) vpt.getVirtuelValue( feature, workspace );
      else if( featureType.getProperty( propertyName ) != null )
        geoProperty = (GM_Object) feature.getProperty( propertyName );
      else
        return null;
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
    else
    {
      System.out.println( "symbolizer...?" );
    }

    // //TODO Patrice Check changes
    // //decorate the display with another get through adapation
    DisplayElement displayElementDecorator = (DisplayElement) feature.getAdapter( DisplayElementDecorator.class );
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
   *          contains the geometry or raster information (Feature or raster)
   * @throws IncompatibleGeometryTypeException
   *           if the selected geometry of the <tt>Feature</tt> is not compatible with the <tt>Symbolizer</tt>
   * @return constructed <tt>DisplayElement</tt>
   */
  public static DisplayElement buildDisplayElement( Object o ) throws IncompatibleGeometryTypeException
  {
    Debug.debugMethodBegin( "DisplayElementFactory", "buildDisplayElement(Object)" );

    DisplayElement displayElement = null;

    Feature feature = (Feature) o;
    // determine the geometry property to be used
    GM_Object geoProperty = feature.getDefaultGeometryProperty();

    // if the geometry property is null, do not build a DisplayElement
    if( geoProperty == null )
    {
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

    Debug.debugMethodEnd();
    return displayElement;
  }

  /**
   * Creates a <tt>PointDisplayElement</tt> using the given geometry and style information.
   * <p>
   * 
   * @param feature
   *          associated <tt>Feature<tt>
   * @param geom geometry information
   * @param sym style information
   * @return constructed <tt>PointDisplayElement</tt>
   */
  public static PointDisplayElement buildPointDisplayElement( Feature feature, GM_Object geom, PointSymbolizer sym )
  {

    // if the geometry is not a point geometry, the centroid(s) of the
    // geometry will be used
    PointDisplayElement displayElement = null;

    if( geom instanceof GM_Point )
    {
      displayElement = new PointDisplayElement_Impl( feature, (GM_Point) geom, sym );
    }
    else if( geom instanceof GM_MultiPoint )
    {
      displayElement = new PointDisplayElement_Impl( feature, (GM_MultiPoint) geom, sym );
    }
    else if( geom instanceof GM_MultiPrimitive )
    {
      GM_Primitive[] primitives = ((GM_MultiPrimitive) geom).getAllPrimitives();
      GM_Point[] centroids = new GM_Point[primitives.length];

      for( int i = 0; i < primitives.length; i++ )
        centroids[i] = primitives[i].getCentroid();

      displayElement = new PointDisplayElement_Impl( feature, GeometryFactory.createGM_MultiPoint( centroids ), sym );
    }
    else
    {
      GM_Point centeroid = createCenteroidOnElement( geom );
      displayElement = new PointDisplayElement_Impl( feature, centeroid, sym );
    }

    return displayElement;
  }

  private static GM_Point createCenteroidOnElement( GM_Object geom )
  {
    if( geom == null )
      return null;
    if( geom instanceof GM_Point )
      return (GM_Point) geom;
    return geom.getCentroid();
  }

  /**
   * Creates a <tt>LineStringDisplayElement</tt> using the given geometry and style information.
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
  public static LineStringDisplayElement buildLineStringDisplayElement( Feature feature, GM_Object geom, LineSymbolizer sym ) throws IncompatibleGeometryTypeException
  {
    LineStringDisplayElement displayElement = null;
    if( geom == null )
      return null;
    if( geom instanceof GM_Curve )
    {
      displayElement = new LineStringDisplayElement_Impl( feature, (GM_Curve) geom, sym );
    }
    else if( geom instanceof GM_MultiCurve )
    {
      displayElement = new LineStringDisplayElement_Impl( feature, (GM_MultiCurve) geom, sym );
    }
    else
    {
      throw new IncompatibleGeometryTypeException( "Tried to create a LineStringDisplayElement from a geometry with " + "an incompatible / unsupported type: '" + geom.getClass().getName() + "'!" );
    }

    return displayElement;
  }

  /**
   * Creates a <tt>PolygonDisplayElement</tt> using the given geometry and style information.
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
  public static PolygonDisplayElement buildPolygonDisplayElement( Feature feature, GM_Object gmObject, PolygonSymbolizer sym ) throws IncompatibleGeometryTypeException
  {
    PolygonDisplayElement displayElement = null;

    if( gmObject instanceof GM_Surface )
    {
      displayElement = new PolygonDisplayElement_Impl( feature, (GM_Surface) gmObject, sym );
    }
    else if( gmObject instanceof GM_MultiSurface )
    {
      displayElement = new PolygonDisplayElement_Impl( feature, (GM_MultiSurface) gmObject, sym );
    }
    else
    {
      throw new IncompatibleGeometryTypeException( "Tried to create a LineStringDisplayElement from a geometry with " + "an incompatible / unsupported type: '" + gmObject.getClass().getName() + "'!" );
    }

    return displayElement;
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
   *           if the geometry property is not a <tt>GM_Point</tt>, a <tt>GM_Surface</tt> or
   *           <tt>GM_MultiSurface</tt>
   * @return constructed <tt>PolygonDisplayElement</tt>
   */
  public static LabelDisplayElement buildLabelDisplayElement( Feature feature, GM_Object gmObject, TextSymbolizer sym ) throws IncompatibleGeometryTypeException
  {

    LabelDisplayElement displayElement = null;

    if( gmObject instanceof GM_Point || gmObject instanceof GM_Surface || gmObject instanceof GM_MultiSurface || gmObject instanceof GM_Curve || gmObject instanceof GM_MultiCurve )
    {
      displayElement = new LabelDisplayElement_Impl( feature, gmObject, sym );
    }
    else
    {
      throw new IncompatibleGeometryTypeException( "Tried to create a LabelDisplayElement from a geometry with " + "an incompatible / unsupported type: '" + gmObject.getClass().getName() + "'!" );
    }

    return displayElement;
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
  public static RasterDisplayElement buildRasterDisplayElement( Feature feature, RasterSymbolizer sym )
  {
    return new RasterDisplayElement_Impl( feature, null, sym );
  }
}