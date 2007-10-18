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

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.renderable.ParameterBlock;
import java.io.Serializable;
import java.util.TreeMap;

import javax.media.jai.JAI;
import javax.media.jai.PlanarImage;
import javax.media.jai.RasterFactory;
import javax.media.jai.TiledImage;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.xml.NS;
import org.kalypso.gis.doubleraster.RectifiedGridCoverageDoubleRaster;
import org.kalypsodeegree.graphics.displayelements.LineStringDisplayElement;
import org.kalypsodeegree.graphics.displayelements.RasterDisplayElement;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceBoundary;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualPropertyUtilities;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage2;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.WMSHelper;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author N. Peiler
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class RasterDisplayElement_Impl extends GeometryDisplayElement_Impl implements RasterDisplayElement, Serializable
{
  private static final long serialVersionUID = 1212494081075757709L;

  public static final int mode_intervalColorMapping = 0;

  public static final int mode_valueColorMapping = 1;

  private TiledImage m_surrogateTiledImage;

  private boolean m_valid = false;

  /**
   * Creates a new RasterDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   * @param symbolizer
   */
  protected RasterDisplayElement_Impl( final Feature feature, final GM_Object[] geometry, final RasterSymbolizer symbolizer )
  {
    super( feature, geometry, symbolizer );
  }

  private TiledImage getImage( )
  {
    if( (m_surrogateTiledImage == null) || !m_valid )
    {
      final RasterSymbolizer rasterSym = (RasterSymbolizer) getSymbolizer();

      final Feature feature = getFeature();
      final RectifiedGridCoverage2 coverage = new RectifiedGridCoverage2( feature );

      final Raster surrogateRaster = getSurrogateRaster( coverage, rasterSym );
      m_surrogateTiledImage = new TiledImage( getSurrogateImage( surrogateRaster ), true );
      m_valid = true;
    }
    return m_surrogateTiledImage;
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  @Override
  public void paint( final Graphics g, final GeoTransform projection )
  {
    // cast Graphics to Graphics2D
    final Graphics2D g2 = (Graphics2D) g;
    final Feature feature = getFeature();
    // get the geometry informations of the RectifiedGridCoverage
    final RectifiedGridDomain rgDomain = (RectifiedGridDomain) feature.getProperty( new QName( NS.GML3, "rectifiedGridDomain" ) );
    // create the target Coordinate system

    try
    {
      // TODO: ugly! Instead, adwe apt something to GM_Curve[] and let the normal LineStirngSymbolizer mechanisms aply
      // TODO: probably we can just remove the line stuff and just add lineSymbolizer to our maps.

      GM_Surface surface = null;
      final VirtualFeatureTypeProperty[] virtualProperties = VirtualPropertyUtilities.getVirtualProperties( feature.getFeatureType() );
      if( virtualProperties.length > 0 )
      {
        final VirtualFeatureTypeProperty vpt = virtualProperties[0];
        final GM_Object geom = (GM_Object) vpt.getVirtuelValue( feature );

        surface = (GM_Surface) geom;
      }
      else
      {
        final GM_Object defaultGeometryProperty = feature.getDefaultGeometryProperty();
        if( defaultGeometryProperty != null )
          surface = (GM_Surface) defaultGeometryProperty;
        else
          throw new Exception("No suitable geometry.");
      }
      final CS_CoordinateSystem cs = surface.getCoordinateSystem();
      final GM_SurfaceBoundary surfaceBoundary = surface.getSurfaceBoundary();
      final GM_Ring exteriorRing = surfaceBoundary.getExteriorRing();
      final GM_Curve curve = GeometryFactory.createGM_Curve( exteriorRing.getAsCurveSegment() );
      final LineStringDisplayElement lineDE = new LineStringDisplayElement_Impl( feature, new GM_Curve[] { curve } );

      lineDE.paint( g2, projection );

      final TiledImage rasterImage = getImage();
      drawRasterImage( g2, projection, rasterImage, rgDomain, cs );
    }
    catch( final Exception e )
    {
      // TODO: allow the display element to throw exceptions
      System.out.println( e );
    }
  }

  private void drawRasterImage( final Graphics2D g2, final GeoTransform projection, final TiledImage rasterImage, final RectifiedGridDomain gridDomain, final CS_CoordinateSystem targetCS ) throws Exception
  {
    final GM_Envelope envelope = gridDomain.getGM_Envelope( targetCS );
    final CS_CoordinateSystem crs = gridDomain.getCoordinateSystem();
    WMSHelper.transformImage( rasterImage, envelope, targetCS, crs, projection, g2 );
  }

  /**
   * get a surrogate image for displaying with byte values of the given raster with int values
   * 
   * @param surrogateRaster
   * @return surrogate image
   */
  private PlanarImage getSurrogateImage( final Raster surrogateRaster )
  {
    PlanarImage surrogateImage = getPlanarImage( surrogateRaster );
    // convert the data type for displaying.
    final ParameterBlock pbConvert = new ParameterBlock();
    pbConvert.addSource( surrogateImage );
    pbConvert.add( DataBuffer.TYPE_BYTE );
    surrogateImage = JAI.create( "format", pbConvert );
    return surrogateImage;
  }

  /**
   * get an image with the given Raster-Object
   * 
   * @param raster
   * @return Image
   */
  private PlanarImage getPlanarImage( final Raster raster )
  {
    final ColorModel colorModel = PlanarImage.createColorModel( raster.getSampleModel() );
    final TiledImage tiledImage = new TiledImage( 0, 0, raster.getWidth(), raster.getHeight(), 0, 0, raster.getSampleModel(), colorModel );
    tiledImage.setData( raster );
    return tiledImage;
  }

  /**
   * creates a surrogate raster of the given rectifiedGridDomain and rangeSet with the given colorTable
   * 
   * @param gridDomain
   * @return surrogate raster
   */
  private Raster getSurrogateRaster( final RectifiedGridCoverage2 coverage, final RasterSymbolizer rasterSym )
  {
    final int mode = rasterSym.getMode();

    TreeMap treeColorMap = null;
    if( mode == RasterDisplayElement_Impl.mode_intervalColorMapping )
      treeColorMap = rasterSym.getIntervalMap();
    else if( mode == RasterDisplayElement_Impl.mode_valueColorMapping )
      treeColorMap = rasterSym.getColorMap();

    try
    {
      final RectifiedGridCoverageDoubleRaster raster = new RectifiedGridCoverageDoubleRaster( coverage.getFeature() );

      final DataBufferRasterWalker pwo = new DataBufferRasterWalker( treeColorMap, mode );
      raster.walk( pwo, new NullProgressMonitor() );

      final Point origin = new Point( 0, 0 );
      final Raster surrogateRaster = RasterFactory.createWritableRaster( pwo.getSampleModel(), pwo.getBuffer(), origin );
      return surrogateRaster;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      // TODO: return default raster ??
      return null;
    }
  }
}