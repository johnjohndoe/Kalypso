/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Logger;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;
import javax.media.jai.TiledImage;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.WMSHelper;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * KalypsoPictureTheme
 * <p>
 * created by
 * 
 * @author kuepfer (20.05.2005)
 */
public class KalypsoPictureTheme extends AbstractKalypsoTheme
{
  private TiledImage m_image;

  private CS_CoordinateSystem m_localCS;

  private static final Logger LOGGER = Logger.getLogger( KalypsoPictureTheme.class.getName() );

  private String m_themeName;

  private String m_linkType;

  private String m_source;

  private RectifiedGridDomain m_domain;

  public KalypsoPictureTheme( final String themeName, final String linktype, String source, CS_CoordinateSystem cs, final IMapModell mapModel ) throws Exception
  {
    super( themeName, linktype.toUpperCase(), mapModel );
    final boolean isRelativePath = source.startsWith( "project:" );

    m_themeName = themeName;
    m_linkType = linktype;
    m_source = source;
    m_localCS = cs;
    String[] result = source.split( "#" );
    String wf = null;
    String extension = "";
    final String baseName = (result[0].substring( 0, (source.lastIndexOf( "." ) + 1) ));
    if( linktype.equalsIgnoreCase( "tif" ) )
      extension = WorldFileReader.SUFFIX_TIFF;
    if( linktype.equalsIgnoreCase( "jpg" ) )
      extension = WorldFileReader.SUFFIX_JPG;
    if( linktype.equalsIgnoreCase( "png" ) )
      extension = WorldFileReader.SUFFIX_PNG;
    // if( linktype.equals( "gif" ) )
    // {
    // wf = ( result[0].substring( 0, ( source.lastIndexOf( "." ) + 1 ) ) )
    // .concat( SUFFIX_GIF );
    // }
    wf = baseName.concat( extension );

    // read worldfile
    InputStream wfStream = null;
    WorldFile worldFile = null;
    try
    {
      if( isRelativePath )
      {
        final IProject project = mapModel.getProject();
        IFile file = project.getFile( wf.split( ":" )[1] );
        if( !file.exists() )
          file = project.getFile( (baseName.concat( extension.toLowerCase() )).split( ":" )[1] );
        wfStream = file.getContents();
      }
      else
      {
        final URL worldFileURL = new URL( wf );
        wfStream = worldFileURL.openStream();
      }
      worldFile = new WorldFileReader().readWorldFile( wfStream );
    }
    catch( final Throwable e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Kein gültiges Worldfile " + wf ) );
    }
    finally
    {
      IOUtils.closeQuietly( wfStream );
    }

    String imageAbsolutePath = "";
    if( isRelativePath )
    {
      final IProject project = mapModel.getProject();
      IFile file = project.getFile( source.split( ":" )[1].split( "#" )[0] );
      imageAbsolutePath = file.getLocation().toOSString();
    }
    else
      try
      {
        URL imageFileURL = new URL( source );
        imageAbsolutePath = imageFileURL.getPath().substring( 1 );
      }
      catch( final MalformedURLException e1 )
      {
        throw new CoreException( StatusUtilities.statusFromThrowable( e1, "Ungültige URL der Datei: " + source ) );
      }
    RenderedOp image = JAI.create( "fileload", imageAbsolutePath );
    m_image = new TiledImage( image, true );
    // TODO: the image keeps does not release the stream onto the tiff
    // maybe we must call image.dispose in order to do this?
    // can we do that here??
    
    // Dejan: neider image.dispose(); nor m_image.dispose();  does not solve the problem :(  - I tried...
    

    final int height = m_image.getHeight();
    final int width = m_image.getWidth();

    final CS_CoordinateSystem imageCS = ConvenienceCSFactory.getInstance().getOGCCSByName( result[1] );

    final GM_Point origin = GeometryFactory.createGM_Point( worldFile.getUlcx(), worldFile.getUlcy(), imageCS );

    final OffsetVector offsetX = new OffsetVector( worldFile.getRasterXGeoX(), worldFile.getRasterXGeoY() );
    final OffsetVector offsetY = new OffsetVector( worldFile.getRasterYGeoX(), worldFile.getRasterYGeoY() );
    final GridRange gridRange = new GridRange_Impl( new double[] { 0, 0 }, new double[] { width, height } );

    m_domain = new RectifiedGridDomain( origin, offsetX, offsetY, gridRange );
    

    // ColorModel cm = m_image.getColorModel();
    // int[] size = cm.getComponentSize();
    // int transparency = cm.getTransparency();
    //
    // ColorSpace colorSpace = cm.getColorSpace();
    // int type = colorSpace.getType();
    //
    // Raster raster = m_image.getData();
    //
    // DataBuffer databuffer = raster.getDataBuffer();
    //
    // int bufferType = databuffer.getDataType();
    //
    // System.out.println( "bounds: " + image.getBounds() + ",\ttransparency: "
    // + transparency + ",\tcolor space type: " + type );
    //
    // ColorModel colorModel = new ComponentColorModel( colorSpace, size, false,
    // false, Transparency.TRANSLUCENT,
    // bufferType );
    //
    // BufferedImage bi = new BufferedImage(colorModel, (WritableRaster)raster,
    // false, getProperties(image));
    //    
    // m_image = bi;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_image != null )
      m_image.dispose();

    super.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean)
   */
  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final boolean selected )
  {
    if( selected )
      return;

    try
    {
      final GM_Envelope envelope = m_domain.getGM_Envelope( m_localCS );
      final CS_CoordinateSystem crs = m_domain.getCoordinateSystem();
      WMSHelper.transformImage( m_image, envelope, m_localCS, crs, p, g );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    GM_Envelope bbox = null;
    try
    {
      return m_domain.getGM_Envelope( m_localCS );
    }
    catch( Exception e2 )
    {
      e2.printStackTrace();
      LOGGER.warning( "Transformation of bbox for full extend failed" );
    }
    return bbox;
  }

  public void fillLayerType( final StyledLayerType layer, String id, boolean visible )
  {
    layer.setName( m_themeName );
    layer.setFeaturePath( "" );

    layer.setVisible( visible );
    layer.setId( id );
    layer.setHref( m_source );
    layer.setLinktype( m_linkType );
    layer.setActuate( "onRequest" );
    layer.setType( "simple" );
  }
}