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
package org.kalypso.ogc.gml;

import java.io.File;
import java.net.URL;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;
import javax.media.jai.TiledImage;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuch
 */
public class KalypsoPictureThemeWorldFile extends KalypsoPictureTheme
{

  public KalypsoPictureThemeWorldFile( final StyledLayerType layerType, final URL context, final IMapModell modell, final CS_CoordinateSystem system ) throws Exception
  {
    super( layerType, context, modell );
   
    
    final String href = layerType.getHref();
    if( (href == null) || !href.contains( "." ) )
    {
      throw (new IllegalStateException());
    }

    final String[] arrFileName = href.split( "\\." );
    if( arrFileName.length <= 1 )
    {
      throw (new IllegalStateException());
    }

    String extension = null;
    if( "tif".equals( getExtension( arrFileName[arrFileName.length - 1].toLowerCase() ) ) )
    {
      extension = WorldFileReader.SUFFIX_TIFF;
    }
    else if( "jpg".equals( getExtension( arrFileName[arrFileName.length - 1].toLowerCase() ) ) )
    {
      extension = WorldFileReader.SUFFIX_JPG;
    }
    else if( "png".equals( getExtension( arrFileName[arrFileName.length - 1].toLowerCase() ) ) )
    {
      extension = WorldFileReader.SUFFIX_PNG;
    }
    else
    {
      throw (new NotImplementedException());
    }

    if( extension == null )
    {
      throw (new IllegalStateException());
    }

    // TODO: foreach length-2
    /* read world file */
    final String wf = arrFileName[0] + "." + extension;

    final URL worldfileUrl = UrlResolverSingleton.resolveUrl( context, wf );
    final IProject project = ResourceUtilities.findProjectFromURL( worldfileUrl );
    final IFile iFileWf = project.getFile( wf );

    final File fileWf = iFileWf.getLocation().toFile();
    final WorldFile wfReader = new WorldFileReader().readWorldFile( fileWf );

    /* imgFile */
    // RenderedOp image = JAI.create( "fileload", iFileImg.getLocation().toOSString() );
    final URL imageUrl = UrlResolverSingleton.resolveUrl( context, getExtension( href ) );
    final RenderedOp image = JAI.create( "url", imageUrl );
    m_image = new TiledImage( image, true );

    // TODO: the image keeps does not release the stream onto the tiff
    // maybe we must call image.dispose in order to do this?
    // can we do that here??
    final int height = m_image.getHeight();
    final int width = m_image.getWidth();

    final GM_Point origin = GeometryFactory.createGM_Point( wfReader.getUlcx(), wfReader.getUlcy(), system );

    final OffsetVector offsetX = new OffsetVector( wfReader.getRasterXGeoX(), wfReader.getRasterXGeoY() );
    final OffsetVector offsetY = new OffsetVector( wfReader.getRasterYGeoX(), wfReader.getRasterYGeoY() );
    final GridRange gridRange = new GridRange_Impl( new double[] { 0, 0 }, new double[] { width, height } );

    m_domain = new RectifiedGridDomain( origin, offsetX, offsetY, gridRange );
    
  }

  private String getExtension( final String string )
  {
    if( string.contains( "#" ) )
    {
      final String[] arr = string.split( "\\#" );
      return arr[0];
    }

    return string;
  }

}
