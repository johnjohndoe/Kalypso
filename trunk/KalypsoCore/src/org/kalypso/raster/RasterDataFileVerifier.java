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
package org.kalypso.raster;

import java.io.File;
import java.io.IOException;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IPath;
import org.geotiff.image.jai.GeoTIFFDirectory;
import org.libtiff.jai.codec.XTIFFField;
import org.opengis.cs.CS_CoordinateSystem;

import com.sun.media.jai.codec.FileSeekableStream;

/**
 * @author kuch
 */
public class RasterDataFileVerifier
{

  private static final String[] validFileExtensions = new String[] { "tif", "tiff", "jpg", "jpeg", "asc", "dat" };

  public enum RASTER_TYPE
  {
    eAsciiGrid,
    eImage,
    eImageWorldFile,
    eImageGeo
  }

  public enum IMAGE_TYPE
  {
    eNoImage,
    eGIF,
    eJPG,
    eTIFF
  }

  public boolean verify( final IPath docLocation )
  {
    /* file exists? */
    if( (docLocation == null) || !docLocation.toFile().exists() )
    {
      return false;
    }

    /* is it a valid file or an dir? */
    if( docLocation.toFile().isDirectory() )
    {
      return false;
    }

    /* valid file extension */
    if( !ArrayUtils.contains( RasterDataFileVerifier.validFileExtensions, docLocation.getFileExtension().toLowerCase() ) )
    {
      return false;
    }

    final RASTER_TYPE raster_type = determineType( docLocation );
    if( raster_type == null )
    {
      return false;
    }

    return true;
  }

  public IRasterMetaReader getRasterMetaReader( final IPath docLocation, final CS_CoordinateSystem cs  )
  {
    final RASTER_TYPE raster_type = determineType( docLocation );
    if( raster_type == null )
    {
      throw (new IllegalStateException());
    }

    switch( raster_type )
    {
      case eAsciiGrid:
        return new RasterMetaReaderAscii(docLocation, cs);

      case eImage:
        return null;

      case eImageGeo:
        return new RasterMetaReaderGeo(docLocation, determineImageType( docLocation ));

      case eImageWorldFile:
        return new RasterMetaReaderWorldFile(docLocation.toFile(), getWorldFile( docLocation ));

      default:
        return null;
    }
  }

  private RASTER_TYPE determineType( final IPath docLocation )
  {
    /* determin image type */
    final IMAGE_TYPE image_type = determineImageType( docLocation );

    /* if it is an image, it can maybe have an world file with coordinates */
    if( !(IMAGE_TYPE.eNoImage.equals( image_type )) )
    {
      final File worldFile = getWorldFile( docLocation );
      if( worldFile != null )
      {
        return RASTER_TYPE.eImageWorldFile;
      }

      if( worldFile == null )
      {
        /* is it an geodata image, like geotif? ATM only geotif is supported */
        if( IMAGE_TYPE.eTIFF.equals( image_type ) && isGeoTiff( docLocation ) )
        {
          return RASTER_TYPE.eImageGeo;
        }
        else
        {
          /* only a simple image */
          return RASTER_TYPE.eImage;
        }
      }
     
    }

    if( isAsciGrid( docLocation ) )
    {
      return RASTER_TYPE.eAsciiGrid;
    }

    return null;
  }

  private boolean isAsciGrid( final IPath docLocation )
  {
    if( docLocation == null )
    {
      throw (new IllegalStateException());
    }

    // TODO: mke a real inspection of file
    final String[] ascExtensions = new String[] { "asc", "dat" };

    if( ArrayUtils.contains( ascExtensions, docLocation.getFileExtension().toLowerCase() ) )
    {
      return true;
    }

    return false;
  }

  private boolean isGeoTiff( final IPath docLocation )
  {
    if( docLocation == null )
    {
      throw (new IllegalStateException());
    }
    FileSeekableStream stream;
    try
    {
      stream = new FileSeekableStream( docLocation.toFile() );
      final GeoTIFFDirectory directory = new GeoTIFFDirectory( stream, 0 );

      final XTIFFField[] geoKeys = directory.getGeoKeys();
      if( geoKeys.length <= 0 )
      {
        return false;
      }

      return true;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return false;
  }

  private File getWorldFile( final IPath docLocation )
  {
    if( docLocation == null )
    {
      throw (new IllegalStateException());
    }

    final File file = docLocation.toFile();
    final File dir = file.getParentFile();
    final String fileName = file.getName();

    if( (fileName == null) || !fileName.contains( "." ) )
    {
      throw (new IllegalStateException());
    }

    final String[] parts = fileName.split( "\\." );

    String prefix = null;
    if( parts.length == 2 )
    {
      prefix = parts[0];
    }
    else
    {
      for( int i = 0; i < parts.length - 1; i++ )
      {
        prefix = prefix + parts[i] + ".";
      }
    }
    final String[] worldFileExtensions = new String[] { "tfw", "gfw", "jpw" };

    for( final String extension : worldFileExtensions )
    {
      final File worldFile = new File( dir + "/" + prefix + "." + extension );
      if( worldFile.exists() )
      {
        return worldFile;
      }
    }

    return null;
  }

  private IMAGE_TYPE determineImageType( final IPath docLocation )
  {
    if( docLocation == null )
    {
      throw (new IllegalStateException());
    }

    final String fileExtension = docLocation.getFileExtension().toLowerCase();

    final String[] jpgTypes = new String[] { "jpg", "jpeg" };
    final String[] tifTypes = new String[] { "tif", "tiff" };

    if( ArrayUtils.contains( jpgTypes, fileExtension ) )
    {
      return IMAGE_TYPE.eJPG;
    }
    else if( ArrayUtils.contains( tifTypes, fileExtension ) )
    {
      return IMAGE_TYPE.eTIFF;
    }
    else if( "gif".equals( fileExtension ) )
    {
      return IMAGE_TYPE.eGIF;
    }

    return IMAGE_TYPE.eNoImage;
  }
}
