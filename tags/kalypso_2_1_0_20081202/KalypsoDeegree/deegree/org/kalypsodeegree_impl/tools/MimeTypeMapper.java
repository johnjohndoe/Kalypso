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
package org.kalypsodeegree_impl.tools;

/**
 * 
 * <p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>*
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version
 */
public class MimeTypeMapper
{

  /**
   * returns true if the submitted content type is defined by the OGC
   * 
   * @param contentType
   */
  public static boolean isOGCType( String contentType )
  {
    return contentType.startsWith( "application/vnd.ogc" );
  }

  /**
   * returns true if the submitted content type is an image type
   * 
   * @param contentType
   */
  public static boolean isImageType( String contentType )
  {
    return contentType.startsWith( "image" );
  }

  /**
   * returns true if the submitted image content type is known by the deegree implementations
   * 
   * @param contentType
   */
  public static boolean isKnownImageType( String contentType )
  {
    return contentType.equalsIgnoreCase( "image/jpeg" ) || contentType.equalsIgnoreCase( "image/jpg" )
        || contentType.equalsIgnoreCase( "image/gif" ) || contentType.equalsIgnoreCase( "image/tif" )
        || contentType.equalsIgnoreCase( "image/tiff" ) || contentType.equalsIgnoreCase( "image/bmp" )
        || contentType.equalsIgnoreCase( "image/svg+xml" ) || contentType.equalsIgnoreCase( "image/svg xml" )
        || contentType.equalsIgnoreCase( "image/png" );
  }

  /**
   * returns true if the submitted content type is known by the deegree OWS implementations
   * 
   * @param contentType
   */
  public static boolean isKnownMimeType( String contentType )
  {
    return contentType.equalsIgnoreCase( "image/jpeg" ) || contentType.equalsIgnoreCase( "image/jpg" )
        || contentType.equalsIgnoreCase( "image/gif" ) || contentType.equalsIgnoreCase( "image/tif" )
        || contentType.equalsIgnoreCase( "image/tiff" ) || contentType.equalsIgnoreCase( "image/png" )
        || contentType.equalsIgnoreCase( "text/html" ) || contentType.equalsIgnoreCase( "text/text" )
        || contentType.equalsIgnoreCase( "text/plain" ) || contentType.equalsIgnoreCase( "text/xml" )
        || contentType.equalsIgnoreCase( "image/bmp" ) || contentType.equalsIgnoreCase( "application/xml" )
        || contentType.equalsIgnoreCase( "application/vnd.ogc.se_xml" )
        || contentType.equalsIgnoreCase( "application/vnd.ogc.se_inimage" )
        || contentType.equalsIgnoreCase( "application/vnd.ogc.se_blank" )
        || contentType.equalsIgnoreCase( "application/vnd.ogc.wms_xml" )
        || contentType.equalsIgnoreCase( "application/vnd.ogc.gml" ) || contentType.equalsIgnoreCase( "image/svg+xml" )
        || contentType.equalsIgnoreCase( "image/svg xml" );
  }

  /**
   * maps a 'simple' format name like gif, jpg or text to the corresponding mime type --> e.g. image/gif, image/jpeg or
   * text/plain
   */
  public static String toMimeType( String contentType )
  {
    Debug.debugMethodBegin();

    String mimetype = "";

    if( isKnownMimeType( contentType ) )
    {
      mimetype = contentType;
    }
    else
    {
      if( contentType.equalsIgnoreCase( "jpeg" ) )
      {
        mimetype = "image/jpeg";
      }
      else if( contentType.equalsIgnoreCase( "jpg" ) )
      {
        mimetype = "image/jpeg";
      }
      else if( contentType.equalsIgnoreCase( "gif" ) )
      {
        mimetype = "image/gif";
      }
      else if( contentType.equalsIgnoreCase( "png" ) )
      {
        mimetype = "image/png";
      }
      else if( contentType.equalsIgnoreCase( "bmp" ) )
      {
        mimetype = "image/bmp";
      }
      else if( contentType.equalsIgnoreCase( "tif" ) )
      {
        mimetype = "image/tiff";
      }
      else if( contentType.equalsIgnoreCase( "tiff" ) )
      {
        mimetype = "image/tiff";
      }
      else if( contentType.equalsIgnoreCase( "svg" ) )
      {
        mimetype = "image/svg+xml";
      }
      else if( contentType.equalsIgnoreCase( "xml" ) )
      {
        mimetype = "text/xml";
      }
      else if( contentType.equalsIgnoreCase( "gml" ) )
      {
        mimetype = "text/gml";
      }
      else if( contentType.equalsIgnoreCase( "text" ) )
      {
        mimetype = "text/plain";
      }
      else if( contentType.equalsIgnoreCase( "inimage" ) )
      {
        mimetype = "application/vnd.ogc.se_inimage";
      }
      else if( contentType.equalsIgnoreCase( "blank" ) )
      {
        mimetype = "application/vnd.ogc.se_blank";
      }
      else if( contentType.equalsIgnoreCase( "gml" ) )
      {
        mimetype = "application/vnd.ogc.gml";
      }
      else
      {
        // unknown mimetype
        mimetype = "unknown/unknown";
      }
    }

    Debug.debugMethodEnd();

    return mimetype;
  }
}