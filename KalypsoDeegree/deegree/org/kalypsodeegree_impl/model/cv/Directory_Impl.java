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
/*
 * Directory_Impl.java
 * 
 * Created on 13. Februar 2003, 16:16
 */
package org.deegree_impl.model.cv;

import java.net.URL;
import java.text.DecimalFormat;
import java.util.ArrayList;

import org.deegree.model.coverage.CoverageCreationException;
import org.deegree.model.coverage.Directory;
import org.deegree.model.coverage.Level;
import org.deegree.model.coverage.Tile;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
class Directory_Impl implements Directory
{
  private GM_Envelope boundingBox = null;

  private Level level = null;

  private String resource = null;

  private Directory[] directories = null;

  private String[] extensions = null;

  private double heightCRS = 0;

  private double widthCRS = 0;

  /** Creates a new instance of Directory_Impl */
  Directory_Impl( String resource, GM_Envelope boundingBox, Level level, String[] extensions,
      double widthCRS, double heightCRS )
  {
    this.resource = resource;
    this.boundingBox = boundingBox;
    this.level = level;
    this.extensions = extensions;
    this.widthCRS = widthCRS;
    this.heightCRS = heightCRS;
  }

  /** Creates a new instance of Directory_Impl */
  Directory_Impl( String resource, GM_Envelope boundingBox, Directory[] directories,
      String[] extensions, double widthCRS, double heightCRS )
  {
    this.resource = resource;
    this.boundingBox = boundingBox;
    this.directories = directories;
    this.extensions = extensions;
    this.widthCRS = widthCRS;
    this.heightCRS = heightCRS;
  }

  /** Creates a new instance of Directory_Impl */
  Directory_Impl( String resource, GM_Envelope boundingBox, Level level, Directory[] directories,
      String[] extensions, double widthCRS, double heightCRS )
  {
    this.resource = resource;
    this.boundingBox = boundingBox;
    this.directories = directories;
    this.level = level;
    this.extensions = extensions;
    this.widthCRS = widthCRS;
    this.heightCRS = heightCRS;
  }

  /**
   * returns the bounding box of the <tt>Directory</tt>
   *  
   */
  public GM_Envelope getBoundingBox()
  {
    return boundingBox;
  }

  /**
   * if the tiles are ordered at a quad tree or something like this an instance
   * of <tt>Directory</tt> will contain one or more <tt>Directory</tt>
   * instances with a smaller bounding box.
   *  
   */
  public Directory[] getDirectories()
  {
    return directories;
  }

  /**
   * returns the embeded <tt>Level</tt> if one exists. otherwise <tt>null</tt>
   * will be returned.
   *  
   */
  public Level getLevel()
  {
    return level;
  }

  /**
   * returns all <tt>Tiles</tt> containted within the directory that fits the
   * submitted bounding box
   *  
   */
  public Tile[] getTiles( GM_Envelope bbox ) throws CoverageCreationException
  {
    Debug.debugMethodBegin();

    //double dia = Math.sqrt(bbox.getWidth()*bbox.getWidth() +
    // bbox.getHeight()*bbox.getHeight());
    ArrayList list = null;

    try
    {
      DecimalFormat fo = new DecimalFormat( "#.0" );
      list = new ArrayList( 1000 );

      double x1 = boundingBox.getMin().getX();

      while( x1 < boundingBox.getMax().getX() )
      {
        double y1 = boundingBox.getMin().getY();

        while( y1 < boundingBox.getMax().getY() )
        {
          GM_Envelope env = GeometryFactory.createGM_Envelope( x1, y1, x1 + widthCRS, y1
              + heightCRS );

          if( env.intersects( bbox ) )
          {
            String sx = fo.format( x1 * 1000 );
            sx = sx.substring( 0, sx.length() - 3 ) + "0";

            String sy = fo.format( ( y1 * 1000 ) / 10 * 10 );
            sy = sy.substring( 0, sy.length() - 3 ) + "0";

            String fl = sx + "_" + sy + "." + extensions[0];
            list.add( new Tile_Impl( new URL( "file:///" + resource + "/" + fl ), env, level ) );

          }

          y1 += heightCRS;
        }

        x1 += widthCRS;
      }
    }
    catch( Exception e )
    {
      throw new CoverageCreationException( e.toString() );
    }

    Debug.debugMethodEnd();
    return (Tile[])list.toArray( new Tile[list.size()] );
  }

  /**
   * returns the resource name of the directory
   */
  public String getResource()
  {
    return resource;
  }

  /**
   * returns the file extension known by the directory
   *  
   */
  public String[] getExtensions()
  {
    return extensions;
  }

  /**
   * returns all <tt>Tiles</tt> containted within the directory that fits the
   * submitted bounding box
   *  
   */
  public Tile[] getTiles() throws CoverageCreationException
  {
    return null;
  }

  /**
   * returns the height of the tiles contained within the diirectory in
   * measurement of its CRS
   *  
   */
  public double getHeightCRS()
  {
    return heightCRS;
  }

  /**
   * returns the width of the tiles contained within the diirectory in
   * measurement of its CRS
   *  
   */
  public double getWidthCRS()
  {
    return widthCRS;
  }
}