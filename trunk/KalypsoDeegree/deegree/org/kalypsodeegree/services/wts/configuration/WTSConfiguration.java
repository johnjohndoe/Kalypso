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
package org.deegree.services.wts.configuration;

import java.awt.image.BufferedImage;

import org.deegree.services.OGCWebService;

/**
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface WTSConfiguration
{
  public static final int LOCALWCS = 0;

  public static final int LOCALWFS = 1;

  public static final int LOCALWMS = 2;

  public static final int REMOTEWCS = 3;

  public static final int REMOTEWFS = 4;

  public static final int REMOTEWMS = 5;

  /**
   * returns the source for submitted DEM. Using the method
   * <tt>getSourceType>/tt>
   * it can be determined if the source is a:
   * <ul>
   *  <li>LOCALWCS</li>
   *  <li>LOCALWFS</li>
   *  <li>REMOTEWCS</li>
   *  <li>REMOTEWFS</li>
   * </ul>
   */
  String getDEMSource( String demName );

  /**
   * returns the source for submitted texture. Using the method
   * <tt>getSourceType>/tt>
   * it can be determined if the source is a:
   * <ul>
   *  <li>LOCALWCS</li>
   *  <li>LOCALWMS</li>
   *  <li>REMOTEWCS</li>
   *  <li>REMOTEWMS</li>
   * </ul>
   */
  String getTextureSource( String textureName );

  /**
   * returns the source for submitted feature. Using the method
   * <tt>getSourceType>/tt> it can be determined if the source is a:
   * <ul>
   *  <li>LOCALWFS</li>
   *  <li>REMOTEWFS</li>     
   * </ul>
   */
  String getFeatureSource( String textureName );

  /**
   * returns the type of the submitted source. possible values are:
   * <ul>
   * <li>LOCALWCS</li>
   * <li>LOCALWFS</li>
   * <li>LOCALWMS</li>
   * <li>REMOTEWCS</li>
   * <li>REMOTEWFS</li>
   * <li>REMOTEWMS</li>
   * </ul>
   */
  int getSourceType( String source );

  /**
   * returns the width of the tiles in pixels the requested footprint is split
   * of when data are requested from a wcs or a wms
   */
  int getTileWidth( String layer );

  /**
   * returns the height of the tiles in pixels the requested footprint is split
   * of when data are requested from a wcs or a wms
   */
  int getTileHeight( String layer );

  /**
   * returns the name of the format the submitted layer shall be requested from
   * a wcs, wms, of wfs
   */
  String getFormatName( String layer );

  /**
   * returns the service that is responsible for handling the submitted layer
   */
  OGCWebService getResponsibleService( String layer );

  /**
   * returns the texture of a feature as <tt>BufferedImage</tt>
   */
  BufferedImage getFeatureTexture( String featureId );

  /**
   * returns the background image associated to the named background
   */
  BufferedImage getBackground( String name );

}