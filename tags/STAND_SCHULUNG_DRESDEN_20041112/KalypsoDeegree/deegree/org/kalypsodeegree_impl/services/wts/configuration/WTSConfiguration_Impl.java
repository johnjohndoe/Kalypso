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
package org.deegree_impl.services.wts.configuration;

import java.awt.image.BufferedImage;
import java.util.HashMap;

import org.deegree.services.OGCWebService;
import org.deegree.services.wts.configuration.WTSConfiguration;

/**
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class WTSConfiguration_Impl implements WTSConfiguration
{

  private HashMap sourceTypes = null;

  private HashMap formatNames = null;

  private HashMap responsibleServices = null;

  private HashMap featureTextures = null;

  private HashMap tileWidth = null;

  private HashMap tileHeight = null;

  private HashMap backgrounds = null;

  private static WTSConfiguration conf = null;

  /** Creates a new instance of WTSConfiguration_Impl */
  protected WTSConfiguration_Impl( HashMap sourceTypes, HashMap formatNames,
      HashMap featureTextures, HashMap responsibleServices, HashMap backgrounds )
  {
    this.sourceTypes = sourceTypes;
    this.formatNames = formatNames;
    this.responsibleServices = responsibleServices;
    this.featureTextures = featureTextures;
    this.backgrounds = backgrounds;
    tileWidth = new HashMap();
    tileHeight = new HashMap();
    conf = this;
  }

  /**
   * singelton: returns the instance of the <tt>WTSConfiguration</tt>
   */
  public static final WTSConfiguration getInstance() throws ConfigurationException
  {
    if( conf == null )
    {
      throw new ConfigurationException( "WTSConfiguration isn't initialized" );
    }
    return conf;
  }

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
   *
   */
  public String getDEMSource( String demName )
  {
    return "LOCALWCS";
  }

  /**
   * returns the source for submitted feature. Using the method
   * <tt>getSourceType>/tt> it can be determined if the source is a:
   * <ul>
   *  <li>LOCALWFS</li>
   *  <li>REMOTEWFS</li>
   * </ul>
   *
   */
  public String getFeatureSource( String textureName )
  {
    return "LOCALWFS";
  }

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
   *
   */
  public String getTextureSource( String textureName )
  {
    return "LOCALWCS";
  }

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
   *  
   */
  public int getSourceType( String source )
  {
    Integer i = (Integer)sourceTypes.get( source.toUpperCase() );
    return i.intValue();
  }

  /**
   * returns the height of the tiles in pixels the requested footprint is split
   * of when data are requested from a wcs or a wms
   *  
   */
  public int getTileHeight( String layer )
  {
    Integer height = (Integer)tileHeight.get( layer.toUpperCase() );
    if( height == null )
    {
      return 128;
    }
    else
    {
      return height.intValue();
    }
  }

  /**
   * returns the width of the tiles in pixels the requested footprint is split
   * of when data are requested from a wcs or a wms
   *  
   */
  public int getTileWidth( String layer )
  {
    Integer width = (Integer)tileWidth.get( layer.toUpperCase() );
    if( width == null )
    {
      return 512;
    }
    else
    {
      return width.intValue();
    }
  }

  /**
   * returns the name of the format the submitted layer shall be requested from
   * a wcs, wms, of wfs
   *  
   */
  public String getFormatName( String layer )
  {
    return (String)formatNames.get( layer.toUpperCase() );
  }

  /**
   * returns the service that is responsible for handling the submitted layer
   *  
   */
  public OGCWebService getResponsibleService( String layer )
  {
    return (OGCWebService)responsibleServices.get( layer.toUpperCase() );
  }

  /**
   * returns the texture of a feature as <tt>BufferedImage</tt>
   * 
   * @param featureId
   *          id of the feature (will be converted into uppercase)
   * @return
   */
  public BufferedImage getFeatureTexture( String featureId )
  {
    return (BufferedImage)featureTextures.get( featureId.toUpperCase() );
  }

  /**
   * returns the background image associated to the named background
   * 
   * @param name
   *          name of the background
   * @return background image or <tt>null</tt> if background isn't known
   */
  public BufferedImage getBackground( String name )
  {
    return (BufferedImage)backgrounds.get( name );
  }

}