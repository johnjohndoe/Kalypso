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
package org.deegree_impl.services.wts.protocol;

import java.awt.Color;
import java.util.Calendar;
import java.util.HashMap;

import org.deegree.model.geometry.GM_Position;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wts.protocol.WTSGetViewRequest;
import org.deegree.services.wts.protocol.WTSGetViewResponse;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.TimeTools;
import org.w3c.dom.Document;

/**
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-03-01
 */
public class WTSProtocolFactory
{
  /**
   * creates a request object from a model (key-value-pair encoding) of a WTS
   * request and a unique ID.
   * 
   * @param id
   *          ID of the request
   * @param model
   *          key-value-pair encoding of the request
   * 
   * @return @throws
   *         WTSProtocolException
   */
  public static OGCWebServiceRequest createRequest( String id, HashMap model )
      throws WTSProtocolException
  {
    String req = (String)model.get( "REQUEST" );
    OGCWebServiceRequest request = null;

    if( req.equalsIgnoreCase( "GETVIEW" ) )
    {
      request = createGetViewRequest( id, model );
    }

    return request;
  }

  /**
   * creates a <tt>WTSGetViewRequest</tt> from a <tt>HashMap</tt> that
   * contains the request parameters as key-value-pairs. Keys are expected to be
   * in upper case notation.
   * 
   * @return an instance of <tt>WTSGetViewRequest</tt>
   * @param model
   *          <tt>HashMap</tt> containing the request parameters
   * @throws WTSProtocolException
   *           will be thrown if mandatory parameter are missing or parameters
   *           don't contain valid values
   */
  public static WTSGetViewRequest createGetViewRequest( String id, HashMap model )
      throws WTSProtocolException
  {
    String version = (String)model.get( "VERSION" );

    if( id == null )
    {
      throw new WTSProtocolException( "ID-value must be set" );
    }

    HashMap vendorSpecificParameter = (HashMap)model.get( "VENDORSPECIFICPARAMETER" );
    String format = (String)model.get( "FORMAT" );

    if( format == null )
    {
      throw new WTSProtocolException( "FORMAT-value must be set" );
    }

    String[] layers = (String[])model.get( "TEXTURE" );
    String[] styles = (String[])model.get( "STYLES" );

    if( ( layers != null ) && ( styles != null ) && ( layers.length != styles.length ) )
    {
      throw new WTSProtocolException( "Different amount of styles and layers are defined" );
    }

    WTSGetViewRequest.Layer[] ls = null;

    if( ( layers != null ) && ( styles != null ) )
    {
      ls = new WTSGetViewRequest.Layer[layers.length];

      for( int i = 0; i < layers.length; i++ )
      {
        ls[i] = WTSGetViewRequest_Impl.createLayer( layers[i], styles[i] );
      }
    }
    else
    {
      ls = new WTSGetViewRequest.Layer[0];
    }

    String crs = (String)model.get( "SRS" );

    if( crs == null )
    {
      throw new WTSProtocolException( "CRS-value must be set" );
    }

    if( model.get( "WIDTH" ) == null )
    {
      throw new WTSProtocolException( "WIDTH-value must be set" );
    }

    int width = Integer.parseInt( (String)model.get( "WIDTH" ) );

    if( model.get( "HEIGHT" ) == null )
    {
      throw new WTSProtocolException( "HEIGHT-value must be set" );
    }

    int height = Integer.parseInt( (String)model.get( "HEIGHT" ) );
    String tmp = (String)model.get( "BGCOLOR" );

    if( tmp == null )
    {
      throw new WTSProtocolException( "BGCOLOR-value must be set" );
    }

    Color bgColor = Color.white;

    try
    {
      bgColor = Color.decode( tmp );
    }
    catch( Exception e )
    {}

    String exceptions = (String)model.get( "EXCEPTIONS" );

    if( exceptions == null )
    {
      exceptions = "application/vnd.ogc.se_xml";
    }

    tmp = (String)model.get( "DATE" );
    if( tmp == null )
    {
      tmp = "2003-06-21";
    }

    if( model.get( "TIME" ) == null )
    {
      tmp += " 12:00:00";
    }
    else
    {
      tmp = tmp + " " + model.get( "TIME" );
    }
    Calendar date = TimeTools.createCalendar( tmp );

    double scale = 1;

    if( model.get( "SCALE" ) != null )
    {
      scale = Double.parseDouble( (String)model.get( "SCALE" ) );
    }

    Object background = (String)model.get( "BACKGROUND" );

    String[] dem = (String[])model.get( "ELEVATION_MODELS" );

    if( dem == null )
    {
      throw new WTSProtocolException( "ELEVATION_MODELS-value must be set" );
    }

    String[] ftc = (String[])model.get( "FEATURE_COLLECTIONS" );
    WTSGetViewRequest.Layer[] ftCollections = null;

    if( ftc != null )
    {
      ftCollections = new WTSGetViewRequest.Layer[ftc.length];

      for( int i = 0; i < ftc.length; i++ )
      {
        ftCollections[i] = WTSGetViewRequest_Impl.createLayer( ftc[i], ftc[i] );
      }
    }
    else
    {
      ftCollections = new WTSGetViewRequest.Layer[0];
    }

    if( model.get( "AOV" ) == null )
    {
      throw new WTSProtocolException( "AOV-value must be set" );
    }

    double aov = Double.parseDouble( (String)model.get( "AOV" ) );
    aov = Math.toRadians( aov );

    if( model.get( "DISTANCE" ) == null )
    {
      throw new WTSProtocolException( "DISTANCE-value must be set" );
    }
    double distance = Double.parseDouble( (String)model.get( "DISTANCE" ) );

    if( model.get( "PITCH" ) == null )
    {
      throw new WTSProtocolException( "PITCH-value must be set" );
    }

    double pitch = Double.parseDouble( (String)model.get( "PITCH" ) );
    pitch = Math.toRadians( pitch );

    if( model.get( "POI" ) == null )
    {
      throw new WTSProtocolException( "POI-value must be set" );
    }

    String[] p_o_i = (String[])model.get( "POI" );
    double[] p = new double[3];
    p[0] = Double.parseDouble( p_o_i[0] );
    p[2] = Double.parseDouble( p_o_i[1] );
    p[1] = Double.parseDouble( p_o_i[2] );

    GM_Position poi = GeometryFactory.createGM_Position( p );

    if( model.get( "YAW" ) == null )
    {
      throw new WTSProtocolException( "YAW-value must be set" );
    }

    double yaw = Double.parseDouble( (String)model.get( "YAW" ) );
    if( yaw > 89.5 && yaw < 90.5 )
      yaw = 91;
    yaw = Math.toRadians( yaw );

    return createGetViewRequest( version, id, vendorSpecificParameter, format, ls, crs, width,
        height, bgColor, aov, distance, pitch, poi, yaw, exceptions, date, scale, background, dem,
        ftCollections );
  }

  /**
   * creates a <tt>WTSGetViewRequest</tt> from a set of parameters
   * 
   * @param id
   *          request id
   * @param vendorSpecificParameter
   *          vendorSpecificParameter parameter(s)
   * @param format
   *          format the result of the request shall be returned
   * @param layers
   *          layers that are requested
   * @param crs
   *          coordinate reference system that scene's data are requested at
   * @param width
   *          width scene in pixel
   * @param height
   *          height scene in pixel
   * @param bgColor
   *          background color of the scene
   * @param aov
   *          viewers angle of view
   * @param distance
   *          distance between viewer and target on an imaginery horizontal
   *          ground in height of the target
   * @param pitch -
   * @param poi
   *          target where the viewer is looking to
   * @param yaw -
   * @param exceptions
   *          format how to return exceptions
   * @param date
   *          month, day and hour for which he scene shall be created
   *          (light-conditions)
   * @param scale
   *          vertical scale of the dem and the objects in the scene (default =
   *          1)
   * @param background
   *          background used for scene (cloud images form example)
   * @param dem
   *          name of the dem that shall be used
   * @param ftCollections
   *          named feature collections that shall be superimposed on the dem
   * @return an instance of <tt>WTSGetViewRequest</tt>
   */
  public static WTSGetViewRequest createGetViewRequest( String version, String id,
      HashMap vendorSpecificParameter, String format, WTSGetViewRequest.Layer[] layers, String crs,
      int width, int height, Color bgColor, double aov, double distance, double pitch,
      GM_Position poi, double yaw, String exceptions, Calendar date, double scale,
      Object background, String[] dem, WTSGetViewRequest.Layer[] ftCollections )
  {
    return new WTSGetViewRequest_Impl( version, id, vendorSpecificParameter, format, layers, crs,
        width, height, bgColor, aov, distance, pitch, poi, yaw, exceptions, date, scale,
        background, dem, ftCollections );
  }

  /**
   * creates a responseobject to a getViewRequest
   * 
   * @param view
   *          created scene
   * @param request
   *          request from which the scene was created
   * @return response object
   */
  public static WTSGetViewResponse createGetViewResponse( Object view, OGCWebServiceRequest request )
  {
    return new WTSGetViewResponse_Impl( view, null, request );
  }

  /**
   * creates a responseobject to a getViewRequest with a submitted error
   * 
   * @param exception
   *          exception that has been raised performing the request
   * @param request
   *          request that has been performed when exception raised
   * @return response object
   */
  public static WTSGetViewResponse createGetViewResponse( Document exception,
      OGCWebServiceRequest request )
  {
    return new WTSGetViewResponse_Impl( null, exception, request );
  }
}