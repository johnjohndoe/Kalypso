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
package org.deegree_impl.services.wcs.protocol;

import java.util.HashMap;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.InconsistentRequestException;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.RangeParamList;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wcs.protocol.WCSGetCoverageResponse;
import org.deegree_impl.services.RangeParamList_Impl;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Element;

/**
 * 
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public class WCSProtocolFactory
{
  /**
   * creates a GetCoverage request from its XML representation
   */
  public static synchronized WCSGetCoverageRequest createWCSGetCoverageRequest( Element element )
  {
    Debug.debugMethodBegin( "WCSProtocolFactory", "createWCSGetCoverage(Element)" );
    Debug.debugMethodEnd();
    return null;
  }

  /**
   * creates a GetCoverage from a model (<tt>HashMap</tt>) containing its
   * parameters
   */
  public static synchronized WCSGetCoverageRequest createWCSGetCoverageRequest( HashMap model )
      throws InconsistentRequestException
  {

    if( model.get( "WIDTH" ) == null && model.get( "RESX" ) == null )
    {
      throw new InconsistentRequestException( "width or resx must be set" );
    }

    if( model.get( "HEIGHT" ) == null && model.get( "RESy" ) == null )
    {
      throw new InconsistentRequestException( "height or resy must be set" );
    }

    if( model.get( "VERSION" ) == null )
    {
      throw new InconsistentRequestException( "version must be set" );
    }

    if( model.get( "ID" ) == null )
    {
      throw new InconsistentRequestException( "ID must be set" );
    }

    if( model.get( "LAYER" ) == null )
    {
      throw new InconsistentRequestException( "a layer must be set" );
    }

    if( model.get( "SRS" ) == null )
    {
      throw new InconsistentRequestException( "a SRS must be set" );
    }

    String responseSRS = (String)model.get( "SRS" );
    if( model.get( "RESPONSESRS" ) != null )
    {
      responseSRS = (String)model.get( "RESPONSESRS" );
    }

    if( model.get( "BBOX" ) == null )
    {
      throw new InconsistentRequestException( "a bounding box must be set" );
    }

    if( model.get( "FORMAT" ) == null )
    {
      throw new InconsistentRequestException( "a format must be set" );
    }

    if( model.get( "EXCEPTIONS" ) == null )
    {
      model.put( "EXCEPTIONS", "application/vnd.ogc.se_xml" );
    }

    int width = -1;
    if( model.get( "WIDTH" ) != null )
    {
      width = Integer.parseInt( model.get( "WIDTH" ).toString() );
    }
    int height = -1;
    if( model.get( "HEIGHT" ) != null )
    {
      height = Integer.parseInt( model.get( "HEIGHT" ).toString() );
    }
    int depth = -1;
    if( model.get( "DEPTH" ) != null )
    {
      depth = Integer.parseInt( model.get( "DEPTH" ).toString() );
    }

    double resx = -1;
    if( model.get( "RESX" ) != null )
    {
      resx = Double.parseDouble( model.get( "RESX" ).toString() );
    }

    double resy = -1;
    if( model.get( "RESY" ) != null )
    {
      resy = Double.parseDouble( model.get( "RESY" ).toString() );
    }

    double resz = -1;
    if( model.get( "RESZ" ) != null )
    {
      resz = Double.parseDouble( model.get( "RESZ" ).toString() );
    }

    RangeParamList rpl = new RangeParamList_Impl( model );

    WCSGetCoverageRequest request = null;

    if( width > 0 )
    {
      request = createWCSGetCoverageRequest( (String)model.get( "VERSION" ), (String)model
          .get( "ID" ), null, (String)model.get( "LAYER" ), (String)model.get( "SRS" ),
          responseSRS, (GM_Envelope)model.get( "BBOX" ), rpl, width, height, depth, (String)model
              .get( "FORMAT" ), (String)model.get( "INTERPOLATIONMETHOD" ), (String)model
              .get( "EXCEPTIONS" ) );
    }
    else
    {
      request = createWCSGetCoverageRequest( (String)model.get( "VERSION" ), (String)model
          .get( "ID" ), null, (String)model.get( "LAYER" ), (String)model.get( "SRS" ),
          responseSRS, (GM_Envelope)model.get( "BBOX" ), rpl, resx, resy, resz, (String)model
              .get( "FORMAT" ), (String)model.get( "INTERPOLATIONMETHOD" ), (String)model
              .get( "EXCEPTIONS" ) );
    }

    return request;
  }

  /**
   * creates a GetCoverage from its parameters with defining the coverages
   * width, height and depth
   */
  public static synchronized WCSGetCoverageRequest createWCSGetCoverageRequest( String version,
      String id, HashMap vendorspecificParameters, String layer, String crs, String responseCrs,
      GM_Envelope boundingBox, RangeParamList rpl, int width, int height, int depth, String format,
      String interpolationMethod, String exceptions ) throws InconsistentRequestException
  {

    if( version == null )
    {
      throw new InconsistentRequestException( "version must be set" );
    }

    if( id == null )
    {
      throw new InconsistentRequestException( "ID must be set" );
    }

    if( layer == null )
    {
      throw new InconsistentRequestException( "a layer must be set" );
    }

    if( crs == null )
    {
      throw new InconsistentRequestException( "a SRS must be set" );
    }

    if( responseCrs == null )
    {
      responseCrs = crs;
    }

    if( boundingBox == null )
    {
      throw new InconsistentRequestException( "a bounding box must be set" );
    }

    if( format == null )
    {
      throw new InconsistentRequestException( "a format must be set" );
    }

    if( exceptions == null )
    {
      exceptions = "application/vnd.ogc.se_xml";
    }

    return new WCSGetCoverageRequest_Impl( version, id, vendorspecificParameters, layer, crs,
        responseCrs, boundingBox, rpl, width, height, depth, format, interpolationMethod,
        exceptions );
  }

  /**
   * creates a GetCoverage from its parameters with defining the coverages
   * resolution in x-, y- und z-direction
   */
  public static synchronized WCSGetCoverageRequest createWCSGetCoverageRequest( String version,
      String id, HashMap vendorspecificParameters, String layer, String crs, String responseCrs,
      GM_Envelope boundingBox, RangeParamList rpl, double resX, double resY, double resZ,
      String format, String interpolationMethod, String exceptions )
  {
    return new WCSGetCoverageRequest_Impl( version, id, vendorspecificParameters, layer, crs,
        responseCrs, boundingBox, rpl, resX, resY, resZ, format, interpolationMethod, exceptions );
  }

  /**
   * 
   * 
   * @param request
   * @param coverage
   * 
   * @return
   */
  public static synchronized WCSGetCoverageResponse createGetCoverageResponse(
      OGCWebServiceRequest request, Object coverage )
  {
    return new WCSGetCoverageResponse_Impl( request, coverage );
  }

  /**
   * 
   * 
   * @param request
   * @param exception
   * 
   * @return
   */
  public static synchronized WCSGetCoverageResponse createGetCoverageResponse(
      OGCWebServiceRequest request, OGCWebServiceException exception )
  {
    return null;// new WCSGetCoverageResponse_Impl(request, exception);
  }
}