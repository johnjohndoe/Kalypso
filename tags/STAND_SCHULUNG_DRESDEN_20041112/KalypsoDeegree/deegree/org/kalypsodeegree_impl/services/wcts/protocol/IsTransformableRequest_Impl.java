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
package org.deegree_impl.services.wcts.protocol;

import java.util.HashMap;

import org.deegree.services.wcts.protocol.IsTransformableRequest;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * <p>
 * This class describes requests, which have to examine the possibility to
 * transform one coordinate system into another.
 * <p>
 * Beside the source and the target coordinate-system, the format of the
 * return-value will be fixed.
 * <p>
 * Furtheron the service offers the possibility to declarate user-defined
 * coordinate-systems over the indication of all parameters both for the source
 * and the target coordinate-system.
 * <p>
 * As a rule every particular coordinate-system will be indicated by an
 * identifier.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-07-10
 */
public class IsTransformableRequest_Impl extends OGCWebServiceRequest_Impl implements
    IsTransformableRequest
{
  private CS_CoordinateSystem destinationCRS = null;

  private CS_CoordinateSystem sourceCRS = null;

  private String version = null;

  /**
   * constructor initializing the class with the &lt;Transformable&gt;
   */
  IsTransformableRequest_Impl( String version, String id, HashMap vendorSpecificParameter,
      CS_CoordinateSystem sourceCRS, CS_CoordinateSystem destinationCRS )
  {
    super( "IsTransformable", "WCTS", version, id, vendorSpecificParameter );
    setSourceCRS( sourceCRS );
    setDestinationCRS( destinationCRS );
  }

  /**
   * gets the SourceCRS
   */
  public CS_CoordinateSystem getSourceCRS()
  {
    return sourceCRS;
  }

  /**
   * @see #getSourceCRS()
   */
  public void setSourceCRS( CS_CoordinateSystem sourceCRS )
  {
    this.sourceCRS = sourceCRS;
  }

  /**
   * gets the DestinationCRS
   */
  public CS_CoordinateSystem getDestinationCRS()
  {
    return destinationCRS;
  }

  /**
   * @see #getDestinationCRS()
   */
  public void setDestinationCRS( CS_CoordinateSystem destinationCRS )
  {
    this.destinationCRS = destinationCRS;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "version = " + version + "\n";
    ret += ( "sourceCRS = " + sourceCRS + "\n" );
    ret += ( "destinationCRS = " + destinationCRS + "\n" );
    return ret;
  }
}