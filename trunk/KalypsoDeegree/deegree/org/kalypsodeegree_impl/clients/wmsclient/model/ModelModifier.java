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
package org.deegree_impl.clients.wmsclient.model;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Point;
import org.deegree.services.wms.protocol.WMSGetMapRequest;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class ModelModifier
{
  /**
   * modify's the submitted <tt>WMSGetMapRequest</tt> by setting a new
   * boundingbox that is $zoomlevel$ % larger ( zoomLevel > 0 ) or $zoomlevel$
   * smaller ( zoomLevel < 0 ) than the current bounding box.
   * 
   * @param model
   * @param center
   * @param zoomLevel
   * 
   * @return modified <tt>WMSGetMapRequest</tt>
   */
  public synchronized static WMSGetMapRequest modify( WMSGetMapRequest model, GM_Point center,
      double zoomLevel )
  {
    double minx = model.getBoundingBox().getMin().getX();
    double miny = model.getBoundingBox().getMin().getY();
    double maxx = model.getBoundingBox().getMax().getX();
    double maxy = model.getBoundingBox().getMax().getY();
    double width = ( maxx - minx ) * zoomLevel;
    double height = ( maxy - miny ) * zoomLevel;

    minx = center.getX() - ( width / 2.0 );
    miny = center.getY() - ( height / 2.0 );
    maxx = center.getX() + ( width / 2.0 );
    maxy = center.getY() + ( height / 2.0 );

    // TODO
    // create new map request

    return model;
  }

  /**
   * creates a <tt>WMSGetMapRequest</tt> that is identical to the submitted
   * one but with a new boundingbox
   * 
   * @param model
   *          old <tt>WMSGetMapRequest</tt>
   * @param bbox
   *          new bounding box
   * 
   * @return modified <tt>WMSGetMapRequest</tt>
   */
  public synchronized static WMSGetMapRequest modify( WMSGetMapRequest model, GM_Envelope bbox )
  {
    //TODO
    //check if bounding box is valid for selected layers
    // TODO
    // create new map request

    return model;
  }
}