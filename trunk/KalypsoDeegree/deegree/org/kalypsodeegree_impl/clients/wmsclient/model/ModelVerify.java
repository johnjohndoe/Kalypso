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
import org.deegree.model.geometry.GM_Position;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class ModelVerify
{
  /**
   * validates the bounding box of the model against the maximal possible
   * bounding box. If the bounding box of the model exceeds the limiting
   * bounding box it will be corrected
   */
  public synchronized static WMSGetMapRequest setToBoundaries( WMSGetMapRequest mrm,
      GM_Envelope maxBBox )
  {
    Debug.debugMethodBegin( "ModelVerify", "setToBoundaries" );

    GM_Envelope env = mrm.getBoundingBox();

    double minx = env.getMin().getX();
    double miny = env.getMin().getY();
    double maxx = env.getMax().getX();
    double maxy = env.getMax().getY();

    if( ( mrm.getBoundingBox().getWidth() > maxBBox.getWidth() )
        || ( mrm.getBoundingBox().getHeight() > maxBBox.getHeight() ) )
    {
      minx = maxBBox.getMin().getX();
      miny = maxBBox.getMin().getY();
      maxx = maxBBox.getMax().getX();
      maxy = maxBBox.getMax().getY();
    }
    else
    {
      if( mrm.getBoundingBox().getMin().getX() < maxBBox.getMin().getX() )
      {
        // shift bounding box to east
        double diff = maxBBox.getMin().getX() - minx;
        minx = minx + diff;
        maxx = maxx + diff;
      }
      else if( maxx > maxBBox.getMax().getX() )
      {
        // shift bounding box to west
        double diff = maxx - maxBBox.getMax().getX();
        minx = minx - diff;
        maxx = maxx - diff;
      }

      if( miny < maxBBox.getMin().getY() )
      {
        // shift bounding box to north
        double diff = maxBBox.getMin().getY() - miny;
        miny = miny + diff;
        maxy = maxy + diff;
      }
      else if( maxy > maxBBox.getMax().getY() )
      {
        // shift bounding box to south
        double diff = maxy - maxBBox.getMax().getY();
        miny = miny - diff;
        maxy = maxy - diff;
      }
    }

    env = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );

    // TODO
    // create new map request
    Debug.debugMethodEnd();
    return mrm;
  }

  /**
   * validates the scale of the model against the allowed minimal scale. if the
   * scale of the model is smaller then minScale the model will be rescaled.
   */
  public synchronized static WMSGetMapRequest setToMinScale( WMSGetMapRequest mrm, double minScale )
  {
    Debug.debugMethodBegin( "ModelVerify", "setToMinScale" );

    GM_Envelope env = mrm.getBoundingBox();

    double minx = env.getMin().getX();
    double miny = env.getMin().getY();
    double maxx = env.getMax().getX();
    double maxy = env.getMax().getY();

    double scale = getScale( minx, miny, maxx, maxy, mrm.getWidth(), mrm.getHeight() );

    if( scale < minScale )
    {
      GM_Position center = GeometryFactory.createGM_Position( ( ( maxx - minx ) / 2.0 ) + minx,
          ( ( maxy - miny ) / 2.0 ) + miny );
      double delta = minScale / scale;
      double width = mrm.getBoundingBox().getWidth() * delta;
      double height = mrm.getBoundingBox().getHeight() * delta;
      minx = center.getX() - ( width / 2.0 );
      maxx = center.getX() + ( width / 2.0 );
      miny = center.getY() - ( height / 2.0 );
      maxy = center.getY() + ( height / 2.0 );
    }

    // TODO
    // create new map request
    Debug.debugMethodEnd();
    return mrm;
  }

  /**
   * returns the scale of the requested map. the scale is measured as defined at
   * the WMS 1.0.0 specifications of the OGC
   */
  private static double getScale( double minx, double miny, double maxx, double maxy, int width,
      int height )
  {
    double sx = Math.sqrt( Math.pow( maxx - minx, 2 ) + Math.pow( maxy - miny, 2 ) );
    double px = Math.sqrt( width * width + height * height );

    return sx / px;
  }
}