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
package org.deegree_impl.services.wts;

import java.io.ByteArrayInputStream;
import java.io.ObjectInputStream;
import java.util.ArrayList;

import javax.media.j3d.Transform3D;
import javax.vecmath.Point3d;
import javax.vecmath.Vector3d;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wcs.protocol.WCSGetCoverageResponse;
import org.deegree.services.wfs.protocol.WFSGetFeatureResponse;
import org.deegree.services.wts.ViewPoint;
import org.deegree.services.wts.configuration.WTSConfiguration;
import org.deegree.services.wts.protocol.WTSGetViewRequest;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.wcs.protocol.WCSProtocolFactory;
import org.deegree_impl.services.wts.configuration.WTSConfiguration_Impl;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * class for calculating and splitting a views footprint into boxes and/or
 * stripes
 */
class Splitter implements OGCWebServiceClient
{
  private ViewPoint vp = null;

  private WTSGetViewRequest request = null;

  private double heightOverGround = 150;

  private double observerHeight = 0;

  /** Creates a new instance of MakeStripes */
  Splitter( WTSGetViewRequest request, ViewPoint vp ) throws Exception
  {
    this.request = request;
    this.vp = vp;
    Point3d ob = vp.getObserverPosition();
    observerHeight = ob.y;

    // create and perform a request against the service that provides
    // the DEM to determine to observers height above ground
    GM_Envelope bbox = GeometryFactory.createGM_Envelope( ob.x - 2, ob.z - 2, ob.x + 2, ob.z + 2 );
    WTSConfiguration conf = WTSConfiguration_Impl.getInstance();
    String layer = request.getElevationModels()[0];
    WCSGetCoverageRequest wcsReq = WCSProtocolFactory.createWCSGetCoverageRequest( "1.0.0", this
        .toString(), null, layer, request.getSrs(), request.getSrs(), bbox, null, 4, 4, -1, conf
        .getFormatName( layer ), null, "application/vnd.ogc.se_xml" );
    OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, wcsReq, "", this );
    conf.getResponsibleService( layer ).doService( event );
  }

  /**
   * will be called by to WCService with the response to determine the observers
   * height above ground
   */
  public void write( Object result )
  {
    try
    {
      if( result instanceof String )
      {
        //handleException( (String)result );
      }
      else
      {
        OGCWebServiceResponse response = null;

        if( result instanceof OGCWebServiceEvent )
        {
          response = ( (OGCWebServiceEvent)result ).getResponse();
        }
        else
        {
          response = (OGCWebServiceResponse)result;
        }

        Document doc = response.getException();

        if( doc == null )
        {
          if( response instanceof WCSGetCoverageResponse )
          {
            handleWCSResponse( (WCSGetCoverageResponse)response );
          }
          else if( response instanceof WFSGetFeatureResponse )
          {
            // TODO
            //handleWFSResponse((WFSGetFeatureResponse) response);
          }
        }
        else
        {
          //handleException( doc );
        }
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * 
   * 
   * @param response
   * 
   * @throws Exception
   */
  private void handleWCSResponse( WCSGetCoverageResponse response ) throws Exception
  {

    ByteArrayInputStream bis = new ByteArrayInputStream( (byte[])response.getResponse() );
    ObjectInputStream ois = new ObjectInputStream( bis );
    float[][] mat = (float[][])ois.readObject();

    if( ( mat != null ) && ( mat.length > 1 ) )
    {
      double sum = 0;

      for( int i = 0; i < mat.length; i++ )
      {
        for( int j = 0; j < mat[0].length; j++ )
        {
          sum += mat[i][j];
        }
      }

      sum = ( sum / ( mat.length * mat[0].length ) );
      heightOverGround = observerHeight - sum;
    }
    else
    {
      heightOverGround = 50;
    }

    while( heightOverGround < 2 )
      heightOverGround += 2;

    System.out.println( "heightOverGround: " + heightOverGround );
  }

  /**
   * tiles the footprint trapez into count pralell stripes with an identical
   * view angle from the viewers position
   */
  public GM_Surface[] makeStripes( int count ) throws InvalidArcException, GM_Exception, Exception
  {
    // wait until the observers height over ground has been determind
    int time = 0;

    while( heightOverGround < -99 )
    {
      try
      {
        Thread.sleep( 20 );
      }
      catch( Exception e )
      {}
      time += 20;
      if( time > 5000 )
      {
        throw new Exception( "couldn't determine obeserver's height above ground" );
      }
    }

    double maxDist = -10000;

    Point3d[] footprint = vp.getFootprint();
    ArrayList surfaces = new ArrayList();

    double height = heightOverGround;

    double maxdir = vp.getVDirection() + ( vp.getAoV() / 2f );
    double mindir = vp.getVDirection() - ( vp.getAoV() / 2f );
    mindir *= 1.0;

    // calc maximum upper angle of view
    if( maxdir > 0 )
    {
      maxdir = Math.toRadians( -0.01 );
    }

    while( calcDist( height, maxdir ) < maxDist )
    {
      double tmp = Math.toDegrees( maxdir );
      tmp -= -0.02;
      maxdir = Math.toRadians( tmp );
    }

    // calculate delta direction (angle)
    double mDist = calcDist( height, mindir ) * 0.9;
    Point3d[] fp = vp.getFootprint( heightOverGround );
    double dDist = -1.5d * Math.abs( fp[3].x - fp[2].x );
    double dist = mDist;

    // start claculating stripes by adding delta directions to the
    // starting minimum angle until the maximum angle resp. the
    // corresponding maximum distance is reached
    ConvenienceCSFactory csF = ConvenienceCSFactory.getInstance();
    CoordinateSystem crs = csF.getCSByName( request.getSrs() );
    CS_CoordinateSystem cs = Adapters.getDefault().export( crs );

    double k = 2.8;
    while( dist > maxDist )
    {
      footprint = calcFootprint( height, dist + dDist, dist );
      dist += dDist;
      dDist *= k;
      GM_Position[] pos = new GM_Position[5];
      pos[0] = GeometryFactory.createGM_Position( footprint[0].x, footprint[0].z );
      pos[1] = GeometryFactory.createGM_Position( footprint[1].x, footprint[1].z );
      pos[2] = GeometryFactory.createGM_Position( footprint[3].x, footprint[3].z );
      pos[3] = GeometryFactory.createGM_Position( footprint[2].x, footprint[2].z );
      pos[4] = GeometryFactory.createGM_Position( footprint[0].x, footprint[0].z );
      surfaces.add( GeometryFactory.createGM_Surface( pos, null, null, cs ) );
    }

    footprint = calcFootprint( height, maxDist, dist );
    GM_Position[] pos = new GM_Position[5];
    pos[0] = GeometryFactory.createGM_Position( footprint[0].x, footprint[0].z );
    pos[1] = GeometryFactory.createGM_Position( footprint[1].x, footprint[1].z );
    pos[2] = GeometryFactory.createGM_Position( footprint[3].x, footprint[3].z );
    pos[3] = GeometryFactory.createGM_Position( footprint[2].x, footprint[2].z );
    pos[4] = GeometryFactory.createGM_Position( footprint[0].x, footprint[0].z );
    surfaces.add( GeometryFactory.createGM_Surface( pos, null, null, cs ) );

    return (GM_Surface[])surfaces.toArray( new GM_Surface[surfaces.size()] );
  }

  /**
   * calculates footprint
   */
  private Point3d[] calcFootprint( double height, double b1, double b2 )
  {
    Point3d observerPosition = vp.getObserverPosition();

    Point3d[] fp = new Point3d[4];

    double h = height;

    // calc corners of the footprint back border
    double l = Math.sqrt( ( h * h ) + ( b1 * b1 ) );
    double bc = Math.tan( vp.getAoV() / 2 ) * l;
    bc *= 1.09;
    fp[0] = new Point3d( bc, 0, -b1 );
    fp[1] = new Point3d( -bc, 0, -b1 );

    // calc corners of the footprint front border
    l = Math.sqrt( ( h * h ) + ( b2 * b2 ) );
    double fc = Math.tan( vp.getAoV() / 2 ) * l;
    fc *= 1.09;
    fp[2] = new Point3d( fc, 0, -b2 );
    fp[3] = new Point3d( -fc, 0, -b2 );

    Transform3D rotTransform = new Transform3D();
    rotTransform.rotY( vp.getHDirection() );
    Transform3D translatTransform = new Transform3D();
    Vector3d vec = new Vector3d( observerPosition.x, observerPosition.y - height,
        observerPosition.z );
    translatTransform.setTranslation( vec );

    for( int i = 0; i < fp.length; i++ )
    {
      rotTransform.transform( fp[i] );
      translatTransform.transform( fp[i] );
    }

    return fp;
  }

  /**
   * 
   * 
   * @param height
   * @param dir
   * 
   * @return
   */
  private double calcDist( double height, double dir )
  {
    return height / Math.tan( dir );
  }

  /**
   * tiles a stripe into count boxes
   */
  GM_Surface[] makeBoxes( GM_Surface stripe, int count ) throws GM_Exception
  {
    GM_Surface[] surfaces = new GM_Surface[count];

    GM_Position[] pos = stripe.getSurfacePatchAt( 0 ).getExteriorRing();

    double m = ( pos[1].getY() - pos[0].getY() ) / ( pos[1].getX() - pos[0].getX() );
    double b1 = pos[1].getY() - ( m * pos[1].getX() );
    double b2 = pos[2].getY() - ( m * pos[2].getX() );

    double tmp1 = ( pos[1].getX() - pos[0].getX() ) / count;
    double tmp2 = ( pos[2].getX() - pos[3].getX() ) / count;

    ConvenienceCSFactory csF = ConvenienceCSFactory.getInstance();
    CoordinateSystem crs = csF.getCSByName( request.getSrs() );
    CS_CoordinateSystem cs = Adapters.getDefault().export( crs );

    GM_Position[] box = new GM_Position[5];

    for( int i = 0; i < count; i++ )
    {
      box = new GM_Position[5];
      double x1 = pos[0].getX() + ( tmp1 * i );
      double y1 = ( m * x1 ) + b1;
      box[0] = GeometryFactory.createGM_Position( x1, y1 );

      x1 = pos[0].getX() + ( tmp1 * ( i + 1 ) );
      y1 = ( m * x1 ) + b1;
      box[1] = GeometryFactory.createGM_Position( x1, y1 );

      x1 = pos[3].getX() + ( tmp2 * i );
      y1 = ( m * x1 ) + b2;
      box[3] = GeometryFactory.createGM_Position( x1, y1 );

      x1 = pos[3].getX() + ( tmp2 * ( i + 1 ) );
      y1 = ( m * x1 ) + b2;
      box[2] = GeometryFactory.createGM_Position( x1, y1 );
      box[4] = box[0];

      surfaces[i] = GeometryFactory.createGM_Surface( box, null, null, cs );
    }

    return surfaces;
  }
}