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
package org.deegree_impl.services.wcs.capabilities;

import org.deegree.model.coverage.CoverageLayer;
import org.deegree.model.coverage.ExtentType;
import org.deegree.model.coverage.Grid;
import org.deegree.model.coverage.GridAxis;
import org.deegree.model.coverage.GridCoverageLayer;
import org.deegree.model.coverage.GridExtentDescription;
import org.deegree.model.coverage.GridRangeDescription;
import org.deegree.model.coverage.RangeSetDescription;
import org.deegree.model.coverage.SpatialExtent;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.capabilities.Service;
import org.deegree.services.wcs.capabilities.WCSCapabilities;
import org.deegree_impl.services.capabilities.OGCWebServiceCapabilities_Impl;
import org.deegree_impl.tools.Debug;

/**
 * A the top level of the ContentMetadata section of the Capabilities XML
 * document for WCS is a CoverageLayerList element, which lists coverage layers
 * in any order.
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
class WCSCapabilities_Impl extends OGCWebServiceCapabilities_Impl implements WCSCapabilities
{

  private CoverageLayer[] coverageLayerList = null;

  WCSCapabilities_Impl( String version, String updateSequence, Service service,
      CoverageLayer[] coverageLayerList )
  {
    super( version, updateSequence, service );
    this.coverageLayerList = coverageLayerList;
  }

  /**
   * returns the list off <tt>CoverageLayer</tt> that are available through
   * the WCS.
   *  
   */
  public CoverageLayer[] getCoverageLayerList()
  {
    return coverageLayerList;
  }

  /**
   * returns an XML representation of the capabilities
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin( this, "toXML" );

    StringBuffer sb = new StringBuffer( "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" );
    try
    {
      sb.append( "<wcs:WCSCapabilities xmlns:wcs=\"http://www.opengis.net/wcs\">" );

      // service section
      sb.append( "<Service><Name>" + getService().getName() + "</Name>" );
      if( getService().getTitle() != null )
      {
        sb.append( "<Title>" + getService().getTitle() + "</Title>" );
      }
      if( getService().getAbstract() != null )
      {
        sb.append( "<Abstract>" + getService().getAbstract() + "</Abstract>" );
      }
      if( getService().getOnlineResource() != null )
      {
        String s = getService().getOnlineResource().getProtocol() + "://";
        s += getService().getOnlineResource().getHost();
        s += getService().getOnlineResource().getPath();
        sb.append( "<OnlineResource>" + s + "</OnlineResource>" );
      }
      if( getService().getFees() != null )
      {
        sb.append( "<Fees>" + getService().getFees() + "</Fees>" );
      }
      if( getService().getAccessConstraints() != null )
      {
        sb.append( "<AccessConstraints>" + getService().getAccessConstraints()
            + "</AccessConstraints>" );
      }
      sb.append( "</Service>" );

      // coverage layer list
      sb.append( "<wcs:CoverageLayerList>" );

      CoverageLayer[] cl = getCoverageLayerList();
      for( int i = 0; i < cl.length; i++ )
      {
        if( cl[i] instanceof GridCoverageLayer )
        {
          sb.append( "<wcs:GridCoverageLayer descriptorResource=" );
          sb.append( "\"" + cl[i].getDescriptorResource() + "\">" );
          sb.append( "<wcs:LayerID>" + cl[i].getLayerID() + "</wcs:LayerID>" );
          if( cl[i].getTitle() != null )
          {
            sb.append( "<wcs:Title>" + cl[i].getTitle() + "</wcs:Title>" );
          }
          GM_Envelope env = cl[i].getLatLonBoundingBox();
          sb.append( "<wcs:LatLonBoundingBox minx=\"" + env.getMin().getX() + "\" " );
          sb.append( "miny=\"" + env.getMin().getY() + "\" " );
          sb.append( "maxx=\"" + env.getMax().getX() + "\" " );
          sb.append( "maxy=\"" + env.getMax().getY() + "\"/>" );

          String[] crs = cl[i].getCRS();
          for( int j = 0; j < crs.length; j++ )
          {
            sb.append( "<wcs:SRS>" + crs[j] + "</wcs:SRS>" );
          }

          // grid extent section
          sb.append( "<wcs:GridExtentDescription>" );
          GridExtentDescription ged = (GridExtentDescription)cl[i].getDomainSetExtentDescription();
          SpatialExtent se = ged.getSpatialExtent();
          sb.append( "<wcs:SpatialExtent srsName=\"" + se.getCRS() + "\">" );
          ExtentType xex = se.getXExtent();
          sb.append( "<wcs:XExtent>" );
          sb.append( "<wcs:min>" + xex.getMin() + "</wcs:min>" );
          sb.append( "<wcs:min>" + xex.getMax() + "</wcs:min>" );
          sb.append( "</wcs:XExtent>" );
          xex = se.getYExtent();
          sb.append( "<wcs:YExtent>" );
          sb.append( "<wcs:min>" + xex.getMin() + "</wcs:min>" );
          sb.append( "<wcs:min>" + xex.getMax() + "</wcs:min>" );
          sb.append( "</wcs:YExtent>" );
          xex = se.getZExtent();
          if( xex != null )
          {
            sb.append( "<wcs:ZExtent>" );
            sb.append( "<wcs:min>" + xex.getMin() + "</wcs:min>" );
            sb.append( "<wcs:min>" + xex.getMax() + "</wcs:min>" );
            sb.append( "</wcs:ZExtent>" );
          }
          sb.append( "</wcs:SpatialExtent>" );

          // grid axis section
          sb.append( "<wcs:GridAxisDescription>" );
          GridAxis[] ga = ged.getGridAxisDescription().getGridAxis();
          for( int j = 0; j < ga.length; j++ )
          {
            sb.append( "<wcs:GridAxis>" );
            sb.append( "<wcs:Name>" + ga[j].getName() + "</wcs:Name>" );
            if( ga[j].getDescription() != null )
            {
              sb.append( "<wcs:description>" + ga[j].getDescription() + "</wcs:description>" );
            }
            int o = ga[j].getOrientation();
            String s = null;
            switch( o )
            {
            case GridAxis.BACK:
              s = "back";
            case GridAxis.DOWN:
              s = "down";
            case GridAxis.FRONT:
              s = "front";
            case GridAxis.LEFT:
              s = "left";
            case GridAxis.RIGHT:
              s = "right";
            case GridAxis.UP:
              s = "up";
            }
            sb.append( "<wcs:orientation>" + s + "</wcs:orientation>" );
            sb.append( "</wcs:GridAxis>" );
          }
          sb.append( "</wcs:GridAxisDescription>" );

          // grid section
          Grid grid = ged.getGrid();
          sb.append( "<wcs:Grid" );
          if( grid.getDimension() > 0 )
          {
            sb.append( " dimension=\"" + grid.getDimension() + "\"" );
          }
          sb.append( " type=\"" + grid.getType() + "\">" );
          sb.append( "<wcs:GridRange>" );
          sb.append( "<wcs:low>" );
          double[] d = grid.getGridRange().getLow();
          for( int j = 0; j < d.length; j++ )
          {
            sb.append( "<wcs:ordinate>" + d[j] + "</wcs:ordinate>" );
          }
          sb.append( "</wcs:low>" );
          sb.append( "<wcs:high>" );
          d = grid.getGridRange().getHigh();
          for( int j = 0; j < d.length; j++ )
          {
            sb.append( "<wcs:ordinate>" + d[j] + "</wcs:ordinate>" );
          }
          sb.append( "</wcs:high>" );
          sb.append( "</wcs:GridRange>" );
          sb.append( "</wcs:Grid>" );
          sb.append( "</wcs:GridExtentDescription>" );

          sb.append( "<wcs:RangeSetDescription>" );
          RangeSetDescription rsd = ( (GridCoverageLayer)cl[i] ).getRangeSetDescription();
          GridRangeDescription[] grd = rsd.getGridRangeDescription();

          for( int j = 0; j < grd.length; j++ )
          {
            sb.append( "<wcs:GridRangeDescription>" );
            sb.append( "<wcs:RangeID>" + grd[j].getID() + "</wcs:RangeID>" );
            if( grd[j].getTitle() != null )
            {
              sb.append( "<wcs:Title>" + grd[j].getTitle() + "</wcs:Title>" );
            }
            sb.append( "</wcs:GridRangeDescription>" );
          }
          sb.append( "</wcs:RangeSetDescription>" );

          sb.append( "</wcs:GridCoverageLayer>" );
        }
      }
      sb.append( "</wcs:CoverageLayerList>" );
      sb.append( "</wcs:WCSCapabilities>" );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }

    Debug.debugMethodEnd();
    return sb.toString();
  }

}