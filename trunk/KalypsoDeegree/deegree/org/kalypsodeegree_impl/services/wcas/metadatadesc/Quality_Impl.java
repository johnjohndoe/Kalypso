/*
 * ---------------- FILE HEADER ------------------------------------------
 * 
 * This file is part of deegree. Copyright (C) 2001 by: EXSE, Department of
 * Geography, University of Bonn http://www.giub.uni-bonn.de/exse/ lat/lon
 * Fitzke/Fretter/Poth GbR http://www.lat-lon.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * Andreas Poth lat/lon Fitzke/Fretter/Poth GbR Meckenheimer Allee 176 53115
 * Bonn Germany E-Mail: poth@lat-lon.de
 * 
 * Jens Fitzke Department of Geography University of Bonn Meckenheimer Allee 166
 * 53115 Bonn Germany E-Mail: jens.fitzke@uni-bonn.de
 * 
 * 
 * ---------------------------------------------------------------------------
 */

package org.deegree_impl.services.wcas.metadatadesc;

import java.util.ArrayList;

import org.deegree.services.wcas.metadatadesc.Quality;

/**
 * Quality_Impl.java
 * 
 * Created on 16. September 2002, 10:33
 */
public class Quality_Impl implements Quality
{

  ArrayList tbd_serviceQuality = null;

  /** Creates a new instance of Quality_Impl */
  public Quality_Impl( String[] tbd_serviceQuality )
  {
    this.tbd_serviceQuality = new ArrayList();
    setTBD_ServiceQuality( tbd_serviceQuality );
  }

  /**
   * gets the TBD_ServiceQuality
   * 
   * @return String
   *  
   */
  public String[] getTBD_ServiceQuality()
  {
    return (String[])tbd_serviceQuality.toArray( new String[tbd_serviceQuality.size()] );
  }

  /**
   * @see getTBD_ServiceQuality
   */
  public void addTBD_ServiceQuality( String tbd_serviceQuality )
  {
    this.tbd_serviceQuality.add( tbd_serviceQuality );
  }

  /**
   * @see getTBD_ServiceQuality
   */
  public void setTBD_ServiceQuality( String[] tbd_serviceQuality )
  {
    this.tbd_serviceQuality.clear();
    for( int i = 0; i < tbd_serviceQuality.length; i++ )
    {
      this.tbd_serviceQuality.add( tbd_serviceQuality[i] );
    }
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "tbd_serviceQuality = " + tbd_serviceQuality + "\n";
    return ret;
  }

}