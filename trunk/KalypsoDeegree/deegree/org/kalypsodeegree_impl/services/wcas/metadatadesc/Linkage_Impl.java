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

import java.net.URL;

import org.deegree.services.wcas.metadatadesc.Linkage;

/**
 * Linkage_Impl.java
 * 
 * Created on 16. September 2002, 10:20
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */
public class Linkage_Impl implements Linkage
{

  private URL href = null;

  private String type = null;

  private URL xlink = null;

  /** Creates a default instance of Linkage_Impl */
  public Linkage_Impl() throws java.net.MalformedURLException
  {
    this.type = "simple";
    this.xlink = new URL( "http://www.w3.org/1999/xlink/" );
  }

  /** Creates a new instance of Linkage_Impl */
  public Linkage_Impl( URL href, String type, URL xlink ) throws java.net.MalformedURLException
  {
    this();
    setHref( href );
    setType( type );
    setXlink( xlink );
  }

  /**
   * use="required"
   * 
   * @return the href-attribute
   *  
   */
  public URL getHref()
  {
    return href;
  }

  /**
   * @see #getHref()
   */
  public void setHref( URL href )
  {
    this.href = href;
  }

  /**
   * fixed="simple"
   * 
   * @return the type-attribute
   *  
   */
  public String getType()
  {
    return type;
  }

  /**
   * @see #getType()
   */
  public void setType( String type )
  {
    this.type = type;
  }

  /**
   * fixed="http://www.w3.org/1999/xlink/"
   * 
   * @return the xlink-attribute
   *  
   */
  public URL getXlink()
  {
    return xlink;
  }

  /**
   * @see #getXlink()
   */
  public void setXlink( URL xlink )
  {
    this.xlink = xlink;
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "href = " + href + "\n";
    ret += "type = " + type + "\n";
    ret += "xlink = " + xlink + "\n";
    return ret;
  }

}