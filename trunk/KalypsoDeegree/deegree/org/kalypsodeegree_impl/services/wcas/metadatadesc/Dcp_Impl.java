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

import org.deegree.services.wcas.metadatadesc.ConnectPoint;
import org.deegree.services.wcas.metadatadesc.Dcp;
import org.deegree.services.wcas.metadatadesc.Parameter;

/**
 * Dcp_Impl.java
 * 
 * Created on 16. September 2002, 09:59
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */
public class Dcp_Impl implements Dcp
{

  ConnectPoint connectpoint = null;

  String invocationname = null;

  ArrayList parameter = null;

  String type = null;

  /** Creates a new instance of Dcp_Impl */
  public Dcp_Impl( ConnectPoint connectpoint, String invocationname, Parameter[] parameter,
      String type )
  {

    this.parameter = new ArrayList();
    setConnectPoint( connectpoint );
    setInvocationName( invocationname );
    setParameter( parameter );
    setType( type );
  }

  /**
   * @return
   */
  public ConnectPoint getConnectPoint()
  {
    return connectpoint;
  }

  /**
   * @see #getConnectPoint()
   */
  public void setConnectPoint( ConnectPoint connectpoint )
  {
    this.connectpoint = connectpoint;
  }

  /**
   * @return
   */
  public String getInvocationName()
  {
    return invocationname;
  }

  /**
   * @see #getInvocationName()
   */
  public void setInvocationName( String invocationname )
  {
    this.invocationname = invocationname;
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public Parameter[] getParameter()
  {
    return (Parameter[])parameter.toArray( new Parameter[parameter.size()] );
  }

  /**
   * @see #getParameter()
   */
  public void addParameter( Parameter parameter )
  {
    this.parameter.add( parameter );
  }

  /**
   * @see #getParameter()
   */
  public void setParameter( Parameter[] parameter )
  {
    this.parameter.clear();
    for( int i = 0; i < parameter.length; i++ )
    {
      this.parameter.add( parameter[i] );
    }
  }

  /**
   * returns the type attribute. use="required" Possible values are:
   * <ul>
   * <li>HTTPGet
   * <li>HTTPPost
   * </ul>
   * 
   * @return attribute "type"
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
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "connectpoint = " + connectpoint + "\n";
    ret += "invocationname = " + invocationname + "\n";
    ret += "parameter = " + parameter + "\n";
    ret += "type = " + type + "\n";
    return ret;
  }

}