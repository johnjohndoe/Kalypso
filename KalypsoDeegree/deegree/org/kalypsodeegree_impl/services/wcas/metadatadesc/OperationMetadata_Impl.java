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

import org.deegree.services.wcas.metadatadesc.Dcp;
import org.deegree.services.wcas.metadatadesc.DependsOn;
import org.deegree.services.wcas.metadatadesc.OperationMetadata;
import org.deegree.services.wcas.metadatadesc.OperationName;
import org.deegree.services.wcas.metadatadesc.Parameter;

/**
 * OperationMetadata_Impl.java
 * 
 * Created on 16. September 2002, 10:27
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */
public class OperationMetadata_Impl implements OperationMetadata
{

  private ArrayList dcp = null;

  private DependsOn dependson = null;

  private String operationdescription = null;

  private OperationName operationname = null;

  private ArrayList parameter = null;

  /** Creates a new instance of OperationMetadata_Impl */
  public OperationMetadata_Impl( Dcp[] dcp, DependsOn dependson, String operationdescription,
      OperationName operationname, Parameter[] parameter )
  {
    this.dcp = new ArrayList();
    this.parameter = new ArrayList();

    setDcp( dcp );
    setDependsOn( dependson );
    setOperationDescription( operationdescription );
    setOperationName( operationname );
    setParameter( parameter );
  }

  /**
   * maxOccurs="unbounded"
   *  
   */
  public Dcp[] getDCP()
  {
    return (Dcp[])dcp.toArray( new Dcp[dcp.size()] );
  }

  /**
   * @see #getDCP()
   */
  public void addDcp( Dcp dcp )
  {
    this.dcp.add( dcp );
  }

  /**
   * @see #getDCP()
   */
  public void setDcp( Dcp[] dcp )
  {
    this.dcp.clear();
    for( int i = 0; i < dcp.length; i++ )
    {
      this.dcp.add( dcp[i] );
    }
  }

  /**
   * minOccurs="0
   *  
   */
  public DependsOn getDependsOn()
  {
    return dependson;
  }

  /**
   * @see #getDependsOn()
   */
  public void setDependsOn( DependsOn dependson )
  {
    this.dependson = dependson;
  }

  /**
   *  
   */
  public String getOperationDescription()
  {
    return operationdescription;
  }

  /**
   * @see #getOperationDescription()
   */
  public void setOperationDescription( String operationdescription )
  {
    this.operationdescription = operationdescription;
  }

  /**
   *  
   */
  public OperationName getOperationName()
  {
    return operationname;
  }

  /**
   * @see #getOperationName()
   */
  public void setOperationName( OperationName operationname )
  {
    this.operationname = operationname;
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   *  
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
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "dcp = " + dcp + "\n";
    ret += "dependson = " + dependson + "\n";
    ret += "operationdescription = " + operationdescription + "\n";
    ret += "operationname = " + operationname + "\n";
    ret += "parameter = " + parameter + "\n";
    return ret;
  }
}