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

import org.deegree.services.wcas.metadatadesc.DependsOn;
import org.deegree.services.wcas.metadatadesc.OperationName;

/**
 * DependsOn_Impl.java
 * 
 * Created on 16. September 2002, 09:59
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */

public class DependsOn_Impl implements DependsOn
{

  private ArrayList operationname = null;

  /**
   * constructor
   */
  public DependsOn_Impl( OperationName[] operationname )
  {
    this.operationname = new ArrayList();

    setOperationName( operationname );
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public OperationName[] getOperationName()
  {
    return (OperationName[])operationname.toArray( new OperationName[operationname.size()] );
  }

  /**
   * @see getOperationName
   */
  public void addOperationName( OperationName operationname )
  {
    this.operationname.add( operationname );
  }

  /**
   * @see getOperationName
   */
  public void setOperationName( OperationName[] operationname )
  {
    this.operationname.clear();
    for( int i = 0; i < operationname.length; i++ )
    {
      this.operationname.add( operationname[i] );
    }
  }

  /**
   * to String
   */
  public String toString()
  {
    String ret = null;
    ret = "operationname = " + operationname + "\n";
    return ret;
  }
}