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

import org.deegree.services.wcas.metadatadesc.ClassificationCode;

/**
 * ClassificationCode_Impl.java
 * 
 * Created on 16. September 2002, 09:56
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$ *
 */

public class ClassificationCode_Impl implements ClassificationCode
{

  private String classification = null;

  /** Creates a new instance of ClassificationCode_Impl */
  public ClassificationCode_Impl( String classification )
  {
    setClassification( classification );
  }

  /**
   * returns the attribute "Classification". use="required". Possible values
   * are:
   * <ul>
   * <li>unclassified
   * <li>codeWord
   * <li>confidental
   * <li>secret
   * <li>restricted
   * <li>topsecret
   * </ul>
   * 
   * @return Classification-attribute
   *  
   */
  public String getClassification()
  {
    return classification;
  }

  /**
   * @see getClassification
   */
  public void setClassification( String classification )
  {
    this.classification = classification;
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "classification = " + classification + "\n";
    return ret;
  }
}