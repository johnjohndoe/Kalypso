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

import org.deegree.services.wcas.metadatadesc.MaximumValue;
import org.deegree.services.wcas.metadatadesc.MinimumValue;
import org.deegree.services.wcas.metadatadesc.Range;

/**
 * Range_Impl.java
 * 
 * Created on 16. September 2002, 10:33
 */
public class Range_Impl implements Range
{

  MaximumValue maximumvalue = null;

  MinimumValue minimumvalue = null;

  /** Creates a new instance of Range_Impl */
  public Range_Impl( MaximumValue maximumvalue, MinimumValue minimumvalue )
  {
    setMaximumValue( maximumvalue );
    setMinimumValue( minimumvalue );
  }

  /**
   * @return
   */
  public MaximumValue getMaximumValue()
  {
    return maximumvalue;
  }

  /**
   * @see #getMaximumValue()
   */
  public void setMaximumValue( MaximumValue maximumvalue )
  {
    this.maximumvalue = maximumvalue;
  }

  /**
   * @return
   */
  public MinimumValue getMinimumValue()
  {
    return minimumvalue;
  }

  /**
   * @see #getMinimumValue()
   */
  public void setMinimumValue( MinimumValue minimumvalue )
  {
    this.minimumvalue = minimumvalue;
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "maximumvalue = " + maximumvalue + "\n";
    ret += "minimumvalue = " + minimumvalue + "\n";
    return ret;
  }

}