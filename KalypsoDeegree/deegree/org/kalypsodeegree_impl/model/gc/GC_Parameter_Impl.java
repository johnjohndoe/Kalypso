/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree
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
 E-Mail: fitzke@giub.uni-bonn.de


 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.gc;

import java.io.Serializable;

import org.opengis.gc.GC_Parameter;
import org.opengis.gc.GC_ParameterInfo;

/**
 * The parameter required for a grid coverage processing operation. This
 * structure contains the parameter name (as defined from the
 * {@link GC_ParameterInfo}structure) and it s value.
 * 
 * @version 1.00
 * @since 1.00
 */
class GC_Parameter_Impl implements GC_Parameter, Serializable
{
  /**
   * Parameter name.
   */
  public String getName()
  {
    return null;
  }

  /**
   * The value for parameter. The type {@link Object}can be any type including
   * a {@link Number}, a {@link String}or an instance of an interface. For
   * example, a grid processor operation will typically require a parameter for
   * the input grid coverage. This parameter may have <code>"Source"</code> as
   * the parameter name and the instance of the grid coverage as the value.
   */
  public Object getValue()
  {
    return null;
  }
}