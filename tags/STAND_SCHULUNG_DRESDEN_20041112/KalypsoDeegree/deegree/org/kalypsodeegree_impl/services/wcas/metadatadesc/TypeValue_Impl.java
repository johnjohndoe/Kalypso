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

import org.deegree.services.wcas.metadatadesc.DataType;
import org.deegree.services.wcas.metadatadesc.EnumValues;
import org.deegree.services.wcas.metadatadesc.InstanceValue;
import org.deegree.services.wcas.metadatadesc.Range;
import org.deegree.services.wcas.metadatadesc.TypeValue;

/**
 * TypeValue_Impl.java
 * 
 * Created on 16. September 2002, 10:37
 */
public class TypeValue_Impl implements TypeValue
{

  private DataType datatype = null;

  private EnumValues enumvalues = null;

  private InstanceValue instancevalue = null;

  private Range range = null;

  /** Creates a new instance of TypeValue_Impl */
  public TypeValue_Impl( DataType datatype, EnumValues enumvalues, InstanceValue instancevalue,
      Range range )
  {

    setDataType( datatype );
    setEnumValues( enumvalues );
    setInstanceValue( instancevalue );
    setRange( range );
  }

  /**
   * @return
   */
  public DataType getDataType()
  {
    return datatype;
  }

  /**
   * @see #getDataType()
   */
  public void setDataType( DataType datatype )
  {
    this.datatype = datatype;
  }

  /**
   * @return
   */
  public EnumValues getEnumValues()
  {
    return enumvalues;
  }

  /**
   * @see #getEnumValues()
   */
  public void setEnumValues( EnumValues enumvalues )
  {
    this.enumvalues = enumvalues;
  }

  /**
   * @return
   */
  public InstanceValue getInstanceValue()
  {
    return instancevalue;
  }

  /**
   * @see #getInstanceValue()
   */
  public void setInstanceValue( InstanceValue instancevalue )
  {
    this.instancevalue = instancevalue;
  }

  /**
   * @return
   */
  public Range getRange()
  {
    return range;
  }

  /**
   * @see #getRange()
   */
  public void setRange( Range range )
  {
    this.range = range;
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "datatype = " + datatype + "\n";
    ret += "enumvalues = " + enumvalues + "\n";
    ret += "instancevalue = " + instancevalue + "\n";
    ret += "range = " + range + "\n";
    return ret;
  }

}