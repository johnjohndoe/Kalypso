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

import org.deegree.services.wcas.metadatadesc.DataType;
import org.deegree.services.wcas.metadatadesc.EnumValues;
import org.deegree.services.wcas.metadatadesc.InstanceValue;
import org.deegree.services.wcas.metadatadesc.OnLineResource;
import org.deegree.services.wcas.metadatadesc.PermittedValues;
import org.deegree.services.wcas.metadatadesc.Range;

/**
 * PermittedValues_Impl.java
 * 
 * Created on 16. September 2002, 10:30
 */
public class PermittedValues_Impl implements PermittedValues
{

  private ArrayList datatype = null;

  private ArrayList enumvalues = null;

  private ArrayList instancevalue = null;

  private OnLineResource onlineresource = null;

  private ArrayList range = null;

  /** Creates a new instance of PermittedValues_Impl */
  public PermittedValues_Impl( DataType[] datatype, EnumValues[] enumvalues,
      InstanceValue[] instancevalue, OnLineResource onlineresource, Range[] range )
  {

    this.datatype = new ArrayList();
    this.enumvalues = new ArrayList();
    this.instancevalue = new ArrayList();
    this.range = new ArrayList();

    setDataType( datatype );
    setEnumValues( enumvalues );
    setInstanceValues( instancevalue );
    setOnLineResource( onlineresource );
    setRange( range );
  }

  /**
   * Choice: minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public DataType[] getDataType()
  {
    return (DataType[])datatype.toArray( new DataType[datatype.size()] );
  }

  /**
   * @see getDataType
   */
  public void addDataType( DataType datatype )
  {
    this.datatype.add( datatype );
  }

  /**
   * @see getDataType
   */
  public void setDataType( DataType[] datatype )
  {
    this.datatype.clear();
    for( int i = 0; i < datatype.length; i++ )
    {
      this.datatype.add( datatype[i] );
    }
  }

  /**
   * Choice: minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public EnumValues[] getEnumValues()
  {
    return (EnumValues[])enumvalues.toArray( new EnumValues[enumvalues.size()] );
  }

  /**
   * @see getEnumValues
   */
  public void addEnumValue( EnumValues enumvalue )
  {
    this.enumvalues.add( enumvalue );
  }

  /**
   * @see getEnumValues
   */
  public void setEnumValues( EnumValues[] enumvalues )
  {
    this.enumvalues.clear();
    for( int i = 0; i < enumvalues.length; i++ )
    {
      this.enumvalues.add( enumvalues[i] );
    }
  }

  /**
   * Choice: minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public InstanceValue[] getInstanceValue()
  {
    return (InstanceValue[])instancevalue.toArray( new InstanceValue[instancevalue.size()] );
  }

  /**
   * @see getInstanceValue
   */
  public void addInstanceValues( InstanceValue instancevalue )
  {
    this.instancevalue.add( instancevalue );
  }

  /**
   * @see getInstanceValue
   */
  public void setInstanceValues( InstanceValue[] instancevalue )
  {
    this.instancevalue.clear();
    for( int i = 0; i < instancevalue.length; i++ )
    {
      this.instancevalue.add( instancevalue[i] );
    }
  }

  /**
   * minOccurs="0"
   * 
   * @return
   */
  public OnLineResource getOnLineResource()
  {
    return onlineresource;
  }

  /**
   * @see getOnLineResource
   */
  public void setOnLineResource( OnLineResource onlineresource )
  {
    this.onlineresource = onlineresource;
  }

  /**
   * Choice: minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public Range[] getRange()
  {
    return (Range[])range.toArray( new Range[range.size()] );
  }

  /**
   * @see getRange
   */
  public void addRange( Range range )
  {
    this.range.add( range );
  }

  /**
   * @see getRange
   */
  public void setRange( Range[] range )
  {
    this.range.clear();
    for( int i = 0; i < range.length; i++ )
    {
      this.range.add( range[i] );
    }
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
    ret += "onlineresource = " + onlineresource + "\n";
    ret += "range = " + range + "\n";
    return ret;
  }

}