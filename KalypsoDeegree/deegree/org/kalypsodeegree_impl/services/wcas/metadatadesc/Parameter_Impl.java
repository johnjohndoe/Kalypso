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

import org.deegree.services.wcas.metadatadesc.Parameter;
import org.deegree.services.wcas.metadatadesc.ParameterName;
import org.deegree.services.wcas.metadatadesc.ParameterType;
import org.deegree.services.wcas.metadatadesc.PermittedValues;

/**
 * Parameter_Impl.java
 * 
 * Created on 16. September 2002, 10:28
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */
public class Parameter_Impl implements Parameter
{

  private String direction = null;

  private String optional = null;

  private String parameterdescription = null;

  private ParameterName parametername = null;

  private ParameterType parametertype = null;

  private PermittedValues permittedvalues = null;

  private String repeatable = null;

  /** Creates a new instance of Parameter_Impl */
  public Parameter_Impl( String direction, String optional, String parameterdescription,
      ParameterName parametername, ParameterType parametertype, PermittedValues permittedvalues,
      String repeatable )
  {

    setDirection( direction );
    setOptional( optional );
    setParameterDescription( parameterdescription );
    setParameterName( parametername );
    setParameterType( parametertype );
    setPermittedValues( permittedvalues );
    setRepeatable( repeatable );
  }

  /**
   * use="required" Possible values are:
   * <ul>
   * <li>in
   * <li>out
   * <li>inout
   * </ul>
   * 
   * @return attribute direction
   *  
   */
  public String getDirection()
  {
    return direction;
  }

  /**
   * @see getDirection
   */
  public void setDirection( String direction )
  {
    this.direction = direction;
  }

  /**
   * returns the optional-attribute. use="required" Possible values are:
   * <ul>
   * <li>yes
   * <li>no
   * </ul>
   * 
   * @return attribute optional
   *  
   */
  public String getOptional()
  {
    return optional;
  }

  /**
   * @see getOptional
   */
  public void setOptional( String optional )
  {
    this.optional = optional;
  }

  /**
   * minOccurs="0"
   * 
   * @return
   */
  public String getParameterDescription()
  {
    return parameterdescription;
  }

  /**
   * @see getParameterDescription
   */
  public void setParameterDescription( String parameterdescription )
  {
    this.parameterdescription = parameterdescription;
  }

  /**
   * @return class Name
   *  
   */
  public ParameterName getParameterName()
  {
    return parametername;
  }

  /**
   * @see getParameterName
   */
  public void setParameterName( ParameterName parametername )
  {
    this.parametername = parametername;
  }

  /**
   * @return class Type
   *  
   */
  public ParameterType getParameterType()
  {
    return parametertype;
  }

  /**
   * @see getParameterType
   */
  public void setParameterType( ParameterType parametertype )
  {
    this.parametertype = parametertype;
  }

  /**
   * @return class PermittedValues
   *  
   */
  public PermittedValues getPermittedValues()
  {
    return permittedvalues;
  }

  /**
   * @see getPermittedValues
   */
  public void setPermittedValues( PermittedValues permittedvalues )
  {
    this.permittedvalues = permittedvalues;
  }

  /**
   * use="required" Possible values are:
   * <ul>
   * <li>true
   * <li>false
   * </ul>
   * 
   * @return attribute repeatable
   *  
   */
  public String getRepeatable()
  {
    return repeatable;
  }

  /**
   * @see getRepeatable
   */
  public void setRepeatable( String repeatable )
  {
    this.repeatable = repeatable;
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "direction = " + direction + "\n";
    ret += "optional = " + optional + "\n";
    ret += "parameterdescription = " + parameterdescription + "\n";
    ret += "parametername = " + parametername + "\n";
    ret += "parametertype = " + parametertype + "\n";
    ret += "permittedvalues = " + permittedvalues + "\n";
    ret += "repeatable = " + repeatable + "\n";
    return ret;
  }

}