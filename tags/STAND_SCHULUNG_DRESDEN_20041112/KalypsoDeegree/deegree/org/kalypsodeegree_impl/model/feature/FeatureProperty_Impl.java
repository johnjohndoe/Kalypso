/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
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
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.model.feature;

import java.io.Serializable;

import org.deegree.model.feature.FeatureProperty;

/**
 * 
 * the interface describes a property entry of a feature. It is made of a name
 * and a value associated to it.
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */

class FeatureProperty_Impl implements FeatureProperty, Serializable
{

  private String name = "";

  private Object value = null;

  /**
   * default constructor
   */
  FeatureProperty_Impl()
  {}

  /**
   * constructor for complete initializing the FeatureProperty
   */
  FeatureProperty_Impl( String name, Object value )
  {
    setName( name );
    setValue( value );
  }

  /**
   * returns the name of the property
   */
  public String getName()
  {
    return name;
  }

  /**
   * sets the name of the property
   */
  public void setName( String name )
  {
    this.name = name;
  }

  /**
   * returns the value of the property
   */
  public Object getValue()
  {
    return value;
  }

  /**
   * sets the value of the property
   */
  public void setValue( Object value )
  {
    this.value = value;
  }

  public String toString()
  {
    String ret = null;
    ret = "name = " + name + "\n";
    ret += "value = " + value + "\n";
    return ret;
  }

}
/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.4  2004/10/11 14:44:28  doemming
 * *** empty log message ***
 * Revision 1.3 2004/10/07 14:09:16 doemming
 * *** empty log message ***
 * 
 * Revision 1.1 2004/09/02 23:57:07 doemming *** empty log message *** Revision
 * 1.3 2004/08/31 14:09:26 doemming *** empty log message *** Revision 1.2
 * 2004/02/09 07:59:57 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:00:38 poth no message
 * 
 * Revision 1.4 2002/08/15 10:00:00 ap no message
 * 
 * Revision 1.3 2002/05/21 16:05:51 ap no message
 * 
 * Revision 1.2 2002/04/05 09:41:40 ap no message
 * 
 * Revision 1.1 2002/04/04 16:22:41 ap no message
 * 
 * Revision 1.2 2001/10/15 14:48:19 ap no message
 * 
 * Revision 1.1 2001/10/05 15:19:43 ap no message
 *  
 */
