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
package org.deegree_impl.services.wms.capabilities;

import org.deegree.services.wms.capabilities.Dimension;
import org.deegree.xml.Marshallable;

/**
 * The Dimension element declares the _existence_ of a dimension. The optional
 * element <Dimension>is used in Capabilities XML to declare that one or more
 * dimensional parameters are relevant to the information holdings of that
 * server. The Dimension element does not provide valid values for a Dimension;
 * that is the role of the Extent element described below. A Dimension element
 * includes a required name, a required measurement units specifier, and an
 * optional unitSymbol.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
class Dimension_Impl implements Dimension, Marshallable
{
  private String name = null;

  private String unitSymbol = null;

  private String units = null;

  /**
   * default constructor
   */
  Dimension_Impl()
  {}

  /**
   * constructor initializing the class with the <Dimension>
   */
  Dimension_Impl( String name, String units, String unitSymbol )
  {
    setName( name );
    setUnits( units );
    setUnitSymbol( unitSymbol );
  }

  /**
   * returns the name of the dimension
   */
  public String getName()
  {
    return name;
  }

  /**
   * sets the name of the dimension
   */
  public void setName( String name )
  {
    this.name = name;
  }

  /**
   * returns the units the dimension is measured
   */
  public String getUnits()
  {
    return units;
  }

  /**
   * sets the units the dimension is measured
   */
  public void setUnits( String units )
  {
    this.units = units;
  }

  /**
   * returns the unit symbols
   */
  public String getUnitSymbol()
  {
    return unitSymbol;
  }

  /**
   * sets the unit symbols
   */
  public void setUnitSymbol( String unitSymbol )
  {
    this.unitSymbol = unitSymbol;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "name = " + name + "\n";
    ret += ( "units = " + units + "\n" );
    ret += ( "unitSymbol = " + unitSymbol + "\n" );
    return ret;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<Dimension" ).append( " name=\"" ).append( name ).append( "\"" ).append(
        " units=\"" ).append( units ).append( "\"" );

    if( unitSymbol != null )
    {
      sb.append( " unitSymbol=\"" ).append( unitSymbol ).append( "\"" );
    }

    sb.append( "/>" );

    return sb.toString();
  }
}