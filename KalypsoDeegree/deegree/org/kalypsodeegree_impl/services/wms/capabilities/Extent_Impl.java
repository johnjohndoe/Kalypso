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

import org.deegree.services.wms.capabilities.Extent;
import org.deegree.xml.Marshallable;

/**
 * The Extent element indicates what _values_ along a dimension are valid.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
class Extent_Impl implements Extent, Marshallable
{
  private String default_ = null;

  private String name = null;

  private boolean useNearestValue = false;

  /**
   * default constructor
   */
  Extent_Impl()
  {}

  /**
   * constructor initializing the class with the <Extent>
   */
  Extent_Impl( String name, String default_, boolean useNearestValue )
  {
    setName( name );
    setDefault( default_ );
    setUseNearestValue( useNearestValue );
  }

  /**
   * returns the name of the extent
   */
  public String getName()
  {
    return name;
  }

  /**
   * sets the name of the extent
   */
  public void setName( String name )
  {
    this.name = name;
  }

  /**
   * returns the default extent
   */
  public String getDefault()
  {
    return default_;
  }

  /**
   * sets the default extent
   */
  public void setDefault( String default_ )
  {
    this.default_ = default_;
  }

  /**
   * returns true if a WMS should use the extent that is nearest to the
   * requested level.
   */
  public boolean useNearestValue()
  {
    return useNearestValue;
  }

  /**
   * sets true if a WMS should use the extent that is nearest to the requested
   * level.
   */
  public void setUseNearestValue( boolean useNearestValue )
  {
    this.useNearestValue = useNearestValue;
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
    ret += ( "default_ = " + default_ + "\n" );
    ret += ( "useNearestValue = " + useNearestValue + "\n" );
    return ret;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<Extent" ).append( " name=\"" ).append( name ).append( "\"" );

    if( default_ != null )
    {
      sb.append( " default=\"" ).append( default_ ).append( "\"" );
    }

    if( useNearestValue )
    {
      sb.append( " nearestValue=\"1\"" );
    }

    sb.append( "/>" );

    return sb.toString();
  }
}