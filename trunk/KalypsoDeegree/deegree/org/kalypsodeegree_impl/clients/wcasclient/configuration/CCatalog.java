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
package org.deegree_impl.clients.wcasclient.configuration;

import java.net.URL;
import java.util.ArrayList;

/**
 * the class describes the contact of the catalog client to a catalog
 * 
 * @author administrator
 */
public class CCatalog
{
  private ArrayList types = null;

  private String name = null;

  private URL address = null;

  /** Creates a new instance of CCatalog */
  public CCatalog( String name, String[] types, URL address ) throws Exception
  {
    this.name = name;

    if( ( types == null ) || ( types.length == 0 ) )
    {
      throw new Exception( "At least one type must be supported by a catalog" );
    }

    this.types = new ArrayList();

    for( int i = 0; i < types.length; i++ )
    {
      this.types.add( types[i] );
    }

    this.address = address;
  }

  /**
   * returns the name of the catalog
   */
  public String getName()
  {
    return name;
  }

  /**
   * returns a list of metadata types (ISO19115, ISO19119 etc.) that are
   * supported by the catalog
   */
  public String[] getTypes()
  {
    return (String[])types.toArray( new String[types.size()] );
  }

  /**
   * returns true if the submitted metadata type is supported by the catalog
   */
  public boolean isTypeSupported( String type )
  {
    return types.contains( type );
  }

  /**
   *  
   */
  public URL getAddress()
  {
    return address;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = name + "\n";
    ret += ( types + "\n" );
    ret += address;
    return ret;
  }
}