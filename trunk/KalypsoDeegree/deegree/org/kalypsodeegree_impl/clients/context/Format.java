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
package org.deegree_impl.clients.context;

import org.deegree.xml.Marshallable;

/**
 * encapsulates the format description as described by the OGC Web Map Context
 * specification
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class Format implements Marshallable
{
  private String name = null;

  private boolean current = false;

  /**
   * Creates a new Format object.
   * 
   * @param name
   *          name of the format
   * @param current
   *          indicates if this is current format of this layer
   * 
   * @throws ContextException
   */
  public Format( String name, boolean current ) throws ContextException
  {
    setName( name );
    setCurrent( current );
  }

  /**
   * returns the name of the format
   * 
   * @return
   */
  public String getName()
  {
    return name;
  }

  /**
   * sets the the name of the format
   * 
   * @param name
   * 
   * @throws ContextException
   */
  public void setName( String name ) throws ContextException
  {
    if( name == null )
    {
      throw new ContextException( "name isn't allowed to be null" );
    }

    this.name = name;
  }

  /**
   * returns true if this is the current format of the layer
   * 
   * @return
   */
  public boolean isCurrent()
  {
    return current;
  }

  /**
   * sets if this is the current format of the layer or not
   * 
   * @param current
   */
  public void setCurrent( boolean current )
  {
    this.current = current;
  }

  /**
   * @see org.deegree.xml.Marshallable#exportAsXML()
   */
  public String exportAsXML()
  {

    StringBuffer sb = new StringBuffer( 200 );
    sb.append( "<Format current='" );
    if( current )
    {
      sb.append( 1 );
    }
    else
    {
      sb.append( 0 );
    }
    sb.append( "'>" ).append( name ).append( "</Format>" );

    return sb.toString();

  }

}