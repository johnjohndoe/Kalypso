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

import java.util.HashMap;
import java.util.Iterator;

import org.deegree.xml.Marshallable;

/**
 * encapsulates a StyleList as defined by the OGC Web Map Context specification
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class StyleList implements Marshallable
{
  private HashMap styles = new HashMap();

  private Style current = null;

  /**
   * Creates a new StyleList object.
   * 
   * @param styles
   */
  public StyleList( Style[] styles ) throws ContextException
  {
    setStyles( styles );
  }

  /**
   * returns an array of all styles known by a layer
   * 
   * @return
   */
  public Style[] getStyles()
  {
    Style[] fr = new Style[styles.size()];
    return (Style[])styles.values().toArray( fr );
  }

  /**
   * sets alla styles known by a layer
   * 
   * @param styles
   * 
   * @throws ContextException
   */
  public void setStyles( Style[] styles ) throws ContextException
  {
    if( ( styles == null ) || ( styles.length == 0 ) )
    {
      throw new ContextException( "at least one style must be defined for a layer" );
    }

    this.styles.clear();

    for( int i = 0; i < styles.length; i++ )
    {
      if( styles[i].isCurrent() )
      {
        current = styles[i];
      }
      this.styles.put( styles[i].getName(), styles[i] );
    }
  }

  /**
   * returns the currentlay
   * 
   * @return
   */
  public Style getCurrentStyle()
  {
    return current;
  }

  /**
   * 
   * 
   * @param name
   * 
   * @return
   */
  public Style getStyle( String name )
  {
    return (Style)styles.get( name );
  }

  /**
   * 
   * 
   * @param style
   */
  public void addStyle( Style style )
  {
    if( style.isCurrent() )
    {
      current.setCurrent( false );
      current = style;
    }
    styles.put( style.getName(), style );
  }

  /**
   * 
   * 
   * @param name
   * 
   * @return
   */
  public Style removeStyle( String name )
  {
    return (Style)styles.remove( name );
  }

  /**
   *  
   */
  public void clear()
  {
    styles.clear();
  }

  public String exportAsXML()
  {

    StringBuffer sb = new StringBuffer( 10000 );
    sb.append( "<StyleList>" );
    Iterator iterator = styles.values().iterator();
    while( iterator.hasNext() )
    {
      sb.append( ( (Marshallable)iterator.next() ).exportAsXML() );
    }
    sb.append( "</StyleList>" );
    return sb.toString();
  }

}