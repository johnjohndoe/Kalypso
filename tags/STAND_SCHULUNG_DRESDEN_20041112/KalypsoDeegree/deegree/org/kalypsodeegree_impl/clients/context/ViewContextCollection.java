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
 * This class encapsulates a collection of references to Web Map Context
 * documents as defined by the OGC Web Map Context specification
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class ViewContextCollection implements Marshallable
{
  private HashMap viewContextReferences = null;

  /**
   * Creates a new ViewContextCollection object.
   * 
   * @param viewContextReferences
   * 
   * @throws ContextException
   */
  public ViewContextCollection( ViewContextReference[] viewContextReferences )
      throws ContextException
  {
    setViewContextReferences( viewContextReferences );
  }

  /**
   * 
   * 
   * @return
   */
  public ViewContextReference[] getViewContextReferences()
  {
    ViewContextReference[] fr = new ViewContextReference[viewContextReferences.size()];
    return (ViewContextReference[])viewContextReferences.values().toArray( fr );
  }

  /**
   * 
   * 
   * @param viewContextReferences
   * 
   * @throws ContextException
   */
  public void setViewContextReferences( ViewContextReference[] viewContextReferences )
      throws ContextException
  {
    if( ( viewContextReferences == null ) || ( viewContextReferences.length == 0 ) )
    {
      throw new ContextException( "at least one viewContextReference must be defined for a layer" );
    }

    this.viewContextReferences.clear();

    for( int i = 0; i < viewContextReferences.length; i++ )
    {
      this.viewContextReferences
          .put( viewContextReferences[i].getTitle(), viewContextReferences[i] );
    }
  }

  /**
   * 
   * 
   * @param name
   * 
   * @return
   */
  public ViewContextReference getViewContextReference( String name )
  {
    return (ViewContextReference)viewContextReferences.get( name );
  }

  /**
   * 
   * 
   * @param viewContextReference
   */
  public void addViewContextReference( ViewContextReference viewContextReference )
  {
    viewContextReferences.put( viewContextReference.getTitle(), viewContextReference );
  }

  /**
   * 
   * 
   * @param name
   * 
   * @return
   */
  public ViewContextReference removeViewContextReference( String name )
  {
    return (ViewContextReference)viewContextReferences.remove( name );
  }

  /**
   *  
   */
  public void clear()
  {
    viewContextReferences.clear();
  }

  /**
   * 
   * 
   * @return
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer( 10000 );
    sb.append( "<ViewContextReferenceList>" );

    Iterator iterator = viewContextReferences.values().iterator();

    while( iterator.hasNext() )
    {
      sb.append( ( (Marshallable)iterator.next() ).exportAsXML() );
    }

    sb.append( "</ViewContextReferenceList>" );
    return sb.toString();
  }
}