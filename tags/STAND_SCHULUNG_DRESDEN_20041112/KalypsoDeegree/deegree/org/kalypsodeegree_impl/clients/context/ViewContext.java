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
 * is the root class of the Web Map Context
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class ViewContext extends AbstractContext implements Marshallable
{
  private LayerList layerList = null;

  /**
   * Creates a new WebMapContext object.
   * 
   * @param general
   *          general informations about the map context and its creator
   * @param layerList
   *          layers contained in the web map context
   */
  public ViewContext( General general, LayerList layerList ) throws ContextException
  {
    super( general );
    setLayerList( layerList );
  }

  /**
   * returns the list of layers contained in this context
   * 
   * @return
   */
  public LayerList getLayerList()
  {
    return layerList;
  }

  /**
   * sets the list of layers to be contained in this context
   * 
   * @param layerList
   * 
   * @throws ContextException
   */
  public void setLayerList( LayerList layerList ) throws ContextException
  {
    if( layerList == null )
    {
      throw new ContextException( "layerList isn't allowed to be null" );
    }
    this.layerList = layerList;
  }

  /**
   * @see org.deegree.xml.Marshallable#exportAsXML()
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer( 50000 );
    sb.append( "<ViewContext xmlns='http://www.opengis.net/context' " );
    sb.append( "xmlns:sld='http://www.opengis.net/sld' " );
    sb.append( "xmlns:xlink='http://www.w3.org/1999/xlink' >" );
    sb.append( ( (Marshallable)getGeneral() ).exportAsXML() );
    sb.append( ( (Marshallable)layerList ).exportAsXML() );
    sb.append( "</ViewContext>" );
    return sb.toString();
  }

}