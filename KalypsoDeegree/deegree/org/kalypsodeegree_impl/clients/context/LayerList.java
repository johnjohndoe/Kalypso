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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.deegree.xml.Marshallable;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class LayerList implements Marshallable
{
  private HashMap layers = new HashMap();

  private List list = new ArrayList();

  /**
   * Creates a new LayerList object.
   * 
   * @param layers
   */
  public LayerList( Layer[] layers )
  {
    setLayers( layers );
  }

  /**
   * returns a layer identifies by its name
   * 
   * @param name
   *          name ofthe layer
   * 
   * @return
   */
  public Layer getLayer( String name )
  {
    return (Layer)layers.get( name );
  }

  /**
   * returns all layers of the web map context
   * 
   * @return
   */
  public Layer[] getLayers()
  {
    Layer[] cl = new Layer[list.size()];
    return (Layer[])list.toArray( cl );
  }

  /**
   * sets all layers of the web map context
   * 
   * @param layers
   */
  public void setLayers( Layer[] layers )
  {
    this.layers.clear();
    this.list.clear();

    if( layers != null )
    {
      for( int i = 0; i < layers.length; i++ )
      {
        this.layers.put( layers[i].getName(), layers[i] );
        list.add( layers[i] );
      }
    }
  }

  /**
   * adds one layer to the the web map context. If a layer with the same name as
   * the passed layer already exits it will be overwritten
   * 
   * @param layer
   */
  public void addLayer( Layer layer )
  {
    this.layers.put( layer.getName(), layer );
    list.add( layer );
  }

  /**
   * removes a layer identified by its name from the web map context
   * 
   * @param name
   *          name of the layer to be removed
   * 
   * @return
   */
  public Layer removeLayer( String name )
  {
    Layer layer = (Layer)this.layers.remove( name );
    list.add( layer );
    return layer;
  }

  /**
   * removes all layers from the web map context
   */
  public void clear()
  {
    this.layers.clear();
    list.clear();
  }

  public String exportAsXML()
  {

    StringBuffer sb = new StringBuffer( 10000 );
    sb.append( "<LayerList>" );
    for( int i = 0; i < list.size(); i++ )
    {
      sb.append( ( (Marshallable)list.get( i ) ).exportAsXML() );
    }
    sb.append( "</LayerList>" );
    return sb.toString();
  }
}