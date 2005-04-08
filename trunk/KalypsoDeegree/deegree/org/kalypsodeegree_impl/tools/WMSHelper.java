package org.kalypsodeegree_impl.tools;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

/**
 * This class contains helper methods to handle wms specific tasks.
 * 
 * @author c.kuepferle@tuhh.de
 *  
 */
public class WMSHelper
{
  /**
   * This method tries to find a common spatial reference system (srs) for a
   * given set of layers. If all layers coorespond to the local crs the local
   * crs is returned, otherwise the srs of the top layer is returned and the
   * client must choose one to transform it to the local coordiante system
   * 
   * @param localCRS
   *          the local spatial reference system
   * @param capabilities
   *          the capabilites document of the web map service
   * @param layerNames
   *          the layers that have to be matched to the local srs
   * @return result an array of possible coordiante systems
   */
  public static CS_CoordinateSystem[] negotiateCRS( CS_CoordinateSystem localCRS,
      WMSCapabilities capabilities, String[] layerNames ) throws Exception
  {
    Layer topLayer = capabilities.getCapability().getLayer();
    CS_CoordinateSystem crs = matchCrs( topLayer, layerNames, localCRS );
    if( crs != null )
      return new CS_CoordinateSystem[]
      {
        localCRS
      };
    //get crs from top layer
    String[] topLayerSRS = topLayer.getSrs();
    List result = new ArrayList();
    for( int i = 0; i < topLayerSRS.length; i++ )
    {
      result.add( ConvenienceCSFactory.getInstance().getOGCCSByName( topLayerSRS[i] ) );
    }
    return (CS_CoordinateSystem[])result.toArray( new CS_CoordinateSystem[result.size()] );
  }

  /**
   * This method tries to match the local coordiante system to a given layer
   * selection.
   * 
   * @param topLayer
   *          the top layer of the layer structur of a web map service
   * @param layerSelection
   *          layers to be matched
   * @param localCRS
   *          the local coordinate system
   * @return returns null if one element of the layers to be matched is not
   *         available in the local coordinate system, otherwise it returns the
   *         local crs
   *  
   */

  private static CS_CoordinateSystem matchCrs( Layer topLayer, String[] layerSelection,
      CS_CoordinateSystem localCRS ) throws Exception
  {
    HashSet collector = new HashSet();

    collect( collector, topLayer, layerSelection );
    for( Iterator iter = collector.iterator(); iter.hasNext(); )
    {
      Layer layer = (Layer)iter.next();
      String[] layerSRS = layer.getSrs();
      if( contains( layerSRS, localCRS.getName() ) )
        continue;
      return null;

    }
    return localCRS;
  }

  /**
   * This method collects all layers from a capabilites document.
   * 
   * @param capabilites
   *          wms capabilites document
   * @param set
   *          the Set where the layers are collected in
   */
  public static void getAllLayers( WMSCapabilities capabilites, Set set )
  {
    try
    {
      Layer topLayer = capabilites.getCapability().getLayer();
      collect( set, topLayer, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * This method collects all layers (or the specified layers) from the top
   * layer of a WMSCapabilites document. If the parameter layerSeletion is empty
   * or null the method collects all layers, otherwise returns all layers with
   * the same name as in the layerSelection.
   * 
   * @param collector
   *          The set that collects the layers found.
   * @param layer
   *          the top layer of the wms capabilites document.
   * @param layerSelection
   *          an array of layer names to search for.
   */
  private static void collect( Set collector, Layer layer, String[] layerSelection )
      throws Exception
  {

    Layer[] layerTree = layer.getLayer();
    for( int i = 0; i < layerTree.length; i++ )
    {
      Layer newLayer = layerTree[i];//.getLayer();
      if( newLayer.getLayer().length > 0 )
      {
        //recursive function call
        collect( collector, newLayer, layerSelection );
      }
      else
      {
        //System.out.println( layerTree[i].getName() );
        if( layerSelection != null  )
        {

          if( contains( layerSelection, layerTree[i].getName() ) )
          {
            collector.add( layerTree[i] );
          }
        }
        else
          collector.add( layerTree[i] );
      }
      continue;
    }
  }

  /**
   * This method checks an array of Strings for a given String to match.
   * 
   * @param array
   *          strings to check for a match.
   * @param toMatch
   *          the string to match
   * @return boolean true if the String is the array, false otherwise
   */

  public static boolean contains( String[] array, String toMatch )
  {
    for( int i = 0; i < array.length; i++ )
    {
      if( array[i].equals( toMatch ) )
        return true;
    }
    return false;
  }
}