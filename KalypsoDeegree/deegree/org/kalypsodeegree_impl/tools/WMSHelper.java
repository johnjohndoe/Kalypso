package org.kalypsodeegree_impl.tools;

import java.rmi.RemoteException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.deegree.services.wms.capabilities.Capability;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.WMSCapabilities;
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

public class WMSHelper
{
  private static final WMSHelper m_helper = new WMSHelper();

  public static WMSHelper getInstance()
  {
    return m_helper;
  }

  public CS_CoordinateSystem negotiateCRS( CS_CoordinateSystem localCRS,
      WMSCapabilities capabiliets, String layerName ) throws Exception
  {
    Layer topLayer = capabiliets.getCapability().getLayer();
    String[] topSRS = topLayer.getSrs();
    for( int i = 0; i < topSRS.length; i++ )
    {
      String crs = topSRS[i];
      if( crs.equals( localCRS.getName() ) )
        return localCRS;

    }
    matchCrs( topLayer.getLayer(), localCRS );
    return null;
  }

  private void matchCrs( Layer[] layer, CS_CoordinateSystem localCRS ) throws Exception
  {
    for( int i = 0; i < layer.length; i++ )
    {
      Layer newLayer = layer[i];
      if( newLayer.getLayer().length > 0 )
      {
        //recursive function call
        matchCrs( newLayer.getLayer(), localCRS );
      }
      else
      {

      }

    }
  }

  public void getAllLayers( WMSCapabilities capabilites, Set set )
  {
    try
    {
      Layer topLayer = capabilites.getCapability().getLayer();
      collect( set, topLayer, false );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private void collect( Set collector, Layer layer, boolean levelsOnly ) throws Exception
  {
    Layer[] layerTree = layer.getLayer();
    for( int i = 0; i < layerTree.length; i++ )
    {
      Layer newLayer = layerTree[i];//.getLayer();
      if( newLayer.getLayer().length > 0 )
      {
        //recursive function call
        collect( collector, newLayer, levelsOnly );
      }
      else
      {
        System.out.println( layerTree[i].getName() );
        collector.add( layerTree[i] );
      }
      continue;
    }
  }

  public void getLevels( Set list, WMSCapabilities capabilites ) throws Exception
  {
    Layer topLayer = capabilites.getCapability().getLayer();
    list.add( topLayer );
    collect( list, topLayer, true );
  }

  private Layer getParent( String layerName, WMSCapabilities capabilities )
  {

    return null;
  }
}