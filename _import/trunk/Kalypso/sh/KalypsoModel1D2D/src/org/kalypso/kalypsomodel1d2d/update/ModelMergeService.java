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
package org.kalypso.kalypsomodel1d2d.update;

import java.awt.Color;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessEstimateSpec;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;

public class ModelMergeService
{
  private static final ModelMergeService modelMergeService = new ModelMergeService();

  private final Map<String, IRoughnessEstimateSpec> femRoughnessStyleMap = new HashMap<String, IRoughnessEstimateSpec>();

  private boolean doReInit = true;

  private boolean isRoughnessToBeDisplay = false;

  private IRoughnessPolygonCollection roughnessPolygonCollection;

  private static final String DEFAULT_STYLE = "_DEFAULT_STYLE_";

  synchronized public String getRoughnessStyle( String elementID )
  {
    IRoughnessCls roughnessById = getRoughnessFromCacheById( elementID );
    if( roughnessById != null )
    {
      return roughnessById.getName();
    }
    else
    {
      return null;
    }
  }

  private final IRoughnessCls getRoughnessFromCacheById( String elementID )
  {
    IRoughnessEstimateSpec spec = femRoughnessStyleMap.get( elementID );
    if( spec != null )
    {
      IRoughnessCls[] clses = spec.mostSpreadRoughness();
      if( clses != null )
      {
        if( clses.length > 0 )
        {
          return clses[0];
        }
      }
    }
    return null;
  }

  synchronized public boolean getIsRoughnessToBeDisplay( )
  {
    return modelMergeService.isRoughnessToBeDisplay;
  }

  synchronized public void setIsRoughnessToBeDisplay( boolean isRoughnessToBeDisplay )
  {
    modelMergeService.isRoughnessToBeDisplay = isRoughnessToBeDisplay;
  }

  synchronized public boolean getDoReInit( )
  {
    return modelMergeService.doReInit;
  }

  synchronized public void doReInit( )
  {
    modelMergeService.doReInit = true;
    System.out.println( "Reiniting model roughness merge service" );
  }

  public synchronized IRoughnessCls getElementRoughnessCls( IPolyElement polyElement )
  {
// if( doReInit )
// {
// femRoughnessStyleMap.clear();
// ITerrainModel terrainModel= Util.getModel( ITerrainModel.class );
// roughnessPolygonCollection = terrainModel.getRoughnessPolygonCollection();
// doReInit = false;
// }
    try
    {
      if( polyElement == null )
      {
        System.out.println( "not a polyelement" );
        return null;
      }
      final String polyElementID = polyElement.getGmlID();
      if( femRoughnessStyleMap.containsKey( polyElementID ) )
      {
        // already computed
        // System.out.println("From Cache "+ polyElementID + clsName);
        return getRoughnessFromCacheById( polyElementID );
      }
      else
      {
// IRoughnessEstimateSpec roughnessEstimateSpec = null;
// if( !roughnessPolygonCollection.isEmpty() )
// {
// roughnessEstimateSpec =
// roughnessPolygonCollection.getRoughnessEstimateSpec(
// polyElement.recalculateElementGeometry() );
// }
// else
// {
// System.out.println("roughness polygone collection is empty");
// }
// femRoughnessStyleMap.put( polyElementID, roughnessEstimateSpec );
// IRoughnessCls cls = getRoughnessFromCacheById( polyElementID );
// System.out.println(
// "StyleName=" +
// (cls==null ? null:cls.getGmlID()) );
// return cls;
        getElementRoughnessEstimate( polyElement );
        return getRoughnessFromCacheById( polyElementID );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }

  }

  public synchronized IRoughnessEstimateSpec getElementRoughnessEstimate( IFE1D2DElement element )
  {
    if( element instanceof IPolyElement )
    {
      return getElementRoughnessEstimate( (PolyElement) element );
    }
    else
    {
      throw new UnsupportedOperationException();
    }
  }

  public synchronized IRoughnessEstimateSpec getElementRoughnessEstimate( IPolyElement polyElement )
  {
    if( doReInit )
    {
      femRoughnessStyleMap.clear();
      ITerrainModel terrainModel = Util.getModel( ITerrainModel.class );
      roughnessPolygonCollection = terrainModel.getRoughnessPolygonCollection();
      doReInit = false;
    }
    try
    {
      if( polyElement == null )
      {
        System.out.println( "not a polyelement" );
        return null;
      }
      final String polyElementID = polyElement.getGmlID();
      if( femRoughnessStyleMap.containsKey( polyElementID ) )
      {
        // already computed
        // System.out.println("From Cache "+ polyElementID + clsName);
        return femRoughnessStyleMap.get( polyElementID );
      }
      else
      {
        IRoughnessEstimateSpec roughnessEstimateSpec = null;
        if( !roughnessPolygonCollection.isEmpty() )
        {
          roughnessEstimateSpec = roughnessPolygonCollection.getRoughnessEstimateSpec( polyElement.recalculateElementGeometry() );
        }
        else
        {
          System.out.println( "roughness polygone collection is empty" );
        }
        femRoughnessStyleMap.put( polyElementID, roughnessEstimateSpec );
        return roughnessEstimateSpec;
      }
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }

  }

  private ModelMergeService( )
  {

  }

  public static final ModelMergeService getInstance( )
  {
    return modelMergeService;
  }

  synchronized public final String getElementRoughnessStyle( IPolyElement polyElement )
  {
    IRoughnessCls cls = getElementRoughnessCls( polyElement );
    if( cls == null )
    {
      return null;
    }
    else
    {
      return cls.getName();
    }

  }

  synchronized public final void removeRoughnessClass( Feature polyElementFeature )
  {
    String id = polyElementFeature.getId();
    femRoughnessStyleMap.remove( id );

  }

  synchronized public final Color getColor( IPolyElement polyElement, Color defaultColor )
  {
    IRoughnessCls rCls = getElementRoughnessCls( polyElement );
    final Color color;
    if( rCls == null )
    {
      color = defaultColor;
    }
    else
    {
      RGB colorStyle = rCls.getColorStyle();
      color = new Color( colorStyle.red, colorStyle.green, colorStyle.blue, 150 );
    }
// System.out.println("Color ="+color);
    return color;
  }

  public IRoughnessCls getElementRoughnessCls( IFE1D2DElement element )
  {
    if( element instanceof PolyElement )
    {
      return getElementRoughnessCls( (PolyElement) element );
    }
    else
    {
      throw new UnsupportedOperationException();
    }
  }

}