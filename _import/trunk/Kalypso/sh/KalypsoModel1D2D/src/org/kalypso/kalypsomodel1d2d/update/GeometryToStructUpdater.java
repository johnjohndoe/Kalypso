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

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IGmlWorkspaceListener;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * Listen to a workspace and given and update the roughness style store
 * 
 * @author Patrice Congo
 * 
 */
public class GeometryToStructUpdater implements IGmlWorkspaceListener
{
  private final ModelMergeService mergeService = ModelMergeService.getInstance();

  /**
   * Work space which is monitored
   */
  private GMLWorkspace workspace;

  public GeometryToStructUpdater( )
  {

  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#getQNames()
   */
  public QName[] getQNames( )
  {
    return new QName[] {
// Kalypso1D2DSchemaConstants.WB1D2D_F_NODE//,
// Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE,
    // Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT
    };
  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#init(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void init( final GMLWorkspace workspace )
  {
    this.workspace = workspace;
    final Feature rootFeature = workspace.getRootFeature();
    final QName rootFeatureQname = rootFeature.getFeatureType().getQName();

    if( Kalypso1D2DSchemaConstants.WB1D2D_F_DiscretisationModel.equals( rootFeatureQname ) )
    {
// System.out.println( "Update roughness merge because of ne dicr-model" );
// ElementRoughnessStyleFunc.clear( );
      mergeService.doReInit();
    }
    else if( KalypsoModelSimulationBaseConsts.SIM_BASE_F_TERRAIN_ELE_MODEL.equals( rootFeature ) )
    {
// System.out.println( "Update roughness merge because of new terrain-model" );
// ElementRoughnessStyleFunc.clear();
      mergeService.doReInit();
    }
    else
    {
      // /
    }

  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    final Feature[] changedFeatures;
    if( modellEvent instanceof FeaturesChangedModellEvent )
    {
      changedFeatures = ((FeaturesChangedModellEvent) modellEvent).getFeatures();
    }
    else if( modellEvent instanceof FeatureStructureChangeModellEvent )
    {
      changedFeatures = ((FeatureStructureChangeModellEvent) modellEvent).getChangedFeatures();
    }
    else
    {
      changedFeatures = new Feature[] {};
    }

    for( final Feature feature : changedFeatures )
    {
      if( TypeInfo.isPolyElementFeature( feature ) )
      {
// ElementRoughnessStyleFunc.removeRoughnessClass( feature );
        mergeService.removeRoughnessClass( feature );
      }
      else if( feature == null )
      {
        // /go ahead
      }
      else
      {

        final QName name = feature.getFeatureType().getQName();
        if( KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON_COLLECTION.equals( name ) )
        {
          mergeService.doReInit();
        }
        else
        {
          // TODO: please do not pollute the console, introduce tracing option instead
// System.out.println("Changed:"+name);
        }
      }

    }
  }
}
