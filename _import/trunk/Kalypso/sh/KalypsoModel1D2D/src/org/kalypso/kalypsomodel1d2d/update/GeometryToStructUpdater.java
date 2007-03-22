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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.PolyElement;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IGmlWorkspaceListener;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * Listen to a workspace and given a geometrical change in the 
 * feature of interes (Node, edge and element transform that change 
 * to update the concern structure and there geometry.
 * E.g. if a node location changes update container edges and elements
 * 
 * TODO: please hide system-outs via trace-mechanism, it pollutes the console
 * 
 * @author Patrice Congo
 *
 */
public class GeometryToStructUpdater implements IGmlWorkspaceListener
{
  /**
   * Work space which is monitored
   */
  private GMLWorkspace workspace;
  
  
  public GeometryToStructUpdater()
  {
    // TODO: please use something configurable instead (e.g. tracing)
//    System.out.println("================GEOM_STRUCT_UPDATER================");
  }
  
  
  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#getQNames()
   */
  public QName[] getQNames( )
  {
    return new QName[]{
//        Kalypso1D2DSchemaConstants.WB1D2D_F_NODE//,
//        Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE,
//        Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT
        };
  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#init(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void init( GMLWorkspace workspace )
  {
//    System.out.println("WorkspaceInit="+workspace);
    this.workspace=workspace;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    // TODO: please use something configurable instead (e.g. tracing)

//    System.out.println("modellEvent="+modellEvent+
//            "\nSource="+modellEvent.getEventSource());
    try
    {
      if(modellEvent==null)
      {
        return;
      }
      else if(modellEvent.isType( ModellEvent.FEATURE_CHANGE ))
      {
        if(modellEvent instanceof FeaturesChangedModellEvent)
        {
          Feature changedFeatures[]=
              ((FeaturesChangedModellEvent)modellEvent).getFeatures();
          if(changedFeatures == null)
          {
            System.out.println("Changed feature is null");
            return;
          }
          else
          {
            List<IFE1D2DElement> eleList= new ArrayList<IFE1D2DElement>();
            for(Feature feature:changedFeatures)
            {
              if(Util.directInstanceOf(  
                          feature, 
                          Kalypso1D2DSchemaConstants.WB1D2D_F_NODE ))
              {
                IFE1D2DNode<IFE1D2DEdge> node=
                  (IFE1D2DNode)feature.getAdapter( IFE1D2DNode.class );
                IFeatureWrapperCollection<IFE1D2DEdge> edges=node.getContainers();
                //IFE1D2DEdge[] edges=node.Edges();
                GMLWorkspace workspace=
                  (GMLWorkspace)modellEvent.getEventSource();
                for(IFE1D2DEdge edge:edges)
                {
                    //TODO reset
//                    ((FE1D2DEdge) edge).resetGeometry();
//                    eleList.addAll( ((FE1D2DEdge) edge).getContainers());
                  
                    System.out.println("setting edge curve");
//                    workspace.fireModellEvent(  
//                        new FeatureStructureChangeModellEvent(
//                            workspace,
//                            edge.getWrappedFeature().getParent(),
//                            FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_MOVE)) ;
                }
                //node.getEdges()
                System.out.println("changedFeature0="+feature);
//              getMapModell();
              }
              else
              {
                System.out.println("changedFeature1="+feature);
//                workspace.fireModellEvent(  
//                    new FeatureStructureChangeModellEvent(
//                        workspace,
//                        feature.getParent(),
//                        FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_MOVE)) ;
              }
            }
            
            // TODO: is this really necessary? Probably really an invalidate problem
            for(IFE1D2DElement ele:eleList)
            {
              if(ele instanceof PolyElement)
              {
                ((PolyElement)ele).resetGeometry();
              }
              else
              {
                System.out.println(
                    "reset only possible for PolyElement impl:"+
                    ele.getClass());
              }
              
            }
            
            
          }
        }
        else
        {
          System.out.println("ModelEventx0="+modellEvent.getClass());
        }
      }
      else
      {
        System.out.println("ModelEventx="+modellEvent.getEventSource());
      }
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }    
  }
  
//  public MapModell getMapModell()
//  {
//    IWorkbenchWindow window=
//      PlatformUI.getWorkbench().getActiveWorkbenchWindow();
//    IEditorPart editor=window.getActivePage().getActiveEditor();
//    if(editor==null)
//    {
//      System.out.println("Editor ist null");
//      return null;
//    }
//    else
//    {
//      final String ID_GIS_MAP_EDITOR=
//        "org.kalypso.ui.editor.mapeditor.GisMapEditor";
//      if(ID_GIS_MAP_EDITOR.equals( 
//                editor.getSite().getId()))
//      {
//        ((GisMapEditor)editor).getMapPanel().repaint( );
//        
//      }
//      else
//      {
//        System.out.println("Editor:"+editor.getClass());
//      }
//      return null;
//    }
//    
//  }
 
}
