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
package org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d;

import java.awt.event.KeyEvent;
import java.util.Arrays;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddJunctionElementFromClAndElement1DCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddJunctionElementFromEdgesCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.select.QNameBasedSelectionFilter;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Implements a Strategy for selection an fe element
 * 
 * @author Patrice Congo
 */
public class CreateJunctionFromSelectedEdges 
                    extends FENetConceptSelectionWidget 
                    implements IWidget
{
  
  private QNameBasedSelectionFilter selectionFilter;

  public CreateJunctionFromSelectedEdges()
  {
    super(Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE);
    selectionFilter= new QNameBasedSelectionFilter();
    selectionFilter.add( 
        Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
    
    setSelectionFilter( selectionFilter );
  }  
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#canBeActivated(org.eclipse.jface.viewers.ISelection, org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public boolean canBeActivated( ISelection selection, MapPanel mapPanel )
  {
    super.canBeActivated( selection, mapPanel );
    return true;
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( KeyEvent e )
  {
    
    int keyCode=e.getKeyCode();
    if(e.VK_CONTROL  == keyCode)
    {
      //skip
      super.keyTyped(e);
    }
    else if(e.VK_SHIFT == keyCode )
    {
      //skip
      super.keyTyped(e);
    }
    else if(e.VK_ENTER == e.getKeyChar() )
    {
     Feature[] selectedFeatures = getSelectedFeature();
     if(selectedFeatures.length !=2 )
     {
       System.out.println(
           "Selection must have 2 element:"+
           selectedFeatures.length);
       return;
     }
     IFE1D2DEdge selected1DEdge = 
           findSelectedElement1DEdge(selectedFeatures);
     IFE1D2DEdge selected2DEdge = 
       findSelectedElement2DEdge(selectedFeatures);
     
     IFEDiscretisationModel1d2d model1d2d = 
         getModel1d2d(Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE);
    AddJunctionElementFromEdgesCmd junctionCmd=
       new AddJunctionElementFromEdgesCmd(
           model1d2d,
           selected1DEdge,
           selected2DEdge);
     
    CommandableWorkspace workspace = 
       getTheme(Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE).getWorkspace();
     ChangeDiscretiationModelCommand changeModelCmd=
           new ChangeDiscretiationModelCommand(
               workspace,model1d2d);
     changeModelCmd.addCommand( junctionCmd );
     try
    {
      workspace.postCommand( changeModelCmd);
    }
    catch( Exception e1 )
    {
      e1.printStackTrace();
    }
    }
    else
    {
      super.keyTyped(e);
    }
  }

  private IFE1D2DEdge findSelectedElement2DEdge( Feature[] selectedFeatures )
  {
    for(Feature feature:selectedFeatures)
    {
     if(Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE.equals( 
                                  feature.getFeatureType().getQName()))
     {
       IFE1D2DEdge edge = (IFE1D2DEdge) feature.getAdapter( IFE1D2DEdge.class );
      if(TypeInfo.is2DEdge( edge ))
       {
         return edge;
       }
     }
    }
    return null;
  }

  private IFE1D2DEdge findSelectedElement1DEdge( Feature[] selectedFeatures )
  {
    for(Feature feature:selectedFeatures)
    {
     if(Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE.equals( 
                                  feature.getFeatureType().getQName()))
     {
       IFE1D2DEdge edge = (IFE1D2DEdge) feature.getAdapter( IFE1D2DEdge.class );
      if(TypeInfo.is1DEdge( edge ))
       {
         return edge;
       }
     }
    }
    return null;
    
  }
}
