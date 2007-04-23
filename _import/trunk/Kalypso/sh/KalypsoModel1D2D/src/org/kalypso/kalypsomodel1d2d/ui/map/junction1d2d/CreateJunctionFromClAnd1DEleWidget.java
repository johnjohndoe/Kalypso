/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddJunctionElementFromClAndElement1DCmd;
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
public class CreateJunctionFromClAnd1DEleWidget 
                    extends FENetConceptSelectionWidget 
                    implements IWidget
{
  
  private QNameBasedSelectionFilter selectionFilter;

  public CreateJunctionFromClAnd1DEleWidget()
  {
    super(
        new QName[]{
          Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine,
          Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D},
          "Junction element hinzufügen",
          "Junction element hinzufügen");
    selectionFilter= new QNameBasedSelectionFilter();
    selectionFilter.add( 
        Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D );
    selectionFilter.add( 
        Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine );
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
     IElement1D selected1DEle = 
           findSelectedelement1D(selectedFeatures);
     IFE1D2DContinuityLine selectedCLine=
           findSelectedCLine( selectedFeatures );
     if(selected1DEle==null || selectedCLine==null)
     {
       System.out.println(
           "Please select an 1d element and a cl:"+Arrays.asList( selectedFeatures ));
     }
     IFEDiscretisationModel1d2d model1d2d = 
         getModel1d2d(Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D);
    AddJunctionElementFromClAndElement1DCmd junctionCmd=
       new AddJunctionElementFromClAndElement1DCmd(
           model1d2d,
           selected1DEle,
           selectedCLine);
     CommandableWorkspace workspace = 
       getTheme(Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D).getWorkspace();
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

  private IFE1D2DContinuityLine findSelectedCLine( Feature[] selectedFeatures )
  {
    for(Feature feature:selectedFeatures)
    {
     if(Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine.equals( 
                                          feature.getFeatureType().getQName()))
     {
       return (IFE1D2DContinuityLine) feature.getAdapter( IFE1D2DContinuityLine.class );
     }
    }
    return null;
  }

  private IElement1D findSelectedelement1D( Feature[] selectedFeatures )
  {
    for(Feature feature:selectedFeatures)
    {
     if(Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D.equals(feature.getFeatureType().getQName()))
     {
       return (IElement1D) feature.getAdapter( IElement1D.class );
     }
    }
    return null;
  }
  
 
}
