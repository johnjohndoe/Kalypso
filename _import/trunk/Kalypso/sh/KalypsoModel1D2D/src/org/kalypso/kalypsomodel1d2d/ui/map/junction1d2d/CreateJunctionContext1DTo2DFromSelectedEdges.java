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
import java.util.Collection;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddJunctionContextEle1DToEle2DFromEdgesCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck.VALIDITY_STATE;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.select.QNameBasedSelectionFilter;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;

/**
 * Provides a widget functionality for creating a element junction
 * from by selecting edges
 * @author Patrice Congo
 */
public class CreateJunctionContext1DTo2DFromSelectedEdges 
                    extends FENetConceptSelectionWidget 
                    implements IWidget, IWidgetWithOptions
{
  
  
  final private ModelDataCheck1DToCLine1DSelection check1D =
                    new ModelDataCheck1DToCLine1DSelection();
  
  final private ModelDataCheck1DTo2D2DSelection check2D = 
                new ModelDataCheck1DTo2D2DSelection();
  final private ModelDataCompositeCheck1D2D modelCheck =
                      new ModelDataCompositeCheck1D2D(check1D,check2D);
  
  final private JunctionContextWidgetDataModel dataModel =
                   new JunctionContextWidgetDataModel(modelCheck);
  
  final private JunctionContextWidgetFace face = 
                      new JunctionContextWidgetFace(dataModel);
  final private IDataModelCommand createModelPart =
    makeCreateJunctionCommandFromDataModel( dataModel );

  public CreateJunctionContext1DTo2DFromSelectedEdges()
  {
    super(
        Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE,
        "Elementkopplung hinzufügen",
        "Elementkopplung hinzufügen");
    setSelectionFilter( 
        QNameBasedSelectionFilter.getFilterForQName( 
                      Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE) );
    dataModel.setDataCheck( 
        JunctionContextWidgetDataModel.SELECTED_ELEMENT1D , 
        check1D );
    dataModel.setDataCheck( 
        JunctionContextWidgetDataModel.SELECTED_ELEMENT2D , 
        check2D );
    dataModel.setData( 
        JunctionContextWidgetDataModel.CREATE_MODEL_PART,
        createModelPart );
  }  
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#selectionMade()
   */
  @Override
  protected void selectionMade( )
  {
    dataModel.setSelected( getSelectedFeature() );
  }
  
  private final  IDataModelCommand makeCreateJunctionCommandFromDataModel( final JunctionContextWidgetDataModel dataModel )
  {
    return new IDataModelCommand()
    {
      /**
       * @see org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d.IDataModelCommand#execute()
       */
      public void execute( )
      {
        if(modelCheck.getValidityState()!=VALIDITY_STATE.VALID)
        {
          
        }
        else
        {
           Collection<IFE1D2DEdge> selected1DEdges = dataModel.getSelected1D();
             
             Collection<IFE1D2DEdge> selected2DEdges = dataModel.getSelected2D();
             
             IFEDiscretisationModel1d2d model1d2d = 
                 getModel1d2d(Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE);
             
             AddJunctionContextEle1DToEle2DFromEdgesCmd junctionCmd =
                   new AddJunctionContextEle1DToEle2DFromEdgesCmd(
                           model1d2d,
                           selected1DEdges.iterator().next(),
                           selected2DEdges.iterator().next());
             
            CommandableWorkspace workspace = 
               getTheme(Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE).getWorkspace();
             ChangeDiscretiationModelCommand changeModelCmd=
                   new ChangeDiscretiationModelCommand(
                       workspace,model1d2d);
             changeModelCmd.addCommand( junctionCmd );
            try
            {
              workspace.postCommand( changeModelCmd);
              dataModel.resetSelections();
            }
            catch( Exception e1 )
            {
              e1.printStackTrace();
            }
        }
      }
    };
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite, org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( Composite parent, FormToolkit toolkit )
  {
    return face.createControl( parent, toolkit );
  }
  
  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    face.disposeControl();
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
    if(KeyEvent.VK_CONTROL  == keyCode)
    {
      //skip
      super.keyTyped(e);
    }
    else if(KeyEvent.VK_SHIFT == keyCode )
    {
      //skip
      super.keyTyped(e);
    }
    else if(KeyEvent.VK_ENTER == e.getKeyChar() )
    {
      createModelPart.execute();
    }
    else
    {
      super.keyTyped(e);
    }
  }
  
  

}
