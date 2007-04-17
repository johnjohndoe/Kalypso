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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddJunctionContext1DToCLineFromEdgesCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.select.QNameBasedSelectionFilter;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Implements a Strategy for selection an fe element
 * 
 * @author Patrice Congo
 */
public class CreateJunctionContext1dToCLineFromSelectedEdges 
                    extends FENetConceptSelectionWidget 
                    implements IWidget,IWidgetWithOptions
{
  
  

  private Composite parentComposite;
  private Composite optionComposite = null;
  private JunctionContextWidgetFace face = 
                      new JunctionContextWidgetFace();

  public CreateJunctionContext1dToCLineFromSelectedEdges()
  {
    super(
        Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE,
        "Junction element hinzufügen",
        "Junction element hinzufügen");
    setSelectionFilter( 
        QNameBasedSelectionFilter.getFilterForQName( 
                      Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE) );
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
//     if(selectedFeatures.length < 2 )
//     {
//       System.out.println(
//           "Selection must have at least 2 element:"+
//           selectedFeatures.length);
//       return;
//     }
     Collection<IFE1D2DEdge> selected1DEdges = 
           ModelOps.collectAll1DEdges( selectedFeatures );//findSelectedElement1DEdge(selectedFeatures);
     Collection<IFE1D2DEdge> selected2DEdges = 
       ModelOps.collectAll2DEdges( selectedFeatures );//findSelectedElement2DEdge(selectedFeatures);
     
     IFEDiscretisationModel1d2d model1d2d = 
         getModel1d2d(Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE);
     if(isBadSelection(selected1DEdges, selected2DEdges,selectedFeatures,model1d2d))
     {
       return;
     }
     AddJunctionContext1DToCLineFromEdgesCmd junctionCmd =
           new AddJunctionContext1DToCLineFromEdgesCmd(
                   model1d2d,
                   selected1DEdges.iterator().next(),
                   selected2DEdges);
     
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

  
  private final boolean isBadSelection( 
                        Collection<IFE1D2DEdge> selected1DEdges, 
                        Collection<IFE1D2DEdge> selected2DEdges, 
                        Feature[] selectedFeatures, 
                        IFEDiscretisationModel1d2d model1d2d )
  {
    final String message;
    if(selected1DEdges.size()!=1)
    {
      message =
          "Wählen sie ein 1D Kante";
      
    }
    else if(selected2DEdges.size()<=0)
    {
      message =
          "Wählen sie ein 2D Kanten";
      
    }
    else if(selected1DEdges.size()+selected2DEdges.size()!=selectedFeatures.length)
    {
      message =
          "Nur 1d und 2d Kanten wählen";      
    }
    else if(!ModelOps.isContinuousLine( 
              new ArrayList<IFE1D2DEdge>( selected2DEdges )))
    {
      message = "Selektierte 2D kanten müssen eine Linie builden";
    }
    else
    {
      message = null;
    }
    if( message !=null )
    {
      showMessage( message );
      return true;
    }
    else
    {
      return false;
    }
  }

  private final void showMessage(final String message)
  {
    
    if(parentComposite != null)
    {
      Display display = parentComposite.getDisplay();
      Runnable runnable = new Runnable()
      {
        /**
         * @see java.lang.Runnable#run()
         */
        public void run( )
        {
          Shell shell = parentComposite.getShell();
          MessageBox messageBox = new MessageBox(shell );
          messageBox.setMessage( message );
          messageBox.open();
        }
      };
      display.syncExec( runnable  );
      
    }
    else
    {
      System.out.println(message);
    }
  }
  
  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite, org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( Composite parent, FormToolkit toolkit )
  {
    this.parentComposite = parent;
    
//    this.optionComposite = new Composite(parent,SWT.NONE);
//    return optionComposite;
    
    return this.face.createControl( parent, toolkit );
    
  }
  
  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
//    if(optionComposite!=null)
//    {
//      if(!optionComposite.isDisposed())
//      {
//        optionComposite.dispose();
//      }
//    }
    this.face.disposeControl();
  }
//  private Collection<IFE1D2DEdge> findSelectedElement2DEdge( Feature[] selectedFeatures )
//  {
//    
//    Set<IFE1D2DEdge> selected2DEdges = new HashSet<IFE1D2DEdge>();
//    for(Feature feature:selectedFeatures)
//    {
//     if( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE.equals( 
//                                  feature.getFeatureType().getQName()))
//     {
//       IFE1D2DEdge edge = (IFE1D2DEdge) feature.getAdapter( IFE1D2DEdge.class );
//       if(TypeInfo.is2DEdge( edge ))
//       {
//         selected2DEdges.add( edge );
//       }
//     }
//    }
//    return selected2DEdges;
//  }

//  private IFE1D2DEdge findSelectedElement1DEdge( Feature[] selectedFeatures )
//  {
//    for(Feature feature:selectedFeatures)
//    {
//     if(Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE.equals( 
//                                  feature.getFeatureType().getQName()))
//     {
//       IFE1D2DEdge edge = (IFE1D2DEdge) feature.getAdapter( IFE1D2DEdge.class );
//      if(TypeInfo.is1DEdge( edge ))
//       {
//         return edge;
//       }
//     }
//    }
//    return null;
//    
//  }
}
