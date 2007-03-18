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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.apache.commons.vfs.tasks.DeleteTask;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.DeleteNativeTerrainElevationWrapper;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * 
 * @author Madanagopal
 * @author Patrice Congo
 *
 */
public class ElevationModelSystemEditorComponent
{
  
  class ElevationListLabelProvider extends LabelProvider
  {

    public Image getImage( Object element )
    {

      return null;
    }

    public String getText( Object element )
    {
      if( element instanceof ITerrainElevationModel )
      {
        
        String name = ((ITerrainElevationModel) element).getName();
        if( name != null )
        {
          return name;
        }
        else
        {
          return ((ITerrainElevationModel) element).getGmlID();
        }
      }
      else
      {
        throw new RuntimeException( "Only terrain elevation model are supported:" + "but got \n\tclass=" + (element == null ? null : element.getClass()) + "\n\t value=" + element );
      }
    }
  }
  
  /* ========================================================================*/
  private TableViewer elevationListTableViewer;
  private ApplyElevationWidgetDataModel dataModel;
  private Image image_goToTerrain;

  private Image image_Down;

  private Image image_Up;
  private FormToolkit toolkit;
  private Composite parent;
  
  final private SelectionListener moveUpListener = 
    new SelectionAdapter()
  {
    public void widgetSelected( SelectionEvent event )
    {
      
      System.out.println("MoveUp:"+elevationListTableViewer.getSelection());
//      moveUpSelectionUp();
      moveSelection( -1 );
      elevationListTableViewer.refresh();
    }

  };
  
  final private SelectionListener moveDownListener = 
    new SelectionAdapter()
  {
    public void widgetSelected( SelectionEvent event )
    {
      moveSelection( 1 );
      elevationListTableViewer.refresh();
    }
  };
  
  final private ISelectionChangedListener elevationModelSelectListener = 
    new ISelectionChangedListener()
  {
    public void selectionChanged( SelectionChangedEvent event )
    {
      try
      {
        IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        if(selection==null)
        {
          System.out.println("Selection is null");
          return ;
        }
        // nameSel = (String) selection.getFirstElement();
        if( selection.getFirstElement() == null )
          throw new NullPointerException( "Null Value while selection.getFirstElement() :" + selection.getFirstElement() );
        else
        {
          if( selection.getFirstElement() instanceof ITerrainElevationModel )
          {
            ITerrainElevationModel firstElement = (ITerrainElevationModel) selection.getFirstElement();
            dataModel.setElevationModel( firstElement );
//          inputText.setText( firstElement.getName() );
          }
        }
      }
      catch( Throwable th )
      {
        th.printStackTrace();
      }

    }
  } ;
  
  
  public ElevationModelSystemEditorComponent()
  {
    
  }
  
  public void createControl(
      ApplyElevationWidgetDataModel dataModel,
      FormToolkit toolkit,
      Composite parent)
  {
    this.toolkit=toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    guiCreateSelectElevationModel( parent );
  }
  
  private void guiCreateSelectElevationModel( Composite elevationComposite )
  {
    FormData elevFormData;

    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( 0, 5 );
    elevFormData.top = new FormAttachment( 0, 5 );
    Label terrainModelLabel = new Label( elevationComposite, SWT.NONE );
    // GridData labelGridData = new GridData( GridData.FILL_BOTH );
    // labelGridData.horizontalSpan = 2;
    terrainModelLabel.setText( "Vorhandene Höhenmodelle" );
    terrainModelLabel.setLayoutData( elevFormData );

    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( 0, 10 );
    elevFormData.top = new FormAttachment( terrainModelLabel, 5 );
    elevFormData.height = 90;

    elevationListTableViewer = new TableViewer( elevationComposite, SWT.FILL | SWT.BORDER );
    Table elevationList = elevationListTableViewer.getTable();
    elevationListTableViewer.setContentProvider( new ArrayContentProvider() );
    elevationListTableViewer.setLabelProvider( new ElevationListLabelProvider() );
    elevationList.setLinesVisible( true );
    elevationList.setLayoutData( elevFormData );
    
    ITerrainElevationModelSystem elevationModelSystem = dataModel.getElevationModelSystem();
    if( elevationModelSystem == null )
    {
      elevationListTableViewer.setInput( new Object[] {} );
    }
    else
    {
      
      IFeatureWrapperCollection<ITerrainElevationModel> terrainElevationModels = elevationModelSystem.getTerrainElevationModels();
      if( terrainElevationModels == null )
      {
        elevationListTableViewer.setInput( new Object[] {} );
      }
      else
      {
        System.out.println( "SIZE : " + terrainElevationModels.size() );
        elevationListTableViewer.setInput( terrainElevationModels/*.toArray()*/ );
      }
    }
    elevationListTableViewer.addSelectionChangedListener( this.elevationModelSelectListener);

    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationList, 5 );
    elevFormData.top = new FormAttachment( terrainModelLabel, 5 );
    Button moveUpBtn = new Button( elevationComposite, SWT.PUSH );
    image_Up = new Image( 
                        elevationComposite.getDisplay(),
                        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                        "icons/elcl16/list_up.gif" ).getImageData() );
    moveUpBtn.setImage( image_Up );
    moveUpBtn.setLayoutData( elevFormData );
    moveUpBtn.addSelectionListener( this.moveUpListener  );
    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationList, 5 );
    elevFormData.top = new FormAttachment( moveUpBtn, 3 );

    Button moveDownBtn = new Button( elevationComposite, SWT.PUSH );
    image_Down = new Image(
                        elevationComposite.getDisplay(),
                        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                        "icons/elcl16/list_down.gif" ).getImageData() );
    moveDownBtn.setImage( image_Down );

    moveDownBtn.setLayoutData( elevFormData );
    moveDownBtn.addSelectionListener( this.moveDownListener  );

    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationList, 5 );
    elevFormData.top = new FormAttachment(moveDownBtn,2);
//    elevFormData.bottom = new FormAttachment( 100, 0 );

    Button showTerrain = new Button( elevationComposite, SWT.PUSH );
//    showTerrain.setText( "Goto Terrain" );
    showTerrain.setToolTipText( "Geländemodell anzeigen und maximieren" );
    image_goToTerrain = new Image( 
                        elevationComposite.getDisplay(), 
                        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                        "icons/elcl16/goTo_Terrain.gif" ).getImageData() );
    showTerrain.setImage( image_goToTerrain );
    showTerrain.setLayoutData( elevFormData );
    showTerrain.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
        ITerrainElevationModel elevationModel = dataModel.getElevationModel();
        if( elevationModel != null )
        {
          dataModel.getMapPanel().setBoundingBox( elevationModel.getBoundingBox() );
        }

      }

    } );
    
    ////delete
    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationList, 5 );
    elevFormData.top = new FormAttachment(showTerrain,2);
    elevFormData.bottom = new FormAttachment( 100, 0 );
    Button deleteTerrain = new Button( elevationComposite, SWT.PUSH );
//  showTerrain.setText( "Goto Terrain" );
    deleteTerrain.setToolTipText( "Geländemodell entfern" );
    
     image_goToTerrain = new Image( 
                      elevationComposite.getDisplay(), 
                      KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                      "icons/elcl16/cut.png" ).getImageData() );
  deleteTerrain.setImage( image_goToTerrain );
  deleteTerrain.setLayoutData( elevFormData );
  deleteTerrain.addSelectionListener( new SelectionAdapter()
  {
    public void widgetSelected( SelectionEvent event )
    {
      try
      {
        deleteElevationModel();
        elevationListTableViewer.refresh();
      }
      catch( Throwable th )
      {
        th.printStackTrace();
      }
    }

  } );

  }

  private final void deleteElevationModel() throws Exception
  {
//    new DeleteNativeTerrainElevationWrapper();
    IFEDiscretisationModel1d2d model1d2d = dataModel.getDiscretisationModel();
    if(model1d2d==null)
    {
      System.out.println("model  is null");
    }
    
    IKalypsoFeatureTheme elevationTheme = dataModel.getElevationTheme();
    if(elevationTheme==null)
    {
      return;
    }
    CommandableWorkspace workspace = elevationTheme.getWorkspace();
    if(workspace==null)
    {
      return;
    }
    
    ITerrainElevationModelSystem modelSystem = 
      dataModel.getElevationModelSystem();
//    IElevationProvider elevationProvider = 
//          dataModel.getElevationModel();
//    if(elevationProvider==null)
//    {
//      elevationProvider = dataModel.getElevationModelSystem();
//      if(elevationProvider == null)
//      {
//        return;
//      }
//    }
    
    ChangeTerrainElevationSystemCommand compositeCommand = 
        new ChangeTerrainElevationSystemCommand(workspace,model1d2d);
    DeleteNativeTerrainElevationWrapper delCmd;
    double elevation;
    
    ISelection selection = elevationListTableViewer.getSelection();
    if(!(selection instanceof IStructuredSelection) )
    {
      return;
    }
    INativeTerrainElevationModelWrapper nativeEleModel;
    for(Object selected:((IStructuredSelection)selection).toList())
    {
      if(selected instanceof INativeTerrainElevationModelWrapper)
      {
        nativeEleModel = (INativeTerrainElevationModelWrapper)selected;
        delCmd = 
          new DeleteNativeTerrainElevationWrapper(
                          modelSystem,nativeEleModel,true);
        delCmd.process();
        compositeCommand.addCommand( delCmd );        
      }
    }
    dataModel.setElevationModel( null );
    elevationListTableViewer.setSelection( 
        new StructuredSelection());
    //just reveal the command to the undo framework
    elevationTheme.postCommand( compositeCommand, null );
    
  }
  private void moveUpSelectionUp()
  {
    ISelection selection = elevationListTableViewer.getSelection();
    if(selection instanceof IStructuredSelection)
    {
      Object firstElement = ((IStructuredSelection)selection).getFirstElement();
      if(firstElement instanceof ITerrainElevationModel)
      {
        IFeatureWrapperCollection<ITerrainElevationModel> elevationModels = 
                                                dataModel.getTerrainElevationModels();
        int i = elevationModels.indexOf( firstElement);
        if(i==-1)
        {
          //not found
          return;
        }
        else if(i==0)
        {
          //first in the list
        }
        else
        {
          int targetPos=i-1;
          ITerrainElevationModel modelToReplace = elevationModels.get( targetPos );
          elevationModels.set( targetPos, (ITerrainElevationModel) firstElement );
          elevationModels.set( i, modelToReplace);
        }
      }
    }
  }
  
  private void moveSelection(int delta)
  {
    ISelection selection = elevationListTableViewer.getSelection();
    if(selection instanceof IStructuredSelection)
    {
      Object firstElement = ((IStructuredSelection)selection).getFirstElement();
      if(firstElement instanceof ITerrainElevationModel)
      {
        IFeatureWrapperCollection<ITerrainElevationModel> elevationModels = 
                                                dataModel.getTerrainElevationModels();
        int i = elevationModels.indexOf( firstElement);
        int targetPos = i+delta;
        int SIZE=elevationModels.size();
        
        if(i<0 || targetPos<0 || targetPos>=SIZE)
        {
          //not found
          return;
        }
        else
        {
          ITerrainElevationModel modelToReplace = elevationModels.get( targetPos );
          elevationModels.set( targetPos, (ITerrainElevationModel) firstElement );
          elevationModels.set( i, modelToReplace);
        }
      }
    }
  }

}
