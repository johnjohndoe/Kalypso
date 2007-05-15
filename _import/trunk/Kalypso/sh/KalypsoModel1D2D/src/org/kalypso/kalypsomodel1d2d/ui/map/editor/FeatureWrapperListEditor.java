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
package org.kalypso.kalypsomodel1d2d.ui.map.editor;

import java.util.List;

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
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.DeleteNativeTerrainElevationWrapper;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.sort.IEnvelopeProvider;

/**
 * 
 * @author Madanagopal
 */
public class FeatureWrapperListEditor
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
  
  private KeyBasedDataModel dataModel;
  
  private Image imageGoToTerrain;

  private Image imageDown;

  private Image imageUp;
  private FormToolkit toolkit;
  private Composite parent;
  
  /**
   * The id for the selection in the data model
   */
  private final String idSselection;
  
  /**
   * The id for the input in the data model
   */
  private final String idInput;
  
  private final String idMapPanel;
  
  private FeatureWrapperListInputProvider inputProvider;
  
  private IEnvelopeProvider selectionEnvelopeProvider;
  
  final String mainGroupTitle = "Bitte Höhenmodell auswählen";
  final String bTextMaximizeSelected = "Geländemodell anzeigen und maximieren";
  final String deleteSelected = "Geländemodell löschen";
  final String defaultTestDecription = "Wählen Sie ein Modell aus.";
  final String titleDescriptionGroup = "Beschreibung";
 
  
  final private SelectionListener moveUpListener = 
    new SelectionAdapter()
  {
    public void widgetSelected( SelectionEvent event )
    {
      
      System.out.println("MoveUp:"+elevationListTableViewer.getSelection());
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
        if( selection.getFirstElement() == null )
          throw new NullPointerException( "Null Value while selection.getFirstElement() :" + selection.getFirstElement() );
        else
        {
          if( selection.getFirstElement() instanceof ITerrainElevationModel )
          {
            ITerrainElevationModel firstElement = (ITerrainElevationModel) selection.getFirstElement();
            dataModel.setData( idSselection, firstElement );
            descriptionText.setText( firstElement.getDescription());
            descriptionText.redraw();
          }
        }
      }
      catch( Throwable th )
      {
        th.printStackTrace();
      }

    }
  } ;
  private Label descriptionLabel;
  private Group descriptionGroupText;
  private Text descriptionText;  
  
  public FeatureWrapperListEditor(
                                  String selectionID, 
                                  String inputID, 
                                  String mapPanelID )
  {
    this.idSselection = selectionID;
    this.idInput= inputID;
    this.idMapPanel = mapPanelID;
  }
  
  public void createControl(
      KeyBasedDataModel dataModel,
      FormToolkit toolkit,
      Composite parent )
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
    terrainModelLabel.setText( mainGroupTitle );
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
    
    //get and set inputs
//    ITerrainElevationModelSystem elevationModelSystem = dataModel.getElevationModelSystem();
//    if( elevationModelSystem == null )
//    {
//      elevationListTableViewer.setInput( new Object[] {} );
//    }
//    else
//    {
//      
//      IFeatureWrapperCollection<ITerrainElevationModel> terrainElevationModels = elevationModelSystem.getTerrainElevationModels();
//      if( terrainElevationModels == null )
//      {
//        elevationListTableViewer.setInput( new Object[] {} );
//      }
//      else
//      {
//        System.out.println( "SIZE : " + terrainElevationModels.size() );
//        elevationListTableViewer.setInput( terrainElevationModels/*.toArray()*/ );
//      }
//    }
    Object inputData = dataModel.getData( idInput );
    List<IFeatureWrapper2> featureWrappers = 
          inputProvider.toInputArray( inputData );
    elevationListTableViewer.setInput( featureWrappers );
    elevationListTableViewer.addSelectionChangedListener( this.elevationModelSelectListener);

    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationList, 5 );
    elevFormData.top = new FormAttachment( terrainModelLabel, 5 );
    Button moveUpBtn = new Button( elevationComposite, SWT.PUSH );
    imageUp = new Image( 
                        elevationComposite.getDisplay(),
                        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                        "icons/elcl16/list_up.gif" ).getImageData() );
    moveUpBtn.setImage( imageUp );
    
    moveUpBtn.setLayoutData( elevFormData );
    moveUpBtn.addSelectionListener( this.moveUpListener  );
    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationList, 5 );
    elevFormData.top = new FormAttachment( moveUpBtn, 3 );

    Button moveDownBtn = new Button( elevationComposite, SWT.PUSH );
    imageDown = new Image(
                        elevationComposite.getDisplay(),
                        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                        "icons/elcl16/list_down.gif" ).getImageData() );
    moveDownBtn.setImage( imageDown );

    moveDownBtn.setLayoutData( elevFormData );
    moveDownBtn.addSelectionListener( this.moveDownListener  );

    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationList, 5 );
    elevFormData.top = new FormAttachment(moveDownBtn,2);
//    elevFormData.bottom = new FormAttachment( 100, 0 );

    Button showTerrain = new Button( elevationComposite, SWT.PUSH );
    //    showTerrain.setText( "Goto Terrain" );
    showTerrain.setToolTipText( bTextMaximizeSelected );
    imageGoToTerrain = new Image( 
                        elevationComposite.getDisplay(), 
                        KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                        "icons/elcl16/goTo_Terrain.gif" ).getImageData() );
    showTerrain.setImage( imageGoToTerrain );
    showTerrain.setLayoutData( elevFormData );
    showTerrain.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
//        ITerrainElevationModel elevationModel = dataModel.getElevationModel();
//        if( elevationModel != null )
//        {
//          dataModel.getMapPanel().setBoundingBox( elevationModel.getBoundingBox() );
//        }
        maximizeSelected();
      }

    } );
    
    ////delete
    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationList, 5 );
    elevFormData.top = new FormAttachment(showTerrain,2);
    elevFormData.bottom = new FormAttachment( 100, 0 );
    Button deleteTerrain = new Button( elevationComposite, SWT.PUSH );
    //  showTerrain.setText( "Goto Terrain" );
    deleteTerrain.setToolTipText( deleteSelected );
    
     imageGoToTerrain = new Image( 
                      elevationComposite.getDisplay(), 
                      KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ),
                      "icons/elcl16/remove.gif" ).getImageData() );
  deleteTerrain.setImage( imageGoToTerrain );
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

  descriptionGroupText = new Group(elevationComposite,SWT.NONE);
  descriptionGroupText.setText( titleDescriptionGroup );
  elevFormData = new FormData();
  elevFormData.left = new FormAttachment(moveUpBtn,5);
  elevFormData.top = new FormAttachment(terrainModelLabel,10);
  elevFormData.bottom = new FormAttachment(100,0);
  elevFormData.right = new FormAttachment(100,0);
  descriptionGroupText.setLayoutData(elevFormData);
  
  FormLayout formDescription = new FormLayout();
  descriptionGroupText.setLayout( formDescription);
  descriptionText = new Text(descriptionGroupText,SWT.MULTI|SWT.WRAP);
  descriptionText.setText( defaultTestDecription);
  FormData formDescripData = new FormData();
  formDescripData.left = new FormAttachment(0,0);
  formDescripData.right = new FormAttachment(100,0);
  formDescripData.top = new FormAttachment(0,0);
  formDescripData.bottom = new FormAttachment(100,0);
  Menu menu = descriptionText.getMenu();
  MenuItem saveMenuItem = new MenuItem( menu, SWT.PUSH );
  saveMenuItem.setText( "save" );
  descriptionText.setLayoutData( formDescripData );
  

  }

  protected final void deleteElevationModel() throws Exception
  {
    
    
  }
  
  
  protected void moveSelection(int delta)
  {
    
  }
  
  protected void maximizeSelected()
  {
    
  }

 
}
