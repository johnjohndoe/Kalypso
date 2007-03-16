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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
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
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;

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
      ITerrainModel terrainModel = dataModel.getTerrainModel();
      if(terrainModel==null)
      {
        return;
      }
      ITerrainElevationModelSystem terrainElevationModelSystem = 
                        terrainModel.getTerrainElevationModelSystem();
      if(terrainElevationModelSystem==null)
      {
        return;
      }
      IFeatureWrapperCollection<ITerrainElevationModel> terrainElevationModels = 
                                  terrainElevationModelSystem.getTerrainElevationModels();
      Object firstElement = ((IStructuredSelection)elevationListTableViewer.getSelection()).getFirstElement();
      //TODO Patrice check change
      String selectedString = 
            (String)firstElement;////stringRemoveBraces( elevationListTableViewer.getSelection().toString() );
      int selectedStringIndex = terrainElevationModels.indexOf( selectedString );

      if( terrainElevationModels.indexOf( selectedString ) != 0 )
      {
        ITerrainElevationModel destString = terrainElevationModels.get( (terrainElevationModels.indexOf( selectedString ) - 1) );
        terrainElevationModels.remove( destString );
        terrainElevationModels.add( selectedStringIndex, (ITerrainElevationModel) destString );
        System.out.println( selectedString + " ," + destString );
        System.out.println( (terrainElevationModels.indexOf( selectedString )) - 1 + " ," + terrainElevationModels.indexOf( selectedString ) );
      }
      elevationListTableViewer.refresh();
    }

  };
  
  final private SelectionListener moveDownListener = 
    new SelectionAdapter()
  {
    public void widgetSelected( SelectionEvent event )
    {
      //TODO patrice check this also
     String selectedString = 
       (String)((IStructuredSelection)elevationListTableViewer.getSelection()).getFirstElement();//stringRemoveBraces( elevationListTableViewer.getSelection().toString() );
     IFeatureWrapperCollection<ITerrainElevationModel> terrainElevationModels = 
                                                  dataModel.getTerrainElevationModels();
     if(terrainElevationModels==null)
     {
       return;
     }
      int selectedStringIndex = terrainElevationModels.indexOf( selectedString );
      if( terrainElevationModels.indexOf( selectedString ) != terrainElevationModels.size() - 1 )
      {
        ITerrainElevationModel destString = terrainElevationModels.get( (terrainElevationModels.indexOf( selectedString ) + 1) );
        terrainElevationModels.remove( destString );
        terrainElevationModels.add( selectedStringIndex, destString );
      }
      elevationListTableViewer.refresh();
//
    }
  };
  
  final private ISelectionChangedListener elevationModelSelectListener = 
    new ISelectionChangedListener()
  {
    public void selectionChanged( SelectionChangedEvent event )
    {
      IStructuredSelection selection = (IStructuredSelection) event.getSelection();

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
//      areaSelectSection.setEnabled( true );
//      areaSelectSection.setExpanded( true );
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
    terrainModelLabel.setText( "Select the Terrain Model" );
    terrainModelLabel.setLayoutData( elevFormData );

    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( 0, 10 );
    elevFormData.top = new FormAttachment( terrainModelLabel, 5 );
    elevFormData.height = 60;

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
        elevationListTableViewer.setInput( terrainElevationModels.toArray() );
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
    elevFormData.bottom = new FormAttachment( 100, 0 );

    Button showTerrain = new Button( elevationComposite, SWT.PUSH );
//    showTerrain.setText( "Goto Terrain" );
    showTerrain.setToolTipText( "Gel‰ndemodell anzeigen und maximieren" );
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
        int i = elevationModels.indexOf( elevationModels );
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
          elevationModels.set( i, (ITerrainElevationModel) firstElement );
          elevationModels.set( i, (ITerrainElevationModel) firstElement );
        }
      }
    }
  }
  
//  private ListViewer areaViewer;
//  
//  public void setInput( Object input )
//  {
//    Object oldInput = areaViewer.getInput();
//    if( oldInput == input )
//    {
//      areaViewer.refresh();
//    }
//    else
//    {
//      areaViewer.setInput( input );
//
//    }
//  }
}
