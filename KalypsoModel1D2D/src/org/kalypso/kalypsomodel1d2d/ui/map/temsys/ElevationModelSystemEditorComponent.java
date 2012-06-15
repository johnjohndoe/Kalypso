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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.DeleteNativeTerrainElevationWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
/**
 * 
 * @author Madanagopal
 * @author Patrice Congo
 * 
 */
public class ElevationModelSystemEditorComponent
{
  protected static class ElevationListLabelProvider extends LabelProvider
  {
    @Override
    public Image getImage( final Object element )
    {
      return null;
    }

    @Override
    public String getText( final Object element )
    {
      if( element instanceof ITerrainElevationModel )
      {
        final String name = ((ITerrainElevationModel) element).getName();
        if( name != null )
          return name;
        else
          return ((ITerrainElevationModel) element).getGmlID();
      }
      else
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.0" ) + Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.1" ) + (element == null ? null : element.getClass()) + "\n\t value=" + element ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
  }

  /* ======================================================================== */
  private TableViewer m_elevationListViewer;

  protected ApplyElevationWidgetDataModel m_dataModel;

  private Group m_descriptionGroupText;

  private Text m_descriptionText;

  public ElevationModelSystemEditorComponent( )
  {

  }

  public void createControl( final ApplyElevationWidgetDataModel dataModel, final Composite parent )
  {
    m_dataModel = dataModel;
    guiCreateSelectElevationModel( parent );
  }

  private void guiCreateSelectElevationModel( final Composite elevationComposite )
  {
    FormData elevFormData = new FormData();
    elevFormData.left = new FormAttachment( 0, 5 );
    elevFormData.top = new FormAttachment( 0, 5 );
    final Label terrainModelLabel = new Label( elevationComposite, SWT.NONE );

    terrainModelLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.6" ) ); //$NON-NLS-1$
    terrainModelLabel.setLayoutData( elevFormData );

    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( 0, 10 );
    elevFormData.top = new FormAttachment( terrainModelLabel, 5 );
    elevFormData.height = 90;

    m_elevationListViewer = new TableViewer( elevationComposite, SWT.FILL | SWT.BORDER );
    final Table elevationListTable = m_elevationListViewer.getTable();
    m_elevationListViewer.setContentProvider( new IStructuredContentProvider()
    {
      @Override
      public Object[] getElements( final Object inputElement )
      {
        if( !(inputElement instanceof ITerrainElevationModelSystem) )
          return new Object[] {};

        final ITerrainElevationModelSystem tems = (ITerrainElevationModelSystem) inputElement;
        final IFeatureWrapperCollection<ITerrainElevationModel> terrainElevationModels = tems.getTerrainElevationModels();
        if( terrainElevationModels == null )
          return new Object[] {};

        return terrainElevationModels.toArray();
      }

      @Override
      public void dispose( )
      {
      }

      @Override
      public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
      {
      }
    } );
    m_elevationListViewer.setLabelProvider( new ElevationListLabelProvider() );
    elevationListTable.setLinesVisible( true );
    elevationListTable.setLayoutData( elevFormData );

    final TableColumn lineColumn = new TableColumn( elevationListTable, SWT.LEFT );
    lineColumn.setWidth( 100 );

    final ITerrainElevationModelSystem elevationModelSystem = m_dataModel.getElevationModelSystem();
    m_elevationListViewer.setInput( elevationModelSystem );

    m_elevationListViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection) event.getSelection() );
      }
    } );

    PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();

    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationListTable, 5 );
    elevFormData.top = new FormAttachment( terrainModelLabel, 5 );
    final Button buttonMoveUp = new Button( elevationComposite, SWT.PUSH );

    final Image moveUpImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_MOVE_UP );
    buttonMoveUp.setImage( moveUpImage );

    buttonMoveUp.setLayoutData( elevFormData );
    buttonMoveUp.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        moveSelection( -1 );
      }
    } );
    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationListTable, 5 );
    elevFormData.top = new FormAttachment( buttonMoveUp, 3 );

    final Button moveDownBtn = new Button( elevationComposite, SWT.PUSH );

    final Image moveDownImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_MOVE_DOWN );
    moveDownBtn.setImage( moveDownImage );

    moveDownBtn.setLayoutData( elevFormData );
    moveDownBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        moveSelection( 1 );
      }
    } );

    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationListTable, 5 );
    elevFormData.top = new FormAttachment( moveDownBtn, 2 );

    final Button buttonShowTerrain = new Button( elevationComposite, SWT.PUSH );

    buttonShowTerrain.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.10" ) ); //$NON-NLS-1$

    final Image showTerrainImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_SHOW );

    buttonShowTerrain.setImage( showTerrainImage );
    buttonShowTerrain.setLayoutData( elevFormData );
    buttonShowTerrain.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        final ITerrainElevationModel elevationModel = m_dataModel.getElevationModel();
        if( elevationModel != null )
        {
          m_dataModel.getMapPanel().setBoundingBox( elevationModel.getBoundingBox() );
        }
      }

    } );

    // delete
    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( elevationListTable, 5 );
    elevFormData.top = new FormAttachment( buttonShowTerrain, 2 );
    elevFormData.bottom = new FormAttachment( 100, 0 );
    final Button buttonDeleteTerrain = new Button( elevationComposite, SWT.PUSH );

    buttonDeleteTerrain.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.12" ) ); //$NON-NLS-1$

    final Image deleteTerrainImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_DELETE );

    buttonDeleteTerrain.setImage( deleteTerrainImage );
    buttonDeleteTerrain.setLayoutData( elevFormData );
    buttonDeleteTerrain.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        deleteElevationModel( event.display.getActiveShell() );
        m_dataModel.getMapPanel().repaintMap();
      }
    } );

    m_descriptionGroupText = new Group( elevationComposite, SWT.NONE );
    m_descriptionGroupText.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.14" ) ); //$NON-NLS-1$
    elevFormData = new FormData();
    elevFormData.left = new FormAttachment( buttonMoveUp, 5 );
    elevFormData.top = new FormAttachment( terrainModelLabel, 10 );
    elevFormData.bottom = new FormAttachment( 100, 0 );
    elevFormData.right = new FormAttachment( 100, 0 );
    m_descriptionGroupText.setLayoutData( elevFormData );

    final FormLayout formDescription = new FormLayout();
    m_descriptionGroupText.setLayout( formDescription );
    m_descriptionText = new Text( m_descriptionGroupText, SWT.MULTI | SWT.WRAP );
    m_descriptionText.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.15" ) ); //$NON-NLS-1$
    final FormData formDescripData = new FormData();
    formDescripData.left = new FormAttachment( 0, 0 );
    formDescripData.right = new FormAttachment( 100, 0 );
    formDescripData.top = new FormAttachment( 0, 0 );
    formDescripData.bottom = new FormAttachment( 100, 0 );
    m_descriptionText.setLayoutData( formDescripData );

  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    try
    {
      if( selection.isEmpty() )
      {
        m_dataModel.setElevationModel( null );
        m_descriptionText.setText( "" ); //$NON-NLS-1$
      }
      else
      {
        if( selection.getFirstElement() instanceof ITerrainElevationModel )
        {
          final ITerrainElevationModel firstElement = (ITerrainElevationModel) selection.getFirstElement();
          m_dataModel.setElevationModel( firstElement );
          m_descriptionText.setText( firstElement.getDescription() );
          m_descriptionText.redraw();
        }
      }
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }
  }

  private final void deleteElevationModel( final Shell shell )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_elevationListViewer.getSelection();
    if( selection.isEmpty() )
      return;

    if( !MessageDialog.openConfirm( shell, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.17"), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.2") ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return;

    final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getDiscretisationModel();
    if( model1d2d == null )
    {
      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.16" ) ); //$NON-NLS-1$
    }

    final IKalypsoFeatureTheme elevationTheme = m_dataModel.getElevationTheme();
    if( elevationTheme == null )
      return;

    final CommandableWorkspace workspace = elevationTheme.getWorkspace();
    if( workspace == null )
      return;

    final ITerrainElevationModelSystem modelSystem = m_dataModel.getElevationModelSystem();
    final ChangeTerrainElevationSystemCommand compositeCommand = new ChangeTerrainElevationSystemCommand( workspace, model1d2d, modelSystem );

    for( final Object selected : selection.toList() )
    {
      if( selected instanceof INativeTerrainElevationModelWrapper )
      {
        final INativeTerrainElevationModelWrapper nativeEleModel = (INativeTerrainElevationModelWrapper) selected;

        final DeleteNativeTerrainElevationWrapper delCmd = new DeleteNativeTerrainElevationWrapper( modelSystem, nativeEleModel, true );
        compositeCommand.addCommand( delCmd, nativeEleModel.getSourceFile() );
      }
    }
    
    
    m_dataModel.setElevationModel( null );
    m_elevationListViewer.setSelection( new StructuredSelection() );

    final TableViewer elevationListTableViewer = m_elevationListViewer;
    /* Also refresh table AFTER models have been deleted */
    elevationTheme.postCommand( compositeCommand, new Runnable()
    {
      @Override
      public void run( )
      {
        ViewerUtilities.refresh( elevationListTableViewer, true );
      }
    } );
    
    MultiStatus deleteFiles = new MultiStatus( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), IStatus.OK, "", null ); //$NON-NLS-1$
    if( deleteFiles.isOK() ){
      try{
        m_dataModel.saveModels();
        deleteFiles.add( compositeCommand.deleteFiles() );
      }
      catch (Exception e) {
        deleteFiles.add( new MultiStatus( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), 1, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand.4"), null ) );
      }
    }

    ErrorDialog.openError( shell, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.3"), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.4"), deleteFiles ); //$NON-NLS-1$ //$NON-NLS-2$

  }

  protected void moveSelection( final int delta )
  {
    final ISelection selection = m_elevationListViewer.getSelection();
    if( selection instanceof IStructuredSelection )
    {
      final Object firstElement = ((IStructuredSelection) selection).getFirstElement();
      if( firstElement instanceof ITerrainElevationModel )
      {
        final IFeatureWrapperCollection<ITerrainElevationModel> elevationModels = m_dataModel.getTerrainElevationModels();
        if( elevationModels == null )
          return;

        final int i = elevationModels.indexOf( firstElement );
        final int targetPos = i + delta;
        final int SIZE = elevationModels.size();

        if( i < 0 || targetPos < 0 || targetPos >= SIZE )
        {
          // not found
          return;
        }
        else
        {
          final ITerrainElevationModel modelToReplace = elevationModels.get( targetPos );
          elevationModels.set( targetPos, (ITerrainElevationModel) firstElement );
          elevationModels.set( i, modelToReplace );
        }
      }
    }

    m_elevationListViewer.refresh();
  }

}
