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

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
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
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.wind.ChangeWindDataSystemCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.wind.DeleteWindDataSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * 
 * @author ig
 * 
 */
public class WindDataModelSystemEditorComponent
{
  protected static class windListLabelProvider extends LabelProvider
  {
    @Override
    public Image getImage( final Object element )
    {
      return null;
    }

    @Override
    public String getText( final Object element )
    {
      if( element instanceof IWindDataModelSystem )
      {
        String lStrName = ((IWindDataModelSystem) element).getName();
        final String name = lStrName;
        if( name != null )
          return name;
        else
          return ((IWindDataModelSystem) element).getGmlID();
      }
      else if( element instanceof IWindDataModel )
      {
        String lStrName = ((IWindDataModel) element).getName();
        String lStrDate = ((IWindDataModel) element).getDateStep().toString();
        final String name = lStrName + " " + lStrDate; //$NON-NLS-1$
        if( name != null )
          return name;
        else
          return ((IWindDataModel) element).getGmlID();
      }
      else
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.0" ) + (element == null ? null : element.getClass()) + "\n\t value=" + element ); //$NON-NLS-1$ //$NON-NLS-2$ 
    }
  }

  private TableViewer m_windListViewer;

  private TableViewer m_windSystemListViewer;

  protected WindDataWidgetDataModel m_dataModel;

  private Group m_descriptionGroupText;

  private Text m_descriptionText;

  private Feature[] m_arrInternalWindSystemsFeatures;

  private IWindModel m_windModel;

  public WindDataModelSystemEditorComponent( )
  {
  }

  public void createControl( final WindDataWidgetDataModel dataModel, final Composite parent )
  {
    m_dataModel = dataModel;
    setInternalWindSystemsFeaturesList();
    guiCreateSelectWindModel( parent );
  }

  void setInternalWindSystemsFeaturesList( )
  {
    m_windModel = (IWindModel) m_dataModel.getData( IWindModel.class.toString() );
    m_arrInternalWindSystemsFeatures = new Feature[m_windModel.getWindDataModelSystems().size()];
    List<IWindDataModelSystem> lListSystems = m_windModel.getWindDataModelSystems();
    Collections.sort( lListSystems, new Comparator<IWindDataModelSystem>()
    {
      public int compare( IWindDataModelSystem o1, IWindDataModelSystem o2 )
      {
        try
        {
          return o1.getOrder() - o2.getOrder();
        }
        catch( Exception e )
        {
          return -1;
        }
      }
    } );
    try
    {
      int i = 0;
      for( final IWindDataModelSystem lWindDataModelSystem : lListSystems )
      {
        lWindDataModelSystem.getFeature();
        lWindDataModelSystem.setOrder( i );
        m_arrInternalWindSystemsFeatures[i++] = lWindDataModelSystem.getFeature();
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private void guiCreateSelectWindModel( final Composite windComposite )
  {
    PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();

    FormData windSystemFormData = new FormData();
    windSystemFormData.left = new FormAttachment( 0, 5 );
    windSystemFormData.top = new FormAttachment( 0, 5 );

    final Label windSystemModelLabel = new Label( windComposite, SWT.NONE );

    windSystemModelLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.1" ) ); //$NON-NLS-1$ 
    windSystemModelLabel.setLayoutData( windSystemFormData );
    //
    m_windSystemListViewer = new TableViewer( windComposite, SWT.FILL | SWT.BORDER | SWT.SCROLL_PAGE );
    final Table windSystemListTable = m_windSystemListViewer.getTable();
    m_windSystemListViewer.setContentProvider( new IStructuredContentProvider()
    {
      public Object[] getElements( final Object inputElement )
      {
        if( !(inputElement instanceof IWindModel) )
          return new Object[] {};

        final IWindModel lWindModel = (IWindModel) inputElement;
        final IFeatureWrapperCollection<IWindDataModelSystem> lWindSystems = lWindModel.getWindDataModelSystems();
        if( lWindSystems == null )
          return new Object[] {};

        return lWindSystems.toArray();
      }

      public void dispose( )
      {
      }

      public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
      {
      }
    } );

    windSystemFormData = new FormData();
    windSystemFormData.left = new FormAttachment( 0, 0 );
    windSystemFormData.top = new FormAttachment( windSystemModelLabel, 5 );
    windSystemFormData.height = 110;

    m_windSystemListViewer.setLabelProvider( new windListLabelProvider() );
    windSystemListTable.setLinesVisible( true );
    windSystemListTable.setLayoutData( windSystemFormData );

    final TableColumn lineSystemColumn = new TableColumn( windSystemListTable, SWT.LEFT );
    lineSystemColumn.setWidth( 120 );

    final IWindModel lWindModel = (IWindModel) m_dataModel.getData( IWindModel.class.toString() );
    m_windSystemListViewer.setInput( lWindModel );

    m_windSystemListViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleWindSystemSelectionChanged( (IStructuredSelection) event.getSelection() );
      }

    } );

    windSystemFormData = new FormData();
    windSystemFormData.left = new FormAttachment( windSystemListTable, 5 );
    windSystemFormData.top = new FormAttachment( windSystemModelLabel, 5 );
    final Button buttonMoveUp = new Button( windComposite, SWT.PUSH );

    final Image moveUpImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_MOVE_UP );
    buttonMoveUp.setImage( moveUpImage );

    buttonMoveUp.setLayoutData( windSystemFormData );
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
    windSystemFormData = new FormData();
    windSystemFormData.left = new FormAttachment( windSystemListTable, 5 );
    windSystemFormData.top = new FormAttachment( buttonMoveUp, 3 );

    final Button buttonMoveDown = new Button( windComposite, SWT.PUSH );

    final Image moveDownImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_MOVE_DOWN );
    buttonMoveDown.setImage( moveDownImage );

    buttonMoveDown.setLayoutData( windSystemFormData );
    buttonMoveDown.addSelectionListener( new SelectionAdapter()
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
    FormData windFormData = new FormData();
    windFormData.left = new FormAttachment( 0, 0 );
    windFormData.top = new FormAttachment( windSystemListTable, 5 );

    final Label windModelLabel = new Label( windComposite, SWT.NONE );

    windModelLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.2" ) ); //$NON-NLS-1$ 
    windModelLabel.setLayoutData( windFormData );

    windFormData = new FormData();
    windFormData.left = new FormAttachment( 0, 0 );
    windFormData.top = new FormAttachment( windModelLabel, 5 );
    windFormData.height = 110;

    m_windListViewer = new TableViewer( windComposite, SWT.FILL | SWT.BORDER | SWT.SCROLL_PAGE );
    final Table windListTable = m_windListViewer.getTable();
    m_windListViewer.setContentProvider( new IStructuredContentProvider()
    {
      public Object[] getElements( final Object inputElement )
      {
        if( !(inputElement instanceof IWindDataModelSystem) )
          return new Object[] {};

        final IWindDataModelSystem lWindSystem = (IWindDataModelSystem) inputElement;
        final IFeatureWrapperCollection<IWindDataModel> lWindModels = lWindSystem.getWindDataModels();
        if( lWindModels == null )
          return new Object[] {};

        return lWindModels.toArray();
      }

      public void dispose( )
      {
      }

      public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
      {
      }
    } );

    m_windListViewer.setLabelProvider( new windListLabelProvider() );
    windListTable.setLinesVisible( true );
    windListTable.setLayoutData( windFormData );

    final TableColumn lineColumn = new TableColumn( windListTable, SWT.LEFT );
    lineColumn.setWidth( 120 );

    m_windListViewer.setInput( m_dataModel.getWindDataModelSystem() );

    m_windListViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection) event.getSelection() );
      }
    } );

    windFormData = new FormData();
    windFormData.left = new FormAttachment( windSystemListTable, 5 );
    windFormData.top = new FormAttachment( buttonMoveDown, 5 );

    final Button buttonShowActualWindBounding = new Button( windComposite, SWT.PUSH );

    buttonShowActualWindBounding.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.3" ) ); //$NON-NLS-1$ 

    final Image showWindImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_SHOW );

    buttonShowActualWindBounding.setImage( showWindImage );
    buttonShowActualWindBounding.setLayoutData( windFormData );
    buttonShowActualWindBounding.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        final IWindDataModelSystem lWindModelSystem = m_dataModel.getWindDataModelSystem();
        if( lWindModelSystem != null )
        {
          try
          {
            m_dataModel.getMapPanel().setBoundingBox( lWindModelSystem.getGridDescriptor().getGM_Envelope( lWindModelSystem.getGridDescriptor().getCoordinateSystem() ) );
          }
          catch( Exception e )
          {
            e.printStackTrace();
          }
        }
      }

    } );

    // delete
    windFormData = new FormData();
    windFormData.left = new FormAttachment( windSystemListTable, 5 );
    windFormData.top = new FormAttachment( buttonShowActualWindBounding, 0 );
    final Button buttonDeleteWind = new Button( windComposite, SWT.PUSH );

    buttonDeleteWind.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.4" ) ); //$NON-NLS-1$ 

    final Image deleteImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_DELETE );

    buttonDeleteWind.setImage( deleteImage );
    buttonDeleteWind.setLayoutData( windFormData );
    buttonDeleteWind.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        deleteWindModelSystem( event.display.getActiveShell() );
        m_dataModel.getMapPanel().repaintMap();
        setInternalWindSystemsFeaturesList();
        handleWindSystemSelectionChanged( null );
      }
    } );

    m_descriptionGroupText = new Group( windComposite, SWT.SCROLL_PAGE );
    m_descriptionGroupText.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.5" ) );//$NON-NLS-1$ 

    windFormData = new FormData();
    windFormData.left = new FormAttachment( 0, 0 );
    windFormData.top = new FormAttachment( windListTable, 10 );
    windFormData.height = 82;
    windFormData.width = 122;
    m_descriptionGroupText.setLayoutData( windFormData );

    final FormLayout formDescription = new FormLayout();
    m_descriptionGroupText.setLayout( formDescription );
    m_descriptionText = new Text( m_descriptionGroupText, SWT.MULTI | SWT.SCROLL_LINE );
    m_descriptionText.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.6" ) ); //$NON-NLS-1$ 

    final FormData formDescripData = new FormData();
    formDescripData.left = new FormAttachment( 0, 0 );
    formDescripData.top = new FormAttachment( windListTable, 10 );
    formDescripData.height = 70;
    formDescripData.width = 120;
    m_descriptionText.setLayoutData( formDescripData );

  }

  public void handleWindSystemSelectionChanged( final IStructuredSelection selection )
  {
    try
    {
      if( selection == null || selection.isEmpty() )
      {
        m_dataModel.setWindDataModelSystem( null );
        m_dataModel.setWindDataModel( null );
        m_descriptionText.setText( "" ); //$NON-NLS-1$
        m_windSystemListViewer.setInput( m_dataModel.getData( IWindModel.class.toString() ) );
      }
      else
      {
        if( selection.getFirstElement() instanceof IWindDataModelSystem )
        {
          final IWindDataModelSystem firstElement = (IWindDataModelSystem) selection.getFirstElement();
          m_dataModel.setWindDataModelSystem( firstElement );
          if( firstElement.getWindDataModels().size() > 0 )
            m_dataModel.setWindDataModel( firstElement.getWindDataModels().get( 0 ) );
          m_windListViewer.setInput( m_dataModel.getWindDataModelSystem() );
        }
      }
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }
  }

  public void handleSelectionChanged( final IStructuredSelection selection )
  {
    try
    {
      if( selection == null || selection.isEmpty() )
      {
        m_dataModel.setWindDataModel( null );
        m_descriptionText.setText( "" ); //$NON-NLS-1$
      }
      else
      {
        if( selection.getFirstElement() instanceof IWindDataModel )
        {
          final IWindDataModel firstElement = (IWindDataModel) selection.getFirstElement();
          m_dataModel.setWindDataModel( firstElement );
          IWindDataModelSystem lWindSystemParent = (IWindDataModelSystem) firstElement.getFeature().getParent().getAdapter( IWindDataModelSystem.class );
          m_descriptionText.setText( firstElement.getDateStep().toString() + "\n" + lWindSystemParent.getDescription() );//$NON-NLS-1$
          m_descriptionText.setToolTipText( lWindSystemParent.getDescription() + "\n" + firstElement.getDateStep().toString() );//$NON-NLS-1$
          m_descriptionText.redraw();
          refreshActualWindView( firstElement.getWindDataModelSystem(), firstElement );
        }
      }
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }
  }

  @SuppressWarnings("deprecation")
  public void refreshActualWindView( final IWindDataModelSystem pWindDataModelSystem, final IWindDataModel pWindDataModel )
  {
    WindDataWidgetDataModel.setSelectedWindSystem( pWindDataModelSystem );
    WindDataWidgetDataModel.setActualWindDataModel( pWindDataModelSystem.getGmlID(), pWindDataModel );

    m_dataModel.getWindTheme().getWorkspace().fireModellEvent( new FeatureStructureChangeModellEvent( m_dataModel.getWindTheme().getWorkspace(), ((IWindModel) m_dataModel.getData( IWindModel.class.toString() )).getFeature(), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_MOVE ) );

    m_dataModel.getMapPanel().repaintMap();
  }

  final void deleteWindModelSystem( final Shell shell )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_windSystemListViewer.getSelection();
    if( selection.isEmpty() )
      return;

    if( !MessageDialog.openConfirm( shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.7" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.8" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return;

    final IKalypsoFeatureTheme windTheme = m_dataModel.getWindTheme();
    if( windTheme == null )
      return;

    final CommandableWorkspace workspace = windTheme.getWorkspace();
    if( workspace == null )
      return;

    final IWindDataModelSystem modelSystem = m_dataModel.getWindDataModelSystem();
    final IWindModel lWindModel = (IWindModel) m_dataModel.getData( IWindModel.class.toString() );
    final ChangeWindDataSystemCommand compositeCommand = new ChangeWindDataSystemCommand( workspace, lWindModel );

    final DeleteWindDataSystem delCmd = new DeleteWindDataSystem( lWindModel, modelSystem, true );
    compositeCommand.addCommand( delCmd );

    m_dataModel.setWindDataModel( null );
    m_dataModel.setWindDataModelSystem( null );
    m_windSystemListViewer.setSelection( new StructuredSelection() );
    m_windListViewer.setSelection( new StructuredSelection() );

    final TableViewer windListTableViewer = m_windListViewer;
    /* Also refresh table AFTER models have been deleted */
    windTheme.postCommand( compositeCommand, new Runnable()
    {
      public void run( )
      {
        ViewerUtilities.refresh( windListTableViewer, true );
      }
    } );

    MultiStatus deleteFiles = new MultiStatus( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), IStatus.OK, "", null ); //$NON-NLS-1$
    if( deleteFiles.isOK() )
    {
      try
      {
        m_dataModel.saveModels();
        deleteFiles.add( compositeCommand.deleteFiles() );
      }
      catch( Exception e )
      {
        deleteFiles.add( new MultiStatus( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), 1, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeWindDataSystemCommand.4" ), null ) );
      }
    }
    ErrorDialog.openError( shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.7" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.WindDataModelSystemEditorComponent.9" ), deleteFiles ); //$NON-NLS-1$ //$NON-NLS-2$

  }

  protected void moveSelection( final int delta )
  {
    final ISelection selection = m_windSystemListViewer.getSelection();
    if( selection instanceof IStructuredSelection )
    {
      final Object firstElement = ((IStructuredSelection) selection).getFirstElement();
      if( firstElement instanceof IWindDataModelSystem )
      {
        final IFeatureWrapperCollection<IWindDataModelSystem> windModels = m_windModel.getWindDataModelSystems();
        if( windModels == null )
          return;

        final int i = windModels.indexOf( firstElement );
        final int targetPos = i + delta;
        final int SIZE = windModels.size();

        if( i < 0 || targetPos < 0 || targetPos >= SIZE )
        {
          // not found
          return;
        }
        else
        {
          final IWindDataModelSystem modelToReplace = windModels.get( targetPos );
          modelToReplace.setOrder( i );
          ((IWindDataModelSystem) firstElement).setOrder( targetPos );

          m_dataModel.setWindDataModelSystem( (IWindDataModelSystem) firstElement );

          setInternalWindSystemsFeaturesList();
        }
      }
    }
    m_windSystemListViewer.refresh();

    final IKalypsoFeatureTheme windTheme = m_dataModel.getWindTheme();
    if( windTheme == null )
      return;

    final CommandableWorkspace workspace = windTheme.getWorkspace();
    if( workspace == null )
      return;

    final IWindModel lWindModel = (IWindModel) m_dataModel.getData( IWindModel.class.toString() );
    final ChangeWindDataSystemCommand compositeCommand = new ChangeWindDataSystemCommand( workspace, lWindModel );
    final TableViewer windListTableViewer = m_windListViewer;
    windTheme.postCommand( compositeCommand, new Runnable()
    {
      public void run( )
      {
        ViewerUtilities.refresh( windListTableViewer, true );
      }
    } );

    try
    {
      m_dataModel.saveModels();
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }

    refreshActualWindView( m_dataModel.getWindDataModelSystem(), m_dataModel.getWindDataModel() );
  }

}
