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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypsodeegree.model.feature.Feature;

/**
 * 
 * @author Patrice Congo
 * @author Madanagopal
 */
class ApplyElevationWidgetFace
{
  static int index = 0;

  private Composite rootPanel;

  private FormToolkit toolkit;

  private Section elevationSelectStatus;

  private Section areaSelectSection;

  static private IPreferenceStore preferenceStore = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private final IPropertyChangeListener storePropertyChangeListener = createPropertyChangeLis();

  final ApplyElevationWidgetDataModel m_dataModel;

  private Section elevationColorSection;

  private final IFeatureSelectionListener featureSelectionListener = new IFeatureSelectionListener()
  {
    @Override
    public void selectionChanged( final Object source, final IFeatureSelection selection )
    {
      if( m_nodeElevationViewer == null )
        return;

      if( m_nodeElevationViewer.getControl().isDisposed() )
        return;

      final List<IFE1D2DNode> nodeList = new ArrayList<IFE1D2DNode>();
      Feature selecFeature = null;
      IFE1D2DNode selecNode = null;
      for( final Object selected : selection.toList() )
      {
        if( selected instanceof Feature )
          selecFeature = (Feature) selected;

        else if( selected instanceof EasyFeatureWrapper )
          selecFeature = ((EasyFeatureWrapper) selected).getFeature();

        if( selecFeature != null )
        {
          selecNode = (IFE1D2DNode) selecFeature.getAdapter( IFE1D2DNode.class );
          if( selecNode != null )
            nodeList.add( selecNode );
        }

      }

      try
      {
        final IWorkbench workbench = PlatformUI.getWorkbench();

        m_nodeElevationViewer.getControl().getDisplay().syncExec( new Runnable()
        {
          @Override
          public void run( )
          {
            final IContentProvider cp = m_nodeElevationViewer.getContentProvider();
            if( cp instanceof ArrayContentProvider )
              m_nodeElevationViewer.setContentProvider( new ArrayContentProvider() );
            else
              m_nodeElevationViewer.setContentProvider( new ArrayContentProvider() );

            m_nodeElevationViewer.setInput( nodeList.toArray( new IFE1D2DNode[] {} ) );
          }
        } );

        final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
        if( activeWorkbenchWindow == null )
        {
          System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidgetFace.1" ) ); //$NON-NLS-1$
          return;
        }

      }
      catch( final Throwable th )
      {
        th.printStackTrace();
      }
    }

  };

  private ColorModelChangeComponent colorModelChangeComponent;

  private ElevationModelSystemEditorComponent eleSystemEditorComponent;

  public ApplyElevationWidgetFace( final ApplyElevationWidgetDataModel dataModel )
  {
    m_dataModel = dataModel;
  }

  public Control createControl( final Composite parent )
  {
    m_dataModel.getMapPanel().getSelectionManager().addSelectionListener( featureSelectionListener );
    preferenceStore.addPropertyChangeListener( storePropertyChangeListener );
    initStoreDefaults();

    parent.setLayout( new FillLayout() );
    rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );
    toolkit = new FormToolkit( parent.getDisplay() );
    final ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    TableWrapData tableWrapData;

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    // Creates Section for "Select Elevation Model"
    elevationSelectStatus = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    elevationSelectStatus.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidgetFace.2" ) ); //$NON-NLS-1$
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    elevationSelectStatus.setLayoutData( tableWrapData );
    elevationSelectStatus.setExpanded( true );

    // Creates Section for "Select A Region - among the List of Nodes drawn on the Viewer Pane"
    areaSelectSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    areaSelectSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidgetFace.3" ) ); //$NON-NLS-1$
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.align = TableWrapData.FILL_GRAB;
    areaSelectSection.setLayoutData( tableWrapData );
    areaSelectSection.setExpanded( true );
    areaSelectSection.setEnabled( true );

    //
    final Button dtmButton = toolkit.createButton( scrolledForm.getBody(), "Show/Refresh Model-Isolines", SWT.PUSH ); //$NON-NLS-1$
    dtmButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IMapPanel mapPanel = m_dataModel.getMapPanel();
        PreviewModelDtm.showModelDtm( parent.getShell(), mapPanel );
      }
    } );

    // Creates Section to Configure the Color for Different Elevations
    elevationColorSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    elevationColorSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidgetFace.4" ) //$NON-NLS-1$
        // "Select Colors for MAX Elevation and MIN Elevation "
    );
    // elevationColorSection.addPaintListener( drawListener );

    tableWrapData = new TableWrapData();// TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    tableWrapData.heightHint = 282;// 168
    tableWrapData.align = TableWrapData.FILL_GRAB;
    elevationColorSection.setLayoutData( tableWrapData );

    elevationColorSection.setExpanded( false );
    elevationColorSection.setEnabled( true );

    createSelectElevationModel( elevationSelectStatus );
    createSelectRegion( areaSelectSection );
    createSelectColor( elevationColorSection );
    return rootPanel;
  }

  private void createSelectColor( final Section elevationColorConfig )
  {
    elevationColorConfig.setLayout( new GridLayout() );

    final Composite clientComposite = toolkit.createComposite( elevationColorConfig, SWT.FLAT );
    elevationColorConfig.setClient( clientComposite );

    colorModelChangeComponent = new ColorModelChangeComponent();
    colorModelChangeComponent.createControl( m_dataModel, toolkit, clientComposite );
  }

  private final void createSelectElevationModel( final Section workStatusSection )
  {
    workStatusSection.setLayout( new FillLayout() );

    final Composite clientComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( clientComposite );
    // clientComposite.setSize( 400, 300 );
    final FormLayout formLayout = new FormLayout();
    clientComposite.setLayout( formLayout );
    final FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, 5 );
    clientComposite.setLayoutData( formData );
    eleSystemEditorComponent = new ElevationModelSystemEditorComponent();
    eleSystemEditorComponent.createControl( m_dataModel, clientComposite );
  }

  public void disposeControl( )
  {
    preferenceStore.removePropertyChangeListener( storePropertyChangeListener );
    if( rootPanel == null )
    {
      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidgetFace.5" ) ); //$NON-NLS-1$
      return;
    }
    if( !rootPanel.isDisposed() )
    {
      rootPanel.dispose();
      toolkit.dispose();
    }

    final IMapPanel mapPanel = m_dataModel.getMapPanel();
    if( mapPanel != null )
      mapPanel.getSelectionManager().addSelectionListener( featureSelectionListener );

    if( colorModelChangeComponent != null )
      colorModelChangeComponent.dispose();
  }

  public static final String HANDLE_WIDTH_NAME = "x.handleWidth";//$NON-NLS-1$

  private AssignNodeElevationFaceComponent m_assignNodeElevationFaceComponent;

  private TableViewer m_nodeElevationViewer;

  private void initStoreDefaults( )
  {
    if( !preferenceStore.contains( HANDLE_WIDTH_NAME ) )
    {
      preferenceStore.setDefault( HANDLE_WIDTH_NAME, 6 );
      preferenceStore.setValue( HANDLE_WIDTH_NAME, 6 );
    }
  }

  private void createSelectRegion( final Section configSection )
  {
    configSection.setLayout( new FillLayout() );

    final Composite clientComposite = toolkit.createComposite( configSection, SWT.FLAT );
    configSection.setClient( clientComposite );

    final FormLayout selectRegionFormLayout = new FormLayout();
    clientComposite.setLayout( selectRegionFormLayout );

    final FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, 0 );
    clientComposite.setLayoutData( formData );
    // guiCreateSelectRegion(clientComposite);
    m_assignNodeElevationFaceComponent = new AssignNodeElevationFaceComponent();
    m_assignNodeElevationFaceComponent.createControl( m_dataModel, toolkit, clientComposite );

    m_nodeElevationViewer = m_assignNodeElevationFaceComponent.getTableViewer();
  }

  private IPropertyChangeListener createPropertyChangeLis( )
  {
    return new IPropertyChangeListener()
    {

      @Override
      public void propertyChange( final PropertyChangeEvent event )
      {
        final Object source = event.getSource();
        if( source instanceof FieldEditor )
        {
          ((FieldEditor) source).store();
        }
        else if( source instanceof ColorSelector )
        {
        }
        else
        {
          System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
      }

    };
  }
}