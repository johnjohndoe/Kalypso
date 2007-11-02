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
package org.kalypso.model.flood.ui.map;

import java.awt.Color;
import java.awt.Graphics;
import java.util.List;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeControlListener;
import org.kalypso.gml.ui.KalypsoGmlUIPlugin;
import org.kalypso.gml.ui.KalypsoGmlUiImages;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.FeatureComposite;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.ui.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorLabelProvider2;
import org.kalypso.ui.editor.gmleditor.util.command.MoveFeatureCommand;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathSegment;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * A widget with option pane, which allows the user to edit a coverage collection.
 * 
 * @author Thomas Jung
 */
public class EventManagementWidget extends AbstractWidget implements IWidgetWithOptions
{
  private final Runnable m_refreshEventViewerRunnable = new Runnable()
  {
    @SuppressWarnings("synthetic-access")
    public void run( )
    {
      ViewerUtilities.refresh( m_eventViewer, true );
    }
  };

  private TreeViewer m_eventViewer;

  private SzenarioDataProvider m_dataProvider;

  private IFloodModel m_model;

  private Object[] m_treeSelection;

  public EventManagementWidget( )
  {
    super( "Ereignisse verwalten", "Ereignisse verwalten" );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    // prepare for exception
    m_dataProvider = null;
    m_model = null;

    super.activate( commandPoster, mapPanel );

    final IHandlerService service = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
    final IEvaluationContext context = service.getCurrentState();
    final SzenarioDataProvider dataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    try
    {
      final IFloodModel model = dataProvider.getModel( IFloodModel.class );
      if( model != null )
      {
        m_dataProvider = dataProvider;
        m_model = model;
      }
    }
    catch( final CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final ScrolledComposite sc = new ScrolledComposite( parent, SWT.H_SCROLL | SWT.V_SCROLL );
    sc.setExpandHorizontal( true );
    sc.setExpandVertical( true );
    final Composite panel = toolkit.createComposite( sc );
    panel.setLayout( new TableWrapLayout() );
    sc.setContent( panel );
    parent.addControlListener( new ScrolledCompositeControlListener( sc ) );

    // Basic Layout

    /* Tree table + info pane */
    final Composite treePanel = toolkit.createComposite( panel, SWT.NONE );
    final TableWrapLayout treePanelLayout = new TableWrapLayout();
    treePanel.setLayoutData( new TableWrapData( TableWrapData.FILL_GRAB, TableWrapData.FILL_GRAB ) );
    treePanelLayout.numColumns = 2;
    treePanelLayout.rightMargin = 0;
    treePanelLayout.topMargin = 0;
    treePanelLayout.bottomMargin = 0;
    treePanelLayout.leftMargin = 0;
    treePanel.setLayout( treePanelLayout );

    m_eventViewer = new TreeViewer( treePanel, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL );
    m_eventViewer.getControl().setLayoutData( new TableWrapData( TableWrapData.FILL_GRAB, TableWrapData.FILL ) );
    toolkit.adapt( m_eventViewer.getControl(), true, true );

    final Composite treeButtonPanel = toolkit.createComposite( treePanel );
    final FillLayout treeButtonPanelLayout = new FillLayout( SWT.VERTICAL );
    treeButtonPanelLayout.spacing = 4;
    treeButtonPanel.setLayout( treeButtonPanelLayout );
    // coverageButtonPanel.setLayout( new ColumnLayout() );
    treeButtonPanel.setLayoutData( new TableWrapData( TableWrapData.FILL, TableWrapData.FILL_GRAB ) );

    /* Info view */
    final Group coverageInfoGroup = new Group( panel, SWT.NONE );
    coverageInfoGroup.setLayout( new GridLayout() );
    coverageInfoGroup.setLayoutData( new TableWrapData( TableWrapData.FILL_GRAB, TableWrapData.FILL_GRAB ) );
    toolkit.adapt( coverageInfoGroup );
    coverageInfoGroup.setText( "Info" );

    final CachedFeatureviewFactory featureviewFactory = new CachedFeatureviewFactory( new FeatureviewHelper() );
    featureviewFactory.addView( getClass().getResource( "resources/event.gft" ) );
    final FeatureComposite featureComposite = new FeatureComposite( null, null, featureviewFactory );
    featureComposite.setFormToolkit( toolkit );
    featureComposite.addChangeListener( new IFeatureChangeListener()
    {
      public void featureChanged( final ICommand changeCommand )
      {
        try
        {
          m_dataProvider.postCommand( IFloodModel.class, changeCommand );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }

      public void openFeatureRequested( final Feature feature, final IPropertyType pt )
      {
      }
    } );

    /* Color Map table */
    final Composite createColorMapPanel = toolkit.createComposite( panel, SWT.BORDER );
    createColorMapPanel.setLayoutData( new TableWrapData( TableWrapData.FILL_GRAB, TableWrapData.FILL_GRAB ) );

    final Composite colormapPanel = toolkit.createComposite( panel, SWT.BORDER );
    final TableWrapLayout colormapPanelLayout = new TableWrapLayout();
    colormapPanelLayout.numColumns = 2;
    colormapPanelLayout.makeColumnsEqualWidth = false;
    colormapPanel.setLayout( colormapPanelLayout );
    colormapPanel.setLayoutData( new TableWrapData( TableWrapData.FILL_GRAB, TableWrapData.FILL_GRAB ) );

    final TableViewer colorMapTableViewer = new TableViewer( colormapPanel, SWT.BORDER );
    colorMapTableViewer.getControl().setLayoutData( new TableWrapData( TableWrapData.FILL_GRAB, TableWrapData.FILL_GRAB ) );
    toolkit.adapt( colorMapTableViewer.getControl(), true, true );

    final Composite colormapPanelButtonPanel = toolkit.createComposite( colormapPanel, SWT.BORDER );

    /* Fill contents */
    initalizeEventViewer( m_eventViewer );
    initalizeTreeActions( toolkit, treeButtonPanel );

    /* Hook Events */
    m_eventViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        m_treeSelection = selection.toArray();

        featureComposite.disposeControl();

        if( m_treeSelection != null && m_treeSelection.length > 0 )
        {
          featureComposite.setFeature( (Feature) m_treeSelection[0] );
          featureComposite.createControl( coverageInfoGroup, SWT.NONE );
          parent.layout( true, true );
        }

        sc.setMinSize( panel.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );

        getMapPanel().repaint();
      }
    } );

    sc.setMinSize( panel.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );

    return panel;
  }

  private void initalizeEventViewer( final StructuredViewer viewer )
  {
    final GMLContentProvider contentProvider = new GMLContentProvider( false );

    viewer.setContentProvider( contentProvider );
    viewer.setLabelProvider( new GMLEditorLabelProvider2() );

    if( m_model != null )
    {
      viewer.setInput( m_model.getWrappedFeature().getWorkspace() );

      final GMLXPathSegment segment = GMLXPathSegment.forQName( IFloodModel.QNAME );
      final GMLXPath pathToModel = new GMLXPath( segment );
      final GMLXPath rootPath = new GMLXPath( pathToModel, IFloodModel.QNAME_PROP_EVENT_MEMBER );
      contentProvider.setRootPath( rootPath );
    }
  }

  private void initalizeTreeActions( final FormToolkit toolkit, final Composite parent )
  {
    // We are reusing images of KalypsoGmlUi here
    final ImageDescriptor addID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_ADD );
    final ImageDescriptor upID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_UP );
    final ImageDescriptor removeID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_REMOVE );
    final ImageDescriptor downID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_DOWN );
    final ImageDescriptor jumptoID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_JUMP );
    // TODO: new image!
    final ImageDescriptor updateDataID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_JUMP );

    createButton( toolkit, parent, new Action( "Add", addID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleAdd( event );
      }
    } );

    createButton( toolkit, parent, new Action( "Remove", removeID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleRemove( event );
      }
    } );

    createButton( toolkit, parent, new Action( "Move Up", upID )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        handleMove( -1 );
      }
    } );

    createButton( toolkit, parent, new Action( "Move Down", downID )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        handleMove( 1 );
      }
    } );

    createButton( toolkit, parent, new Action( "Jump To", jumptoID )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        handleJumpTo();
      }
    } );

    createButton( toolkit, parent, new Action( "Update Data", updateDataID )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        handleUpdateData();
      }
    } );
  }

  private void createButton( final FormToolkit toolkit, final Composite parent, final IAction action )
  {
    final Button button = toolkit.createButton( parent, null, SWT.PUSH );
    final Image image = action.getImageDescriptor().createImage( true );
    button.setImage( image );
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Event event = new Event();
        event.display = e.display;
        // ...

        action.runWithEvent( event );
      }
    } );

    button.addDisposeListener( new DisposeListener()
    {
      public void widgetDisposed( final DisposeEvent e )
      {
        image.dispose();
      }
    } );
  }

  protected void handleUpdateData( )
  {
    if( m_treeSelection == null )
      return;

    // TODO: collect all selected tins

    // TODO: update all collected tins
  }

  protected void handleJumpTo( )
  {
    if( m_treeSelection == null )
      return;

    final GM_Envelope envelope = envelopeForSelection();
    if( envelope == null )
      return;

    final GM_Envelope scaledBox = GeometryUtilities.scaleEnvelope( envelope, 1.05 );
    getMapPanel().setBoundingBox( scaledBox );
  }

  protected void handleMove( final int step )
  {
    if( m_treeSelection == null )
      return;

    if( m_treeSelection.length != 1 )
      return;

    final Feature selectedFeature = (Feature) m_treeSelection[0];

    final Feature parentFeature = selectedFeature.getParent();
    final IPropertyType pt = selectedFeature.getParentRelation();

    final List featureList = (List) parentFeature.getProperty( pt );
    final int newIndex = featureList.indexOf( selectedFeature ) + step;
    if( newIndex < 0 || newIndex >= featureList.size() )
      return;

    final MoveFeatureCommand command = new MoveFeatureCommand( parentFeature, pt, selectedFeature, step );

    final SzenarioDataProvider sdProvider = m_dataProvider;
    try
    {
      sdProvider.postCommand( IFloodModel.class, command );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  protected void handleAdd( final Event event )
  {
    // TODO
    // final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) getMapPanel().getMapModell().getActiveTheme();
    // theme.postCommand( new EmptyCommand( "", false ), m_refreshCoverageViewerRunnable );
  }

  protected void handleRemove( final Event event )
  {
    if( m_treeSelection == null )
      return;

    try
    {
      for( final Object element : m_treeSelection )
      {
        final Feature featureToRemove = (Feature) element;

        // TODO: delete underlying files/folders/....
        // /* Delete underlying grid grid file */
        // final RectifiedGridCoverage coverage = (RectifiedGridCoverage) m_selectedCoverage;
        // final RangeSetType rangeSet = coverage.getRangeSet();
        // final String fileName = rangeSet.getFile().getFileName();
        // final URL url = new URL( workspace.getContext(), fileName );
        // // TODO : make file from url (see binaryGeoGrid)
        // // TODO: delete file
        // // TODO: but into helper class
        // final IStatus status = CoverageManagmentHelper.deleteGridFile( url );
        // ErrorDialog.openError( event.display.getActiveShell(), "Löschen von Raster-Daten fehlgeschlagen",
        // "Rasterdatei (" + m_selectedCoverage.getName() + ") konnte nicht gelöscht werden.", status );

        // if( status == Status.OK_STATUS )
        {
          /* Delete coverage from collection */
          final Feature parentFeature = featureToRemove.getParent();
          final IRelationType pt = featureToRemove.getParentRelation();

          final CommandableWorkspace workspace = m_dataProvider.getCommandableWorkSpace( IFloodModel.class );

          final DeleteFeatureCommand command = new DeleteFeatureCommand( workspace, parentFeature, pt, featureToRemove );
          workspace.postCommand( command );
          // TODO: should run after command is finished....
          m_refreshEventViewerRunnable.run();
        }
      }
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  private void updateInfoBox( )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    if( m_treeSelection == null )
      return;

    final GM_Envelope envelope = envelopeForSelection();
    if( envelope != null )
    {
      final GM_Position minPoint = getMapPanel().getProjection().getDestPoint( envelope.getMin() );
      final GM_Position maxPoint = getMapPanel().getProjection().getDestPoint( envelope.getMax() );

      final int x = (int) Math.min( minPoint.getX(), maxPoint.getX() );
      final int y = (int) Math.min( minPoint.getY(), maxPoint.getY() );

      final int width = (int) Math.abs( minPoint.getX() - maxPoint.getX() );
      final int height = (int) Math.abs( minPoint.getY() - maxPoint.getY() );

      g.setColor( Color.RED );
      g.drawRect( x, y, width, height );
    }
  }

  private GM_Envelope envelopeForSelection( )
  {
    if( m_treeSelection == null )
      return null;

    GM_Envelope result = null;

    for( final Object selectedObject : m_treeSelection )
    {
      if( selectedObject instanceof Feature )
      {
        final Feature feature = (Feature) selectedObject;
        final GM_Envelope envelope = feature.getEnvelope();
        if( envelope != null )
        {
          if( result == null )
            result = envelope;
          else
            result = result.getMerged( envelope );
        }
      }
    }

    return result;
  }

}
