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
import java.net.URL;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.viewers.StatusAndDelegateContentProvider;
import org.kalypso.contribs.eclipse.jface.viewers.StatusAndDelegateLabelProvider;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.gml.provider.IGmlSource;
import org.kalypso.gml.ui.KalypsoGmlUIPlugin;
import org.kalypso.gml.ui.KalypsoGmlUiImages;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.flood.KalypsoModelFloodImages;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.ui.map.operations.AddEventOperation;
import org.kalypso.model.flood.ui.map.operations.ImportTinOperation;
import org.kalypso.model.flood.ui.map.operations.RemoveEventOperation;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.FeatureComposite;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.gmleditor.ui.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypso.ui.editor.gmleditor.util.command.MoveFeatureCommand;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.ui.editor.sldEditor.PolygonColorMapContentProvider;
import org.kalypso.ui.editor.sldEditor.PolygonColorMapLabelProvider;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.NamedLayer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
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
  private TreeViewer m_eventViewer;

  protected SzenarioDataProvider m_dataProvider;

  private IFloodModel m_model;

  private Object[] m_treeSelection;

  private TableViewer m_colorMapTableViewer;

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
    // TRICKY: Scrolling behaviour:
    // - vertical: all components have a minimum height and fill the whole area
    // - vertical: if size is too small, a global scroll-bar appears
    // - horizontal: the panel has a minimum widht, if the total area is too small, a global scrollbar appears
    // - horizontal: the components (tree/table) have their additional horizontal scrollbars which appars if the content
    // of the individual control is too big

    final ScrolledComposite sc = new ScrolledComposite( parent, SWT.V_SCROLL | SWT.H_SCROLL );
    sc.setMinWidth( 200 );
    sc.setExpandVertical( true );
    sc.setExpandHorizontal( true );

    final Composite panel = toolkit.createComposite( sc, SWT.NONE );
    panel.setLayout( new GridLayout() );

    sc.setContent( panel );
    parent.addControlListener( new ControlAdapter()
    {
      /**
       * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
       */
      @Override
      public void controlResized( final ControlEvent e )
      {
        final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
        panel.setSize( size );
        sc.setMinHeight( size.y );
      }
    } );

    // Basic Layout

    /* Tree table + info pane */
    final Composite treePanel = toolkit.createComposite( panel, SWT.NONE );
    final GridLayout treePanelLayout = new GridLayout( 2, false );
    final GridData treePanelData = new GridData( SWT.FILL, SWT.FILL, true, false );
    treePanelData.heightHint = 200;
    treePanel.setLayoutData( treePanelData );
    treePanelLayout.marginHeight = 0;
    treePanelLayout.marginWidth = 0;
    treePanel.setLayout( treePanelLayout );

    m_eventViewer = new TreeViewer( treePanel, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );
    final GridData eventViewerData = new GridData( SWT.FILL, SWT.FILL, true, false );
    eventViewerData.heightHint = 100;
    m_eventViewer.getControl().setLayoutData( eventViewerData );
    toolkit.adapt( m_eventViewer.getControl(), true, false );

    final Composite treeButtonPanel = toolkit.createComposite( treePanel );
    final FillLayout treeButtonPanelLayout = new FillLayout( SWT.VERTICAL );
    treeButtonPanelLayout.spacing = 4;
    treeButtonPanel.setLayout( treeButtonPanelLayout );
    treeButtonPanel.setLayoutData( new GridData( SWT.CENTER, SWT.BEGINNING, false, true ) );

    /* Info view */
    final Group coverageInfoGroup = new Group( panel, SWT.H_SCROLL );
    coverageInfoGroup.setLayout( new GridLayout() );
    final GridData infoGroupData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    coverageInfoGroup.setLayoutData( infoGroupData );
    toolkit.adapt( coverageInfoGroup );
    coverageInfoGroup.setText( "Info" );

    final CachedFeatureviewFactory featureviewFactory = new CachedFeatureviewFactory( new FeatureviewHelper() );
    featureviewFactory.addView( getClass().getResource( "resources/event.gft" ) );
    featureviewFactory.addView( getClass().getResource( "resources/tinReference.gft" ) );
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
    final Composite colormapPanel = toolkit.createComposite( panel, SWT.NONE );
    final GridLayout colormapPanelLayout = new GridLayout();
    colormapPanelLayout.numColumns = 2;
    colormapPanelLayout.makeColumnsEqualWidth = false;
    colormapPanelLayout.marginWidth = 0;
    colormapPanelLayout.marginHeight = 0;

    colormapPanel.setLayout( colormapPanelLayout );
    final GridData colormapPanelData = new GridData( SWT.FILL, SWT.FILL, true, true );
    colormapPanel.setLayoutData( colormapPanelData );

    m_colorMapTableViewer = new TableViewer( colormapPanel, SWT.BORDER | SWT.H_SCROLL );
    final GridData colormapTableData = new GridData( SWT.FILL, SWT.FILL, true, true );

    m_colorMapTableViewer.getControl().setLayoutData( colormapTableData );
    toolkit.adapt( m_colorMapTableViewer.getControl(), true, true );

    final Composite colormapPanelButtonPanel = toolkit.createComposite( colormapPanel, SWT.NONE );
    final GridLayout colormapButtonPanelLayout = new GridLayout();
    colormapButtonPanelLayout.marginHeight = 0;
    colormapButtonPanelLayout.marginWidth = 0;
    colormapPanelButtonPanel.setLayout( colormapButtonPanelLayout );
    colormapPanelButtonPanel.setLayoutData( new GridData( SWT.CENTER, SWT.BEGINNING, false, false ) );

    /* Fill contents */
    initalizeEventViewer( m_eventViewer );
    initalizeTreeActions( toolkit, treeButtonPanel );

    initializeColorMapTableViewer( m_colorMapTableViewer );
    initalizeColorMapActions( toolkit, colormapPanelButtonPanel );

    /* Hook Events */
    m_eventViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @SuppressWarnings("synthetic-access")
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        m_treeSelection = selection.toArray();

        featureComposite.disposeControl();

        updateStylePanel( m_colorMapTableViewer );

        if( m_treeSelection != null && m_treeSelection.length > 0 )
        {
          featureComposite.setFeature( (Feature) m_treeSelection[0] );
          featureComposite.createControl( coverageInfoGroup, SWT.NONE );
        }

        parent.layout( true, true );

        final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
        panel.setSize( size );
        sc.setMinHeight( size.y );

        getMapPanel().repaint();
      }
    } );

    final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    panel.setSize( size );
    sc.setMinHeight( size.y );
    // sc.setMinSize( panel.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );

    return panel;
  }

  private void initalizeColorMapActions( final FormToolkit toolkit, final Composite parent )
  {
    // We are reusing images of KalypsoGmlUi here
    final ImageDescriptor generateID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_ADD );

    createButton( toolkit, parent, new Action( "Generate ColorMap", generateID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleGenerateColorMap( event );
      }
    } );

  }

  protected void handleGenerateColorMap( final Event event )
  {
    // open colorMap dialog
    final PolygonColorMap input = (PolygonColorMap) m_colorMapTableViewer.getInput();
    if( input != null )
    {
      final EventStyleDialog dialog = new EventStyleDialog( event.display.getActiveShell(), input );
      if( dialog.open() == Window.OK )
      {
        m_colorMapTableViewer.refresh();

        // TODO: save sld-file

        m_colorMapTableViewer.getControl().getParent().getParent().layout( true, true );
      }
    }
  }

  protected void updateStylePanel( final TableViewer viewer )
  {
    final IRunoffEvent event = getCurrentEvent();
    if( event == null )
    {
      viewer.setInput( StatusUtilities.createInfoStatus( "Keine Ereignis ausgewählt. Wählen Sie ein Ereignis in der ereignisliste, um die Darstellung zu editieren." ) );
      return;
    }

    /* get sld from event folder */
    final IFile styleFile = getSldFile( event );
    final PolygonColorMap colorMap = findColorMap( styleFile );

    viewer.setInput( colorMap );
  }

  private PolygonColorMap findColorMap( final IFile styleFile )
  {
    try
    {
      final StyledLayerDescriptor sld = SLDFactory.createSLD( ResourceUtilities.createURL( styleFile ) );
      final NamedLayer wspLayer = sld.getNamedLayer( "wspLayer" );
      final UserStyle style = (UserStyle) wspLayer.getStyle( "wspUserStyle" );
      final FeatureTypeStyle wspFts = style.getFeatureTypeStyle( "wspFts" );
      final Rule wspRule = wspFts.getRule( "wspRule" );
      final SurfacePolygonSymbolizer symb = (SurfacePolygonSymbolizer) wspRule.getSymbolizers()[0];
      return symb.getColorMap();
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();

      return null;
    }
  }

  public static IFile getSldFile( final IRunoffEvent event )
  {
    final IFolder eventFolder = getEventFolder( event );
    return eventFolder.getFile( "wsp.sld" );
  }

  public static IFolder getEventFolder( final IRunoffEvent event )
  {
    final IFolder eventsFolder = getEventsFolder();
    return eventsFolder.getFolder( event.getDataPath() );
  }

  public static IFolder getEventsFolder( )
  {
    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();
    return scenarioFolder.getFolder( "events" );
  }

  private IRunoffEvent getCurrentEvent( )
  {
    if( m_treeSelection == null || m_treeSelection.length == 0 )
      return null;

    final Feature feature = (Feature) m_treeSelection[0];

    final IRunoffEvent event = (IRunoffEvent) feature.getAdapter( IRunoffEvent.class );
    if( event != null )
      return event;

    final Feature parent = feature.getParent();
    if( parent == null )
      return null;

    return (IRunoffEvent) parent.getAdapter( IRunoffEvent.class );
  }

  private void initializeColorMapTableViewer( final TableViewer viewer )
  {
    viewer.setContentProvider( new PolygonColorMapContentProvider() );
    viewer.setLabelProvider( new PolygonColorMapLabelProvider( viewer ) );

    final Table viewerTable = viewer.getTable();
    viewerTable.setLinesVisible( true );
    viewerTable.setHeaderVisible( true );
    viewerTable.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  private void initalizeEventViewer( final StructuredViewer viewer )
  {
    final GMLContentProvider gmlcp = new GMLContentProvider( false );
    final IContentProvider cp = new StatusAndDelegateContentProvider( gmlcp );
    final ILabelProvider lp = new StatusAndDelegateLabelProvider( new GMLLabelProvider() );

    viewer.setContentProvider( cp );
    viewer.setLabelProvider( lp );

    if( m_model == null )
      viewer.setInput( StatusUtilities.createErrorStatus( "Flood-Modell nicht geladen." ) );
    else
    {
      viewer.setInput( m_model.getWrappedFeature().getWorkspace() );

      final GMLXPathSegment segment = GMLXPathSegment.forQName( IFloodModel.QNAME );
      final GMLXPath pathToModel = new GMLXPath( segment );
      final GMLXPath rootPath = new GMLXPath( pathToModel, IFloodModel.QNAME_PROP_EVENT_MEMBER );
      gmlcp.setRootPath( rootPath );

      final IFeatureWrapperCollection<IRunoffEvent> events = m_model.getEvents();
      if( events.size() > 0 )
        m_eventViewer.setSelection( new StructuredSelection( events.get( 0 ) ), true );
    }
  }

  private void initalizeTreeActions( final FormToolkit toolkit, final Composite parent )
  {
    // We are reusing images of KalypsoGmlUi here
    final ImageDescriptor addEventID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_ADD );
    final ImageDescriptor importTinID = KalypsoModelFloodPlugin.getImageProvider().getImageDescriptor( KalypsoModelFloodImages.DESCRIPTORS.EVENT_IMPORT_TIN );
    final ImageDescriptor upID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_UP );
    final ImageDescriptor removeID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_REMOVE );
    final ImageDescriptor downID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_DOWN );
    final ImageDescriptor jumptoID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_JUMP );
    final ImageDescriptor updateDataID = KalypsoModelFloodPlugin.getImageProvider().getImageDescriptor( KalypsoModelFloodImages.DESCRIPTORS.EVENT_UPDATE_TIN );

    final Action addEventAction = new Action( "AddEvent", addEventID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleAddEvent( event );
      }
    };
    addEventAction.setDescription( "Neues Ereignis" );

    final Action importTinAction = new Action( "ImportTin", importTinID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleImportTin( event );
      }
    };
    importTinAction.setDescription( "Wasserspiegel importieren" );

    final Action removeAction = new Action( "Remove", removeID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleRemove( event );
      }
    };
    removeAction.setDescription( "Ereignis/Wasserspiegel löschen" );

    final Action moveUpAction = new Action( "Move Up", upID )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        handleMove( -1 );
      }
    };
    moveUpAction.setDescription( "Nach Oben verschieben" );

    final Action moveDownAction = new Action( "Move Down", downID )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        handleMove( 1 );
      }
    };
    moveDownAction.setDescription( "Nach Unten verschieben" );

    final Action jumpToAction = new Action( "Jump To", jumptoID )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        handleJumpTo();
      }
    };
    jumpToAction.setDescription( "Springe zu" );

    final Action updateDataAction = new Action( "Update Data", updateDataID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleUpdateData( event );
      }
    };
    updateDataAction.setDescription( "Daten aktualisieren" );

    createButton( toolkit, parent, addEventAction );
    createButton( toolkit, parent, importTinAction );
    createButton( toolkit, parent, removeAction );
    createButton( toolkit, parent, moveUpAction );
    createButton( toolkit, parent, moveDownAction );
    createButton( toolkit, parent, jumpToAction );
    createButton( toolkit, parent, updateDataAction );
  }

  private void createButton( final FormToolkit toolkit, final Composite parent, final IAction action )
  {
    final Button button = toolkit.createButton( parent, null, SWT.PUSH );
    final Image image = action.getImageDescriptor().createImage( true );
    button.setImage( image );
    button.setToolTipText( action.getDescription() );
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

  protected void handleUpdateData( final Event event )
  {
    if( m_treeSelection == null )
      return;

    /* Collect selected tins: either directly selected or all children of selected events */
    final Collection<ITinReference> tinRefs = new HashSet<ITinReference>();
    for( final Object o : m_treeSelection )
    {
      if( o instanceof IAdaptable )
      {
        final ITinReference tinRef = (ITinReference) ((IAdaptable) o).getAdapter( ITinReference.class );
        if( tinRef != null )
          tinRefs.add( tinRef );
        else
        {
          final IRunoffEvent runoffEvent = (IRunoffEvent) ((IAdaptable) o).getAdapter( IRunoffEvent.class );
          if( runoffEvent != null )
            tinRefs.addAll( runoffEvent.getTins() );
        }
      }
    }

    final Shell shell = event.display.getActiveShell();
    final ListSelectionDialog dialog = new ListSelectionDialog( shell, tinRefs, new ArrayContentProvider(), new GMLLabelProvider(), "Welche Wasserspiegel sollen aktualisiert werden?" );
    dialog.setInitialSelections( tinRefs.toArray() );
    if( dialog.open() != Window.OK )
      return;

    final Object[] result = dialog.getResult();
    final ITinReference[] tinsToUpdate = new ITinReference[result.length];
    for( int i = 0; i < tinsToUpdate.length; i++ )
      tinsToUpdate[i] = (ITinReference) result[i];

    final ICoreRunnableWithProgress operation = new UpdateTinsOperation( tinsToUpdate );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
      KalypsoModelFloodPlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( shell, "Daten aktualisieren", "Fehler beim Aktualisieren der Daten", resultStatus );
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

  protected void handleAddEvent( final Event event )
  {
    final IInputValidator inputValidator = new IInputValidator()
    {
      public String isValid( String newText )
      {
        if( newText == null || newText.length() == 0 )
          return "Name darf nicht leer sein";

        return null;
      }
    };

    // show input dialog
    final Shell shell = event.display.getActiveShell();
    final InputDialog dialog = new InputDialog( shell, "Ereignis hinzufügen", "Bitte geben Sie den Namen des neuen Ereignis ein:", "", inputValidator );
    if( dialog.open() != Window.OK )
      return;

    final String eventName = dialog.getValue();
    final IFloodModel model = m_model;
    final IFolder eventsFolder = getEventsFolder();
    final AbstractCascadingLayerTheme wspThemes = findWspTheme();
    Assert.isNotNull( wspThemes, "Wasserspiegel-Themen nicht vorhanden" );

    final URL sldContent = getClass().getResource( "resources/wsp.sld" );

    final ICoreRunnableWithProgress operation = new AddEventOperation( eventName, model, eventsFolder, wspThemes, m_dataProvider, sldContent );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
      KalypsoModelFloodPlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( shell, "Ereignis hinzufügen", "Fehler beim Erzeugen des Ereignisses", resultStatus );
  }

  /**
   * Finds THE wsp cascading theme containing the event themes.
   */
  private AbstractCascadingLayerTheme findWspTheme( )
  {
    final IMapModell mapModell = getMapPanel().getMapModell();
    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
    for( final IKalypsoTheme kalypsoTheme : allThemes )
    {
      // REMARK: not nice, but not otherwise possible: use name to find the theme.
      if( kalypsoTheme instanceof AbstractCascadingLayerTheme && kalypsoTheme.getName().equals( "Wasserspiegellagen" ) )
        return (AbstractCascadingLayerTheme) kalypsoTheme;
    }

    return null;
  }

  protected void handleImportTin( final Event event )
  {
    final Shell shell = event.display.getActiveShell();

    // get selected event
    final IRunoffEvent runoffEvent = findFirstEvent( m_treeSelection );
    if( runoffEvent == null )
    {
      MessageDialog.openConfirm( shell, "Wasserspiegel importieren", "Wählen Sie das Ereignis aus, zu welchem zu Wasserspiegel hinzufügen möchten." );
      return;
    }

    // TODO: present dialog to user
    final IGmlSource[] sources = new IGmlSource[] {};

    final ICoreRunnableWithProgress operation = new ImportTinOperation( runoffEvent.getTins(), sources );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
      KalypsoModelFloodPlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( shell, "Wasserspiegel importieren", "Fehler beim Import eines Wasserspiegels", resultStatus );
  }

  /**
   * Searches for the first occurency of {@link IRunoffEvent} in the current selection.<br>
   * If a tin is selected its parent will be returned.
   */
  private IRunoffEvent findFirstEvent( final Object[] treeSelection )
  {
    if( treeSelection == null )
      return null;

    for( final Object object : m_treeSelection )
    {
      if( object instanceof IAdaptable )
      {
        final IAdaptable a = (IAdaptable) object;
        final IRunoffEvent runoffEvent = (IRunoffEvent) a.getAdapter( IRunoffEvent.class );
        if( runoffEvent != null )
          return runoffEvent;

        final ITinReference tinRef = (ITinReference) a.getAdapter( ITinReference.class );
        if( tinRef != null )
        {
          final IRunoffEvent r2 = tinRef.getRunoffEvent();
          if( r2 != null )
            return r2;
        }
      }
    }

    return null;
  }

  protected void handleRemove( final Event event )
  {
    if( m_treeSelection == null )
      return;

    final AbstractCascadingLayerTheme wspThemes = findWspTheme();

    final ICoreRunnableWithProgress operation = new RemoveEventOperation( m_treeSelection, m_dataProvider, wspThemes );

    final Shell shell = event.display.getActiveShell();

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
      KalypsoModelFloodPlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( shell, "Ereignis hinzufügen", "Fehler beim Erzeugen des Ereignisses", resultStatus );
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
