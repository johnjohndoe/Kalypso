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
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.resource.ImageDescriptor;
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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeControlListener;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gml.ui.KalypsoGmlUIPlugin;
import org.kalypso.gml.ui.KalypsoGmlUiImages;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.flood.KalypsoModelFloodImages;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.FeatureComposite;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.ui.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
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
  private final Runnable m_refreshEventViewerRunnable = new Runnable()
  {
    @SuppressWarnings("synthetic-access")
    public void run( )
    {
      ViewerUtilities.refresh( m_eventViewer, true );
    }
  };

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

    m_eventViewer = new TreeViewer( treePanel, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );
    m_eventViewer.getControl().setLayoutData( new TableWrapData( TableWrapData.FILL_GRAB, TableWrapData.FILL ) );
    toolkit.adapt( m_eventViewer.getControl(), true, true );

    final Composite treeButtonPanel = toolkit.createComposite( treePanel );
    final FillLayout treeButtonPanelLayout = new FillLayout( SWT.VERTICAL );
    treeButtonPanelLayout.spacing = 4;
    treeButtonPanel.setLayout( treeButtonPanelLayout );
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
    final Composite colormapPanel = toolkit.createComposite( panel, SWT.NONE );
    final TableWrapLayout colormapPanelLayout = new TableWrapLayout();
    colormapPanelLayout.numColumns = 2;
    colormapPanelLayout.makeColumnsEqualWidth = false;
    colormapPanelLayout.bottomMargin = 0;
    colormapPanelLayout.leftMargin = 0;
    colormapPanelLayout.rightMargin = 0;
    colormapPanelLayout.topMargin = 0;
    colormapPanel.setLayout( colormapPanelLayout );
    final TableWrapData colormapPanelData = new TableWrapData( TableWrapData.FILL_GRAB, TableWrapData.FILL_GRAB );
    colormapPanel.setLayoutData( colormapPanelData );

    m_colorMapTableViewer = new TableViewer( colormapPanel, SWT.BORDER );
    final TableWrapData colormapTableData = new TableWrapData( TableWrapData.FILL, TableWrapData.FILL_GRAB );
    colormapTableData.heightHint = 200;
    m_colorMapTableViewer.getControl().setLayoutData( colormapTableData );
    toolkit.adapt( m_colorMapTableViewer.getControl(), true, true );

    final Composite colormapPanelButtonPanel = toolkit.createComposite( colormapPanel, SWT.NONE );
    final GridLayout colormapButtonPanelLayout = new GridLayout();
    colormapButtonPanelLayout.marginHeight = 0;
    colormapButtonPanelLayout.marginWidth = 0;
    // colormapButtonPanelLayout.spacing = 4;
    colormapPanelButtonPanel.setLayout( colormapButtonPanelLayout );
    colormapPanelButtonPanel.setLayoutData( new TableWrapData( TableWrapData.FILL, TableWrapData.FILL ) );

    /* Fill contents */
    initalizeEventViewer( m_eventViewer );
    initalizeTreeActions( toolkit, treeButtonPanel );

    initializeColorMapTableViewer( m_colorMapTableViewer );
    initalizeColorMapActions( toolkit, colormapPanelButtonPanel );

    /* Hook Events */
    m_eventViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
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
          parent.layout( true, true );
        }

        sc.setMinSize( panel.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );

        getMapPanel().repaint();
      }
    } );

    sc.setMinSize( panel.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );

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
      // TODO: set input to null and hide/disable style editor
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

  private IFile getSldFile( final IRunoffEvent event )
  {
    final IFolder eventFolder = getEventFolder( event );
    return eventFolder.getFile( "wsp.sld" );
  }

  private IFolder getEventFolder( final IRunoffEvent event )
  {
    final IFolder eventsFolder = getEventsFolder();
    return eventsFolder.getFolder( event.getDataPath() );
  }

  IFolder getEventsFolder( )
  {
    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();
    final IFolder eventsFolder = scenarioFolder.getFolder( "events" );
    return eventsFolder;
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
    viewerTable.setLayoutData( new TableWrapData( TableWrapData.FILL_GRAB, TableWrapData.FILL_GRAB ) );
  }

  private void initalizeEventViewer( final StructuredViewer viewer )
  {
    final GMLContentProvider contentProvider = new GMLContentProvider( false );

    viewer.setContentProvider( contentProvider );
    viewer.setLabelProvider( new GMLLabelProvider() );

    if( m_model != null )
    {
      viewer.setInput( m_model.getWrappedFeature().getWorkspace() );

      final GMLXPathSegment segment = GMLXPathSegment.forQName( IFloodModel.QNAME );
      final GMLXPath pathToModel = new GMLXPath( segment );
      final GMLXPath rootPath = new GMLXPath( pathToModel, IFloodModel.QNAME_PROP_EVENT_MEMBER );
      contentProvider.setRootPath( rootPath );

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
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        handleUpdateData();
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

  protected void handleUpdateData( )
  {
    if( m_treeSelection == null )
      return;

    // TODO: collect all selected tins

    // TODO: update all collected tins

    // TODO: check for existing sld . If none exists, create one
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

    final IFloodModel model = m_model;

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          monitor.beginTask( "Ereignis hinzufügen", 6 );

          final String dialogValue = FileUtilities.validateName( dialog.getValue(), "_" );

          /* Create a unique name */
          final IFeatureWrapperCollection<IRunoffEvent> events = model.getEvents();
          final Set<String> names = new HashSet<String>();
          for( final IRunoffEvent runoffEvent : events )
            names.add( runoffEvent.getName() );

          int count = 0;
          String newFolderName = dialogValue;
          while( names.contains( newFolderName ) )
            newFolderName = dialogValue + count++;

          final IPath dataPath = Path.fromPortableString( newFolderName );

          ProgressUtilities.worked( monitor, 1 );

          /* Create new folder and fill with defaults */
          final IFolder eventsFolder = getEventsFolder();
          final IFolder newEventFolder = eventsFolder.getFolder( dataPath );
          newEventFolder.create( false, true, new SubProgressMonitor( monitor, 2 ) );

          final IFile sldFile = newEventFolder.getFile( "wsp.sld" );
          final String sldContent = FileUtilities.toString( getClass().getResource( "resources/wsp.sld" ), "UTF-8" );
          ProgressUtilities.worked( monitor, 1 );

          final InputStream sldSource = IOUtils.toInputStream( sldContent, "UTF-8" );
          sldFile.create( sldSource, false, new SubProgressMonitor( monitor, 1 ) );

          /* Add event-themes to map */

          /* Add new feature */
          final CommandableWorkspace workspace = m_dataProvider.getCommandableWorkSpace( IFloodModel.class );
          final Feature parentFeature = events.getWrappedFeature();
          final IRelationType parentRelation = events.getWrappedList().getParentFeatureTypeProperty();
          final IFeatureType featureType = parentRelation.getTargetFeatureType();
          final Feature newEventFeature = workspace.createFeature( parentFeature, parentRelation, featureType, 0 );

          final IRunoffEvent newEvent = (IRunoffEvent) newEventFeature.getAdapter( IRunoffEvent.class );
          newEvent.setName( dialog.getValue() );
          newEvent.setDataPath( dataPath );

          final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1, newEventFeature, null, true );
          workspace.postCommand( command );

          ProgressUtilities.worked( monitor, 1 );

          return Status.OK_STATUS;
        }
        catch( IOException e )
        {
          throw new InvocationTargetException( e );
        }
        catch( Exception e )
        {
          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
      KalypsoModelFloodPlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( shell, "Ereignis hinzufügen", "Fehler beim Erzeugen des Ereignisses", resultStatus );
  }

  protected void handleImportTin( final Event event )
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
