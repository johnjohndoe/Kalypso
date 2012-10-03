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
package org.kalypso.model.flood.ui.map;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.Range;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridLayoutFactory;
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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.viewers.StatusAndDelegateContentProvider;
import org.kalypso.contribs.eclipse.jface.viewers.StatusAndDelegateLabelProvider;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.KalypsoCoreExtensions;
import org.kalypso.core.gml.provider.GmlSourceChooserWizard;
import org.kalypso.core.gml.provider.IGmlSourceProvider;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.gml.ui.KalypsoGmlUIPlugin;
import org.kalypso.gml.ui.KalypsoGmlUiImages;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.flood.KalypsoModelFloodImages;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.model.flood.ui.map.operations.AddEventOperation;
import org.kalypso.model.flood.ui.map.operations.ImportTinOperation;
import org.kalypso.model.flood.ui.map.operations.RemoveEventOperation;
import org.kalypso.model.flood.util.FloodModelHelper;
import org.kalypso.ogc.gml.CascadingThemeHelper;
import org.kalypso.ogc.gml.GisTemplateUserStyle;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoStyle;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoUserStyle;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.FeatureComposite;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractThemeInfoWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.gmleditor.command.MoveFeatureCommand;
import org.kalypso.ui.editor.gmleditor.part.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.part.GMLLabelProvider;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.ui.editor.sldEditor.PolygonColorMapContentProvider;
import org.kalypso.ui.editor.sldEditor.PolygonColorMapLabelProvider;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizerUtils;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathSegment;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * A widget with option pane, which allows the user to manage (add/remove) run-off events and to import water level data
 * for each event.
 *
 * @author Thomas Jung
 */
public class EventManagementWidget extends AbstractWidget implements IWidgetWithOptions
{
  private final static URL SLD_TEMPLATE_LOCATION = EventManagementWidget.class.getResource( "resources/wsp.sld" );//$NON-NLS-1$

  private final AbstractThemeInfoWidget m_infoWidget = new AbstractThemeInfoWidget( "", "" )//$NON-NLS-1$//$NON-NLS-2$
  {
  };

  private TreeViewer m_eventViewer;

  protected IScenarioDataProvider m_dataProvider;

  private IFloodModel m_model;

  private Object[] m_treeSelection;

  private TableViewer m_colorMapTableViewer;

  public EventManagementWidget( )
  {
    super( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.2" ), Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.3" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    m_infoWidget.setNoThemesTooltip( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.4" ) ); //$NON-NLS-1$
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    // prepare for exception
    m_dataProvider = null;
    m_model = null;

    super.activate( commandPoster, mapPanel );

    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    try
    {
      final IFloodModel model = dataProvider.getModel( IFloodModel.class.getName() );
      if( model != null )
      {
        m_dataProvider = dataProvider;
        m_model = model;
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    m_infoWidget.activate( commandPoster, mapPanel );
  }

  @Override
  public void finish( )
  {
    super.finish();

    m_infoWidget.finish();
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final ToolBarManager treeManager = new ToolBarManager( SWT.VERTICAL );
    initalizeTreeActions( treeManager );

    final ToolBarManager colormapManager = new ToolBarManager( SWT.VERTICAL );
    initalizeColorMapActions( colormapManager );

    // TRICKY: Scrolling behavior:
    // - vertical: all components have a minimum height and fill the whole area
    // - vertical: if size is too small, a global scroll-bar appears
    // - horizontal: the panel has a minimum width, if the total area is too small, a global scrollbar appears
    // - horizontal: the components (tree/table) have their additional horizontal scrollbars which appars if the content
    // of the individual control is too big

    // FIXME: probably we should use scrolled form instead
    final ScrolledComposite sc = new ScrolledComposite( parent, SWT.V_SCROLL | SWT.H_SCROLL );
    sc.setMinWidth( 200 );
    sc.setExpandVertical( true );
    sc.setExpandHorizontal( true );

    final Composite panel = toolkit.createComposite( sc, SWT.NONE );
    panel.setLayout( new GridLayout() );

    sc.setContent( panel );
    parent.addControlListener( new ControlAdapter()
    {
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
    final GridData treePanelData = new GridData( SWT.FILL, SWT.FILL, true, false );
    treePanelData.heightHint = 200;
    treePanel.setLayoutData( treePanelData );
    GridLayoutFactory.fillDefaults().numColumns( 2 ).applyTo( treePanel );

    m_eventViewer = new TreeViewer( treePanel, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );
    final GridData eventViewerData = new GridData( SWT.FILL, SWT.FILL, true, false );
    eventViewerData.heightHint = 100;
    m_eventViewer.getControl().setLayoutData( eventViewerData );
    toolkit.adapt( m_eventViewer.getControl(), true, false );

    final ToolBar treeToolbar = treeManager.createControl( treePanel );
    toolkit.adapt( treeToolbar );
    treeToolbar.setLayoutData( new GridData( SWT.FILL, SWT.FILL, false, true ) );

    treeToolbar.addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        treeManager.dispose();
      }
    } );

    /* Info view */
    final Pair<Group, FeatureComposite> infoGroupComponents = createInfoGroup( toolkit, panel );

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

    final ToolBar colormapToolBar = colormapManager.createControl( colormapPanel );
    toolkit.adapt( colormapToolBar );
    colormapToolBar.setLayoutData( new GridData( SWT.CENTER, SWT.BEGINNING, false, false ) );

    colormapToolBar.addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        colormapManager.dispose();
      }
    } );

    /* Fill contents */
    initalizeEventViewer( m_eventViewer );
    initializeColorMapTableViewer( m_colorMapTableViewer );

    /* Hook Events */
    m_eventViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleEventSelectionChanged( sc, infoGroupComponents, (IStructuredSelection) event.getSelection() );
      }
    } );

    handleEventSelectionChanged( sc, infoGroupComponents, (IStructuredSelection) m_eventViewer.getSelection() );


    final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    panel.setSize( size );
    sc.setMinHeight( size.y );

    return panel;
  }

  protected void handleEventSelectionChanged( final ScrolledComposite sc, final Pair<Group, FeatureComposite> infoGroupComponents, final IStructuredSelection selection )
  {
    m_treeSelection = selection.toArray();

    final Group infoGroup = infoGroupComponents.getKey();
    final FeatureComposite infoComposite = infoGroupComponents.getValue();

    infoComposite.disposeControl();

    final IRunoffEvent runoffEvent = getSelectedEvent();
    IKalypsoFeatureTheme runoffEventTheme = findThemeForEvent( runoffEvent );

    try
    {
      // Always check, if sld file exists
      if( runoffEvent != null )
        AddEventOperation.checkSLDFile( runoffEvent, getEventFolder( runoffEvent ), SLD_TEMPLATE_LOCATION );

      final IKalypsoCascadingTheme wspThemes = findWspTheme();
      if( runoffEventTheme == null && runoffEvent != null )
      {
        /* A bit crude: if the theme does not yet exist, we create it right now */
        AddEventOperation.addEventThemes( wspThemes, runoffEvent );
      }
      /* Also add result theme if results are available */
      if( runoffEvent != null && getResultFolder( runoffEvent ).exists() && FloodModelHelper.findResultTheme( runoffEvent, wspThemes ) == -1 )
        FloodModelHelper.addResultTheme( runoffEvent, wspThemes, -1 );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    runoffEventTheme = findThemeForEvent( runoffEvent );

    // TODO: add theme if missing
    if( runoffEventTheme == null )
      m_infoWidget.setThemes( null );
    else
      m_infoWidget.setThemes( new IKalypsoTheme[] { runoffEventTheme } );

    updateStylePanel( runoffEventTheme );

    if( m_treeSelection != null && m_treeSelection.length > 0 )
    {
      infoComposite.setFeature( (Feature) m_treeSelection[0] );
      infoComposite.createControl( infoGroup, SWT.NONE );
    }

    sc.getParent().layout( true, true );

    final Control panel = sc.getContent();

    final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    panel.setSize( size );
    sc.setMinHeight( size.y );

    getMapPanel().repaintMap();
  }

  private Pair<Group, FeatureComposite> createInfoGroup( final FormToolkit toolkit, final Composite panel )
  {
    final Group eventInfoGroup = new Group( panel, SWT.H_SCROLL );
    eventInfoGroup.setLayout( new GridLayout() );
    final GridData infoGroupData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    eventInfoGroup.setLayoutData( infoGroupData );
    toolkit.adapt( eventInfoGroup );
    eventInfoGroup.setText( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.5" ) ); //$NON-NLS-1$

    final CachedFeatureviewFactory featureviewFactory = new CachedFeatureviewFactory( new FeatureviewHelper() );
    featureviewFactory.addView( getClass().getResource( "resources/event.gft" ) ); //$NON-NLS-1$
    featureviewFactory.addView( getClass().getResource( "resources/tinReference.gft" ) ); //$NON-NLS-1$
    final FeatureComposite featureComposite = new FeatureComposite( null, null, featureviewFactory );
    featureComposite.setFormToolkit( toolkit );
    featureComposite.addChangeListener( new IFeatureChangeListener()
    {
      @Override
      public void featureChanged( final ICommand changeCommand )
      {
        handleInfoFeatureChanged( changeCommand );
      }

      @Override
      public void openFeatureRequested( final Feature feature, final IPropertyType pt )
      {
      }
    } );

    return Pair.of( eventInfoGroup, featureComposite );
  }

  protected void handleInfoFeatureChanged( final ICommand changeCommand )
  {
    try
    {
      m_dataProvider.postCommand( IFloodModel.class.getName(), changeCommand );
      updateThemeNames();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * initializes the button action for the style panel.
   */
  private void initalizeColorMapActions( final ToolBarManager manager )
  {
    manager.add( new GenerateColorMapAction( this ) );

    manager.update( true );
  }

  /**
   * update the style panel with the {@link PolygonColorMap}
   */
  private void updateStylePanel( final IKalypsoFeatureTheme runoffEventTheme )
  {
    if( runoffEventTheme == null )
    {
      // MessageDialog.openConfirm( shell, "Kartenthemen ",
      // "W‰hlen Sie das Ereignis aus, zu welchem zu Wasserspiegel hinzuf¸gen mˆchten." );
      // m_colorMapTableViewer.setInput( StatusUtilities.createInfoStatus(
      // "Keine Ereignis ausgew‰hlt. W‰hlen Sie ein Ereignis in der Ereignisliste, um die Darstellung zu editieren." )
      // );
      return;
    }

    final PolygonColorMap colorMap = findColorMap( runoffEventTheme );
    m_colorMapTableViewer.setInput( colorMap );
  }

  PolygonColorMap findColorMap( final IKalypsoFeatureTheme runoffEventTheme )
  {
    try
    {
      final IKalypsoStyle[] styles = runoffEventTheme.getStyles();
      final IKalypsoUserStyle style = findUserStyle( styles, "wspUserStyle" ); //$NON-NLS-1$
      if( style != null )
      {
        final FeatureTypeStyle wspFts = style.getFeatureTypeStyle( "wspFts" ); //$NON-NLS-1$
        final Rule wspRule = wspFts.getRule( "wspRule" ); //$NON-NLS-1$
        final SurfacePolygonSymbolizer symb = (SurfacePolygonSymbolizer) wspRule.getSymbolizers()[0];
        return symb.getColorMap();
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }

  private IKalypsoUserStyle findUserStyle( final IKalypsoStyle[] styles, final String name )
  {
    for( final IKalypsoStyle style : styles )
    {
      if( style instanceof IKalypsoUserStyle && ((IKalypsoUserStyle) style).getName().equals( name ) )
        return (IKalypsoUserStyle) style;
    }

    return null;
  }

  public static IFolder getEventFolder( final IRunoffEvent event )
  {
    final IFolder eventsFolder = getEventsFolder();
    return eventsFolder.getFolder( event.getDataPath() );
  }

  public static IFolder getResultFolder( final IRunoffEvent event )
  {
    final IFolder eventsFolder = getEventsFolder();
    final IFolder folder = eventsFolder.getFolder( event.getDataPath() );
    return folder.getFolder( "results" ); //$NON-NLS-1$
  }

  public static IFolder getEventsFolder( )
  {
    final IFolder szenarioFolder = KalypsoAFGUIFrameworkPlugin.getActiveWorkContext().getCurrentCase().getFolder();
    return szenarioFolder.getFolder( "events" ); //$NON-NLS-1$
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
    final CoverageFilterViewerFilter coverageFilter = new CoverageFilterViewerFilter();

    viewer.setContentProvider( cp );
    viewer.setLabelProvider( lp );
    viewer.addFilter( coverageFilter );

    if( m_model == null )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelFloodPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.21" ) ); //$NON-NLS-1$
      viewer.setInput( status );
    }
    else
    {
      viewer.setInput( m_model.getWorkspace() );

      final GMLXPathSegment segment = GMLXPathSegment.forQName( IFloodModel.QNAME );
      final GMLXPath pathToModel = new GMLXPath( segment );
      final GMLXPath rootPath = new GMLXPath( pathToModel, IFloodModel.QNAME_PROP_EVENT_MEMBER );
      gmlcp.setRootPath( rootPath );

      final IFeatureBindingCollection<IRunoffEvent> events = m_model.getEvents();
      if( events.size() > 0 )
      {
        m_eventViewer.setSelection( new StructuredSelection( events.get( 0 ) ), true );
      }
    }
  }

  private void initalizeTreeActions( final ToolBarManager manager )
  {
    // FIXME: move all those actions into own classes

    // We are reusing images of KalypsoGmlUi here
    final ImageDescriptor addEventID = KalypsoModelFloodPlugin.getImageProvider().getImageDescriptor( KalypsoModelFloodImages.DESCRIPTORS.EVENT_ADD );
    final ImageDescriptor importTinID = KalypsoModelFloodPlugin.getImageProvider().getImageDescriptor( KalypsoModelFloodImages.DESCRIPTORS.TIN_ADD );
    final ImageDescriptor upID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_UP );
    final ImageDescriptor removeID = KalypsoModelFloodPlugin.getImageProvider().getImageDescriptor( KalypsoModelFloodImages.DESCRIPTORS.DELETE );
    final ImageDescriptor downID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_DOWN );
    final ImageDescriptor jumptoID = KalypsoModelFloodPlugin.getImageProvider().getImageDescriptor( KalypsoModelFloodImages.DESCRIPTORS.TIN_JUMPTO );
    final ImageDescriptor updateDataID = KalypsoModelFloodPlugin.getImageProvider().getImageDescriptor( KalypsoModelFloodImages.DESCRIPTORS.TIN_UPDATE );

    final Action addEventAction = new Action( "AddEvent", addEventID ) //$NON-NLS-1$
    {
      @Override
      public void runWithEvent( final Event event )
      {
        handleAddEvent( event );
      }
    };
    addEventAction.setDescription( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.23" ) ); //$NON-NLS-1$

    final Action importTinAction = new Action( "ImportTin", importTinID ) //$NON-NLS-1$
    {
      @Override
      public void runWithEvent( final Event event )
      {
        handleImportTin( event );
      }
    };
    importTinAction.setDescription( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.25" ) ); //$NON-NLS-1$

    final Action removeAction = new Action( "Remove", removeID ) //$NON-NLS-1$
    {
      @Override
      public void runWithEvent( final Event event )
      {
        handleRemove( event );
      }
    };
    removeAction.setDescription( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.27" ) ); //$NON-NLS-1$

    final Action moveUpAction = new Action( "Move Up", upID ) //$NON-NLS-1$
    {
      @Override
      public void runWithEvent( final Event event )
      {
        handleMove( event, -1 );
      }
    };
    moveUpAction.setDescription( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.29" ) ); //$NON-NLS-1$

    final Action moveDownAction = new Action( "Move Down", downID ) //$NON-NLS-1$
    {
      @Override
      public void runWithEvent( final Event event )
      {
        handleMove( event, 1 );
      }
    };
    moveDownAction.setDescription( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.31" ) ); //$NON-NLS-1$

    final Action jumpToAction = new Action( "Jump To", jumptoID ) //$NON-NLS-1$
    {
      @Override
      public void run( )
      {
        handleJumpTo();
      }
    };
    jumpToAction.setDescription( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.33" ) ); //$NON-NLS-1$

    final Action updateDataAction = new Action( "Update Data", updateDataID ) //$NON-NLS-1$
    {
      @Override
      public void runWithEvent( final Event event )
      {
        handleUpdateData( event );
      }
    };
    updateDataAction.setDescription( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.35" ) ); //$NON-NLS-1$

    manager.add( addEventAction );
    manager.add( importTinAction );
    manager.add( removeAction );
    manager.add( moveUpAction );
    manager.add( moveDownAction );
    manager.add( jumpToAction );
    manager.add( updateDataAction );

    manager.update( true );
  }

  // private void createButton( final FormToolkit toolkit, final Composite parent, final IAction action )
  // {
  // final Button button = toolkit.createButton( parent, null, SWT.PUSH );
  // final Image image = action.getImageDescriptor().createImage( true );
  // button.setImage( image );
  // button.setToolTipText( action.getDescription() );
  // button.addSelectionListener( new SelectionAdapter()
  // {
  // @Override
  // public void widgetSelected( final SelectionEvent e )
  // {
  // final Event event = new Event();
  // event.display = e.display;
  // // ...
  //
  // action.runWithEvent( event );
  // }
  // } );
  //
  // button.addDisposeListener( new DisposeListener()
  // {
  // @Override
  // public void widgetDisposed( final DisposeEvent e )
  // {
  // image.dispose();
  // }
  // } );
  // }

  protected void handleUpdateData( final Event event )
  {
    if( m_treeSelection == null )
    {
      return;
    }

    /* Collect selected tins: either directly selected or all children of selected events */
    final Collection<ITinReference> tinRefs = new HashSet<>();
    for( final Object o : m_treeSelection )
    {
      if( o instanceof IAdaptable )
      {
        final ITinReference tinRef = (ITinReference) ((IAdaptable) o).getAdapter( ITinReference.class );
        if( tinRef != null )
        {
          tinRefs.add( tinRef );
        }
        else
        {
          final IRunoffEvent runoffEvent = (IRunoffEvent) ((IAdaptable) o).getAdapter( IRunoffEvent.class );
          if( runoffEvent != null )
          {
            tinRefs.addAll( runoffEvent.getTins() );
          }
        }
      }
    }

    final Shell shell = event.display.getActiveShell();
    final ListSelectionDialog dialog = new ListSelectionDialog( shell, tinRefs, new ArrayContentProvider(), new GMLLabelProvider(), Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.36" ) ); //$NON-NLS-1$
    dialog.setInitialSelections( tinRefs.toArray() );
    if( dialog.open() != Window.OK )
    {
      return;
    }

    final Object[] result = dialog.getResult();
    final ITinReference[] tinsToUpdate = new ITinReference[result.length];
    for( int i = 0; i < tinsToUpdate.length; i++ )
    {
      tinsToUpdate[i] = (ITinReference) result[i];
    }

    final ICoreRunnableWithProgress operation = new UpdateTinsOperation( tinsToUpdate, m_dataProvider );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
    {
      KalypsoModelFloodPlugin.getDefault().getLog().log( resultStatus );
    }
    ErrorDialog.openError( shell, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.37" ), Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.38" ), resultStatus ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  protected void handleJumpTo( )
  {
    if( m_treeSelection == null )
    {
      return;
    }

    final GM_Envelope envelope = envelopeForSelection();
    if( envelope == null )
    {
      return;
    }

    final GM_Envelope scaledBox = GeometryUtilities.scaleEnvelope( envelope, 1.05 );
    getMapPanel().setBoundingBox( scaledBox );
  }

  protected void handleMove( final Event event, final int step )
  {
    if( m_treeSelection == null )
    {
      return;
    }

    if( m_treeSelection.length != 1 )
    {
      return;
    }

    final Feature selectedFeature = (Feature) m_treeSelection[0];

    final Feature parentFeature = selectedFeature.getOwner();
    final IPropertyType pt = selectedFeature.getParentRelation();

    final List< ? > featureList = (List< ? >) parentFeature.getProperty( pt );
    final int newIndex = featureList.indexOf( selectedFeature ) + step;
    if( newIndex < 0 || newIndex >= featureList.size() )
    {
      return;
    }

    final MoveFeatureCommand command = new MoveFeatureCommand( parentFeature, pt, selectedFeature, step );

    final IScenarioDataProvider sdProvider = m_dataProvider;
    try
    {
      sdProvider.postCommand( IFloodModel.class.getName(), command );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModelFloodPlugin.getDefault().getLog().log( status );
      ErrorDialog.openError( event.display.getActiveShell(), Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.39" ), Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.40" ), status ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  protected void handleAddEvent( final Event event )
  {
    final IInputValidator inputValidator = new IInputValidator()
    {
      @Override
      public String isValid( final String newText )
      {
        if( newText == null || newText.length() == 0 )
        {
          return Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.41" ); //$NON-NLS-1$
        }

        return null;
      }
    };

    // show input dialog
    final Shell shell = event.display.getActiveShell();
    final InputDialog dialog = new InputDialog( shell, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.42" ), Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.43" ), "", inputValidator ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    if( dialog.open() != Window.OK )
      return;

    final String eventName = dialog.getValue();
    final IFolder eventsFolder = getEventsFolder();

    final IFloodModel model = m_model;
    final IKalypsoCascadingTheme wspThemes = findWspTheme();

    final AddEventOperation operation = new AddEventOperation( eventName, model, eventsFolder, wspThemes, m_dataProvider, SLD_TEMPLATE_LOCATION );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
    {
      KalypsoModelFloodPlugin.getDefault().getLog().log( resultStatus );
    }

    ErrorDialog.openError( shell, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.49" ), Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.50" ), resultStatus ); //$NON-NLS-1$ //$NON-NLS-2$

    final IRunoffEvent newEvent = operation.getNewEvent();
    m_eventViewer.setSelection( new StructuredSelection( newEvent ) );
  }

  private IKalypsoCascadingTheme findWspTheme( )
  {
    final IMapModell mapModell = getMapPanel().getMapModell();
    final IKalypsoCascadingTheme wspThemes = CascadingThemeHelper.getNamedCascadingTheme( mapModell, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.64" ), "waterlevelThemes" ); //$NON-NLS-1$ //$NON-NLS-2$
    Assert.isNotNull( wspThemes, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.6" ) ); //$NON-NLS-1$
    return wspThemes;
  }

  protected void handleImportTin( final Event event )
  {
    final Shell shell = event.display.getActiveShell();
    final String windowTitle = Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.51" ); //$NON-NLS-1$

    // get selected event
    final IRunoffEvent runoffEvent = getSelectedEvent();
    if( runoffEvent == null )
    {
      MessageDialog.openInformation( shell, windowTitle, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.52" ) ); //$NON-NLS-1$
      return;
    }

    /* Get source provider for tins */
    final IGmlSourceProvider[] provider = KalypsoCoreExtensions.createGmlSourceProvider( "org.kalypso.core.tin.waterlevel" ); //$NON-NLS-1$
    if( provider.length == 0 )
    {
      MessageDialog.openInformation( shell, windowTitle, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.54" ) ); //$NON-NLS-1$
      return;
    }

    /* Show dialog to user and import tins afterwards */
    final IFeatureBindingCollection<ITinReference> tins = runoffEvent.getTins();

    final ImportTinOperation operation = new ImportTinOperation( m_dataProvider, tins, getMapPanel() );
    final GmlSourceChooserWizard wizard = new GmlSourceChooserWizard( provider, operation );
    final IDialogSettings dialogSettings = DialogSettingsUtils.getDialogSettings( KalypsoModelFloodPlugin.getDefault(), getClass().getName() );
    wizard.setDialogSettings( dialogSettings );
    wizard.setWindowTitle( windowTitle );

    final WizardDialog2 wizardDialog = new WizardDialog2( shell, wizard );
    wizardDialog.setRememberSize( true );
    if( wizardDialog.open() == Window.OK )
    {
      final ITinReference[] newTinRefs = operation.getNewTinRefs();
      m_eventViewer.setSelection( new StructuredSelection( newTinRefs ) );

      createInitialColorMap( runoffEvent, newTinRefs );
    }
  }

  private void createInitialColorMap( final IRunoffEvent runoffEvent, final ITinReference[] newTinRefs )
  {
    if( ArrayUtils.isEmpty( newTinRefs ) )
      return;

    final IKalypsoFeatureTheme eventTheme = findThemeForEvent( runoffEvent );
    final PolygonColorMap colorMap = findColorMap( eventTheme );

    if( colorMap == null )
      return;

    /* In order to show anything to the user, create a default color map, if no colors have been defined yet */
    final Range<BigDecimal> minMax = GenerateColorMapAction.computeTinRange( newTinRefs );
    final BigDecimal min = minMax.getMinimum();
    final BigDecimal max = minMax.getMaximum();
    final BigDecimal stepWidth = new BigDecimal( 0.1 );
    final Color fromColor = new Color( 0, 255, 255, 200 );
    final Color toColor = new Color( 0, 0, 255, 200 );

    final PolygonColorMapEntry fromEntry = StyleFactory.createPolygonColorMapEntry( fromColor, fromColor, min, min );
    final PolygonColorMapEntry toEntry = StyleFactory.createPolygonColorMapEntry( toColor, toColor, max, max );

    final List<PolygonColorMapEntry> entries = PolygonSymbolizerUtils.createColorMap( fromEntry, toEntry, stepWidth, min, max, false );
    colorMap.replaceColorMap( entries );

    handleColormapChanged( eventTheme );
  }

  /**
   * Searches for the first occurrence of {@link IRunoffEvent} in the current selection.<br>
   * If a tin is selected its parent will be returned.
   */
  private IRunoffEvent findFirstEvent( final Object[] treeSelection )
  {
    if( treeSelection == null )
      return null;

    for( final Object object : treeSelection )
    {
      if( object instanceof IAdaptable )
      {
        final IAdaptable a = (IAdaptable) object;
        final IRunoffEvent runoffEvent = (IRunoffEvent) a.getAdapter( IRunoffEvent.class );
        if( runoffEvent != null )
        {
          return runoffEvent;
        }

        final ITinReference tinRef = (ITinReference) a.getAdapter( ITinReference.class );
        if( tinRef != null )
        {
          final IRunoffEvent r2 = tinRef.getRunoffEvent();
          if( r2 != null )
          {
            return r2;
          }
        }
      }
    }

    return null;
  }

  protected void handleRemove( final Event event )
  {
    if( m_treeSelection == null )
    {
      return;
    }

    final IMapModell mapModell = getMapPanel().getMapModell();

    final IKalypsoCascadingTheme wspThemes = CascadingThemeHelper.getNamedCascadingTheme( mapModell, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.55" ), "waterlevelThemes" );//$NON-NLS-1$ //$NON-NLS-2$

    final Shell shell = event.display.getActiveShell();

    final ICoreRunnableWithProgress operation = new RemoveEventOperation( m_treeSelection, m_dataProvider, wspThemes );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
    {
      KalypsoModelFloodPlugin.getDefault().getLog().log( resultStatus );
    }
    ErrorDialog.openError( shell, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.57" ), Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.58" ), resultStatus ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void disposeControl( )
  {
  }

  @Override
  public void mouseMoved( final MouseEvent e )
  {
    m_infoWidget.mouseMoved( e );
  }

  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    if( m_treeSelection != null )
    {
      for( final Object selectedObject : m_treeSelection )
      {
        if( selectedObject instanceof Feature )
        {
          final Object adaptedObject = adaptToKnownObject( selectedObject );

          if( adaptedObject instanceof ITinReference )
          {
            paintEnvelope( g, ((ITinReference) adaptedObject).getEnvelope() );
          }
          else if( adaptedObject instanceof IRunoffEvent )
          {
            final IFeatureBindingCollection<ITinReference> tins = ((IRunoffEvent) adaptedObject).getTins();
            paintEnvelope( g, tins.getBoundingBox() );

            for( final ITinReference tinReference : tins )
            {
              paintEnvelope( g, tinReference.getEnvelope() );
            }
          }

          final GM_Envelope envelope = envelopeForSelected( selectedObject );
          paintEnvelope( g, envelope );
        }
      }
    }

    m_infoWidget.paint( g );
  }

  private void paintEnvelope( final Graphics g, final GM_Envelope envelope )
  {
    if( envelope == null )
    {
      return;
    }

    final GeoTransform projection = getMapPanel().getProjection();

    final GM_Position minPoint = projection.getDestPoint( envelope.getMin() );
    final GM_Position maxPoint = projection.getDestPoint( envelope.getMax() );

    final int x = (int) Math.min( minPoint.getX(), maxPoint.getX() );
    final int y = (int) Math.min( minPoint.getY(), maxPoint.getY() );

    final int width = (int) Math.abs( minPoint.getX() - maxPoint.getX() );
    final int height = (int) Math.abs( minPoint.getY() - maxPoint.getY() );

    g.setColor( Color.RED );
    g.drawRect( x, y, width, height );
  }

  private GM_Envelope envelopeForSelection( )
  {
    if( m_treeSelection == null )
    {
      return null;
    }

    GM_Envelope result = null;

    for( final Object selectedObject : m_treeSelection )
    {
      final GM_Envelope envelope = envelopeForSelected( selectedObject );
      if( envelope != null )
      {
        if( result == null )
        {
          result = envelope;
        }
        else
        {
          result = result.getMerged( envelope );
        }
      }
    }

    return result;
  }

  private GM_Envelope envelopeForSelected( final Object selectedObject )
  {
    final Object adaptedObject = adaptToKnownObject( selectedObject );

    if( adaptedObject instanceof ITinReference )
    {
      return ((ITinReference) adaptedObject).getEnvelope();
    }

    if( adaptedObject instanceof IRunoffEvent )
    {
      return ((IRunoffEvent) adaptedObject).getTins().getBoundingBox();
    }

    return null;
  }

  private Object adaptToKnownObject( final Object object )
  {
    if( !(object instanceof Feature) )
    {
      return null;
    }

    final Feature feature = (Feature) object;

    final Object event = feature.getAdapter( IRunoffEvent.class );
    if( event != null )
    {
      return event;
    }

    return feature.getAdapter( ITinReference.class );
  }

  private void updateThemeNames( )
  {
    final IMapModell mapModell = getMapPanel().getMapModell();

    final IKalypsoCascadingTheme wspThemes = CascadingThemeHelper.getNamedCascadingTheme( mapModell, Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.59" ), "waterlevelThemes" );//$NON-NLS-1$ //$NON-NLS-2$

    final IRunoffEvent event = getSelectedEvent();
    final IKalypsoTheme[] allThemes = wspThemes.getAllThemes();
    for( final IKalypsoTheme kalypsoTheme : allThemes )
    {
      if( kalypsoTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) kalypsoTheme;
        final IKalypsoStyle[] styles = featureTheme.getStyles();
        for( final IKalypsoStyle style : styles )
        {
          if( style instanceof GisTemplateUserStyle )
          {
            final GisTemplateUserStyle pooledUserStyle = (GisTemplateUserStyle) style;
            final PoolableObjectType poolKey = pooledUserStyle.getPoolKey();

            final String styleLocationForEventWsp = AddEventOperation.styleLocationForEventWsp( event );

            if( poolKey.getLocation().equals( styleLocationForEventWsp ) )
            {
              final String name = kalypsoTheme.getLabel();

              // HACK!

              if( name.contains( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.61" ) ) ) //$NON-NLS-1$
              {
                kalypsoTheme.setName( new I10nString( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.62", event.getName() ) ) ); //$NON-NLS-1$
                kalypsoTheme.setProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, "org.kalypso.ogc.gml.map.themeinfo.TriangulatedSurfaceThemeInfo?format=Wasserspiegel (" + event.getName() //$NON-NLS-1$
                    + ") %.2f NN+m" ); //$NON-NLS-1$
              }
              if( name.contains( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.66" ) ) ) //$NON-NLS-1$
              {
                kalypsoTheme.setName( new I10nString( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.67", event.getName() ) ) ); //$NON-NLS-1$
                kalypsoTheme.setProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, "org.kalypso.ogc.gml.map.themeinfo.TriangulatedSurfaceThemeInfo?format=Flieﬂtiefen (" + event.getName() + ") %.2f NN+m" ); //$NON-NLS-1$ //$NON-NLS-2$
              }
              if( name.contains( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.71" ) ) ) //$NON-NLS-1$
              {
                kalypsoTheme.setName( new I10nString( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.72", event.getName() ) ) ); //$NON-NLS-1$
                kalypsoTheme.setProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, "org.kalypso.ogc.gml.map.themeinfo.TriangulatedSurfaceThemeInfo?format=Flieﬂtiefen (" + event.getName() + ") %.2f NN+m" ); //$NON-NLS-1$ //$NON-NLS-2$
              }

              if( name.contains( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.76" ) ) ) //$NON-NLS-1$
              {
                kalypsoTheme.setName( new I10nString( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.77", event.getName() ) ) ); //$NON-NLS-1$
              }

            }
          }
        }
        // check for result coverages
        final FeatureList featureList = featureTheme.getFeatureList();
        if( featureList != null )
        {
          for( final Object object : featureList )
          {
            if( object instanceof Feature )
            {
              final Feature feature = (Feature) object;

              // the papa papa of the coverage is the event
              final Feature parent = feature.getOwner().getOwner();
              if( parent != null )
              {
                if( parent.getId().equals( event.getId() ) )
                {
                  final String name = kalypsoTheme.getLabel();

                  // HACK!
                  if( name.contains( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.79" ) ) ) //$NON-NLS-1$
                  {
                    kalypsoTheme.setName( new I10nString( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.80", event.getName() ) ) ); //$NON-NLS-1$
                    kalypsoTheme.setProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, "org.kalypso.gml.ui.map.CoverageThemeInfo?format=Flieﬂtiefen (" + event.getName() + ") %.2f NN+m" ); //$NON-NLS-1$ //$NON-NLS-2$
                  }
                  if( name.contains( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.84" ) ) ) //$NON-NLS-1$
                  {
                    kalypsoTheme.setName( new I10nString( Messages.getString( "org.kalypso.model.flood.ui.map.EventManagementWidget.85", event.getName() ) ) ); //$NON-NLS-1$
                    kalypsoTheme.setProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, "org.kalypso.gml.ui.map.CoverageThemeInfo?format=Flieﬂtiefen (" + event.getName() + ") %.2f NN+m" ); //$NON-NLS-1$ //$NON-NLS-2$
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  @Override
  public String getPartName( )
  {
    return null;
  }

  public IRunoffEvent getSelectedEvent( )
  {
    return findFirstEvent( m_treeSelection );
  }

  IKalypsoFeatureTheme findThemeForEvent( final IRunoffEvent event )
  {
    return FloodModelHelper.findThemeForEvent( getMapPanel().getMapModell(), event );
  }

  void handleColormapChanged( final IKalypsoFeatureTheme runoffEventTheme )
  {
    try
    {
      final IKalypsoStyle[] styles = runoffEventTheme.getStyles();
      for( final IKalypsoStyle style : styles )
      {
        style.fireStyleChanged();
        style.save( new NullProgressMonitor() );
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    // CHECK: probably the style will be automatically reloaded by the pool, so updating the colormap here may not
    // really set the right colorMap entry here...
    updateStylePanel( runoffEventTheme );
  }
}