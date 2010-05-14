/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypso1d2d.pjt.map;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.FeatureComposite;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractThemeInfoWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.ui.wizards.results.SelectCalcUnitForHydrographWizard;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * A widget with option pane, which allows the user to create, edit and delete hydrographs for 1d2d result files.<BR>
 * The user can add / remove hydrograph locations for each calculation unit.
 *
 * @author Thomas Jung
 */
public class HydrographManagementWidget extends AbstractWidget implements IWidgetWithOptions
{
  private IHydrographCollection m_hydrographs;

  private IHydrograph m_selectedHydrograph;

  /* predicate for hydrograph themes */
  private static final IKalypsoThemePredicate HYDROGRAPH_PREDICATE = new IKalypsoThemePredicate()
  {
    public boolean decide( final IKalypsoTheme theme )
    {
      if( !(theme instanceof IKalypsoFeatureTheme) )
        return false;

      final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
      final FeatureList featureList = ft.getFeatureList();
      final Feature hydrographsFeature = featureList == null ? null : featureList.getParentFeature();

      if( hydrographsFeature == null )
        return false;

      return GMLSchemaUtilities.substitutes( hydrographsFeature.getFeatureType(), IHydrographCollection.QNAME );
    }
  };

  private final AbstractThemeInfoWidget m_infoWidget = new AbstractThemeInfoWidget( "", "" ) //$NON-NLS-1$ //$NON-NLS-2$
  {
  };

  private final ModellEventListener m_modellistener = new ModellEventListener()
  {
    public void onModellChange( final ModellEvent modellEvent )
    {
      refreshControl();
    }
  };

  private final Runnable m_refreshHydrographViewerRunnable = new Runnable()
  {
    @SuppressWarnings("synthetic-access")
    public void run( )
    {
      ViewerUtilities.refresh( m_hydrographViewer, true );
    }
  };

  private ListViewer m_hydrographViewer;

  private IKalypsoFeatureTheme m_theme;

  private ComboViewer m_themeCombo;

  private final IMapModellListener m_mapModelListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeAdded(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeAdded( final IMapModell source, final IKalypsoTheme theme )
    {
      refreshThemeCombo();
      refreshControl();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeRemoved(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, boolean)
     */
    @SuppressWarnings("synthetic-access")
    @Override
    public void themeRemoved( final IMapModell source, final IKalypsoTheme theme, final boolean lastVisibility )
    {
      /* check if the current theme has been removed */
      if( theme.equals( m_theme ) )
      {
        setHydrographs( null, null );
      }
      refreshThemeCombo();
      refreshControl();
    }
  };

  private final String m_featureTemplateGft = "resources/hydrograph.gft"; //$NON-NLS-1$

  private Button m_addHydrographCollectionButton;

  private Button m_processHydrographCollectionButton;

  private IWidget m_delegateWidget;

  public HydrographManagementWidget( )
  {
    super( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.3"), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.4") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  protected void refreshControl( )
  {
    ViewerUtilities.refresh( m_hydrographViewer, true );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    /* Search for existing hydrograph themes */
    final IMapModell mapModell = mapPanel == null ? null : mapPanel.getMapModell();
    mapModell.addMapModelListener( m_mapModelListener );

    refreshThemeCombo();

    m_infoWidget.activate( commandPoster, mapPanel );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
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
        // final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
        // panel.setSize( size );
        // sc.setMinHeight( size.y );
      }
    } );
    // Basic Layout

    /* Theme selection combo + add / remove calc unit hydrograph theme buttons */
    final Composite themeSelectionPanel = toolkit.createComposite( panel, SWT.NONE );
    final GridLayout themeGridLayout = new GridLayout( 5, false );
    themeGridLayout.marginWidth = 0;
    themeSelectionPanel.setLayout( themeGridLayout );
    final GridData themeGridLayoutData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    themeSelectionPanel.setLayoutData( themeGridLayoutData );
    toolkit.createLabel( themeSelectionPanel, Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.5"), SWT.NONE ); //$NON-NLS-1$
    m_themeCombo = new ComboViewer( themeSelectionPanel, SWT.READ_ONLY | SWT.DROP_DOWN );
    final GridData comboGridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    m_themeCombo.getControl().setLayoutData( comboGridData );

    // buttons
    createAddCalcUnitButtonControl( themeSelectionPanel, toolkit );
    createRemoveCalcUnitButtonControl( themeSelectionPanel, toolkit );
    createProcessHydrographButtonControl( themeSelectionPanel, toolkit );

    /* Hydrograph table + info pane */
    final Composite hydrographPanel = toolkit.createComposite( panel, SWT.NONE );
    final GridLayout hydrographPanelLayout = new GridLayout( 2, false );
    final GridData hydrographPanelData = new GridData( SWT.FILL, SWT.FILL, true, false );
    hydrographPanelData.heightHint = 140;
    hydrographPanel.setLayoutData( hydrographPanelData );
    hydrographPanelLayout.marginHeight = 0;
    hydrographPanelLayout.marginWidth = 0;
    hydrographPanel.setLayout( hydrographPanelLayout );

    m_hydrographViewer = new ListViewer( hydrographPanel, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );
    final GridData hydrographViewerData = new GridData( SWT.FILL, SWT.FILL, true, false );
    hydrographViewerData.heightHint = 100;
    m_hydrographViewer.getControl().setLayoutData( hydrographViewerData );
    toolkit.adapt( m_hydrographViewer.getControl(), true, false );

    final Composite hydrographButtonPanel = toolkit.createComposite( hydrographPanel );
    final FillLayout hydrographButtonPanelLayout = new FillLayout( SWT.VERTICAL );
    hydrographButtonPanelLayout.spacing = 4;
    hydrographButtonPanel.setLayout( hydrographButtonPanelLayout );
    hydrographButtonPanel.setLayoutData( new GridData( SWT.CENTER, SWT.BEGINNING, false, true ) );

    /* Info view */
    final Group hydrographInfoGroup = new Group( panel, SWT.H_SCROLL );
    hydrographInfoGroup.setLayout( new GridLayout() );
    final GridData infoGroupData = new GridData( SWT.FILL, SWT.FILL, true, true );
    hydrographInfoGroup.setLayoutData( infoGroupData );
    toolkit.adapt( hydrographInfoGroup );
    hydrographInfoGroup.setText( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.6") ); //$NON-NLS-1$

    final CachedFeatureviewFactory featureviewFactory = new CachedFeatureviewFactory( new FeatureviewHelper() );
    featureviewFactory.addView( getClass().getResource( m_featureTemplateGft ) );
    final FeatureComposite featureComposite = new FeatureComposite( null, null, featureviewFactory );
    featureComposite.setFormToolkit( toolkit );

    featureComposite.addChangeListener( new IFeatureChangeListener()
    {
      @SuppressWarnings("synthetic-access")
      public void featureChanged( final ICommand changeCommand )
      {
        m_theme.postCommand( changeCommand, null );
        updateHydrographProperties();
      }

      public void openFeatureRequested( final Feature feature, final IPropertyType pt )
      {
      }
    } );

    // Fill contents
    initalizeHydrographViewer( m_hydrographViewer );
    initalizeHydrographActions( toolkit, hydrographButtonPanel );

    /* Hook Events */
    m_hydrographViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleListSelectionChanged( parent, hydrographInfoGroup, featureComposite, event );
      }
    } );

    m_themeCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleThemeComboSelected( event );
      }
    } );

    initializeThemeCombo();

    if( m_hydrographs != null && m_hydrographs.size() > 0 )
      m_hydrographViewer.setSelection( new StructuredSelection( m_hydrographs.get( 0 ) ) );

    final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    panel.setSize( size );
    sc.setMinHeight( size.y );

    return panel;

  }

  private void createAddCalcUnitButtonControl( final Composite parent, final FormToolkit toolkit )
  {
    m_addHydrographCollectionButton = toolkit.createButton( parent, null, SWT.PUSH );
    m_addHydrographCollectionButton.setToolTipText( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.7") ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    final Image addImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.HYDROGRAPH_COLLECTION_ADD );

    m_addHydrographCollectionButton.setImage( addImage );

    m_addHydrographCollectionButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final SelectCalcUnitForHydrographWizard addCalcUnitWizard = new SelectCalcUnitForHydrographWizard();
        addCalcUnitWizard.init( PlatformUI.getWorkbench(), new StructuredSelection() );
        addCalcUnitWizard.setMapModel( (IKalypsoLayerModell) getMapPanel().getMapModell() );
        final IHandlerService handlerService = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
        final IEvaluationContext context = handlerService.getCurrentState();
        final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );

        final WizardDialog2 wizardDialog2 = new WizardDialog2( shell, addCalcUnitWizard );

        if( wizardDialog2.open() == Window.OK )
        {
          MapModellHelper.waitForAndErrorDialog( shell, getMapPanel(), addCalcUnitWizard.getWindowTitle(), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.8") ); //$NON-NLS-1$

          refreshThemeCombo();
        }
      }
    } );

  }

  private void createRemoveCalcUnitButtonControl( final Composite parent, final FormToolkit toolkit )
  {
    m_processHydrographCollectionButton = toolkit.createButton( parent, null, SWT.PUSH );
    m_processHydrographCollectionButton.setToolTipText( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.9") ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    final Image removeImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.HYDROGRAPH_COLLECTION_REMOVE );

    m_processHydrographCollectionButton.setImage( removeImage );

    m_processHydrographCollectionButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IMapPanel mapPanel = getMapPanel();
        if( mapPanel == null )
          return;

        final IMapModell mapModell = mapPanel.getMapModell();
        mapModell.removeTheme( m_theme );
      }
    } );

  }

  private void createProcessHydrographButtonControl( final Composite parent, final FormToolkit toolkit )
  {
    m_processHydrographCollectionButton = toolkit.createButton( parent, null, SWT.PUSH );
    m_processHydrographCollectionButton.setToolTipText( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.10") ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    final Image processImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.HYDROGRAPH_COLLECTION_PROCESS );

    m_processHydrographCollectionButton.setImage( processImage );

    m_processHydrographCollectionButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent event )
      {
        saveModell();
        handleprocessHydrograph( event );
      }
    } );

  }

  protected void saveModell( )
  {

    // save the model
    final Runnable refreshRunnable = m_refreshHydrographViewerRunnable;
    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @SuppressWarnings("synthetic-access")
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        if( m_theme == null )
          return StatusUtilities.createInfoStatus( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.11") ); //$NON-NLS-1$

        m_theme.postCommand( new EmptyCommand( "", false ), refreshRunnable ); //$NON-NLS-1$

        try
        {
          /* save the model */
          final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
          final CommandableWorkspace workspace = m_theme.getWorkspace();
          pool.saveObject( workspace, new NullProgressMonitor() );

          return Status.OK_STATUS;
        }
        catch( final LoaderException e )
        {
          e.printStackTrace();

          throw new InvocationTargetException( e );
        }
      }
    };

    final IStatus status = ProgressUtilities.busyCursorWhile( operation );
    ErrorDialog.openError( m_hydrographViewer.getControl().getShell(), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.13"), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.14"), status ); //$NON-NLS-1$ //$NON-NLS-2$

  }

  protected void handleprocessHydrograph( final SelectionEvent event )
  {
    final Shell shell = event.display.getActiveShell();

    if( m_hydrographs == null )
      MessageDialog.openInformation( shell, Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.15"), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.16") ); //$NON-NLS-1$ //$NON-NLS-2$

    /* get the current calc unit results */
    final Map<IPath, Date> results = m_hydrographs.getResults();

    /* get the scenario folder */
    final IHandlerService handlerService = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

    /* process */
    final ICoreRunnableWithProgress processOperation = new HydrographProcessResultOperation( m_hydrographs, results, scenarioFolder );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( processOperation );
    if( !resultStatus.isOK() )
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( shell, Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.17"), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.18"), resultStatus ); //$NON-NLS-1$ //$NON-NLS-2$

    saveModell();

  }

  private void initializeThemeCombo( )
  {
    m_themeCombo.setContentProvider( new ArrayContentProvider() );
    m_themeCombo.setLabelProvider( new LabelProvider() );

    refreshThemeCombo();
  }

  protected void handleThemeComboSelected( final SelectionChangedEvent event )
  {
    setHydrographs( null, null );

    final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
    final Object firstElement = selection.getFirstElement();

    if( firstElement instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) firstElement;
      final FeatureList featureList = ft.getFeatureList();
      final Feature hydrographsFeature = featureList == null ? null : featureList.getParentFeature();
      if( hydrographsFeature != null )
        setHydrographs( (IHydrographCollection) hydrographsFeature.getAdapter( IHydrographCollection.class ), ft );
    }
  }

  private void setHydrographs( final IHydrographCollection hydrographs, final IKalypsoFeatureTheme theme )
  {
    // remove listener
    if( m_theme != null )
    {
      final CommandableWorkspace workspace = m_theme.getWorkspace();
      if( workspace == null )
        return;
      workspace.removeModellListener( m_modellistener );
      m_theme = null;
    }
    m_hydrographs = hydrographs;
    m_theme = theme;

    // add listener
    if( m_theme != null )
      m_theme.getWorkspace().addModellListener( m_modellistener );

    if( m_theme == null )
      m_infoWidget.setThemes( null );
    else
      m_infoWidget.setThemes( new IKalypsoTheme[] { m_theme } );

    // updateStylePanel();
    final ListViewer hydrgraphViewer = m_hydrographViewer;
    if( hydrgraphViewer != null && !hydrgraphViewer.getControl().isDisposed() )
    {
      hydrgraphViewer.getControl().getDisplay().syncExec( new Runnable()
      {
        public void run( )
        {
          if( !hydrgraphViewer.getControl().isDisposed() )
          {
            hydrgraphViewer.setInput( hydrographs );
            if( hydrographs != null && hydrographs.size() > 0 )
              hydrgraphViewer.setSelection( new StructuredSelection( hydrographs.get( 0 ) ), true );
          }
        }
      } );
    }
  }

  protected void handleListSelectionChanged( final Composite parent, final Group hydrographInfoGroup, final FeatureComposite featureComposite, final SelectionChangedEvent event )
  {
    final Runnable refreshRunnable = m_refreshHydrographViewerRunnable;
    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @SuppressWarnings("synthetic-access")
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        m_theme.postCommand( new EmptyCommand( "", false ), refreshRunnable ); //$NON-NLS-1$

        try
        {
          /* save the model */
          final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
          final CommandableWorkspace workspace = m_theme.getWorkspace();
          pool.saveObject( workspace, new NullProgressMonitor() );

          return Status.OK_STATUS;
        }
        catch( final LoaderException e )
        {
          e.printStackTrace();

          throw new InvocationTargetException( e );
        }
      }
    };

    final IStatus status = ProgressUtilities.busyCursorWhile( operation );
    ErrorDialog.openError( m_hydrographViewer.getControl().getShell(), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.20"), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.21"), status ); //$NON-NLS-1$ //$NON-NLS-2$

    final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
    m_selectedHydrograph = (IHydrograph) selection.getFirstElement();

    featureComposite.disposeControl();

    if( m_selectedHydrograph != null )
    {
      featureComposite.setFeature( m_selectedHydrograph.getFeature() );
      featureComposite.createControl( hydrographInfoGroup, SWT.NONE );
      parent.layout( true, true );
    }

    // final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    // panel.setSize( size );
    // sc.setMinHeight( size.y );

    getMapPanel().repaintMap();

  }

  private void initalizeHydrographActions( final FormToolkit toolkit, final Composite parent )
  {
    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();

    final ImageDescriptor addID = imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.ADD );
    final ImageDescriptor selectID = imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.HYDROGRAPH_SELECT );
    final ImageDescriptor removeID = imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.HYDROGRAPH_REMOVE );
    final ImageDescriptor jumptoID = imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.HYDROGRAPH_JUMP_TO );
    final ImageDescriptor exportID = imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.HYDROGRAPH_EXPORT );

    final Action addAction = new Action( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.22"), addID ) //$NON-NLS-1$
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleHydrographAdded( event );
      }
    };
    addAction.setDescription( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.23") ); //$NON-NLS-1$

    final Action selectAction = new Action( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.24"), selectID ) //$NON-NLS-1$
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleHydrographSelected( event );
      }
    };
    selectAction.setDescription( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.25") ); //$NON-NLS-1$

    final Action removeAction = new Action( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.26"), removeID ) //$NON-NLS-1$
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleHydrographRemoved( event );
      }
    };
    removeAction.setDescription( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.27") ); //$NON-NLS-1$

    createButton( toolkit, parent, addAction );
    createButton( toolkit, parent, selectAction );
    createButton( toolkit, parent, removeAction );

    final Action exportAction = new Action( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.28"), exportID ) //$NON-NLS-1$
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleHydrographExport( event );
      }
    };
    exportAction.setDescription( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.29") ); //$NON-NLS-1$

    final Action jumpToAction = new Action( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.30"), jumptoID ) //$NON-NLS-1$
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        handleHydrographJumpTo();
      }
    };
    jumpToAction.setDescription( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.31") ); //$NON-NLS-1$

    createButton( toolkit, parent, exportAction );
    createButton( toolkit, parent, jumpToAction );

  }

  protected void handleHydrographExport( @SuppressWarnings("unused") final Event event )
  {
    // TODO Auto-generated method stub

  }

  protected void handleHydrographSelected( @SuppressWarnings("unused") final Event event )
  {
    // set widget
    final EditHydrographWidget widget = new EditHydrographWidget( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.32"), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.33"), false, IHydrograph.QNAME_PROP_LOCATION, m_theme, this ); //$NON-NLS-1$ //$NON-NLS-2$
    setDelegate( widget );
  }

  protected void handleHydrographRemoved( @SuppressWarnings("unused") final Event event )
  {
    // set widget
    final RemoveHydrographWidget widget = new RemoveHydrographWidget( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.34"), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.35"), false, IHydrograph.QNAME_PROP_LOCATION, m_theme ); //$NON-NLS-1$ //$NON-NLS-2$
    setDelegate( widget );
  }

  protected void handleHydrographAdded( @SuppressWarnings("unused") final Event event )
  {
    // set widget
    final CreateHydrographWidget widget = new CreateHydrographWidget( Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.36"), Messages.getString("org.kalypso.kalypso1d2d.pjt.map.HydrographManagementWidget.37"), IHydrograph.QNAME, m_theme ); //$NON-NLS-1$ //$NON-NLS-2$
    setDelegate( widget );
  }

  protected void handleHydrographJumpTo( )
  {
    if( m_selectedHydrograph == null )
      return;

    final GM_Object location = m_selectedHydrograph.getLocation();

    GM_Envelope scaledBox;
    GM_Envelope envelope = GeometryUtilities.getEnvelope( location );
    if( location instanceof GM_Point )
    {
      final GM_Point point = (GM_Point) location;
      final GM_Position position = point.getPosition();
      final double newMaxX = position.getX() + 30;
      final double newMinX = position.getX() - 30;
      final double newMaxY = position.getY() + 30;
      final double newMinY = position.getY() - 30;
      final GM_Position min = GeometryFactory.createGM_Position( newMinX, newMinY );
      final GM_Position max = GeometryFactory.createGM_Position( newMaxX, newMaxY );
      envelope = GeometryFactory.createGM_Envelope( min, max, point.getCoordinateSystem() );
    }
    else if( location instanceof GM_Curve )
    {
      final GM_Curve line = (GM_Curve) location;
      envelope = line.getEnvelope();
    }

    if( envelope != null )
    {
      scaledBox = GeometryUtilities.scaleEnvelope( envelope, 1.1 );
      getMapPanel().setBoundingBox( scaledBox );
    }
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

  private void initalizeHydrographViewer( final ListViewer viewer )
  {
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        final IHydrograph hydrograph = (IHydrograph) element;
        return hydrograph.getName();
      }
    } );
    viewer.setInput( m_hydrographs );

  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    if( m_theme != null && m_modellistener != null )
    {
      final CommandableWorkspace workspace = m_theme.getWorkspace();
      if( workspace != null )
        workspace.removeModellListener( m_modellistener );
    }
  }

  /**
   * search the map / outline for existing hydrograph themes
   */
  protected void refreshThemeCombo( )
  {
    if( m_themeCombo == null || m_themeCombo.getControl().isDisposed() )
    {
      return;
    }

    /* get the map */
    final IMapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel == null ? null : mapPanel.getMapModell();
    final IKalypsoTheme[] themes = mapModell.getAllThemes();
    final List<IKalypsoTheme> themesForCombo = new ArrayList<IKalypsoTheme>();
    for( final IKalypsoTheme theme : themes )
    {
      if( HYDROGRAPH_PREDICATE.decide( theme ) )
        themesForCombo.add( theme );
      else if( theme instanceof IMapModell )
      {
        final IKalypsoTheme[] allThemes = ((IMapModell) theme).getAllThemes();
        for( final IKalypsoTheme kalypsoTheme : allThemes )
        {
          if( HYDROGRAPH_PREDICATE.decide( kalypsoTheme ) )
            themesForCombo.add( kalypsoTheme );
        }
      }
    }

    if( themesForCombo.size() >= 0 )
    {
      final Control control = m_themeCombo.getControl();
      final ComboViewer themeCombo = m_themeCombo;
      control.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( control.isDisposed() )
            return; // may be disposed meanwhile

          themeCombo.setInput( themesForCombo );
          themeCombo.refresh();
          if( themesForCombo.size() > 0 )
            themeCombo.setSelection( new StructuredSelection( themesForCombo.get( 0 ) ) );
        }
      } );
    }
  }

  protected void updateHydrographProperties( )
  {
    if( m_theme == null )
      m_infoWidget.setThemes( null );
    else
      m_infoWidget.setThemes( new IKalypsoTheme[] { m_theme } );

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final java.awt.Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.moved( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final java.awt.Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.dragged( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final java.awt.Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.leftClicked( p );
  }

  private void setDelegate( final IWidget delegateWidget )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.finish();

    m_delegateWidget = delegateWidget;

    if( m_delegateWidget != null )
      m_delegateWidget.activate( getCommandTarget(), getMapPanel() );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.paint( g );

    if( m_selectedHydrograph != null )
      paintHydrographInMap( g );
  }

  private void paintHydrographInMap( final Graphics g )
  {
    final Graphics2D g2 = (Graphics2D) g;
    final GM_Point point = (GM_Point) m_selectedHydrograph.getLocation();
    final IMapPanel mapPanel = getMapPanel();

    if( mapPanel == null )
      return;

    final GeoTransform projection = mapPanel.getProjection();
    if( projection == null || point == null )
      return;

    final int x = (int) projection.getDestX( point.getX() );
    final int y = (int) projection.getDestY( point.getY() );

    final int sizeOuter = 16;
    final Color defaultColor = g2.getColor();
    final Color color = new Color( 255, 30, 30 );
    g2.setColor( color );

    final Stroke defaultStroke = g2.getStroke();
    final BasicStroke stroke = new BasicStroke( 3 );
    g2.setStroke( stroke );

    g2.drawRect( x - sizeOuter / 2, y - sizeOuter / 2, sizeOuter, sizeOuter );

    g2.setColor( defaultColor );
    g2.setStroke( defaultStroke );
  }

  public void setHydrographViewerSelection( final StructuredSelection selection )
  {
    final Display display = m_hydrographViewer.getControl().getDisplay();
    display.asyncExec( new Runnable()
    {
      @SuppressWarnings("synthetic-access")
      public void run( )
      {
        if( selection != null && m_hydrographViewer != null )
          m_hydrographViewer.setSelection( selection, true );
      }
    } );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#getPartName()
   */
  @Override
  public String getPartName( )
  {
    return null;
  }
}