/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.featureview.views;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.commons.command.ICommandManager;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeCreator;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.featureview.KalypsoFeatureViewPlugin;
import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * The Feature View shows a single feature as a form.
 * <p>
 * The view analyses the currently selection, and shows the first element which is a
 * {@link org.kalypsodeegree.model.feature.Feature}.
 * <p>
 * <p>
 * Features:
 * </p>
 * <ul>
 * <li>Shows the current selected feature in either a view or an editor. The latter can only be recieved, if the editor
 * adapts {@link org.eclipse.jface.viewers.ISelectionProvider}or
 * {@linked org.eclipse.jface.viewers.IPostSelectionProvider}.</li>
 * <li>In preference, the view listens to post-selections, in order to change the shown feature not too often.</li>
 * <li>If the returned selection is a {@link org.kalypso.ogc.gml.selection.IFeatureSelection}, changes (i.e. made by
 * the user) in the feature-control will be immediately postet into the given
 * {@link org.kalypso.ogc.gml.mapmodel.CommandableWorkspace}.</li>
 * <li>If the current selection changes to something not viewable, the last shown feature continues to be shown. If the
 * source of the shown feature is closed, the view releases the feature.</li>
 * <li>While the view is updated (a new feature was selected),</li>
 * <li>If the feature changes (either by editing it inside the view or from outside), the view will redisplay the new
 * content.</li>
 * <li></li>
 * </ul>
 * TODO 's:
 * <ul>
 * <li>Save data: ceate a global action handler to save gml-data. Every workbench part which changes a GmlWorkspace,
 * including this view, should register for this handler.</li>
 * <li>Are there any global (feature-)actions which apply to this view? If yes, it may be a good idea to implement
 * ISelectionProvider</li>
 * <li></li>
 * </ul>
 * 
 * @see org.eclipse.jface.viewers.IPostSelectionProvider
 */
public class FeatureView extends ViewPart implements ModellEventListener
{
  private static final String _KEIN_FEATURE_SELEKTIERT_ = "<kein Feature selektiert>";

  /**
   * Settings constant for section name (value <code>FeatureView</code>).
   */
  private static final String STORE_SECTION = "FeatureView"; //$NON-NLS-1$

  /**
   * Settings constant for show tables (value <code>FeatureView.STORE_SHOW_TABLES</code>).
   */
  private static final String STORE_SHOW_TABLES = "FeatureView.STORE_SHOW_TABLES"; //$NON-NLS-1$

  private final IFeatureChangeListener m_fcl = new IFeatureChangeListener()
  {
    public void featureChanged( final FeatureChange change )
    {
      final GMLWorkspace workspace = m_featureComposite.getFeature().getWorkspace();
      final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, new FeatureChange[] { change } );

      m_target.setCommandManager( m_commandManager );
      m_target.postCommand( command, null );
    }

    public void openFeatureRequested( final Feature feature, final IPropertyType ftp )
    {
      // just show this feature in the view, don't change the selection this doesn't work
      // don't change the command manager, changing the feature only work inside the same workspace
      activateFeature( feature, false );
    }
  };

  protected final FeatureComposite m_featureComposite = new FeatureComposite( null, KalypsoCorePlugin.getDefault().getSelectionManager() );

  protected final JobExclusiveCommandTarget m_target = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );

  protected ICommandManager m_commandManager = null;

  private Group m_mainGroup;

  /** Recreates the Feature-Composite inside a scrollable container. */
  private final ScrolledCompositeCreator m_creator = new ScrolledCompositeCreator( null )
  {
    @Override
    protected Control createContents( final Composite scrollParent, int style )
    {
      return m_featureComposite.createControl( scrollParent, style );
    }
  };

  private ISchedulingRule m_mutextRule = new MutexRule();

  private ISelectionListener m_selectionListener = new ISelectionListener()
  {
    public void selectionChanged( final IWorkbenchPart part, final ISelection selection )
    {
      // controls of my feature composite may create events, don't react
      if( selection != null )
        FeatureView.this.selectionChanged( selection );
    }
  };

  private IDialogSettings m_settings;

  private Action m_showTablesAction = null;

  private FormToolkit m_toolkit;

  public FeatureView( )
  {
    final IDialogSettings viewsSettings = KalypsoFeatureViewPlugin.getDefault().getDialogSettings();

    m_settings = viewsSettings.getSection( STORE_SECTION );
    if( m_settings == null )
    {
      m_settings = viewsSettings.addNewSection( STORE_SECTION );
      // set default values
      m_settings.put( STORE_SHOW_TABLES, true );
    }

    m_featureComposite.setShowTables( m_settings.getBoolean( STORE_SHOW_TABLES ) );
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
   */
  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    final IWorkbenchPage page = site.getPage();
    page.getWorkbenchWindow().getSelectionService().addPostSelectionListener( m_selectionListener );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    activateFeature( null, false ); // to unhook listeners
    m_featureComposite.dispose();

    final IWorkbenchPage page = getSite().getPage();
    page.getWorkbenchWindow().getSelectionService().removePostSelectionListener( m_selectionListener );
  }

  public void setShowTables( final boolean showTables )
  {
    final Feature currentFeature = getCurrentFeature();
    m_featureComposite.setShowTables( showTables );
    m_settings.put( STORE_SHOW_TABLES, showTables );

    activateFeature( currentFeature, true );
  }

  public boolean isShowTables( )
  {
    return m_featureComposite.isShowTables();
  }

  protected void selectionChanged( final ISelection selection )
  {
    if( selection instanceof IFeatureSelection )
    {
      final IFeatureSelection featureSel = (IFeatureSelection) selection;

      final Feature feature = featureFromSelection( featureSel );
      m_commandManager = featureSel.getWorkspace( feature );
      activateFeature( feature, false );
    }
    else
    {
      m_commandManager = null;
      activateFeature( null, false );
    }
  }

  private Feature featureFromSelection( final IFeatureSelection featureSel )
  {
    final Feature focusedFeature = featureSel.getFocusedFeature();
    if( focusedFeature != null )
      return focusedFeature;

    for( final Iterator sIt = featureSel.iterator(); sIt.hasNext(); )
    {
      final Object object = sIt.next();
      if( object instanceof Feature )
        return (Feature) object;
    }
    return null;
  }

  @Override
  public void createPartControl( final Composite parent )
  {
    // Comment this in, if you want to use the FormToolkit for FeatureView.
    // m_toolkit = new FormToolkit( parent.getDisplay() );

    m_featureComposite.set_formToolkit( m_toolkit );

    m_mainGroup = new Group( parent, SWT.NONE );
    m_mainGroup.setLayout( new GridLayout() );
    m_mainGroup.setText( _KEIN_FEATURE_SELEKTIERT_ );

    /* If a toolkit is set, use it for the main group. */
    if( m_toolkit != null )
      m_toolkit.adapt( m_mainGroup, true, true );

    m_featureComposite.addChangeListener( m_fcl );

    activateFeature( null, false );

    // add showTables-Action to menu-bar
    // we do this here, because adding it via the org.eclipse.ui.viewActions extension-point
    // does not allow to set the checked state dynamically
    m_showTablesAction = new Action( "Tabellen anzeigen", Action.AS_CHECK_BOX )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        setShowTables( isChecked() );
      }
    };
    m_showTablesAction.setChecked( isShowTables() );
    getViewSite().getActionBars().getMenuManager().add( m_showTablesAction );

    final ISelection selection = getSite().getWorkbenchWindow().getSelectionService().getSelection();
    selectionChanged( selection );
  }

  /**
   * Passing the focus request to the viewer's control.
   */
  @Override
  public void setFocus( )
  {
    m_mainGroup.setFocus();
  }

  /**
   * @param force
   *          if true, alwys reset this view, else, only if feature has really changed.
   */
  protected void activateFeature( final Feature feature, final boolean force )
  {
    final Group mainGroup = m_mainGroup;
    final ScrolledCompositeCreator creator = m_creator;

    final Feature oldFeature = m_featureComposite.getFeature();
    final GMLWorkspace oldWorkspace = oldFeature == null ? null : oldFeature.getWorkspace();
    final GMLWorkspace workspace = feature == null ? null : feature.getWorkspace();
    if( !force && oldWorkspace == workspace && oldFeature == feature )
      return;

    final Job job = new UIJob( getSite().getShell().getDisplay(), "Feature anzeigen" )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {

        if( oldWorkspace != null )
        {
          oldWorkspace.removeModellListener( FeatureView.this );
          getSite().setSelectionProvider( null );
        }

        if( m_featureComposite != null )
          m_featureComposite.disposeControl();
        final Control scroller = creator.getScrolledComposite();
        if( scroller != null )
          scroller.dispose();

        if( m_featureComposite == null )
          return Status.OK_STATUS;

        m_featureComposite.setFeature( feature );

        final String groupLabel;
        if( workspace != null && feature != null && mainGroup != null && (!mainGroup.isDisposed()) )
        {
          workspace.addModellListener( FeatureView.this );

          creator.createControl( mainGroup, SWT.V_SCROLL, SWT.NONE );

          FormToolkit toolkit = getToolkit();

          Control contentControl = creator.getContentControl();

          ScrolledComposite scrolledComposite = creator.getScrolledComposite();
          scrolledComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

          /* If a toolkit is set, use it. */
          if( toolkit != null )
          {
            toolkit.adapt( contentControl, true, true );
            toolkit.adapt( scrolledComposite, true, true );
          }

          groupLabel = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
        }
        else
          groupLabel = _KEIN_FEATURE_SELEKTIERT_;

        if( !mainGroup.isDisposed() )
        {
          mainGroup.setText( groupLabel );
          mainGroup.layout();
        }

        return Status.OK_STATUS;
      }
    };

    job.setRule( m_mutextRule );
    job.setUser( true );

    // This is the way to do it, but still blocks the user interface.
    // If this is still too slow, split the job into ui and non-ui
    // parts and run the non-ui part in a non-blocking thread.
    // NOTE: running via IProgrssSerive#runInUi gives a
    // ugly (and slow) modal pop-ups everytime the user changes the selection
    job.schedule();

    // this is the official way to do it
    // but gives no response (busy-cursor) to the user
    // final IWorkbenchSiteProgressService siteService = (IWorkbenchSiteProgressService)getSite().getAdapter(
    // IWorkbenchSiteProgressService.class );
    // siteService.schedule( job, 0 /* now */, true /* use half-busy cursor in part */);
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    // TODO: why doesn't the feature composite itself is a modelllisztener and reacts to the changes?
    if( modellEvent.isType( ModellEvent.FEATURE_CHANGE ) )
    {
      final Group mainGroup = m_mainGroup;
      final Control control = m_featureComposite.getControl();
      if( mainGroup != null && !mainGroup.isDisposed() && control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            // As the label of the main group may depend on the values of the feature, we must update it as well.
            final Feature feature = m_featureComposite.getFeature();
            final String groupLabel = feature == null ? _KEIN_FEATURE_SELEKTIERT_ : FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
            if( !mainGroup.isDisposed() )
              mainGroup.setText( groupLabel );

            if( !control.isDisposed() )
              m_featureComposite.updateControl();
          }
        } );
      }
    }
  }

  public GMLWorkspace getCurrentworkspace( )
  {
    final Feature feature = m_featureComposite.getFeature();
    return feature == null ? null : feature.getWorkspace();
  }

  public Feature getCurrentFeature( )
  {
    return m_featureComposite.getFeature();
  }

  /** Returns the view template of the current feature composite and its children. */
  public FeatureviewType[] getCurrentViewTemplates( )
  {
    final Feature feature = getCurrentFeature();
    if( feature == null )
      return null;

    final Set<FeatureviewType> types = new HashSet<FeatureviewType>( 5 );
    m_featureComposite.collectViewTypes( types );

    return types.toArray( new FeatureviewType[types.size()] );
  }

  public FormToolkit getToolkit( )
  {
    return m_toolkit;
  }

  public void setToolkit( FormToolkit toolkit )
  {
    m_toolkit = toolkit;
  }
}