package org.kalypso.featureview.views;

import java.util.Iterator;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexSchedulingRule;
import org.kalypso.contribs.eclipse.jface.viewers.SelectionProviderAdapter;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeCreator;
import org.kalypso.contribs.eclipse.ui.PartAdapter2;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.ICommandableFeatureSelection;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.Annotation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * The Feature View shows a single feature as a form.
 * <p>
 * The view analyses the currently selection, and shows the first element which is a
 * {@link org.kalypsodeegree.model.feature.Feature}.
 * <p>
 * 
 * <p>
 * Features:
 * </p>
 * <ul>
 * <li>Shows the current selected feature in either a view or an editor. The latter can only be recieved, if the editor
 * adapts {@link org.eclipse.jface.viewers.ISelectionProvider}or
 * {@linked org.eclipse.jface.viewers.IPostSelectionProvider}.</li>
 * <li>In preference, the view listens to post-selections, in order to change the shown feature not too often.</li>
 * <li>If the returned selection is a {@link org.kalypso.ogc.gml.selection.ICommandableFeatureSelection}, changes
 * (i.e. made by the user) in the feature-control will be immediately postet into the given
 * {@link org.kalypso.ogc.gml.mapmodel.CommandableWorkspace}.</li>
 * <li>If the current selection changes to something not viewable, the last shown feature continues to be shown. If the
 * source of the shown feature is closed, the view releases the feature.</li>
 * <li>While the view is updated (a new feature was selected),</li>
 * <li>If the feature changes (either by editing it inside the view or from outside), the view will redisplay the new
 * content.</li>
 * <li></li>
 * </ul>
 * 
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
 *  
 */
public class FeatureView extends ViewPart implements ISelectionChangedListener, ISelectionListener, ModellEventListener
{
  private static final String _KEIN_FEATURE_SELEKTIERT_ = "<kein Feature selektiert>";

  /** An empty selection provider for the SelectionChangeEvent */
  private static final ISelectionProvider m_selProvider = new SelectionProviderAdapter()
  {
    public ISelection getSelection()
    {
      return null;
    }

    public void setSelection( final ISelection selection )
    {}
  };

  protected final FeatureComposite m_featureComposite = new FeatureComposite( null, null );

  protected final JobExclusiveCommandTarget m_target = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );

  private Group m_mainGroup;

  /** Recreates the Feature-Composite inside a scrollable container. */
  private final ScrolledCompositeCreator m_creator = new ScrolledCompositeCreator( null )
  {
    protected Control createContents( final Composite scrollParent, int style )
    {
      return m_featureComposite.createControl( scrollParent, style );
    }
  };

  private ISchedulingRule m_mutextRule = new MutexSchedulingRule();

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
   */
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    final IWorkbenchPage page = site.getPage();
    page.addPartListener( new PartAdapter2()
    {
      /**
       * @see org.kalypso.contribs.eclipse.ui.PartAdapter2#partOpened(org.eclipse.ui.IWorkbenchPartReference)
       */
      public void partOpened( IWorkbenchPartReference partRef )
      {
        activateSelectionListener( partRef.getPart( false ) );
      }

      public void partClosed( IWorkbenchPartReference partRef )
      {
        final IWorkbenchPart part = partRef.getPart( false );
        if( part == FeatureView.this )
          page.removePartListener( this );
        else
          deactivateSelectionListener( part );
      }
    } );

    page.getWorkbenchWindow().getSelectionService().addSelectionListener( FeatureView.this );

    final IEditorReference[] editorReferences = page.getEditorReferences();
    for( int i = 0; i < editorReferences.length; i++ )
    {
      final IEditorReference reference = editorReferences[i];
      activateSelectionListener( reference.getPart( false ) );
    }
  }

  public void selectionChanged( final IWorkbenchPart part, final ISelection selection )
  {
    // controls of my feature composite may create events, don't react
    if( part != this && selection != null )
      selectionChanged( new SelectionChangedEvent( m_selProvider, selection ) );
  }

  protected final void activateSelectionListener( final IWorkbenchPart part )
  {
    final ISelectionProvider provider = getSelectionProviderFromPart( part );
    if( provider instanceof IPostSelectionProvider )
      ( (IPostSelectionProvider)provider ).addPostSelectionChangedListener( FeatureView.this );
    else if( provider != null )
      provider.addSelectionChangedListener( FeatureView.this );
  }

  protected final void deactivateSelectionListener( final IWorkbenchPart part )
  {
    final ISelectionProvider provider = getSelectionProviderFromPart( part );
    if( provider instanceof IPostSelectionProvider )
      ( (IPostSelectionProvider)provider ).removePostSelectionChangedListener( FeatureView.this );
    else if( provider != null )
      provider.removeSelectionChangedListener( FeatureView.this );

    if( provider != null )
    {
      // only if the current selection correspond to the shown feature
      // clean myself
      final ISelection selection = provider.getSelection();
      if( selection instanceof ICommandableFeatureSelection )
      {
        final Feature feature = featureFromSelection( (ICommandableFeatureSelection)selection );
        if( m_featureComposite.getFeature() == feature )
          activateFeature( null, null );
      }
    }
  }

  /**
   * Retrieves an {@link ISelectionProvider}from the given part by the {@link org.eclipse.core.runtime.IAdaptable}
   * -Mechanism. In preference, a {@link IPostSelectionProvider}is taken.
   * 
   * @return null, if no provider is found
   */
  private static ISelectionProvider getSelectionProviderFromPart( final IWorkbenchPart part )
  {
    if( part instanceof IEditorPart )
    {
      final IPostSelectionProvider selProvider = (IPostSelectionProvider)part.getAdapter( IPostSelectionProvider.class );
      if( selProvider != null )
        return selProvider;

      return (ISelectionProvider)part.getAdapter( ISelectionProvider.class );
    }

    return null;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    activateFeature( null, null ); // to unhook listeners
    m_featureComposite.dispose();

    final IWorkbenchPage page = getSite().getPage();
    final IEditorReference[] editorReferences = page.getEditorReferences();
    for( int i = 0; i < editorReferences.length; i++ )
    {
      final IEditorReference reference = editorReferences[i];
      deactivateSelectionListener( reference.getPart( false ) );
    }

    page.getWorkbenchWindow().getSelectionService().removeSelectionListener( this );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    final ISelection selection = event.getSelection();
    if( selection instanceof ICommandableFeatureSelection && !selection.isEmpty() )
    {
      final ICommandableFeatureSelection featureSel = (ICommandableFeatureSelection)selection;

      final Feature feature = featureFromSelection( featureSel );
      activateFeature( featureSel.getCommandableWorkspace(), feature );
    }
  }

  private Feature featureFromSelection( final ICommandableFeatureSelection featureSel )
  {
    final Feature focusedFeature = featureSel.getFocusedFeature();
    if( focusedFeature != null )
      return focusedFeature;

    for( final Iterator sIt = featureSel.iterator(); sIt.hasNext(); )
    {
      final Object object = sIt.next();
      if( object instanceof Feature )
        return (Feature)object;
    }
    return null;
  }

  public void createPartControl( final Composite parent )
  {
    m_mainGroup = new Group( parent, SWT.NONE );
    m_mainGroup.setLayout( new GridLayout() );
    m_mainGroup.setText( _KEIN_FEATURE_SELEKTIERT_ );

    m_featureComposite.addChangeListener( new IFeatureChangeListener()
    {
      public void featureChanged( final FeatureChange change )
      {
        // we know that it is a commandable workspace
        final CommandableWorkspace workspace = (CommandableWorkspace)m_featureComposite.getWorkspace();
        final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, new FeatureChange[]
        { change } );

        m_target.setCommandManager( workspace );
        m_target.postCommand( command, null );
      }

      public void openFeatureRequested( final Feature feature )
      {
      // TODO implement it
      }
    } );

    makeActions();
    hookContextMenu();
    contributeToActionBars();

    activateFeature( null, null );

    final IWorkbenchPart activePart = getSite().getPage().getActivePart();
    if( activePart instanceof IViewPart )
    {
      final ISelectionProvider selectionProvider = ( (IViewPart)activePart ).getViewSite().getSelectionProvider();
      if( selectionProvider != null )
        selectionChanged( activePart, selectionProvider.getSelection() );
    }
    else if( activePart instanceof IEditorPart )
    {
      final ISelectionProvider selectionProvider = getSelectionProviderFromPart( activePart );
      if( selectionProvider != null )
        selectionChanged( activePart, selectionProvider.getSelection() );
    }
  }

  private void hookContextMenu()
  {
  //    MenuManager menuMgr = new MenuManager( "#PopupMenu" );
  //    menuMgr.setRemoveAllWhenShown( true );
  //    menuMgr.addMenuListener( new IMenuListener()
  //    {
  //      public void menuAboutToShow( IMenuManager manager )
  //      {
  //        FeatureView.this.fillContextMenu( manager );
  //      }
  //    } );
  //    Menu menu = menuMgr.createContextMenu( viewer.getControl() );
  //    viewer.getControl().setMenu( menu );
  //    getSite().registerContextMenu( menuMgr, viewer );
  }

  private void contributeToActionBars()
  {
  //    IActionBars bars = getViewSite().getActionBars();
  //    fillLocalPullDown( bars.getMenuManager() );
  //    fillLocalToolBar( bars.getToolBarManager() );
  }

  //  private void fillLocalPullDown( IMenuManager manager )
  //  {
  //    manager.add( action1 );
  //    manager.add( new Separator() );
  //    manager.add( action2 );
  //  }

  //  private void fillContextMenu( IMenuManager manager )
  //  {
  //    manager.add( action1 );
  //    manager.add( action2 );
  //    manager.add( new Separator() );
  //    drillDownAdapter.addNavigationActions( manager );
  //    // Other plug-ins can contribute there actions here
  //    manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
  //  }

  //  private void fillLocalToolBar( IToolBarManager manager )
  //  {
  //    manager.add( action1 );
  //    manager.add( action2 );
  //    manager.add( new Separator() );
  //    drillDownAdapter.addNavigationActions( manager );
  //  }

  private void makeActions()
  {
  //    action1 = new Action()
  //    {
  //      public void run()
  //      {
  //        showMessage( "Action 1 executed" );
  //      }
  //    };
  //    action1.setText( "Action 1" );
  //    action1.setToolTipText( "Action 1 tooltip" );
  //    action1.setImageDescriptor( PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(
  //        ISharedImages.IMG_OBJS_INFO_TSK ) );
  //
  //    action2 = new Action()
  //    {
  //      public void run()
  //      {
  //        showMessage( "Action 2 executed" );
  //      }
  //    };
  //    action2.setText( "Action 2" );
  //    action2.setToolTipText( "Action 2 tooltip" );
  //    action2.setImageDescriptor( PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(
  //        ISharedImages.IMG_OBJS_INFO_TSK ) );
  //    doubleClickAction = new Action()
  //    {
  //      public void run()
  //      {
  //        ISelection selection = viewer.getSelection();
  //        Object obj = ( (IStructuredSelection)selection ).getFirstElement();
  //        showMessage( "Double-click detected on " + obj.toString() );
  //      }
  //    };
  }

  /**
   * Passing the focus request to the viewer's control.
   */
  public void setFocus()
  {
    m_mainGroup.setFocus();
  }

  private void activateFeature( final CommandableWorkspace workspace, final Feature feature )
  {
    final Group mainGroup = m_mainGroup;
    final ScrolledCompositeCreator creator = m_creator;

    final Job job = new UIJob( getSite().getShell().getDisplay(), "Feature anzeigen" )
    {
      public IStatus runInUIThread( IProgressMonitor monitor )
      {
        // we know that we always have CommandableWorkspace
        final CommandableWorkspace oldWorkspace = (CommandableWorkspace)m_featureComposite.getWorkspace();
        final Feature oldFeature = m_featureComposite.getFeature();

        if( oldWorkspace == workspace && oldFeature == feature )
          return Status.OK_STATUS;

        if( oldWorkspace != null )
          oldWorkspace.removeModellListener( FeatureView.this );

        if( m_featureComposite != null )
          m_featureComposite.disposeControl();
        final Control scroller = creator.getScrolledComposite();
        if( scroller != null )
          scroller.dispose();

        if( m_featureComposite == null )
          return Status.OK_STATUS;

        m_featureComposite.setFeature( workspace, feature );

        final String groupLabel;
        if( workspace != null && feature != null )
        {
          workspace.addModellListener( FeatureView.this );

          creator.createControl( mainGroup, SWT.V_SCROLL, SWT.NONE );
          creator.getScrolledComposite().setLayoutData( new GridData( GridData.FILL_BOTH ) );

          final FeatureType featureType = feature.getFeatureType();
          final Annotation annotation = featureType.getAnnotation( "de" );
          final String label = annotation == null ? featureType.getName() : annotation.getLabel();
          groupLabel = label + " - " + feature.getId();
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

    // This is the way to to it, but still blocks the user interface.
    // If this is still too slow, split the job into ui and non-ui
    // parts and run the non-ui part in a non-blocking thread.
    // NOTE: running via IProgrssSerive#runInUi gives a
    // ugly (and slow) modal pop-ups everytime the user changes the selection
    job.schedule();

    // this is the official way to do it
    // but gives no response (busy-cursor) to the user
    //    final IWorkbenchSiteProgressService siteService = (IWorkbenchSiteProgressService)getSite().getAdapter(
    //        IWorkbenchSiteProgressService.class );
    //    siteService.schedule( job, 0 /* now */, true /* use half-busy cursor in part */);
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent.isType( ModellEvent.FEATURE_CHANGE ) )
    {
      final Control control = m_featureComposite.getControl();
      if( control != null && !control.isDisposed() )
        control.getDisplay().asyncExec( new Runnable()
        {
          public void run()
          {
            m_featureComposite.updateControl();
          }
        } );
    }
  }
}