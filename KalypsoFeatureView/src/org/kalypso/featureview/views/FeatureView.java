package org.kalypso.featureview.views;

import java.util.Iterator;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeCreator;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.Annotation;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

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

  private final IFeatureChangeListener m_fcl = new IFeatureChangeListener()
  {
    public void featureChanged( final FeatureChange change )
    {
      // we know that it is a commandable workspace
      final CommandableWorkspace workspace = (CommandableWorkspace) m_featureComposite.getWorkspace();
      final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, new FeatureChange[] { change } );

      m_target.setCommandManager( workspace );
      m_target.postCommand( command, null );
    }

    public void openFeatureRequested( final Feature feature, final IPropertyType ftp )
    {
      final CommandableWorkspace workspace = (CommandableWorkspace) m_featureComposite.getWorkspace();
      // just show this feature in the view, don't change the selection this doesn't work
      activateFeature( workspace, feature );
    }
  };

  protected final FeatureComposite m_featureComposite = new FeatureComposite( null, null, KalypsoCorePlugin.getDefault().getSelectionManager() );

  protected final JobExclusiveCommandTarget m_target = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );

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
    activateFeature( null, null ); // to unhook listeners
    m_featureComposite.dispose();

    final IWorkbenchPage page = getSite().getPage();
    page.getWorkbenchWindow().getSelectionService().removePostSelectionListener( m_selectionListener );
  }

  protected void selectionChanged( final ISelection selection )
  {
    if( selection instanceof IFeatureSelection )
    {
      final IFeatureSelection featureSel = (IFeatureSelection) selection;

      final Feature feature = featureFromSelection( featureSel );
      activateFeature( featureSel.getWorkspace( feature ), feature );
      return;
    }

    activateFeature( null, null );
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
    m_mainGroup = new Group( parent, SWT.NONE );
    m_mainGroup.setLayout( new GridLayout() );
    m_mainGroup.setText( _KEIN_FEATURE_SELEKTIERT_ );

    m_featureComposite.addChangeListener( m_fcl );

    activateFeature( null, null );

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

  protected void activateFeature( final CommandableWorkspace workspace, final Feature feature )
  {
    final Group mainGroup = m_mainGroup;
    final ScrolledCompositeCreator creator = m_creator;

    // TODO: check first, if there is anything to do, else
    // we get a hour-glass everytime we select anything
    final Job job = new UIJob( getSite().getShell().getDisplay(), "Feature anzeigen" )
    {
      @Override
      public IStatus runInUIThread( IProgressMonitor monitor )
      {
        // we know that we always have CommandableWorkspace
        final CommandableWorkspace oldWorkspace = (CommandableWorkspace) m_featureComposite.getWorkspace();
        final Feature oldFeature = m_featureComposite.getFeature();

        if( oldWorkspace == workspace && oldFeature == feature )
          return Status.OK_STATUS;

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

        m_featureComposite.setFeature( workspace, feature );

        final String groupLabel;
        if( workspace != null && feature != null && mainGroup != null && (!mainGroup.isDisposed()) )
        {
          workspace.addModellListener( FeatureView.this );
          // getSite().setSelectionProvider( workspace.getSelectionManager() );

          creator.createControl( mainGroup, SWT.V_SCROLL, SWT.NONE );
          creator.getScrolledComposite().setLayoutData( new GridData( GridData.FILL_BOTH ) );

          final IFeatureType featureType = feature.getFeatureType();
          final Annotation annotation = AnnotationUtilities.getAnnotation( featureType );
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
    // final IWorkbenchSiteProgressService siteService = (IWorkbenchSiteProgressService)getSite().getAdapter(
    // IWorkbenchSiteProgressService.class );
    // siteService.schedule( job, 0 /* now */, true /* use half-busy cursor in part */);
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
          public void run( )
          {
            m_featureComposite.updateControl();
          }
        } );
    }
  }

  public GMLWorkspace getCurrentworkspace( )
  {
    return m_featureComposite.getWorkspace();
  }

  public Feature getCurrentFeature( )
  {
    return m_featureComposite.getFeature();
  }

  public FeatureviewType getCurrentViewTemplates( )
  {
    final Feature feature = getCurrentFeature();
    if( feature == null )
      return null;

    return m_featureComposite.getFeatureview( feature.getFeatureType() );
  }
}