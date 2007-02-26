package org.kalypso.afgui.viz;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.commands.Category;
import org.eclipse.core.commands.Command;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.forms.events.ExpansionAdapter;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.events.IExpansionListener;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IHelp;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowPart;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

import de.renew.workflow.WorkflowConnector;

public class WorkflowControl
{
  private static final String TASKS_COMMANDS_CATEGORY = "org.kalypso.kalypso1d2d.pjt.TasksCommands";

  private static final Logger logger = Logger.getLogger( WorkflowControl.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  final static String KEY_ITASK_ACTIONS = "_KEY_ITASK_ACTIONS_";

  final static String KEY_IPHASE = IPhase.class.getName();

  final static String KEY_ITASKGROUP = ITaskGroup.class.getName();

  final static String KEY_ISUBTASKGROUP = ISubTaskGroup.class.getName();

  private IWorkflow m_workflow;

  private Composite top;

  private FormToolkit toolkit;

  ScrolledForm m_form;

  ScrolledForm aTBComp;

  ScrolledForm tTBComp;

  ToolBarManager tTBMng;

  ToolBarManager aTBMng;

  ExpansionAdapter expansionAdapter = new ExpansionAdapter()
  {
    @Override
    public void expansionStateChanged( ExpansionEvent e )
    {
      m_form.reflow( true );
    }
  };

  SectionListener m_subTaskGroupListener;

  SectionListener m_taskGroupListener;

  SectionListener m_phaseListener;

  IProject m_activeProject;

  Map<Object, List<Control>> taskControlMap;

  private String m_phaseFromMemento;

  private String m_taskGroupFromMemento;

  private String m_subTaskGroupFromMemento;

  public WorkflowControl( final IWorkflow workflow )
  {
    this.m_workflow = workflow;
  }

  public void createControl( final Composite parent )
  {
    // top
    top = new Composite( parent, SWT.FILL );
    top.setLayout( new FillLayout() );

    m_subTaskGroupListener = new SectionListener();
    m_taskGroupListener = new SectionListener();
    m_phaseListener = new SectionListener();

    createBaseContainersBottomToolbars();
    createWorkFlowView();
  }

  public void setActiveProject( IProject activeProject )
  {
    this.m_activeProject = activeProject;
  }

  public void setWorkflow( IWorkflow workflow )
  {
    this.m_workflow = workflow;
    createWorkFlowView();
  }

  public void setVisible( boolean visible )
  {
    top.setVisible( visible );
    top.getParent().update();
    tTBMng.removeAll();
    tTBMng.update( true );
    aTBMng.removeAll();
    aTBMng.update( true );
  }

  private void createWorkFlowView( )
  {
    // remove old layout element in form
    for( final Control c : m_form.getBody().getChildren() )
    {
      // recycle getData(key)
      c.dispose();
    }

    if( m_workflow == null )
    {
      return;
    }

    m_form.getBody().setLayout( new FormLayout() );
    FormData fd = new FormData();
    fd.left = new FormAttachment( 0, 0 );
    fd.right = new FormAttachment( 100, 0 );
    fd.top = new FormAttachment( 0, 0 );

    Section phaseToExpand = null;
    Section taskGroupToExpand = null;
    Section subTaskGroupToExpand = null;

    // /IPhase
    final ArrayList<Section> phaseCntls = new ArrayList<Section>();
    for( final IPhase phase : m_workflow.getPhases() )
    {
      final Section madeSec = createPhaseExpandable( phase, m_form );
      phaseCntls.add( madeSec );
      madeSec.setLayoutData( fd );
      fd = new FormData();
      fd.left = new FormAttachment( 0, 0 );
      fd.right = new FormAttachment( 100, 0 );
      fd.top = new FormAttachment( madeSec );
      madeSec.addExpansionListener( m_phaseListener );
      if( phase.getURI().equals( m_phaseFromMemento ) )
      {
        phaseToExpand = madeSec;
      }
    }

    final ArrayList<Section> taskGroupECs = new ArrayList<Section>();
    for( final Section ec : phaseCntls )
    {
      final IPhase phase = (IPhase) ec.getData( KEY_IPHASE );
      final List<ITaskGroup> curTGList = phase.getTaskGroups();
      if( !curTGList.isEmpty() )
      {
        final Composite comp = toolkit.createComposite( ec, SWT.BORDER );
        comp.setLayout( new GridLayout() );
        for( final ITaskGroup tg : phase.getTaskGroups() )
        {
          final Section madeSec = createTaskGroupExpandable( tg, comp, phase );
          taskGroupECs.add( madeSec );
          madeSec.addExpansionListener( m_taskGroupListener );
          if( tg.getURI().equals( m_taskGroupFromMemento ) )
          {
            taskGroupToExpand = madeSec;
          }
        }
        ec.setClient( comp );
      }
    }

    // /SubTaskGroup
    final ArrayList<Section> stgSecs = new ArrayList<Section>();
    for( final Section sec : taskGroupECs )
    {
      final ITaskGroup taskGroup = (ITaskGroup) sec.getData( KEY_ITASKGROUP );
      final List<ISubTaskGroup> curSTGList = taskGroup.getSubTaskGroups();
      if( !curSTGList.isEmpty() )
      {
        final Composite comp = toolkit.createComposite( sec, SWT.BORDER );
        comp.setLayout( new GridLayout() );
        for( final ISubTaskGroup stg : curSTGList )
        {
          final IPhase phase = (IPhase) sec.getData( KEY_IPHASE );
          final Section madeSec = createSubTaskGroupExpandable( stg, comp, taskGroup, phase );
          madeSec.addExpansionListener( m_subTaskGroupListener );
          if( stg.getURI().equals( m_subTaskGroupFromMemento ) )
          {
            subTaskGroupToExpand = madeSec;
          }
          stgSecs.add( madeSec );
        }
        sec.setClient( comp );
      }
    }

    if( phaseToExpand != null )
    {
      phaseToExpand.setExpanded( true );
      if( taskGroupToExpand != null )
      {
        taskGroupToExpand.setExpanded( true );
        if( subTaskGroupToExpand != null )
        {
          subTaskGroupToExpand.setExpanded( true );
        }
      }
    }

    m_form.reflow( true );
    tTBComp.reflow( true );
    aTBComp.reflow( true );
  }

  private Section createSubTaskGroupExpandable( final ISubTaskGroup stg, final Composite ec, final ITaskGroup parentTG, final IPhase phase )
  {
    final Section childEC = toolkit.createSection( ec, Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE );
    final String name = getWorkflowPartName( stg );
    childEC.setText( name );
    childEC.setData( KEY_IPHASE, phase );
    childEC.setData( KEY_ISUBTASKGROUP, stg );
    childEC.setData( KEY_ITASKGROUP, parentTG );
    return childEC;
  }

  private void createBaseContainersBottomToolbars( )
  {
    toolkit = new FormToolkit( top.getDisplay() );
    Composite containerForm = toolkit.createComposite( top );

    containerForm.setLayout( new FormLayout() );

    FormData fd;

    fd = new FormData();
    fd.width = 270;// TODO check how not to use width
    fd.left = new FormAttachment( 0, 0 );
    fd.bottom = new FormAttachment( 60, 0 );
    fd.top = new FormAttachment( 0, 0 );
    m_form = toolkit.createScrolledForm( containerForm );
    m_form.setLayoutData( fd );

    // SEPARAtor workflow (task activities)
    Label wSepTA = toolkit.createSeparator( containerForm, SWT.HORIZONTAL | SWT.BOLD );
    fd = new FormData();
    fd.height = 1;
    fd.left = new FormAttachment( 0, 0 );
    // fd.bottom= new FormAttachment(100,0);
    fd.top = new FormAttachment( m_form );// 30,0);
    fd.right = new FormAttachment( 100, 0 );// aTBComp);
    wSepTA.setLayoutData( fd );

    // tasks bottom left
    tTBComp = toolkit.createScrolledForm( containerForm );
    // toolkit.createComposite(
    // containerForm,
    // SWT.BORDER|SWT.BOLD);

    fd = new FormData();
    fd.left = new FormAttachment( 0, 0 );
    fd.bottom = new FormAttachment( 100, 0 );
    fd.top = new FormAttachment( wSepTA );// form);//30,0);
    fd.right = new FormAttachment( 50, 0 );
    tTBComp.setLayoutData( fd );

    // SEPARAtor task activities
    Label l = toolkit.createSeparator( containerForm, SWT.VERTICAL | SWT.BOLD );
    fd = new FormData();
    fd.width = 1;
    fd.left = new FormAttachment( tTBComp );
    fd.bottom = new FormAttachment( 100, 0 );
    fd.top = new FormAttachment( m_form );// 30,0);
    // fd.right=new FormAttachment(al);//aTBComp);
    l.setLayoutData( fd );

    // activities
    fd = new FormData();
    // fd.width=16;
    fd.left = new FormAttachment( l );// form);
    fd.right = new FormAttachment( 100, 0 );
    fd.bottom = new FormAttachment( 100, 0 );
    fd.top = new FormAttachment( m_form );
    aTBComp = toolkit.createScrolledForm( containerForm );
    aTBComp.setLayoutData( fd );
    aTBComp.getBody().setLayout( new TableWrapLayout() );
    ToolBar aTB = new ToolBar( aTBComp.getBody(), SWT.V_SCROLL | SWT.WRAP | SWT.VERTICAL );

    aTBMng = new ToolBarManager( aTB );
    toolkit.adapt( aTB );

    tTBComp.getBody().setLayout( new TableWrapLayout() );
    ToolBar tb = new ToolBar( tTBComp.getBody(), SWT.V_SCROLL | SWT.WRAP | SWT.VERTICAL );

    tTBMng = new ToolBarManager( tb );
    toolkit.adapt( tb );
  }

  private Section createTaskGroupExpandable( ITaskGroup tg, Composite ec, IPhase phase )
  {
    Section childEC = toolkit.createSection( ec, Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    childEC.setText( getWorkflowPartName( tg ) );
    childEC.setToolTipText( getWorkflowPartHelp( tg ) );
    childEC.setData( KEY_ITASKGROUP, tg );
    childEC.setData( KEY_IPHASE, phase );
    return childEC;
  }

  private Section createPhaseExpandable( IPhase phase, ScrolledForm form )
  {
    Section ec = toolkit.createSection( form.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    ec.setText( getWorkflowPartName( phase ) );
    ec.setToolTipText( getWorkflowPartHelp( phase ) );
    ec.setData( KEY_IPHASE, phase );
    return ec;
  }

  String getWorkflowPartName( IWorkflowPart wp )
  {
    String name = wp.getName();
    final String uri = wp.getURI();
    if( name == null || name.equals( "" ) )
    {
      name = uri;
    }
    return name;// .concat( uri.equals( m_lastExecuted ) ? "*" : "" );
  }

  private String getWorkflowPartHelp( IWorkflowPart wp )
  {
    IHelp help = wp.getHelp();

    if( help == null )
    {
      return "";
    }
    else
    {
      String helpString = help.getHelp();
      if( helpString == null )
      {
        return "";
      }
      else
      {
        return helpString;
      }
    }
  }  

  Command getCommand( final ICommandService commandService, final String commandId )
  {
    final Command command = commandService.getCommand( commandId );
    if( !command.isDefined() )
    {
      final Category category = commandService.getCategory( "org.kalypso.afgui.tasks" );
      if( !category.isDefined() )
      {
        category.define( TASKS_COMMANDS_CATEGORY, null );
      }
      command.define( commandId, null, category );
    }
    return command;
  }

  boolean isTaskPossible( final Command command )
  {
    return // command.isEnabled() &&
    (!WorkflowConnector.isWorkflowMode() || WorkflowConnector.getConnector().canRequest( command.getId() ));
  }

  /**
   * Saves the state for the workflow view
   * 
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  public void saveState( final IMemento memento )
  {
    final Section lastPhase = m_phaseListener.getLastExpanded();
    if( lastPhase != null )
    {
      final IPhase phase = (IPhase) lastPhase.getData( KEY_IPHASE );
      memento.putString( KEY_IPHASE, phase != null ? phase.getURI() : null );
    }
    final Section lastTaskGroup = m_taskGroupListener.getLastExpanded();
    if( lastTaskGroup != null )
    {
      final ITaskGroup taskGroup = (ITaskGroup) lastTaskGroup.getData( KEY_ITASKGROUP );
      memento.putString( KEY_ITASKGROUP, taskGroup != null ? taskGroup.getURI() : null );
    }
    final Section lastSubTaskGroup = m_subTaskGroupListener.getLastExpanded();
    if( lastSubTaskGroup != null )
    {
      final ISubTaskGroup subTaskGroup = (ISubTaskGroup) lastSubTaskGroup.getData( KEY_ISUBTASKGROUP );
      memento.putString( KEY_ISUBTASKGROUP, subTaskGroup != null ? subTaskGroup.getURI() : null );
    }
  }

  public void restoreState( final IMemento memento )
  {
    if( memento != null )
    {
      m_phaseFromMemento = memento.getString( KEY_IPHASE );
      m_taskGroupFromMemento = memento.getString( KEY_ITASKGROUP );
      m_subTaskGroupFromMemento = memento.getString( KEY_ISUBTASKGROUP );
    }
  }

  // ///////////////////////////////////////////////
  private class SectionListener implements IExpansionListener
  {
    private Section m_lastExpanded;

    public SectionListener( )
    {
    }

    @SuppressWarnings("unchecked")
    public void expansionStateChanged( final ExpansionEvent e )
    {
      tTBMng.removeAll();
      aTBMng.removeAll();
      final Section section = (Section) e.getSource();
      List<TaskAction<ITask>> actions = (List<TaskAction<ITask>>) section.getData( KEY_ITASK_ACTIONS );
      if( actions == null )
      {
        actions = makeTaskActions( section );
      }
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
      for( TaskAction ta : actions )
      {
        final Command command = getCommand( commandService, ta.getId() );
        if( isTaskPossible( command ) )
        {
          tTBMng.add( ta );
        }
      }
      if( m_lastExpanded != section )
      {
        if( m_lastExpanded != null )
        {
          final Control cs[] = m_lastExpanded.getChildren();
          if( cs != null )
          {
            for( final Control innerSection : cs )
            {
              if( innerSection instanceof Section )
              {
                ((Section) innerSection).setExpanded( false );
              }
            }
          }
          m_lastExpanded.setExpanded( false );
        }
        m_lastExpanded = section;
        m_form.reflow( false );
      }
      else
      {
        section.setExpanded( true );
      }
      tTBMng.update( true );
      tTBComp.reflow( true );
      aTBMng.update( true );
      aTBComp.reflow( true );
    }

    private List<TaskAction<ITask>> makeTaskActions( final Section section )
    {
      final List<TaskAction<ITask>> actions = new ArrayList<TaskAction<ITask>>();
      final ArrayList<ITask> tasks = new ArrayList<ITask>();

//      final IPhase p = (IPhase) section.getData( KEY_IPHASE );
//      if( p != null )
//      {
//        tasks.addAll( p.getTasks() );
//      }
      final ITaskGroup tg = (ITaskGroup) section.getData( KEY_ITASKGROUP );
      if( tg != null )
      {
        for( final ITask task : tg.getTasks() )
        {
          tasks.add( task );
        }
      }
      final ISubTaskGroup stg = (ISubTaskGroup) section.getData( KEY_ISUBTASKGROUP );
      if( stg != null )
      {
        for( final ITask task : stg.getTasks() )
        {
          tasks.add( task );
        }
      }
      for( final ITask task : tasks )
      {
        final TaskAction<ITask> ta = new TaskAction<ITask>( task );
        actions.add( ta );
      }
      section.setData( KEY_ITASK_ACTIONS, actions );
      return actions;
    }

    public void expansionStateChanging( ExpansionEvent e )
    {

    }

    public Section getLastExpanded( )
    {
      return m_lastExpanded;
    }
  }

  // ///////////////////////////////////////////////////////////
  class TaskAction<E extends IWorkflowPart> extends Action
  {
    private final E m_workflowPart;

    private List<TaskAction<IActivity>> m_activityActions;

    public TaskAction( final E workflowPart )
    {
      this.m_workflowPart = workflowPart;
    }

    @Override
    public String getText( )
    {
      return getWorkflowPartName( m_workflowPart );
    }

    @Override
    public void runWithEvent( final Event event )
    {
      if( m_workflowPart instanceof ITask )
      {
        final ITask task = (ITask) m_workflowPart;
        aTBMng.removeAll();
        new Label( aTBMng.getControl(), SWT.BORDER ).setText( task.getName() );
        if( m_activityActions == null )
        {
          m_activityActions = new ArrayList<TaskAction<IActivity>>();
          for( final IActivity a : task.getActivities() )
          {
            final TaskAction<IActivity> action = new TaskAction<IActivity>( a );
            m_activityActions.add( action );
            aTBMng.add( action );
          }
        }
        else
        {
          // TODO nullPEx while using iterator throw compactloo
          final IWorkbench workbench = PlatformUI.getWorkbench();
          final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
          for( int i = 0; i < m_activityActions.size(); i++ )
          {
            final TaskAction<IActivity> ta = m_activityActions.get( i );
            final Command command = getCommand( commandService, ta.getId() );
            if( isTaskPossible( command ) )
            {
              aTBMng.add( ta );
            }
          }
        }
        aTBMng.update( true );
        aTBComp.reflow( true );
      }
      doTaskOrActivity( this, event );
    }

    @Override
    public String getId( )
    {
      return m_workflowPart.getURI();
    }

    @Override
    public int getStyle( )
    {
      return Action.AS_PUSH_BUTTON;
    }

    @Override
    public String getToolTipText( )
    {
      try
      {
        return m_workflowPart.getHelp().getHelp();
      }
      catch( final Throwable th )
      {
        return null;
      }
    }

    public E getWorkflowPart( )
    {
      return m_workflowPart;
    }
    
    private final void doTaskOrActivity( final TaskAction task, final Event event )
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
      final String name = task.getId();
      try
      {
        final Command command = getCommand( commandService, name );
        final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
        handlerService.executeCommand( command.getId(), event );
      }
      catch( final Throwable e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        ErrorDialog.openError( m_form.getShell(), "Workflow Commmand", "Kommando konnte nicht ausgeführt werden: " + name, status );
        KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( status );
        logger.log( Level.SEVERE, "Failed to execute command: " + name, e );
      }
    }
  }

}
