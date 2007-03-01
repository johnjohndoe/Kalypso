/**
 * 
 */
package org.kalypso.afgui.views;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.commands.Category;
import org.eclipse.core.commands.Command;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.workflow.Activity;
import org.kalypso.workflow.Phase;
import org.kalypso.workflow.Task;
import org.kalypso.workflow.TaskGroup;
import org.kalypso.workflow.Workflow;

/**
 * @author Stefan Kurzbach
 */
public class WorkflowControl2
{
  final static Logger logger = Logger.getLogger( WorkflowControl2.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  private static final String MEMENTO_LAST_SELECTION = "lastSelection";

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  TreeViewer m_treeViewer;

  TreePath m_lastTreePath;

  private String m_selectionFromMemento;

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite top = new Composite( parent, SWT.FILL );
    top.setLayout( new FillLayout() );
    m_treeViewer = new TreeViewer( top, SWT.SINGLE )
    {
      /**
       * @see org.eclipse.jface.viewers.TreeViewer#doUpdateItem(org.eclipse.swt.widgets.Item, java.lang.Object)
       */
      @Override
      protected void doUpdateItem( final Item item, final Object element )
      {
        super.doUpdateItem( item, element );
        if( item != null && element instanceof Activity )
        {
          item.setData( "_HELP", ((Activity) element).getHelp() );
        }
      }
    };
    m_treeViewer.setContentProvider( new WorkflowContentProvider2() );

    final Tree tree = m_treeViewer.getTree();
    final Display display = tree.getDisplay();
    // Disable native tooltip
    tree.setToolTipText( "" );

    // Implement a "fake" tooltip
    final Listener labelListener = new Listener()
    {
      public void handleEvent( final Event event )
      {
        final Label control = (Label) event.widget;
        final Shell shell = control.getShell();
        switch( event.type )
        {
          case SWT.MouseDown:
            final Event e = new Event();
            e.item = ((TreeItem) control.getData( "_TREEITEM" ));
            // set the selection as if
            // the mouse down event went through to the tree
            tree.setSelection( new TreeItem[] { (TreeItem) e.item } );
            tree.notifyListeners( SWT.Selection, e );
            tree.setFocus();
          case SWT.MouseExit:
            shell.dispose();
            break;
        }
      }
    };

    final Listener treeListener = new Listener()
    {
      Shell tip = null;

      Label label = null;

      public void handleEvent( final Event event )
      {
        if( event.type == SWT.MouseHover )
        {
          final TreeItem item = tree.getItem( new Point( event.x, event.y ) );
          if( item != null )
          {
            if( tip != null && !tip.isDisposed() )
              tip.dispose();
            tip = new Shell( display, SWT.ON_TOP | SWT.NO_FOCUS | SWT.TOOL );
            tip.setBackground( display.getSystemColor( SWT.COLOR_INFO_BACKGROUND ) );
            final FillLayout layout = new FillLayout();
            layout.marginWidth = 2;
            tip.setLayout( layout );
            label = new Label( tip, SWT.NONE );
            label.setForeground( display.getSystemColor( SWT.COLOR_INFO_FOREGROUND ) );
            label.setBackground( display.getSystemColor( SWT.COLOR_INFO_BACKGROUND ) );
            label.setData( "_TREEITEM", item );
            final Object help = item.getData( "_HELP" );
            if( help != null )
            {
              label.setText( (String) help );
            }
            label.addListener( SWT.MouseExit, labelListener );
            label.addListener( SWT.MouseDown, labelListener );
            final Point size = tip.computeSize( SWT.DEFAULT, SWT.DEFAULT );
            final Rectangle rect = item.getBounds( 0 );
            final Point pt = tree.toDisplay( rect.x, rect.y );
            tip.setBounds( pt.x, pt.y, size.x, size.y );
            tip.setVisible( true );
          }
        }
      }
    };
    tree.addListener( SWT.Dispose, treeListener );
    tree.addListener( SWT.KeyDown, treeListener );
    tree.addListener( SWT.MouseMove, treeListener );
    tree.addListener( SWT.MouseHover, treeListener );

    m_treeViewer.setLabelProvider( new WorkflowLabelProvider( m_treeViewer ) );
    m_treeViewer.addDoubleClickListener( new IDoubleClickListener()
    {
      private static final String TASKS_COMMANDS_CATEGORY = "org.kalypso.kalypso1d2d.pjt.TasksCommands";

      public void doubleClick( final DoubleClickEvent event )
      {
        final ITreeSelection selection = (ITreeSelection) event.getSelection();
        final Object first = selection.getFirstElement();
        if( first != null )
        {
          if( first instanceof Task )
          {
            doTask( (Task) first );
          }
        }
      }

      private final void doTask( final Task task )
      {
        final IWorkbench workbench = PlatformUI.getWorkbench();
        final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
        final String name = task.getURI();
        try
        {
          final Command command = getCommand( commandService, name );
          final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
          handlerService.executeCommand( command.getId(), null );
        }
        catch( final Throwable e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          ErrorDialog.openError( m_treeViewer.getControl().getShell(), "Workflow Commmand", "Kommando konnte nicht ausgeführt werden: " + name, status );
          KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( status );
          logger.log( Level.SEVERE, "Failed to execute command: " + name, e );
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
    } );
    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      private Object m_lastSelectedElement;

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ITreeSelection selection = (ITreeSelection) event.getSelection();
        final Object first = selection.getFirstElement();
        if( first != null && m_lastSelectedElement != first  )
        {
          final TreePath newTreePath = selection.getPathsFor( first )[0];
          m_lastSelectedElement = null;
          if( m_lastTreePath != null )
          {
            final Object segment = m_lastTreePath.getSegment( Math.min( m_lastTreePath.getSegmentCount(), newTreePath.getSegmentCount() ) - 1 );
            m_treeViewer.collapseToLevel( segment, TreeViewer.ALL_LEVELS );
            m_lastSelectedElement = newTreePath.getLastSegment();
          }
          m_treeViewer.expandToLevel( first, 1 );
          m_lastTreePath = newTreePath;
          m_treeViewer.setSelection( new StructuredSelection( first ) );
        }
      }
    } );
  }

  private TreePath findPart( final String uri, final Workflow workflow )
  {
    return findPartInPhases( uri, workflow.getPhases() );
  }

  private TreePath findPartInPhases( final String uri, final List<Phase> phases )
  {
    TreePath result = null;
    for( final Phase phase : phases )
    {
      if( phase.getURI().equals( uri ) )
      {
        return new TreePath( new Object[] { phase } );
      }
      else
      {
        result = findPartInTaskGroups( uri, phase.getTaskGroups(), new TreePath( new Object[] { phase } ) );
      }
      if( result != null )
      {
        return result;
      }
    }
    return result;
  }

  private TreePath findPartInTaskGroups( final String uri, final List< ? extends Task> taskOrTaskGroups, final TreePath prefix )
  {
    TreePath result = null;
    for( final Task taskOrTaskGroup : taskOrTaskGroups )
    {
      if( taskOrTaskGroup.getURI().equals( uri ) )
      {
        return prefix.createChildPath( taskOrTaskGroup );
      }
      else if( taskOrTaskGroup instanceof TaskGroup )
      {
        result = findPartInTaskGroups( uri, ((TaskGroup) taskOrTaskGroup).getTasks(), prefix.createChildPath( taskOrTaskGroup ) );
      }
      if( result != null )
      {
        return result;
      }
    }
    return result;
  }

  public void setVisible( final boolean visible )
  {
    m_treeViewer.getControl().setVisible( visible );
  }

  public void restoreState( final IMemento memento )
  {
    if( memento != null )
    {
      m_selectionFromMemento = memento.getString( MEMENTO_LAST_SELECTION );
    }
  }

  public void saveState( final IMemento memento )
  {
    if( m_lastTreePath != null )
    {
      final Object lastSegment = m_lastTreePath.getLastSegment();
      memento.putString( MEMENTO_LAST_SELECTION, ((Activity) lastSegment).getURI() );
    }
  }

  public void setWorkflow( final Workflow workflow )
  {
    m_treeViewer.setInput( workflow );
    m_treeViewer.refresh();
    if( m_selectionFromMemento != null && workflow != null )
    {
      final TreePath findPart = findPart( m_selectionFromMemento, workflow );
      if( findPart != null && findPart.getParentPath() != null )
      {
        m_treeViewer.setSelection( new TreeSelection( findPart.getParentPath() ), true );
      }
      m_selectionFromMemento = null;
    }
  }
}
