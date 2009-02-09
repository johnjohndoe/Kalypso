package org.kalypso.renew.debug;

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.TableLayout;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.part.ViewPart;

import de.renew.workflow.connector.IWorklistChangeListener;
import de.renew.workflow.connector.WorkflowConnector;

public class DebugView extends ViewPart implements IWorklistChangeListener
{

  public static final String VIEW_NAME = "WorkflowConnector.debugView";

  private WorkflowConnector m_connector;

  private TableViewer m_tableViewer;

  @Override
  public void createPartControl( final Composite parent )
  {
    final Composite container = new Composite( parent, SWT.FILL );
    container.setLayout( new FillLayout() );
    // final GridDataFactory gridDataFactory = GridDataFactory.fillDefaults();

    final Table table = new Table( container, SWT.SINGLE | SWT.V_SCROLL );
    // table.setLayoutData(gridDataFactory.create());

    final TableLayout layout = new TableLayout();
    layout.addColumnData( new ColumnWeightData( 20, 25, true ) );
    layout.addColumnData( new ColumnWeightData( 20, 25, true ) );
    layout.addColumnData( new ColumnWeightData( 20, 25, true ) );
    layout.addColumnData( new ColumnWeightData( 20, 25, true ) );
    layout.addColumnData( new ColumnWeightData( 20, 25, true ) );
    table.setLayout( layout );

    final TableColumn col1 = new TableColumn( table, SWT.CENTER );
    col1.setText( "Task Name" );
    final TableColumn col2 = new TableColumn( table, SWT.CENTER );
    col2.setText( "Task Class" );
    final TableColumn col3 = new TableColumn( table, SWT.CENTER );
    col3.setText( "Task Settings" );
    final TableColumn col4 = new TableColumn( table, SWT.CENTER );
    col4.setText( "Workitem Parameter" );
    final TableColumn col5 = new TableColumn( table, SWT.CENTER );
    col5.setText( "Workitem Priority" );
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    m_tableViewer = new TableViewer( table );
    m_tableViewer.setContentProvider( new ArrayContentProvider() );
    m_tableViewer.setLabelProvider( new DebugViewTabelLabelProvider() );

    final Control control = m_tableViewer.getControl();
    final MenuManager menuManager = new MenuManager( "Workitem Menu", VIEW_NAME );
    menuManager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    menuManager.addMenuListener( new IMenuListener()
    {

      public void menuAboutToShow( final IMenuManager manager )
      {
        // TODO Auto-generated method stub

      }
    } );
    control.setMenu( menuManager.createContextMenu( control ) );
    getSite().registerContextMenu( menuManager, m_tableViewer );
    getSite().setSelectionProvider( m_tableViewer );

    m_connector = (WorkflowConnector) WorkflowConnector.getConnector();
    m_connector.addWorklistChangeListener( this );
  }

  @Override
  public void setFocus( )
  {
    //
  }

  public void worklistChanged( )
  {
    m_tableViewer.setInput( m_connector.getAvailables() );
  }
}
