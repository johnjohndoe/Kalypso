/**
 * 
 */
package org.kalypso.afgui.views;


import javax.swing.text.html.HTMLDocument.HTMLReader.ParagraphAction;

import org.apache.log4j.Logger;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableTreeViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.EActivityAction;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.events.WorkflowChangeEvent;
import org.kalypso.afgui.model.events.WorkflowChangeEventListerner;


/**
 * @author Patrice Congo
 *
 */
public class WorkflowView extends ViewPart
{
	final static private Logger logger=
			Logger.getLogger(WorkflowView.class);
	final static public String ID="org.kalypso.afgui.views.WorkflowView"; 
	
	private Viewer view;
	private IContentProvider provider;
	private IWorkflow workflow;
	
	private Action doAction;
	private Action getHelpAction;
	private Action nextAction;
	private Action previousAction;
	private Action upAction;
	private Action downAction;
	private Action topAction;
	private Action getDataAction;
	
	private WorkflowChangeEventListerner wfceListener=
		new WorkflowChangeEventListerner()
		{

			public void onWorkflowChanged(WorkflowChangeEvent event)
			{
				logger.info(event);
				IWorkflow workflow=(IWorkflow)event.getSource();
//				if(workflow.getRuntineStatus().getCurrentAction()==
//												EActivityAction.GET_HELP)
//				{
//					return;
//				}	
//				IActivity activity=
//					 workflow.getCurrentActivity();
//				Object newInput;
//				if(activity==null)
//				{
//					newInput= new IWorkflow[]{workflow};
//				}
//				else
//				{
//					newInput= new IActivity[]{activity};
//				}
//				logger.info("-------------##>"+((Object[])newInput)[0]);
//				view.setInput(newInput);
				
			}
			
		};
	
	private IDoubleClickListener dkListener= new IDoubleClickListener()
	{

		public void doubleClick(DoubleClickEvent event)
		{
			doAction.run();
		}
		
	};
	
	private ISelectionChangedListener scListener=
		new ISelectionChangedListener()
		{

			public void selectionChanged(SelectionChangedEvent event)
			{
				logger.info(event);
				((TreeViewer)view).expandAll();
			}
			
		};
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent)
	{
		workflow=
			KalypsoAFGUIFrameworkPlugin.getDefault().getWorkflowSystem().getCurrentWorkFlow();
		workflow.addWorkflowChangedEventListener(wfceListener);
		
		provider= new WorkflowViewContentProvider(workflow);
		//createTableTreeView(parent);
		//createTreeColumn(parent);
		createSimpleTreeView(parent);
		makeActions();
		Action[] actions=new Action[]{
				doAction,getDataAction,getHelpAction,
				previousAction,nextAction,topAction,
				upAction,downAction};
		
		MenuBarCreator.createMenubar(
				//getViewSite().getPart(),
				this.getViewSite(),
				(IDoubleClickListener)null,
				actions,
				null,//pageBook,//treeView,
				view.getControl(),//ontrol(),//.treeView.getControl(),
				(ISelectionProvider)null,//treeView,
				"");
		
		/////
		IActionBars bars = getViewSite().getActionBars();
		//fillLocalPullDown(bars.getMenuManager());
		IToolBarManager mng=bars.getToolBarManager();
		for(Action a:actions)
		{
			mng.add(a);
		}
		view.setInput(new IWorkflow[]{workflow});
		
	}
	
	private void createTableTreeView(Composite parent)
	{
		TableTreeViewer ttView;
		ttView= new TableTreeViewer(parent,SWT.FILL);		
		ttView.setContentProvider(provider);
		Table table = ttView.getTableTree().getTable();
	    new TableColumn(table, SWT.CENTER);
	    new TableColumn(table, SWT.CENTER);
	    new TableColumn(table, SWT.RIGHT);//.setText("Points");
	    new TableColumn(table, SWT.RIGHT);//.setText("Rebounds");
	    new TableColumn(table, SWT.RIGHT);//.setText("Assists");
	    ttView.setLabelProvider(
	    		new TableLabelProvider(
	    				(WorkflowViewContentProvider)provider));
		ttView.setInput(workflow);
		
		 // Expand everything
	    ttView.expandAll();

	    // Pack the columns
	    for (int i = 0, n = table.getColumnCount(); i < n; i++) {
	      table.getColumn(i).pack();
	    }

	    // Turn on the header and the lines
	    table.setHeaderVisible(false);
	    table.setLinesVisible(false);
	    
	    ttView.addDoubleClickListener(dkListener);
	    view=ttView;
	}
	
	private void createTreeColumn(Composite parent)
	{
		TreeViewer tv= new TreeViewer(parent);
		
		Tree tree = tv.getTree();//new Tree(parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
	    //tree.setHeaderVisible(true);
		TreeColumn column;
		for(int i=EActivityAction.values().length-1;i>=0;i--)
		{
			column=new TreeColumn(tree, SWT.LEFT);
			column.setWidth(100);
		    column.setText("Column 1");
		    
		}
//	    TreeColumn column1 = new TreeColumn(tree, SWT.LEFT);
//	    //column1.setText("Column 1");
//	    column1.setWidth(100);
//	    TreeColumn column2 = new TreeColumn(tree, SWT.CENTER);
//	    //column2.setText("Column 2");
//	    //column2.setWidth(200);
//	    TreeColumn column3 = new TreeColumn(tree, SWT.RIGHT);
//	    //column3.setText("Column 3");
//	    column3.setWidth(200);
//	    //TreeViewer tv= new TreeViewer(tree);
	    tv.setContentProvider(provider);
	    
	    tv.setLabelProvider(
	    		new TableLabelProvider(
	    				(WorkflowViewContentProvider)provider));
	    tv.setInput(IWorkflow.class);//(workflow);
	    for (int i = 0, n = tree.getColumnCount(); i < n; i++) {
		      tree.getColumn(i).pack();
		}
	    tree.setHeaderVisible(false);
	    tv.addDoubleClickListener(dkListener);
	    tv.addSelectionChangedListener(scListener);
	    
	    view=tv;
	}
	
	private void createSimpleTreeView(Composite parent)
	{
		TreeViewer tv= new TreeViewer(parent);
		tv.setContentProvider(provider);
	    tv.setLabelProvider(
	    		new TableLabelProvider(
	    				(WorkflowViewContentProvider)provider));
	    tv.setInput(new IWorkflow[]{workflow});//(workflow);
	    tv.addDoubleClickListener(dkListener);
	    tv.addSelectionChangedListener(scListener);
	    Tree tree = tv.getTree();
	    TreeColumn column1 = new TreeColumn(tree, SWT.LEFT);
	    column1.setWidth(400);
	    view=tv;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus()
	{

	}
	
	private void makeActions()
	{
		ImageRegistry reg=KalypsoAFGUIFrameworkPlugin.getDefault().getImageRegistry();
		
		///////////////////////////////////////////////
		doAction = new Action() {
			public void run() {
				logger.info("DODODODODODOD");
				IStructuredSelection sel=
							(IStructuredSelection)view.getSelection();
				IActivity activity=(IActivity)sel.getFirstElement();
//				workflow.updateWorkflow(activity, EActivityAction.DO);
				logger.info("sel:"+sel);
			}
		};
		doAction.setText("Do Activity");
		doAction.setToolTipText("Do activity");
//		doAction.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
//				getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		doAction.setImageDescriptor(reg.getDescriptor(EActivityAction.DO.toString()));
		
///////////////////////////////////////////////
		getHelpAction = new Action() {
			public void run() {
				IStructuredSelection sel=
					(IStructuredSelection)view.getSelection();
				IActivity activity=(IActivity)sel.getFirstElement();
//				workflow.updateWorkflow(activity, EActivityAction.GET_HELP);				//showMessage("Action 2 executed");
				
			}
		};
		getHelpAction.setText("Activity Help");
		getHelpAction.setToolTipText("View Activity Help");

		getHelpAction.setImageDescriptor(
				reg.getDescriptor(EActivityAction.GET_HELP.toString()));
		
///////////////////////////////////////////////
		nextAction = new Action() {
			public void run() {
				//showMessage("Action 2 executed");
				
			}
		};
		nextAction.setText("Show newxt Activities");
		nextAction.setToolTipText("Show next possible Activity");

		nextAction.setImageDescriptor(
				reg.getDescriptor(EActivityAction.NEXT.toString()));
///////////////////////////////////////////////
		previousAction = new Action() {
			public void run() {
				//showMessage("Action 2 executed");
				
			}
		};
		previousAction.setText("Activity Help");
		previousAction.setToolTipText("View Activity Help");

		previousAction.setImageDescriptor(
				reg.getDescriptor(EActivityAction.PREVIOUS.toString()));
///////////////////////////////////////////////
		upAction = new Action() {
			public void run() {
				IStructuredSelection sel=
					(IStructuredSelection)view.getSelection();
				IActivity activity=(IActivity)sel.getFirstElement();
//				workflow.updateWorkflow(activity, EActivityAction.UP);				
			}
		};
		upAction.setText("Parent Activity level");
		upAction.setToolTipText("Go to parent activity level");

		upAction.setImageDescriptor(
				reg.getDescriptor(EActivityAction.UP.toString()));
///////////////////////////////////////////////
		downAction = new Action() {
			public void run() {
				IStructuredSelection sel=
					(IStructuredSelection)view.getSelection();
				IActivity activity=(IActivity)sel.getFirstElement();
//				workflow.updateWorkflow(activity, EActivityAction.DOWN);	
				
			}
		};
		downAction.setText("Child Activities");
		downAction.setToolTipText("Show child Activities");

		downAction.setImageDescriptor(
				reg.getDescriptor(EActivityAction.DOWN.toString()));
///////////////////////////////////////////////
		topAction = new Action() {
			public void run() {
//				IStructuredSelection sel=
//					(IStructuredSelection)view.getSelection();
//				IActivity activity=(IActivity)sel.getFirstElement();
//				workflow.updateWorkflow(null, EActivityAction.DO);
			}
		};
		topAction.setText("Root Activities Root");
		topAction.setToolTipText("Show Root Activities");

		topAction.setImageDescriptor(
				reg.getDescriptor(EActivityAction.TOP.toString()));
///////////////////////////////////////////////
		getDataAction = new Action() {
			public void run() {
				//showMessage("Action 2 executed");
				
			}
		};
		getDataAction.setText("Show Activity Data");
		getDataAction.setToolTipText("Show Activity Data");

		getDataAction.setImageDescriptor(
				reg.getDescriptor(EActivityAction.SHOW_DATA.toString()));
	}
}
