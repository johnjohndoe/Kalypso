/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.editor.styleeditor.dialogs;


import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableTreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableTree;
import org.eclipse.swt.custom.TableTreeItem;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.w3c.dom.Document;

public class FilterDialog extends Dialog implements ISelectionChangedListener{

	private TableTreeViewer m_viewer;

	private FilterDialogTreeNode mRoot;

	private Document mDocument = null;
	
	private Group configureGroup = null;
	private Composite groupComposite = null;

	public FilterDialog(Shell parent) {
		super(parent);
	}

	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
		} else {
		}
		super.buttonPressed(buttonId);
	}

	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setText("Filter Dialog");
		shell.setSize(500,300);
	}

	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		composite.setSize(500,300);
		composite.setLayout(new GridLayout(2,true));		
		composite.layout();				
		applyDialogFont(composite);

		final TableTree tree = new TableTree(composite, SWT.SINGLE |SWT.FULL_SELECTION |SWT.H_SCROLL);		
		GridData tableTreeData = new GridData();
		tableTreeData.widthHint = 224;
		tableTreeData.heightHint = 97;	
		tree.setLayoutData(tableTreeData);
		//tree.setSize(224,97);		
		
		m_viewer = new TableTreeViewer(tree);	
		m_viewer.addSelectionChangedListener(this);		
		m_viewer.setContentProvider(new FilterDialogTreeContentProvider());
		m_viewer.setLabelProvider(new FilterDialogLabelProvider());		
		mRoot = new FilterDialogTreeNode("Filter                                                              ", FilterDialogTreeNode.ROOT_TYPE);

		if (mDocument != null) {
			//        prepareTreeData( );
		}

		//	    m_viewer.addDoubleClickListener( this );

		m_viewer.setInput(mRoot);
		m_viewer.setSelection(new StructuredSelection(mRoot.getParent()));
		createContextMenu(m_viewer.getControl(), mRoot);
		
		configureGroup = new Group(composite,SWT.NULL);
		configureGroup.setText("Configure Filter");
		groupComposite = new Composite(configureGroup,SWT.NULL);
		
		return composite;
	}
	
	private void createContextMenu(Control menuControl, final FilterDialogTreeNode node) 
	{
		MenuManager menuMgr = new MenuManager("#PopUp");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() 
		{
			public void menuAboutToShow(IMenuManager manager) 
			{
				boolean full = true;
				int childCount = node.getChildren().length;
				
				switch(node.getType())
				{
					case FilterDialogTreeNode.LOGICAL_NODE_TYPE:
					{
						if(node.getName().equals("NOT"))
						{
							if(childCount <1){
								full = true;				
							}
							else
								full = false;
						}
						else
						{
							if(childCount <=1)
								full = true;
							else
							{							
								full = false;
							}
						}
						break;
					}
					case FilterDialogTreeNode.ROOT_TYPE:
					{
						if(childCount <1)
							full = true;
						break;
					}
					default:
					{
						full = false;
					}
				}
				
				if(full)
				{
					manager.add(new Separator("Logical"));
					manager.add(new MenuAction("AND", FilterDialogTreeNode.LOGICAL_NODE_TYPE));
					manager.add(new MenuAction("OR", FilterDialogTreeNode.LOGICAL_NODE_TYPE));
					manager.add(new MenuAction("NOT", FilterDialogTreeNode.LOGICAL_NODE_TYPE));
					manager.add(new Separator("Comparison"));
					manager.add(new MenuAction("BETWEEN", FilterDialogTreeNode.COMPARISON_NODE_TYPE));
					manager.add(new MenuAction("LIKE", FilterDialogTreeNode.COMPARISON_NODE_TYPE));
					manager.add(new MenuAction("NULL", FilterDialogTreeNode.COMPARISON_NODE_TYPE));
					manager.add(new MenuAction("EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE));
					manager.add(new MenuAction("LESS_THAN", FilterDialogTreeNode.COMPARISON_NODE_TYPE));
					manager.add(new MenuAction("GREATER_THAN", FilterDialogTreeNode.COMPARISON_NODE_TYPE));
					manager.add(new MenuAction("LESS_THAN_OR_EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE));
					manager.add(new MenuAction("GREATER_THAN_OR_EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE));
					manager.add(new MenuAction("EQUAL_TO", FilterDialogTreeNode.COMPARISON_NODE_TYPE));					
					manager.add( new Separator("FeatureFilter"));
					manager.add(new MenuAction("FEATURE_FILTER", FilterDialogTreeNode.FEATUREID_NODE_TYPE));
					manager.add( new Separator());
				}
				manager.add(new MenuAction("Delete", -1));
			}
		});

		Menu menu = menuMgr.createContextMenu(menuControl);
		menuControl.setMenu(menu);
	}

	protected class MenuAction extends Action {
		private int type = -1;
		public MenuAction(String text, int type) {
			super(text);
			this.type = type;
		}

		public void run() {
			String name = this.getText();			

			IStructuredSelection selection = (IStructuredSelection) m_viewer.getSelection();

			if (selection != null)
			{
				Object selectedElement = selection.getFirstElement();
				FilterDialogTreeNode node = (FilterDialogTreeNode) selectedElement;					
				FilterDialogTreeNode selectionNode = null;
				
				if(type != -1)
				{
					FilterDialogTreeNode childNode = new FilterDialogTreeNode(name, type);
					node.addNode(childNode);
					selectionNode = node;
					m_viewer.refresh(true);
				}
				else
				{			
					FilterDialogTreeNode parent = node.getParent();
					if(parent != null)
						parent.removeNode(node);	
					selectionNode = parent;
				}

				m_viewer.setInput(mRoot);
				m_viewer.expandAll();
				if(selectionNode != null)
					m_viewer.setSelection(new StructuredSelection(selectionNode));
			}
		}
	}

	/* (non-Javadoc)
	 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
	 */
	public void selectionChanged(SelectionChangedEvent event) 
	{					
		TableTreeViewer ttv = (TableTreeViewer) event.getSource();		
		IStructuredSelection s = (IStructuredSelection)ttv.getSelection();		
		if(s.getFirstElement() instanceof FilterDialogTreeNode)
		{					
			FilterDialogTreeNode selectedNode = (FilterDialogTreeNode) s.getFirstElement();
			createContextMenu(m_viewer.getControl(),selectedNode);
			
			if(selectedNode.getType() == FilterDialogTreeNode.FEATUREID_NODE_TYPE)
			{				
				if(groupComposite != null)
					groupComposite.dispose();
				groupComposite = new Composite(configureGroup,SWT.BORDER);
				
				
				Text inputText = new Text(groupComposite, SWT.BORDER);		
				inputText.setSize(100,15);
				inputText.setText("sdf"+selectedNode.getName());	
				configureGroup.pack(true);
				groupComposite.pack(true);
				System.out.println("SDF");
			}
		}				
	}
}