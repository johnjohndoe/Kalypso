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
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableTreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableTree;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.editor.styleeditor.panels.ColorChooserPanel;
import org.w3c.dom.Document;

public class FilterDialog extends Dialog {

	private TableTreeViewer m_viewer;

	private FilterDialogTreeNode mRoot;

	private Document mDocument = null;

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
		shell.setSize(300,300);
	}

	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		composite.setSize(300,300);
		ColorChooserPanel panel = new ColorChooserPanel(composite, "sdf", null);
		applyDialogFont(composite);

		final TableTree tree = new TableTree(composite, SWT.SINGLE | SWT.INSERT);
	
		tree.setSize(200,300);
		
		//		tree.addSelectionListener( this );

		m_viewer = new TableTreeViewer(tree);
		m_viewer.setContentProvider(new FilterDialogTreeContentProvider());
		m_viewer.setLabelProvider(new FilterDialogLabelProvider());

		mRoot = new FilterDialogTreeNode("Root", true);

		if (mDocument != null) {
			//        prepareTreeData( );
		}

		//	    m_viewer.addDoubleClickListener( this );

		m_viewer.setInput(mRoot);
		m_viewer.setSelection(new StructuredSelection(mRoot.getRoot()));
		createContextMenu( m_viewer.getControl( ) );
		
		return composite;
	}

	private void createContextMenu(Control menuControl) {
	   
		MenuManager menuMgr = new MenuManager("#PopUp");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				manager.add(new Separator("Logical"));
				manager.add(new MenuAction("AND"));
				manager.add(new MenuAction("OR"));
				manager.add(new MenuAction("NOT"));
				manager.add(new Separator("Comparison"));
				manager.add(new MenuAction("IS_BETWEEN"));
				manager.add(new MenuAction("IS_LIKE"));
				manager.add(new MenuAction("IS_NULL"));
				manager.add(new MenuAction("IS_EQUAL_TO"));
				manager.add(new MenuAction("IS_LESS_THAN"));
				manager.add(new MenuAction("IS_GREATER_THAN"));
				manager.add(new MenuAction("IS_LESS_THAN_OR_EQUAL_TO"));
				manager.add(new MenuAction("IS_GREATER_THAN_OR_EQUAL_TO"));
				manager.add(new MenuAction("IS_EQUAL_TO"));
				manager.add(new MenuAction("IS_EQUAL_TO"));
				manager.add( new Separator("FeatureFilter"));
				manager.add(new MenuAction("FEATURE_FILTER"));
				
			}
		});

		Menu menu = menuMgr.createContextMenu(menuControl);
//		menuControl.setMenu(menu);
	}

	protected class MenuAction extends Action {
		public MenuAction(String text) {
			super(text);
		}

		public void run() {
			String type = this.getText();
			type = type.substring(4);

			IStructuredSelection selection = (IStructuredSelection) m_viewer.getSelection();

			if (selection != null) {
				Object selectedElement = selection.getFirstElement();
				FilterDialogTreeNode node = (FilterDialogTreeNode) selectedElement;
				FilterDialogTreeNode childNode = new FilterDialogTreeNode(type, FilterDialogTreeNode.NODE_TYPE);
				node.addNode(childNode);

				m_viewer.setInput(mRoot);
				m_viewer.expandAll();
				m_viewer.setSelection(new StructuredSelection(childNode));
			}
		}
	}

}