/**
 * 
 */
package org.kalypso.afgui.views;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.part.DrillDownAdapter;
/**
 * 
 * @author Patrice Congo
 */
class MenuBarCreator
{
	//private DrillDownAdapter drillDownAdapter; 
	
	private MenuBarCreator()
	{
		
	}
	
	static public void createMenubar(
						//ViewPart viewPart,
						IViewSite viewSite,
						IDoubleClickListener doubleClickListener,
						Action[] actions,
						Viewer viewer,
						Control menuControl,
						ISelectionProvider menuControlSelectionProvider,
						String end)
	{
		
		//makeActions();
		hookContextMenu(
				viewSite,
				actions,
				menuControl,
				menuControlSelectionProvider);
		hookDoubleClickAction(viewer,doubleClickListener);
		contributeToActionBars(viewSite,actions,viewer);
	}
	
	static final private void hookContextMenu(
			IViewSite viewSite,
			final Action[] actions,
			final Control menuControl,
			final ISelectionProvider controlSelectionProvider) 
	{
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				fillContextMenu(manager,actions);
			}
		});
		Menu menu = 
			menuMgr.createContextMenu(menuControl);
		menuControl.setMenu(menu);
		viewSite.registerContextMenu(menuMgr, controlSelectionProvider);
	}
	
	static final private void contributeToActionBars(
							IViewSite viewSite,
							Action[] actions,
							Viewer viewer) 
	{
		IActionBars bars = viewSite.getActionBars();
		fillLocalPullDown(bars.getMenuManager(),actions);
		if(viewer instanceof TreeViewer)
		{
			fillLocalToolBar(
					bars.getToolBarManager(),
					actions,
					(TreeViewer)viewer);
		}
	}

	static final private void fillLocalPullDown(IMenuManager manager, Action[] actions) {
		if(manager==null || actions==null)
		{
			return;
		}
		for(int i=0;i<actions.length;i++)
		{
			manager.add(actions[i]);
			manager.add(new Separator());
		}    		
	}

	static final private void fillContextMenu(
						IMenuManager manager,
						Action[] actions) 
	{
		if(manager==null || actions==null)
		{
			return;
		}
		for(int i=0;i<actions.length;i++)
		{
			manager.add(actions[i]);
    		manager.add(new Separator());
		}
		//drillDownAdapter.addNavigationActions(manager);
		// Other plug-ins can contribute there actions here
		manager.add(
				new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	static final private void fillLocalToolBar(	IToolBarManager manager,
									Action[] actions,
									TreeViewer viewer) 
	{
		if(manager==null || actions==null)
		{
			return;
		}
		
		for(int i=0;i<actions.length;i++)
		{
			manager.add(actions[i]);
    		manager.add(new Separator());
		}
		
		DrillDownAdapter drillDownAdapter= 
    			new DrillDownAdapter((TreeViewer)viewer);
    	drillDownAdapter.addNavigationActions(manager);
		
	}

	
	static final private void hookDoubleClickAction(
							Viewer viewer,
							IDoubleClickListener doubleClickListener) 
	{
		if(viewer==null || doubleClickListener==null)
		{
			return;
		}
		if(viewer instanceof StructuredViewer)
		{    		
    		((StructuredViewer)
    				viewer).addDoubleClickListener(doubleClickListener);
		}
	}
}