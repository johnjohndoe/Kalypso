package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.editor.mapeditor.views.StyleEditorViewPart;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.util.list.IListManipulator;

/**
 * @author belger
 */
public class RemoveRuleAction extends AbstractOutlineAction
{
  public RemoveRuleAction( final String text, final ImageDescriptor image, final String tooltipText, final GisMapOutlineViewer outlineViewer,final IListManipulator listManip  )
  {
    super( text, image, tooltipText, outlineViewer, listManip );
    refresh();
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {          
    Object o = ( (IStructuredSelection)getOutlineviewer().getSelection() ).getFirstElement();
    if( o instanceof RuleTreeObject )
    {
    	RuleTreeObject obj = (RuleTreeObject) o;
    	KalypsoUserStyle userStyle = obj.getStyle();
    	userStyle.getFeatureTypeStyles()[0].removeRule(obj.getRule());
    	userStyle.fireModellEvent(new ModellEvent(userStyle, ModellEvent.STYLE_CHANGE));	
    	
    	IWorkbenchWindow window = Workbench.getInstance().getActiveWorkbenchWindow();
    	StyleEditorViewPart part;
		try {
			part = (StyleEditorViewPart)window.getActivePage().showView("org.kalypso.editor.mapeditor.views.styleeditor" );
			if( part != null )
			{
	    	 	part.setSelectionChangedProvider( getOutlineviewer() );
	    	 	part.initStyleEditor(userStyle, obj.getFeatureType());	    	 		    	 
			}
		} catch (PartInitException e) {			
			e.printStackTrace();
		}
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    refresh();
  }

  protected void refresh()
  {
    boolean bEnable = false;

    final IStructuredSelection s = (IStructuredSelection)getOutlineviewer().getSelection();

    if( s.getFirstElement() instanceof RuleTreeObject)
      bEnable = true;    
    setEnabled( bEnable );
  }
}