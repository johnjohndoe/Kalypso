package org.kalypso.editor.mapeditor.views;


import org.deegree.graphics.Layer;
import org.deegree.model.feature.FeatureType;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.editor.mapeditor.GisMapOutlinePage;
import org.kalypso.editor.mapeditor.ThemeStyleTreeObject;
import org.kalypso.editor.styleeditor.SLDEditorGuiBuilder;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;

/**
 * 
 *  
 */

public class StyleEditorViewPart extends ViewPart implements ISelectionChangedListener {

//	private Label label = null;

	private boolean test = true;
	
	private GisMapOutlinePage gmop = null;
	
	private SLDEditorGuiBuilder guiBuilder = null;

	
	public StyleEditorViewPart() 
	{
		
	}
	
	public void setSelectionChangedProvider(GisMapOutlinePage page)
	{
		if(this.gmop != page)
		{
			this.gmop = page;
			gmop.addSelectionChangedListener(this);
		}		
	}


	/**
	 * @see org.eclipse.ui.IWorkbenchPart#dispose()
	 */
	public void dispose() {		
		super.dispose();
		if(gmop != null)
			gmop.removeSelectionChangedListener(this);
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	public void createPartControl(Composite parent) 
	{
		guiBuilder = new SLDEditorGuiBuilder(parent, this);		
	}
	
	public void initStyleEditor(KalypsoUserStyle userStyle, FeatureType featureType)
	{
		guiBuilder.buildSWTGui(userStyle, featureType);
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPart#setFocus()
	 */
	public void setFocus() {
	
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
	 */
	public void selectionChanged(SelectionChangedEvent event) 
	{					
		Object o=((IStructuredSelection)event.getSelection()).getFirstElement(); 
		if(o instanceof ThemeStyleTreeObject)
	  	{
	  		KalypsoFeatureLayer layer=((ThemeStyleTreeObject)o).getTheme().getLayer();
	  		if(layer instanceof KalypsoFeatureLayer)
	  		{
	  			FeatureType ft=((KalypsoFeatureLayer)layer).getFeatureType();	  			  		
	  			KalypsoUserStyle kalypsoStyle = ((ThemeStyleTreeObject) o).getStyle();					  			
	  			initStyleEditor(kalypsoStyle, ft);							  		
	  		}
	  	} 
		else if(o instanceof KalypsoTheme)
			initStyleEditor(null, null); 
	}
}