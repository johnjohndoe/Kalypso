/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de
 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.
 
This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
 
You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 
Contact:
 
Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de
 
Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166 
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de
 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.graphics;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.deegree.graphics.FeatureLayer;
import org.deegree.graphics.Highlighter;
import org.deegree.graphics.Layer;
import org.deegree.graphics.MapView;
import org.deegree.graphics.RasterLayer;
import org.deegree.graphics.Selector;
import org.deegree.graphics.Theme;
import org.deegree.graphics.ThemeEventController;
import org.deegree.graphics.displayelements.DisplayElement;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.Feature;
import org.deegree_impl.graphics.displayelements.DisplayElementFactory;
import org.deegree_impl.tools.Debug;

import org.deegree_ext.model.sort.DisplayContext;
import org.deegree_ext.model.sort.JMSpatialIndex;
import org.deegree_ext.model.sort.JMSpatialIndexFactory;
import de.tuhh.wb.jm.view.JMMapView;

import java.util.Iterator;
import org.deegree.model.geometry.GM_Envelope;

/**
 * A Theme is for usual a homogenious collection of Features coupled with
 * a portrayal model for their graphical representation. Considering the OGC
 * Styled Layer Descriptor specification this is not nessecary the case. In
 * confirmation with the SLD a theme can be build from a lot of thematic
 * completly different feature types.<p></p>
 * From a theoretical point of view this isn't very satisfying. But it will
 * be supported by the <tt>Theme</tt> class.<p></p>
 * Assigned to the Theme are:
 * <ul>
 *	<li>a Layer that contains the data (features)
 *	<li>a Portrayal model that determines how the features shall be rendered
 *	<li>a Selector that offers method for selection and de-selection of
 *		features
 *	<li>a event listener that handles event occuring on a theme that's
 *		for usual part of a map.
 * </ul>
 *
 * <p>------------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
class Theme_Impl implements Theme {
    
    private String name 		= null;
    private Layer layer			= null;
    private UserStyle[] styles          = null;

    private JMSpatialIndex indexDE=null;
    //    private ArrayList displayElements	= null;    
    
    /**
     * the MapView (map) the theme is associated to
     */
    private MapView parent				= null;
    
    /**
     * this ArrayList contains all DisplayElements (and so the features) that
     * are marked as selected.
     */
    private List selector		= Collections.synchronizedList( new ArrayList() );
    private List highlighter		= Collections.synchronizedList( new ArrayList() );
    private List eventController	= Collections.synchronizedList( new ArrayList() );
    
        
    Theme_Impl(String name, Layer layer, UserStyle[] styles) {
        this.layer = layer;
        this.name = name;
        setStyles( styles );
    }
    
    /**
     * sets the parent MapView of the Theme.
     */
    public void setParent(MapView parent) {
        this.parent = parent;
    }
    
    /**
     * returns the name of the layer
     */
    public String getName() {
        return name;
    }
    
    /**
     * renders the layer to the submitted graphic context
     */
    public void paint(Graphics g) {        
        Debug.debugMethodBegin( this, "paint(Graphics)" );
	System.out.println("DE elements in index: "+indexDE.rsize());
        double scale = parent.getScale();
	GM_Envelope env=JMMapView.getMapView().getBoundingBox();
	List displayElements=new ArrayList();
	indexDE.query(env,displayElements);

	Iterator it=displayElements.iterator();
	while(it.hasNext())
	    {
		((DisplayContext)it.next()).paint(g,parent.getProjection(),scale);
	    }
	if(de.tuhh.wb.jm.Debug.showIndexEnv())
	    indexDE.paint(g,parent.getProjection());	
	        Debug.debugMethodEnd();
    }
    
    /**
     * renders the display elements matching the submitted ids
     */
    public void paint(Graphics g, String[] ids) {
        Debug.debugMethodBegin( this, "paint(Graphics,String[])" );
        
        int x = g.getClipBounds().x;
        int y = g.getClipBounds().y;
        int width = g.getClipBounds().width;
        int height = g.getClipBounds().height;
        
        parent.getProjection ().setDestRect(  x, y, width+x, height+y );
                
        Debug.debugMethodEnd();
    }
    
    /**
     * renders the selected display elements of the layer
     */
    public void paintSelected(Graphics g) {

        Debug.debugMethodBegin( this, "paintSeleced" );

        double scale = parent.getScale();
	GM_Envelope env=JMMapView.getMapView().getBoundingBox();
	List displayContainer=new ArrayList();
	indexDE.query(env,displayContainer);

	Iterator it=displayContainer.iterator();
	while(it.hasNext())
	    {
		((DisplayContext)it.next()).paintSelected(g,parent.getProjection(),scale);
	    }
        Debug.debugMethodEnd();
    }
    
    /**
     * renders the highlighted display elements of the layer
     */
    public void paintHighlighted(Graphics g) {
    }
    
    /**
     * A selector is a class that offers methods for selecting and
     * deselecting single DisplayElements or groups of DisplayElements.
     * A selector may offers methods like 'select all DisplayElements
     * within a specified bounding box' or 'select all DisplayElements
     * thats area is larger than 120 km²' etc.
     */
    public void addSelector(Selector selector) {
        this.selector.add( selector );
        selector.addTheme( this );
    }
    
    /**
     * @see org.deegree_impl.graphics.Theme_Impl#addSelector(Selector)
     */
    public void removeSelector(Selector selector) {
        this.selector.remove( selector );
        selector.removeTheme( this );
    }
    
    /**
     * A Highlighter is a class that is responsible for managing the highlight
     * capabilities for one or more Themes.
     */
    public void addHighlighter(Highlighter highlighter) {
        this.highlighter.add( highlighter );
        highlighter.addTheme( this );
    }
    
    /**
     * @see org.deegree_impl.graphics.Theme_Impl#addHighlighter(Highlighter)
     */
    public void removeHighlighter(Highlighter highlighter) {
        this.highlighter.remove( highlighter );
        highlighter.removeTheme( this );
    }
    
    /**
     * adds an eventcontroller to the theme that's reponsible for handling
     * events that targets the theme.
     */
    public void addEventController(ThemeEventController controller) {
        eventController.add( controller );
        controller.addTheme( this );
    }
    
    /**
     * @see org.deegree_impl.graphics.Theme_Impl#addEventController(ThemeEventController)
     */
    public void removeEventController(ThemeEventController controller) {
        eventController.remove( controller );
        controller.removeTheme( this );
    }
    
    /**
     * stes the styles used for this <tt>Theme</tt>. If this method will be 
     * called all <tt>DisplayElement</tt>s will be recreated to consider the
     * new style definitions.
     */
    public void setStyles(UserStyle[] styles) {
        this.styles = styles;
	
        indexDE = JMSpatialIndexFactory.createSpatialIndex(layer.getBoundingBox());

        DisplayElementFactory fac = new DisplayElementFactory();
        if ( layer instanceof FeatureLayer ) {
            try {
                for (int i = 0; i < ((FeatureLayer)layer).getSize(); i++) 
		    {
			Feature feature = ((FeatureLayer)layer).getFeature (i);
			indexDE.add(new DisplayContext(feature,styles));
                }
            } catch(Exception e) {
		e.printStackTrace();
		System.out.println(e);	
            }
        } else {
            try {
                // instance of RasterLayer
                RasterLayer rl = (RasterLayer)layer;
                DisplayElement[] de = DisplayElementFactory.createDisplayElement( rl.getRaster(), styles );
		//		GM_Envelope env=rl.getBoundingBox();
                for(int k = 0; k < de.length; k++) 
		    { 
			//			displayElementsIndex.add(env,de[k]);
		    //		    displayElements.add( de[k] );
                    }
            } catch(Exception e) {
		e.printStackTrace();

                System.out.println(e);	
            }
        }
    }
    
    /**
     * returns the styles used for this <tt>Theme</tt>. 
     */
    public UserStyle[] getStyles() {
        return styles;
    }
        
    /**
     * returns the layer that holds the data of the theme
     */
    public Layer getLayer() {
        return layer;
    }
    
    /**
     *Returns all <tt>DisplayElements</tt> that this <tt>Theme</tt> contains.
     * <p>
     * @return <tt>ArrayList</tt> containing <tt>DisplayElements</tt>
     */
    public ArrayList getDisplayElements() {
	ArrayList result=new ArrayList();
	indexDE.queryAll(result);
	System.out.println("  TODO Theme_Impl.getDisplayElements() !!");
	return result;
    }
    
    public JMSpatialIndex getSpatialIndex()
    {
	return (JMSpatialIndex)indexDE;
    }

    public void setDisplayElements(ArrayList de) {
        //this.displayElements = de;
	}

}
