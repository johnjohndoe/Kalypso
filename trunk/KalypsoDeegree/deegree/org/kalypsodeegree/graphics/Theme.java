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

package org.deegree.graphics;

import java.awt.Graphics;
import java.util.ArrayList;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.sort.JMSpatialIndex;

/**
 *
 * <p>------------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public interface Theme {

    /**
     * Returns all <tt>DisplayElements</tt> that this <tt>Theme</tt> contains.
     * <p>
     * @return <tt>ArrayList</tt> containing <tt>DisplayElements</tt>
     */
    ArrayList getDisplayElements ();

    /**
     * sets the display elements of the theme
     */
    void setDisplayElements (ArrayList de);

    /**
     * sets the parent MapView of the Theme.
     */
    void setParent(MapView parent);

    /**
     * returns the name of the layer
     */
    String getName();

    /**
     * renders the layer to the submitted graphic context
     */
    void paint(Graphics g);

    /**
     * renders the display elements matching the submitted ids
     */
    void paint(Graphics g, String[] ids);

    /**
     * renders the selected display elements of the layer
     */
    void paintSelected(Graphics g);

    /**
     * renders the highlighted display elements of the layer
     */
    public void paintHighlighted(Graphics g);

    /**
     * A selector is a class that offers methods for selecting and
     * deselecting single DisplayElements or groups of DisplayElements.
     * A selector may offers methods like 'select all DisplayElements
     * within a specified bounding box' or 'select all DisplayElements
     * thats area is larger than 120 km²' etc.
     */
    void addSelector(Selector selector);

    /**
     * @see Theme#addSelector(Selector)
     */
    void removeSelector(Selector selector);

    /**
     * A Highlighter is a class that is responsible for managing the highlight
     * capabilities for one or more Themes.
     */
    void addHighlighter(Highlighter highlighter);

    /**
     * @see Theme#addHighlighter(Highlighter)
     */
    void removeHighlighter(Highlighter highlighter);

    /**
     * adds an eventcontroller to the Theme that's reponsible for handling
     * events that targets the Theme.
     */
    void addEventController(ThemeEventController obj);

    /**
     * @see Theme#addEventController(ThemeEventController)
     */
    void removeEventController(ThemeEventController obj);

    /**
     * sets the styles used for this <tt>Theme</tt>. If this method will be
     * called all <tt>DisplayElement</tt>s will be recreated to consider the
     * new style definitions.
     */
    void setStyles(UserStyle[] styles);

    JMSpatialIndex getSpatialIndex();

    /**
     * returns the styles used for this <tt>Theme</tt>.
     */
    UserStyle[] getStyles();

    /**
     * returns the layer that holds the data of the theme
     */
    Layer getLayer();


}
