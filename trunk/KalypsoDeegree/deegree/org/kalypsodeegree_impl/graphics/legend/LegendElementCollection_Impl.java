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
package org.deegree_impl.graphics.legend;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.util.ArrayList;

import org.deegree.graphics.legend.*;

import org.deegree_impl.tools.Debug;

/**
 * <tt>LegendElementCollection</tt> is a collection of <tt>LegendElement</tt>s 
 * and is a <tt>LegendElement</tt> too. It can be used to group elements or to 
 * create more complex elements.<p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class LegendElementCollection_Impl extends LegendElement_Impl implements LegendElementCollection {

	ArrayList collection = null;
	String title = "";

	/**
	 * empty constructor
	 *
	 */
	public LegendElementCollection_Impl() {
		super();
		this.collection = new ArrayList();
	}

	/**
	 * empty constructor
	 *
	 */
	public LegendElementCollection_Impl(String title) {
		super();
		this.collection = new ArrayList();
		this.title = title;
	}

	/**
	 * adds a <tt>LegendElement</tt> to the collection.
	 *
	 * @param legendElement to add
	 */
	public void addLegendElement(LegendElement legendElement) {
		this.collection.add(legendElement);
	}

	/**
	 * 
	 * @return
	 */
	public LegendElement[] getLegendElements() {
		return (LegendElement[])collection.toArray(new LegendElement[collection.size()]);
	}

	/**
	 * sets the title of the <tt>LegendElement</tt>. The title will be displayed
	 * on top of the <tt>LegendElementCollection</tt>
	 *
	 * @param title title of the <tt>LegendElement</tt>
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * returns the title of the <tt>LegendElement</tt>
	 *
	 * @return 
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * returns the number of legend elements as an int
	 * @return number of legend elements
	 */
	public int getSize() {
		return this.collection.size();
	}

	/**
	 * @return
	 */
	public BufferedImage exportAsImage() throws LegendException {
		Debug.debugMethodBegin("LegendElementCollection_Impl", "exportAsImage");

		int[] titleFontMetrics;
		int titleheight = 0; // height of the title (default: 0, none)

		int maxheight = 0; // maximum width of resulting Image
		int maxwidth = 0; // maximum height of resulting Image
		int buffer = 10; // bufferspace between LegendElements and Title (eventually)

		LegendElement[] le = getLegendElements();
		BufferedImage[] imagearray = new BufferedImage[le.length];
		BufferedImage bi = null;

		for (int i = 0; i < le.length; i++) {
			imagearray[i] = le[i].exportAsImage();
			maxheight += (imagearray[i].getHeight() + buffer);
			maxwidth += imagearray[i].getWidth();
		}

		// printing the title (or not)
		Graphics g = null;
		if (getTitle() != null && getTitle().length() > 0) {
			titleFontMetrics = calculateFontMetrics(getTitle());
			titleheight = titleFontMetrics[1] + titleFontMetrics[2];
			maxheight += titleheight;

			bi = new BufferedImage(maxwidth, maxheight, BufferedImage.TYPE_INT_ARGB);
			g = bi.getGraphics();
			g.setColor(java.awt.Color.BLACK);
			// DEBUG: g.drawRect(0, 0, maxheight, maxwidth);
			g.drawString(getTitle(), 0, 0 + titleheight);
		} else {
			bi = new BufferedImage(maxwidth, maxheight, BufferedImage.TYPE_INT_ARGB);
			g = bi.getGraphics();
		}
		
		
		for (int j = 0; j < imagearray.length; j++) {
			g.drawImage(imagearray[j],
						0,
						(imagearray[j].getHeight() + buffer) * j + titleheight + buffer,
						imagearray[j].getWidth(),
						imagearray[j].getHeight(),
						null);
		}


		// bi = le[0].exportAsImage();
		Debug.debugMethodEnd();
		return bi;
	}

}