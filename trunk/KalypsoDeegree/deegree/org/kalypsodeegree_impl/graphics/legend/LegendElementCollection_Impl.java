/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.legend;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.util.ArrayList;

import org.kalypsodeegree.graphics.legend.LegendElement;
import org.kalypsodeegree.graphics.legend.LegendElementCollection;
import org.kalypsodeegree.graphics.legend.LegendException;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * <tt>LegendElementCollection</tt> is a collection of <tt>LegendElement</tt>
 * s and is a <tt>LegendElement</tt> too. It can be used to group elements or
 * to create more complex elements.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class LegendElementCollection_Impl extends LegendElement_Impl implements
    LegendElementCollection
{

  ArrayList collection = null;

  String title = "";

  /**
   * empty constructor
   *  
   */
  public LegendElementCollection_Impl()
  {
    super();
    this.collection = new ArrayList();
  }

  /**
   * empty constructor
   *  
   */
  public LegendElementCollection_Impl( String title )
  {
    super();
    this.collection = new ArrayList();
    this.title = title;
  }

  /**
   * adds a <tt>LegendElement</tt> to the collection.
   * 
   * @param legendElement
   *          to add
   */
  public void addLegendElement( LegendElement legendElement )
  {
    this.collection.add( legendElement );
  }

  /**
   * 
   * @return
   */
  public LegendElement[] getLegendElements()
  {
    return (LegendElement[])collection.toArray( new LegendElement[collection.size()] );
  }

  /**
   * sets the title of the <tt>LegendElement</tt>. The title will be
   * displayed on top of the <tt>LegendElementCollection</tt>
   * 
   * @param title
   *          title of the <tt>LegendElement</tt>
   */
  public void setTitle( String title )
  {
    this.title = title;
  }

  /**
   * returns the title of the <tt>LegendElement</tt>
   * 
   * @return
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * returns the number of legend elements as an int
   * 
   * @return number of legend elements
   */
  public int getSize()
  {
    return this.collection.size();
  }

  /**
   * @return
   */
  public BufferedImage exportAsImage() throws LegendException
  {
    Debug.debugMethodBegin( "LegendElementCollection_Impl", "exportAsImage" );

    int[] titleFontMetrics;
    int titleheight = 0; // height of the title (default: 0, none)

    int maxheight = 0; // maximum width of resulting Image
    int maxwidth = 0; // maximum height of resulting Image
    int buffer = 10; // bufferspace between LegendElements and Title
    // (eventually)

    LegendElement[] le = getLegendElements();
    BufferedImage[] imagearray = new BufferedImage[le.length];
    BufferedImage bi = null;

    for( int i = 0; i < le.length; i++ )
    {
      imagearray[i] = le[i].exportAsImage();
      maxheight += ( imagearray[i].getHeight() + buffer );
      // maxwidth += imagearray[i].getWidth();
      if( maxwidth < imagearray[i].getWidth() )
      {
        maxwidth = imagearray[i].getWidth();
      }
    }

    // printing the title (or not)
    Graphics g = null;
    if( getTitle() != null && getTitle().length() > 0 )
    {
      titleFontMetrics = calculateFontMetrics( getTitle() );
      titleheight = titleFontMetrics[1] + titleFontMetrics[2];
      maxheight += titleheight;

      // is title wider than the maxwidth?
      if( maxwidth <= titleFontMetrics[0] )
      {
        maxwidth = titleFontMetrics[0];
      }

      bi = new BufferedImage( maxwidth, maxheight, BufferedImage.TYPE_INT_ARGB );
      g = bi.getGraphics();
      g.setColor( java.awt.Color.BLACK );
      // DEBUG: g.drawRect(0, 0, maxheight, maxwidth);
      g.drawString( getTitle(), 0, 0 + titleheight );
    }
    else
    {
      bi = new BufferedImage( maxwidth, maxheight, BufferedImage.TYPE_INT_ARGB );
      g = bi.getGraphics();
    }

    for( int j = 0; j < imagearray.length; j++ )
    {
      g.drawImage( imagearray[j], 0, ( imagearray[j].getHeight() + buffer ) * j + titleheight
          + buffer, imagearray[j].getWidth(), imagearray[j].getHeight(), null );
    }

    // bi = le[0].exportAsImage();
    Debug.debugMethodEnd();
    return bi;
  }
}

/*******************************************************************************
 * ****************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * LegendElementCollection_Impl.java,v $ Revision 1.7 2004/06/01 15:55:05 poth
 * no message
 * 
 * Revision 1.6 2004/04/07 10:58:46 axel_schaefer bugfix
 * 
 * 
 *  
 ******************************************************************************/