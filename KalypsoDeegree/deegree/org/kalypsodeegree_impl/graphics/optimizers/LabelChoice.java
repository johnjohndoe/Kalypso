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
package org.kalypsodeegree_impl.graphics.optimizers;

import org.kalypsodeegree.graphics.displayelements.Label;
import org.kalypsodeegree.graphics.displayelements.LabelDisplayElement;

public class LabelChoice
{

  // LabelDisplayElement that this LabelChoice belongs to
  private LabelDisplayElement element;

  // index of the currently selected Label
  private int selected;

  // candidates of Labels
  private Label[] candidates;

  // quality of each Label
  private float[] qualities;

  // boundingbox of all contained labels
  private int maxX, maxY, minX, minY;

  public LabelChoice( LabelDisplayElement element, Label[] candidates, float[] qualities, int selected, int maxX,
      int maxY, int minX, int minY )
  {
    this.element = element;
    this.candidates = candidates;
    this.qualities = qualities;
    this.selected = selected;
    this.maxX = maxX;
    this.maxY = maxY;
    this.minX = minX;
    this.minY = minY;
  }

  //	public void paint (Graphics2D g) {
  //		for (int i = 0; i < candidates.length; i++) {
  //			((Label) candidates [i]).paintBoundaries(g);
  //		}
  //	}

  public void selectLabelRandomly()
  {
    selected = (int)( Math.random() * ( candidates.length - 1 ) + 0.5 );
  }

  public void setSelected( int selected )
  {
    this.selected = selected;
  }

  public int getSelected()
  {
    return selected;
  }

  public float getQuality()
  {
    return qualities[selected];
  }

  public Label getSelectedLabel()
  {
    return candidates[selected];
  }

  public LabelDisplayElement getElement()
  {
    return element;
  }

  public int getMaxX()
  {
    return maxX;
  }

  public int getMaxY()
  {
    return maxY;
  }

  public int getMinX()
  {
    return minX;
  }

  public int getMinY()
  {
    return minY;
  }

  /**
   * Determines if the <tt>LabelChoice<tt> can intersect another
   * <tt>LabelChoice</tt> by any chance, i.e. there are two
   * <tt>Labels</tt> from each choice that intersect.   
   * <p>
   * @param that LabelChoice to test
   * @return true if the LabelChoices can intersect
   */
  public boolean intersects( LabelChoice that )
  {

    int west1 = getMinX();
    int south1 = getMinY();
    int east1 = getMaxX();
    int north1 = getMaxY();

    int west2 = that.getMinX();
    int south2 = that.getMinY();
    int east2 = that.getMaxX();
    int north2 = that.getMaxY();

    // special cases: one box lays completly inside the other one
    if( ( west1 <= west2 ) && ( south1 <= south2 ) && ( east1 >= east2 ) && ( north1 >= north2 ) )
    {
      return true;
    }
    if( ( west1 >= west2 ) && ( south1 >= south2 ) && ( east1 <= east2 ) && ( north1 <= north2 ) )
    {
      return true;
    }
    // in any other case of intersection, at least one line of the BBOX has
    // to cross a line of the other BBOX
    // check western boundary of box 1
    // "touching" boxes must not intersect
    if( ( west1 >= west2 ) && ( west1 < east2 ) )
    {
      if( ( south1 <= south2 ) && ( north1 > south2 ) )
      {
        return true;
      }

      if( ( south1 < north2 ) && ( north1 >= north2 ) )
      {
        return true;
      }
    }
    // check eastern boundary of box 1
    // "touching" boxes must not intersect
    if( ( east1 > west2 ) && ( east1 <= east2 ) )
    {
      if( ( south1 <= south2 ) && ( north1 > south2 ) )
      {
        return true;
      }

      if( ( south1 < north2 ) && ( north1 >= north2 ) )
      {
        return true;
      }
    }
    // check southern boundary of box 1
    // "touching" boxes must not intersect
    if( ( south1 >= south2 ) && ( south1 < north2 ) )
    {
      if( ( west1 <= west2 ) && ( east1 > west2 ) )
      {
        return true;
      }

      if( ( west1 < east2 ) && ( east1 >= east2 ) )
      {
        return true;
      }
    }
    // check northern boundary of box 1
    // "touching" boxes must not intersect
    if( ( north1 > south2 ) && ( north1 <= north2 ) )
    {
      if( ( west1 <= west2 ) && ( east1 > west2 ) )
      {
        return true;
      }

      if( ( west1 < east2 ) && ( east1 >= east2 ) )
      {
        return true;
      }
    }
    return false;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    if( candidates.length > 0 )
    {
      return candidates[0].toString();
    }
    return "empty";
  }
}