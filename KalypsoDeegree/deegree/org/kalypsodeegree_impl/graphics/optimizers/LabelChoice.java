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
package org.deegree_impl.graphics.optimizers;

import org.deegree.graphics.displayelements.Label;
import org.deegree.graphics.displayelements.LabelDisplayElement;

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

  public LabelChoice( LabelDisplayElement element, Label[] candidates, float[] qualities,
      int selected, int maxX, int maxY, int minX, int minY )
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

  public String toString()
  {
    if( candidates.length > 0 )
    {
      return candidates[0].toString();
    }
    else
    {
      return "empty";
    }
  }
}