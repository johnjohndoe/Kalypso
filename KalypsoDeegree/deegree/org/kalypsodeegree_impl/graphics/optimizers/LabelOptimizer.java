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

import java.awt.Graphics2D;
import java.util.ArrayList;

import org.kalypsodeegree.graphics.Theme;
import org.kalypsodeegree.graphics.displayelements.Label;
import org.kalypsodeegree.graphics.displayelements.LabelDisplayElement;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;

/**
 * Selects optimized <tt>Label</tt> s (graphical representations generated
 * from <tt>LabelDisplayElements</tt>) that have a minimimal amount of
 * overlapping.
 * <p>
 * The labeling and optimization approach uses ideas from papers by Ingo Petzold
 * on automated label placement.
 * <p>
 * TODO: The handling of rotated labels is currently broken. Don't use rotated
 * <tt>LabelDisplayElement</tt> s with this optimizer at the moment!
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class LabelOptimizer extends AbstractOptimizer
{

  // contains the LabelDisplayElements that are to be optimized
  private ArrayList displayElements = new ArrayList( 1000 );

  // contains the LabelChoices
  private ArrayList choices = new ArrayList( 1000 );

  // collision matrix of LabelChoices that may collide
  private boolean[][] candidates;

  /**
   * Creates a new instance of LabelOptimizer.
   */
  public LabelOptimizer()
  {}

  /**
   * Creates a new instance of LabelOptimizer for the given <tt>Themes</tt>.
   * 
   * @param themes
   */
  public LabelOptimizer( Theme[] themes )
  {

    // collect all LabelDisplayElements from all Themes
    for( int i = 0; i < themes.length; i++ )
    {
      addTheme( themes[i] );
    }
  }

  /**
   * Adds a <tt>Theme<tt> that the <tt>Optimizer<tt> should consider.
   * @param theme
   */
  public void addTheme( Theme theme )
  {
    if( !themes.contains( theme ) )
    {
      ArrayList themeElements = theme.getDisplayElements();
      for( int i = 0; i < themeElements.size(); i++ )
      {
        Object o = themeElements.get( i );
        if( o instanceof LabelDisplayElement )
        {
          LabelDisplayElement element = (LabelDisplayElement)o;
          TextSymbolizer symbolizer = (TextSymbolizer)element.getSymbolizer();
          // only add element if "auto" is set
          if( symbolizer.getLabelPlacement().getPointPlacement() != null
              && symbolizer.getLabelPlacement().getPointPlacement().isAuto() )
          {
            displayElements.add( o );
            //					} else if (symbolizer.getLabelPlacement().getLinePlacement() !=
            // null) {
            //						displayElements.add (o);
          }
        }
      }
      themes.add( theme );
    }
  }

  /**
   * Finds optimized <tt>Label</tt> representations for the registered
   * <tt>LabelDisplayElement</tt>s.
   * <p>
   * 
   * @param g
   */
  public void optimize( Graphics2D g )
  {

    choices.clear();
    double scale = mapView.getScale( g );
    GeoTransform projection = mapView.getProjection();

    // used to signal the LabelDisplayElement that it should
    // not create Labels itself (in any case)
    Label[] dummyLabels = new Label[0];

    // collect LabelChoices for all LabelDisplayElements
    for( int i = 0; i < displayElements.size(); i++ )
    {

      LabelDisplayElement element = (LabelDisplayElement)displayElements.get( i );
      if( !element.doesScaleConstraintApply( scale ) )
        continue;

      element.setLabels( dummyLabels );
      choices.addAll( LabelChoiceFactory.createLabelChoices( element, g, projection ) );
    }

    buildCollisionMatrix();

    // do the magic
    try
    {
      anneal();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    // sets the optimized labels to the LabelDisplayElements, so
    // they are considered when the DisplayElements are painted the next
    // time

    for( int i = 0; i < choices.size(); i++ )
    {
      LabelChoice choice = (LabelChoice)choices.get( i );
      choice.getElement().addLabel( choice.getSelectedLabel() );
    }
  }

  /**
   * Builds the collision matrix for all <tt>LabelChoice</tt>s.
   */
  private void buildCollisionMatrix()
  {
    //		long now = System.currentTimeMillis();
    candidates = new boolean[choices.size()][choices.size()];
    for( int i = 0; i < choices.size(); i++ )
    {
      LabelChoice choice1 = (LabelChoice)choices.get( i );
      for( int j = i + 1; j < choices.size(); j++ )
      {
        LabelChoice choice2 = (LabelChoice)choices.get( j );
        if( choice1.intersects( choice2 ) )
        {
          candidates[i][j] = true;
        }
      }
    }
    //		System.out.println ("Building of collision matrix took: " +
    // (System.currentTimeMillis() - now) + " millis.");
  }

  /**
   * Performs the "Simulated Annealing" for the <tt>LabelChoices</tt>.
   */
  private void anneal()
  {
    double currentValue = objectiveFunction();

    double temperature = 1.0;
    int counter = 0;
    int successCounter = 0;
    int failCounter = 0;

    int n = choices.size();

    //		System.out.println ("Starting Annealing with value: " + currentValue);

    while( counter <= 2500 && currentValue > ( n + 0.8 * 40 ) )
    {

      counter++;
      if( successCounter % 5 == 0 )
      {
        temperature *= 0.9;
      }

      // choose one Label from one LabelChoice randomly
      int choiceIndex = (int)( Math.random() * ( n - 1 ) + 0.5 );
      LabelChoice choice = (LabelChoice)choices.get( choiceIndex );
      int oldPos = choice.getSelected();
      choice.selectLabelRandomly();

      double value = objectiveFunction();

      // does the new placement imply an improvement?
      if( value < currentValue )
      {
        // yes -> keep it
        currentValue = value;
        successCounter++;
        failCounter = 0;
      }
      else
      {
        // no -> only keep it with a certain probability
        if( Math.random() < temperature )
        {
          currentValue = value;
          failCounter = 0;
        }
        else
        {
          // change it back to the old placement
          choice.setSelected( oldPos );
          failCounter++;
        }
      }
    }
    //		System.out.println ("Final value: " + currentValue);
    //		System.out.println ("Annealing took: " + (System.currentTimeMillis() -
    // now) + " millis.");
  }

  /**
   * Calculates a quality value for the currently selected combination of
   * <tt>Label</tt>s.
   * <p>
   * 
   * @return
   */
  private double objectiveFunction()
  {
    float value = 0.0f;

    for( int i = 0; i < choices.size(); i++ )
    {
      LabelChoice choice1 = (LabelChoice)choices.get( i );
      Label label1 = choice1.getSelectedLabel();
      value += choice1.getQuality() + 1.0f;

      for( int j = i + 1; j < choices.size(); j++ )
      {
        if( candidates[i][j] )
        {
          LabelChoice choice2 = (LabelChoice)choices.get( j );
          Label label2 = choice2.getSelectedLabel();
          if( label1.intersects( label2 ) )
          {
            value += 40.0f;
          }
        }
      }
    }
    return value;
  }
}