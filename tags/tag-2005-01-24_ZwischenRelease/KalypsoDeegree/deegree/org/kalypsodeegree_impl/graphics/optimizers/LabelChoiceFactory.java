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
package org.deegree_impl.graphics.optimizers;

import java.awt.Graphics2D;
import java.awt.font.FontRenderContext;
import java.awt.font.LineMetrics;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;

import org.deegree.filterencoding.FilterEvaluationException;
import org.deegree.graphics.displayelements.Label;
import org.deegree.graphics.displayelements.LabelDisplayElement;
import org.deegree.graphics.sld.LabelPlacement;
import org.deegree.graphics.sld.LinePlacement;
import org.deegree.graphics.sld.PointPlacement;
import org.deegree.graphics.sld.TextSymbolizer;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.graphics.displayelements.LabelFactory;
import org.deegree_impl.model.geometry.GeometryFactory;

/**
 * Factory class for <tt>LabelChoice</tt> -objects.
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class LabelChoiceFactory
{

  /**
   * Determines <tt>LabelChoice</tt> s for the given
   * <tt>LabelDisplayElement</tt>.
   * <p>
   * 
   * @param element
   * @param g
   * @param projection
   * @return
   */
  static ArrayList createLabelChoices( LabelDisplayElement element, Graphics2D g,
      GeoTransform projection )
  {

    ArrayList choices = new ArrayList();

    try
    {
      Feature feature = element.getFeature();
      String caption = element.getLabel().evaluate( feature );

      // sanity check: empty labels are ignored
      if( caption == null || caption.trim().equals( "" ) )
      {
        return choices;
      }

      GM_Object geometry = element.getGeometry();
      TextSymbolizer symbolizer = (TextSymbolizer)element.getSymbolizer();

      // gather font information
      org.deegree.graphics.sld.Font sldFont = symbolizer.getFont();
      java.awt.Font font = new java.awt.Font( sldFont.getFamily( feature ), sldFont
          .getStyle( feature )
          | sldFont.getWeight( feature ), sldFont.getSize( feature ) );
      g.setFont( font );
      FontRenderContext frc = g.getFontRenderContext();
      Rectangle2D bounds = font.getStringBounds( caption, frc );
      LineMetrics metrics = font.getLineMetrics( caption, frc );
      int w = (int)bounds.getWidth();
      int h = (int)bounds.getHeight();
      //int descent = (int) metrics.getDescent ();

      LabelPlacement lPlacement = symbolizer.getLabelPlacement();

      // element is associated to a point geometry
      if( geometry instanceof GM_Point )
      {

        // get screen coordinates
        int[] coords = LabelFactory.calcScreenCoordinates( projection, geometry );
        int x = coords[0];
        int y = coords[1];

        // use placement information from SLD
        PointPlacement pPlacement = lPlacement.getPointPlacement();
        //double [] anchorPoint = pPlacement.getAnchorPoint( feature );
        double[] displacement = pPlacement.getDisplacement( feature );
        double rotation = pPlacement.getRotation( feature );

        Label[] labels = new Label[8];
        labels[0] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ), metrics,
            feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
            { 0.0, 0.0 }, new double[]
            { displacement[0], displacement[1] } );
        labels[1] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ), metrics,
            feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
            { 0.0, 1.0 }, new double[]
            { displacement[0], -displacement[1] } );
        labels[2] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ), metrics,
            feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
            { 1.0, 1.0 }, new double[]
            { -displacement[0], -displacement[1] } );
        labels[3] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ), metrics,
            feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
            { 1.0, 0.0 }, new double[]
            { -displacement[0], displacement[1] } );
        labels[4] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ), metrics,
            feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
            { 0.0, 0.5 }, new double[]
            { displacement[0], 0 } );
        labels[5] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ), metrics,
            feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
            { 0.5, 1.0 }, new double[]
            { 0, -displacement[1] } );
        labels[6] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ), metrics,
            feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
            { 1.0, 0.5 }, new double[]
            { -displacement[0], 0 } );
        labels[7] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ), metrics,
            feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
            { 0.5, 0.0 }, new double[]
            { 0, displacement[1] } );
        float[] qualities = new float[]
        { 0.0f, 0.5f, 0.33f, 0.27f, 0.15f, 1.0f, 0.1f, 0.7f };
        choices.add( new LabelChoice( element, labels, qualities, 0, labels[1].getMaxX(), labels[1]
            .getMaxY(), labels[3].getMinX(), labels[3].getMinY() ) );

        // element is associated to a polygon geometry
      }
      else if( geometry instanceof GM_Surface || geometry instanceof GM_MultiSurface )
      {

        // get screen coordinates
        int[] coords = LabelFactory.calcScreenCoordinates( projection, geometry );
        int x = coords[0];
        int y = coords[1];

        // use placement information from SLD
        PointPlacement pPlacement = lPlacement.getPointPlacement();
        //				double [] anchorPoint = pPlacement.getAnchorPoint( feature );
        //				double [] displacement = pPlacement.getDisplacement( feature );
        double rotation = pPlacement.getRotation( feature );

        // center label within the intersection of the screen surface and the
        // polygon geometry
        GM_Surface screenSurface = GeometryFactory.createGM_Surface( projection.getSourceRect(),
            null );
        GM_Object intersection = screenSurface.intersection( geometry );

        if( intersection != null )
        {
          GM_Position source = intersection.getCentroid().getPosition();
          x = (int)( projection.getDestX( source.getX() ) + 0.5 );
          y = (int)( projection.getDestY( source.getY() ) + 0.5 );
          Label[] labels = new Label[3];
          labels[0] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ),
              metrics, feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
              { 0.5, 0.5 }, new double[]
              { 0, 0 } );
          labels[1] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ),
              metrics, feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
              { 0.5, 0.0 }, new double[]
              { 0, 0 } );
          labels[2] = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ),
              metrics, feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, new double[]
              { 0.5, 1.0 }, new double[]
              { 0, 0 } );

          float[] qualities = new float[]
          { 0.0f, 0.25f, 0.5f };
          choices.add( new LabelChoice( element, labels, qualities, 0, labels[0].getMaxX(),
              labels[2].getMaxY(), labels[0].getMinX(), labels[1].getMinY() ) );
        }

        // element is associated to a line geometry
      }
      else if( geometry instanceof GM_Curve || geometry instanceof GM_MultiCurve )
      {

        GM_Surface screenSurface = GeometryFactory.createGM_Surface( projection.getSourceRect(),
            null );
        GM_Object intersection = screenSurface.intersection( geometry );

        if( intersection != null )
        {
          ArrayList list = null;
          if( intersection instanceof GM_Curve )
          {
            list = createLabelChoices( (GM_Curve)intersection, element, g, projection );
          }
          else if( intersection instanceof GM_MultiCurve )
          {
            list = createLabelChoices( (GM_MultiCurve)intersection, element, g, projection );
          }
          else
          {
            throw new Exception( "Intersection produced unexpected geometry type: '"
                + intersection.getClass().getName() + "'!" );
          }
          choices = list;
        }
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return choices;
  }

  /**
   * Determines <tt>LabelChoice</tt> s for the given <tt>GM_MultiCurve</tt>
   * where a <tt>Label</tt> could be drawn. For each <tt>LabelChoice</tt>,
   * three candidates are generated: one on the line, one above it and one
   * below.
   * <p>
   * 
   * @param element
   * @param g
   * @param projection
   * @return ArrayList containing <tt>LabelChoice</tt> -objects
   * @throws FilterEvaluationException
   */
  static ArrayList createLabelChoices( GM_MultiCurve multiCurve, LabelDisplayElement element,
      Graphics2D g, GeoTransform projection ) throws FilterEvaluationException
  {

    ArrayList choices = new ArrayList( 1000 );
    for( int i = 0; i < multiCurve.getSize(); i++ )
    {
      GM_Curve curve = multiCurve.getCurveAt( i );
      choices.addAll( createLabelChoices( curve, element, g, projection ) );
    }
    return choices;
  }

  /**
   * Determines <tt>LabelChoice</tt> s for the given <tt>GM_Curve</tt> where
   * a <tt>Label</tt> could be drawn. For each <tt>LabelChoice</tt>, three
   * candidates are generated: one on the line, one above it and one below.
   * <p>
   * 
   * @param curve
   * @param element
   * @param g
   * @param projection
   * @return ArrayList containing <tt>LabelChoice</tt> -objects
   * @throws FilterEvaluationException
   */
  static ArrayList createLabelChoices( GM_Curve curve, LabelDisplayElement element, Graphics2D g,
      GeoTransform projection ) throws FilterEvaluationException
  {

    Feature feature = element.getFeature();

    // determine the placement type and parameters from the TextSymbolizer
    double perpendicularOffset = 0.0;
    int placementType = LinePlacement.TYPE_ABSOLUTE;
    double lineWidth = 3.0;
    int gap = 6;
    TextSymbolizer symbolizer = ( (TextSymbolizer)element.getSymbolizer() );
    if( symbolizer.getLabelPlacement() != null )
    {
      LinePlacement linePlacement = symbolizer.getLabelPlacement().getLinePlacement();
      if( linePlacement != null )
      {
        placementType = linePlacement.getPlacementType( element.getFeature() );
        perpendicularOffset = linePlacement.getPerpendicularOffset( element.getFeature() );
        lineWidth = linePlacement.getLineWidth( element.getFeature() );
        gap = linePlacement.getGap( element.getFeature() );
      }
    }

    // get width & height of the caption
    String caption = element.getLabel().evaluate( element.getFeature() );
    org.deegree.graphics.sld.Font sldFont = symbolizer.getFont();
    java.awt.Font font = new java.awt.Font( sldFont.getFamily( element.getFeature() ), sldFont
        .getStyle( element.getFeature() )
        | sldFont.getWeight( element.getFeature() ), sldFont.getSize( element.getFeature() ) );
    g.setFont( font );
    FontRenderContext frc = g.getFontRenderContext();
    Rectangle2D bounds = font.getStringBounds( caption, frc );
    LineMetrics metrics = font.getLineMetrics( caption, frc );
    double width = bounds.getWidth();
    double height = bounds.getHeight();

    // get screen coordinates of the line
    int[][] pos = LabelFactory.calcScreenCoordinates( projection, curve );

    // ideal distance from the line
    double delta = height / 2.0 + lineWidth / 2.0;

    // walk along the linestring and "collect" possible label positions
    int w = (int)width;
    int lastX = pos[0][0];
    int lastY = pos[1][0];
    int count = pos[2][0];
    int boxStartX = lastX;
    int boxStartY = lastY;

    ArrayList choices = new ArrayList( 1000 );
    ArrayList eCandidates = new ArrayList( 100 );
    int i = 0;
    int kk = 0;
    while( i < count && kk < 100 )
    {
      kk++;
      int x = pos[0][i];
      int y = pos[1][i];

      // segment found where endpoint of label should be located?
      if( LabelFactory.getDistance( boxStartX, boxStartY, x, y ) >= w )
      {

        int[] p0 = new int[]
        { boxStartX, boxStartY };
        int[] p1 = new int[]
        { lastX, lastY };
        int[] p2 = new int[]
        { x, y };

        int[] p = LabelFactory.findPointWithDistance( p0, p1, p2, w );
        x = p[0];
        y = p[1];

        lastX = x;
        lastY = y;
        int boxEndX = x;
        int boxEndY = y;

        // does the linesegment run from right to left?
        if( x <= boxStartX )
        {
          boxEndX = boxStartX;
          boxEndY = boxStartY;
          boxStartX = x;
          boxStartY = y;
          x = boxEndX;
          y = boxEndY;
        }

        double rotation = LabelFactory.getRotation( boxStartX, boxStartY, x, y );
        double[] deviation = LabelFactory.calcDeviation( new int[]
        { boxStartX, boxStartY }, new int[]
        { boxEndX, boxEndY }, eCandidates );

        switch( placementType )
        {
        case LinePlacement.TYPE_ABSOLUTE:
        {
          Label label = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ),
              metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int)width,
              (int)height, rotation, new double[]
              { 0.0, 0.5 }, new double[]
              { ( w - width ) / 2, perpendicularOffset } );
          choices.add( new LabelChoice( element, new Label[]
          { label }, new float[]
          { 0.0f }, 0, label.getMaxX(), label.getMaxY(), label.getMinX(), label.getMinY() ) );
          break;
        }
        case LinePlacement.TYPE_ABOVE:
        {
          Label upperLabel = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ),
              metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int)width,
              (int)height, rotation, new double[]
              { 0.0, 0.5 }, new double[]
              { ( w - width ) / 2, delta + deviation[0] } );
          choices.add( new LabelChoice( element, new Label[]
          { upperLabel }, new float[]
          { 0.0f }, 0, upperLabel.getMaxX(), upperLabel.getMaxY(), upperLabel.getMinX(), upperLabel
              .getMinY() ) );
          break;
        }
        case LinePlacement.TYPE_BELOW:
        {
          Label lowerLabel = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ),
              metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int)width,
              (int)height, rotation, new double[]
              { 0.0, 0.5 }, new double[]
              { ( w - width ) / 2, -delta - deviation[1] } );
          choices.add( new LabelChoice( element, new Label[]
          { lowerLabel }, new float[]
          { 0.0f }, 0, lowerLabel.getMaxX(), lowerLabel.getMaxY(), lowerLabel.getMinX(), lowerLabel
              .getMinY() ) );
          break;
        }
        case LinePlacement.TYPE_CENTER:
        {
          Label centerLabel = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ),
              metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int)width,
              (int)height, rotation, new double[]
              { 0.0, 0.5 }, new double[]
              { ( w - width ) / 2, 0.0 } );
          choices.add( new LabelChoice( element, new Label[]
          { centerLabel }, new float[]
          { 0.0f }, 0, centerLabel.getMaxX(), centerLabel.getMaxY(), centerLabel.getMinX(),
              centerLabel.getMinY() ) );
          break;
        }
        case LinePlacement.TYPE_AUTO:
        {
          Label upperLabel = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ),
              metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int)width,
              (int)height, rotation, new double[]
              { 0.0, 0.5 }, new double[]
              { ( w - width ) / 2, delta + deviation[0] } );
          Label lowerLabel = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ),
              metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int)width,
              (int)height, rotation, new double[]
              { 0.0, 0.5 }, new double[]
              { ( w - width ) / 2, -delta - deviation[1] } );
          Label centerLabel = LabelFactory.createLabel( caption, font, sldFont.getColor( feature ),
              metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int)width,
              (int)height, rotation, new double[]
              { 0.0, 0.5 }, new double[]
              { ( w - width ) / 2, 0.0 } );
          choices.add( new LabelChoice( element, new Label[]
          { lowerLabel, upperLabel, centerLabel }, new float[]
          { 0.0f, 0.25f, 1.0f }, 0, centerLabel.getMaxX(), lowerLabel.getMaxY(), centerLabel
              .getMinX(), upperLabel.getMinY() ) );
          break;
        }
        default:
        {}
        }

        boxStartX = lastX;
        boxStartY = lastY;
        eCandidates.clear();
      }
      else
      {
        eCandidates.add( new int[]
        { x, y } );
        lastX = x;
        lastY = y;
        i++;
      }
    }

    // pick LabelChoices on the linestring
    ArrayList pick = new ArrayList();
    int n = choices.size();
    for( int j = n / 2; j < choices.size(); j += ( gap + 1 ) )
    {
      pick.add( choices.get( j ) );
    }
    for( int j = n / 2 - ( gap + 1 ); j > 0; j -= ( gap + 1 ) )
    {
      pick.add( choices.get( j ) );
    }
    return pick;
  }
}