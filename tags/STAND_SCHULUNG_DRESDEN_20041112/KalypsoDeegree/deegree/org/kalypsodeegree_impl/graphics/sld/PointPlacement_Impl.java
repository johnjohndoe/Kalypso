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
package org.deegree_impl.graphics.sld;

import org.deegree.graphics.sld.ParameterValueType;
import org.deegree.graphics.sld.PointPlacement;
import org.deegree.model.feature.Feature;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;

/**
 * Incarnation of a sld:PointPlacement-element. For a PointPlacement, the anchor
 * point of the label and a linear displacement from the point can be specified,
 * to allow a graphic symbol to be plotted directly at the point. This might be
 * useful to label a city, for example. For a LinePlacement, a perpendicular
 * offset can be specified, to allow the line itself to be plotted also. This
 * might be useful for labelling a road or a river, for example.
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class PointPlacement_Impl implements PointPlacement, Marshallable
{
  private ParameterValueType rotation = null;

  private ParameterValueType[] anchorPoint = null;

  private ParameterValueType[] displacement = null;

  // should the placement be optimized?
  private boolean auto = false;

  PointPlacement_Impl()
  {}

  /**
   * Creates a new PointPlacement_Impl object.
   * 
   * @param anchorPoint
   * @param displacement
   * @param rotation
   */
  public PointPlacement_Impl( ParameterValueType[] anchorPoint, ParameterValueType[] displacement,
      ParameterValueType rotation, boolean auto )
  {
    this.anchorPoint = anchorPoint;
    this.displacement = displacement;
    this.rotation = rotation;
    this.auto = auto;
  }

  /**
   * The AnchorPoint element of a PointPlacement gives the location inside of a
   * label to use for anchoring the label to the main-geometry point.
   * <p>
   * </p>
   * The coordinates are given as two floating-point numbers in the AnchorPointX
   * and AnchorPointY elements each with values between 0.0 and 1.0 inclusive.
   * The bounding box of the label to be rendered is considered to be in a
   * coorindate space from 0.0 (lower-left corner) to 1.0 (upper-right corner),
   * and the anchor position is specified as a point in this space. The default
   * point is X=0, Y=0.5, which is at the middle height of the left-hand side of
   * the label. A system may choose different anchor points to de-conflict
   * labels.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return 2 double values: x ([0]) and y ([0])
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  public double[] getAnchorPoint( Feature feature ) throws FilterEvaluationException
  {
    double[] anchorPointVal =
    { 0.0, 0.5 };

    if( anchorPoint != null )
    {
      anchorPointVal[0] = Double.parseDouble( anchorPoint[0].evaluate( feature ) );
      anchorPointVal[1] = Double.parseDouble( anchorPoint[1].evaluate( feature ) );
    }

    return anchorPointVal;
  }

  /**
   * @see PointPlacement#getAnchorPoint(Feature)
   *      <p>
   * @param anchorPoint
   *          anchorPoint for the PointPlacement
   */
  public void setAnchorPoint( double[] anchorPoint )
  {
    ParameterValueType pvt = null;
    ParameterValueType[] pvtArray = new ParameterValueType[anchorPoint.length];
    for( int i = 0; i < anchorPoint.length; i++ )
    {
      pvt = StyleFactory.createParameterValueType( "" + anchorPoint[i] );
      pvtArray[i] = pvt;
    }
    this.anchorPoint = pvtArray;
  }

  /**
   * The Displacement element of a PointPlacement gives the X and Y
   * displacements from the main-geometry point to render a text label.
   * <p>
   * </p>
   * This will often be used to avoid over-plotting a graphic symbol marking a
   * city or some such feature. The displacements are in units of pixels above
   * and to the right of the point. A system may reflect this displacement about
   * the X and/or Y axes to de-conflict labels. The default displacement is X=0,
   * Y=0.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return 2 double values: x ([0]) and y ([0])
   * @throws FilterEvaluationException
   *           if the evaluation fails*
   */
  public double[] getDisplacement( Feature feature ) throws FilterEvaluationException
  {
    double[] displacementVal =
    { 0.0, 0.0 };

    if( displacement != null )
    {
      displacementVal[0] = Double.parseDouble( displacement[0].evaluate( feature ) );
      displacementVal[1] = Double.parseDouble( displacement[1].evaluate( feature ) );
    }

    return displacementVal;
  }

  /**
   * @see PointPlacement#getDisplacement(Feature)
   *      <p>
   * @param displacement
   */
  public void setDisplacement( double[] displacement )
  {
    ParameterValueType pvt = null;
    ParameterValueType[] pvtArray = new ParameterValueType[displacement.length];
    for( int i = 0; i < displacement.length; i++ )
    {
      pvt = StyleFactory.createParameterValueType( "" + displacement[i] );
      pvtArray[i] = pvt;
    }
    this.displacement = pvtArray;
  }

  /**
   * The Rotation of a PointPlacement gives the clockwise rotation of the label
   * in degrees from the normal direction for a font (left-to-right for Latin-
   * derived human languages at least).
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return double value describing the rotation parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails*
   */
  public double getRotation( Feature feature ) throws FilterEvaluationException
  {
    double rot = 0.0;

    if( rotation != null )
    {
      rot = Double.parseDouble( rotation.evaluate( feature ) );
    }

    return rot;
  }

  /**
   * @see PointPlacement#getRotation(Feature)
   * @param rotation
   *          the rotation to be set for the PointPlacement
   */
  public void setRotation( double rotation )
  {
    ParameterValueType pvt = null;
    pvt = StyleFactory.createParameterValueType( "" + rotation );
    this.rotation = pvt;
  }

  /**
   * Returns whether the placement should be optimized or not.
   * <p>
   * 
   * @return true, if it should be optimized
   */
  public boolean isAuto()
  {
    return auto;
  }

  /**
   * 
   * <p>
   * 
   * @param auto
   */
  public void setAuto( boolean auto )
  {
    this.auto = auto;
  }

  /**
   * exports the content of the PointPlacement as XML formated String
   * 
   * @return xml representation of the PointPlacement
   */  
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<PointPlacement" );
    if( auto )
    {
      sb.append( " auto='true'" );
    }
    sb.append( ">" );
    if( anchorPoint != null && anchorPoint.length > 1 )
    {
      sb.append( "<AnchorPoint>" ).append( "<AnchorPointX>" );
      sb.append( ( (Marshallable)anchorPoint[0] ).exportAsXML() );
      sb.append( "</AnchorPointX>" ).append( "<AnchorPointY>" );
      sb.append( ( (Marshallable)anchorPoint[1] ).exportAsXML() );
      sb.append( "</AnchorPointY>" ).append( "</AnchorPoint>" );
    }
    if( displacement != null && displacement.length > 1 )
    {
      sb.append( "<Displacement>" ).append( "<DisplacementX>" );
      if( anchorPoint == null || anchorPoint[0] == null )
        sb.append( 0.0 );
      else
        sb.append( ( (Marshallable)anchorPoint[0] ).exportAsXML() );
      sb.append( "</DisplacementX>" ).append( "<DisplacementY>" );
      if( anchorPoint == null || anchorPoint[1] == null )
        sb.append( 0.5 );
      else
        sb.append( ( (Marshallable)anchorPoint[1] ).exportAsXML() );
      sb.append( "</DisplacementY>" ).append( "</Displacement>" );
    }
    if( rotation != null )
    {
      sb.append( "<Rotation>" );
      sb.append( ( (Marshallable)rotation ).exportAsXML() );
      sb.append( "</Rotation>" );
    }

    sb.append( "</PointPlacement>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }  
}