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

import org.deegree.graphics.sld.LinePlacement;
import org.deegree.graphics.sld.ParameterValueType;
import org.deegree.model.feature.Feature;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;

/**
 * Incarnation of an sld:LinePlacement-element.
 * <p>
 * Contains some deegree-specific extensions:
 * <ul>
 * <li>PerpendicularOffset: may be used as defined by the OGC, but it can also
 * be set to one of the special values 'center', 'above', 'below', 'auto'
 * <li>Gap: defines the distance between two captions on the line string
 * <li>LineWidth: provides the thickness of the styled line (needed as
 * information for the correct positioning of labels above and below the line
 * string)
 * </ul>
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class LinePlacement_Impl implements LinePlacement, Marshallable
{
  private ParameterValueType perpendicularOffset = null;

  private ParameterValueType lineWidth = null;

  private ParameterValueType gap = null;

  public LinePlacement_Impl( ParameterValueType perpendicularOffset, ParameterValueType lineWidth,
      ParameterValueType gap )
  {
    this.perpendicularOffset = perpendicularOffset;
    this.lineWidth = lineWidth;
    this.gap = gap;
  }

  /**
   * The PerpendicularOffset element of a LinePlacement gives the perpendicular
   * distance away from a line to draw a label. The distance is in pixels and is
   * positive to the left-hand side of the line string. Negative numbers mean
   * right. The default offset is 0.
   * <p>
   * deegree-specific extension: if the element has one of the values: 'center',
   * 'above', 'below', 'auto', the return value is invalid
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the offset (only valid if type is TYPE_ABSOLUTE)
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  public double getPerpendicularOffset( Feature feature ) throws FilterEvaluationException
  {

    double pValue = 0.0;

    if( perpendicularOffset != null )
    {
      String stringValue = perpendicularOffset.evaluate( feature );
      if( ( !stringValue.equals( "center" ) ) && ( !stringValue.equals( "above" ) )
          && ( !stringValue.equals( "below" ) ) && ( !stringValue.equals( "auto" ) ) )
      {
        try
        {
          pValue = Double.parseDouble( stringValue );
        }
        catch( NumberFormatException e )
        {
          throw new FilterEvaluationException( "Element 'PerpendicularOffset' "
              + "must be equal to 'center', 'above', 'below' or 'auto' or it "
              + "must denote a valid double value!" );
        }

      }
    }
    return pValue;
  }

  /**
   * @see org.deegree_impl.graphics.sld.LinePlacement_Impl#getPerpendicularOffset(Feature)
   *      <p>
   * @param perpendicularOffset
   */
  public void setPerpendicularOffset( double perpendicularOffset )
  {
    ParameterValueType pvt = StyleFactory.createParameterValueType( "" + perpendicularOffset );
    this.perpendicularOffset = pvt;
  }

  /**
   * Returns the placement type (one of the constants defined in
   * <tt>LinePlacement</tt>).
   * <p>
   * 
   * @param feature
   * @return @throws
   *         FilterEvaluationException
   */
  public int getPlacementType( Feature feature ) throws FilterEvaluationException
  {
    int type = TYPE_ABSOLUTE;

    if( perpendicularOffset != null )
    {
      String stringValue = perpendicularOffset.evaluate( feature );
      if( stringValue.equals( "center" ) )
      {
        type = TYPE_CENTER;
      }
      else if( stringValue.equals( "above" ) )
      {
        type = TYPE_ABOVE;
      }
      else if( stringValue.equals( "below" ) )
      {
        type = TYPE_BELOW;
      }
      else if( stringValue.equals( "auto" ) )
      {
        type = TYPE_AUTO;
      }
    }
    return type;
  }

  /**
   * Sets the placement type (one of the constants defined in
   * <tt>LinePlacement</tt>).
   * <p>
   * 
   * @param placementType
   */
  public void setPlacementType( int placementType )
  {
    ParameterValueType pvt = null;
    String type = null;
    if( placementType == 1 )
    {
      type = "above";
    }
    else if( placementType == 2 )
    {
      type = "below";
    }
    else if( placementType == 3 )
    {
      type = "center";
    }
    else if( placementType == 4 )
    {
      type = "auto";
    }
    pvt = StyleFactory.createParameterValueType( "" + type );
    this.perpendicularOffset = pvt;
  }

  /**
   * Provides the thickness of the styled line (needed as information for the
   * correct positioning of labels above and below the line string).
   * <p>
   * 
   * @param feature
   * @return @throws
   *         FilterEvaluationException
   */
  public double getLineWidth( Feature feature ) throws FilterEvaluationException
  {
    double width = 3;

    if( lineWidth != null )
    {
      width = Double.parseDouble( lineWidth.evaluate( feature ) );
    }
    return width;
  }

  /**
   * Provides the thickness of the styled line (needed as information for the
   * correct positioning of labels above and below the line string).
   * <p>
   * 
   * @param lineWidth
   *          the lineWidth to be set
   */
  public void setLineWidth( double lineWidth )
  {
    ParameterValueType pvt = StyleFactory.createParameterValueType( "" + lineWidth );
    this.lineWidth = pvt;
  }

  /**
   * Defines the distance between two captions on the line string. One unit is
   * the width of the label caption.
   * <p>
   * 
   * @param feature
   * @return @throws
   *         FilterEvaluationException
   */
  public int getGap( Feature feature ) throws FilterEvaluationException
  {
    int gapValue = 6;

    if( gap != null )
    {
      gapValue = Integer.parseInt( gap.evaluate( feature ) );
    }
    return gapValue;
  }

  /**
   * Defines the distance between two captions on the line string. One unit is
   * the width of the label caption.
   * <p>
   * 
   * @param gap
   *          the gap to be set
   */
  public void setGap( int gap )
  {
    ParameterValueType pvt = StyleFactory.createParameterValueType( "" + gap );
    this.gap = pvt;
  }

  /**
   * exports the content of the Font as XML formated String
   * 
   * @return xml representation of the Font
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<LinePlacement>" );
    if( perpendicularOffset != null )
    {
      sb.append( "<PerpendicularOffset>" );
      sb.append( ( (Marshallable)perpendicularOffset ).exportAsXML() );
      sb.append( "</PerpendicularOffset>" );
    }
    if( lineWidth != null )
    {
      sb.append( "<LineWidth>" );
      sb.append( ( (Marshallable)lineWidth ).exportAsXML() );
      sb.append( "</LineWidth>" );
    }
    if( gap != null )
    {
      sb.append( "<Gap>" );
      sb.append( ( (Marshallable)gap ).exportAsXML() );
      sb.append( "</Gap>" );
    }
    sb.append( "</LinePlacement>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }
}