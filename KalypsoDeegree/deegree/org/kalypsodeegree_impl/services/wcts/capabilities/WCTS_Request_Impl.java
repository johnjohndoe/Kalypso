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
package org.deegree_impl.services.wcts.capabilities;

import org.deegree.services.wcts.capabilities.ActionType;
import org.deegree.services.wcts.capabilities.WCTS_Request;

/**
 * <p>
 * This WCTS_Request distributes 4 elements, which are part of the
 * &lt;Request&gt; element: &lt;GetCapabilities&gt;, &lt;Transform&gt;,
 * &lt;IsTransformable&gt; and the optional &lt;DescribeTransformation&gt;
 * <p>
 * Everyone of these element consits the same subelements, which are combined in
 * the ActionType class.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version 2002-07-10
 */
public class WCTS_Request_Impl implements WCTS_Request
{
  private ActionType describeTransformation = null;

  private ActionType getCapabilities = null;

  private ActionType isTransformable = null;

  private ActionType transform = null;

  /**
   * constructor initializing the class with the <WCTS_Request>
   */
  WCTS_Request_Impl( ActionType getCapabilities, ActionType transform, ActionType isTransformable,
      ActionType describeTransformation )
  {
    setGetCapabilities( getCapabilities );
    setTransform( transform );
    setIsTransformable( isTransformable );
    setDescribeTransformation( describeTransformation );
  }

  /**
   * returns the mandatory &lt;GetCapabilities&gt; element
   */
  public ActionType getGetCapabilities()
  {
    return getCapabilities;
  }

  /**
   * @see getGetCapabilities
   */
  public void setGetCapabilities( ActionType getCapabilities )
  {
    this.getCapabilities = getCapabilities;
  }

  /**
   * returns the mandatory &lt;GetTransform&gt; element
   */
  public ActionType getTransform()
  {
    return transform;
  }

  /**
   * @see getTransform
   */
  public void setTransform( ActionType transform )
  {
    this.transform = transform;
  }

  /**
   * returns the mandatory &lt;IsTransformable&gt; element
   */
  public ActionType getIsTransformable()
  {
    return isTransformable;
  }

  /**
   * @see getIsTransformable
   */
  public void setIsTransformable( ActionType isTransformable )
  {
    this.isTransformable = isTransformable;
  }

  /**
   * returns the optional &lt;DescribeTransformation&gt; element
   */
  public ActionType getDescribeTransformation()
  {
    return describeTransformation;
  }

  /**
   * @see getDescribeTransformation
   */
  public void setDescribeTransformation( ActionType describeTransformation )
  {
    this.describeTransformation = describeTransformation;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "getCapabilities = " + getCapabilities + "\n";
    ret += ( "transform = " + transform + "\n" );
    ret += ( "isTransformable = " + isTransformable + "\n" );
    ret += ( "describeTransformation = " + describeTransformation + "\n" );
    return ret;
  }
}