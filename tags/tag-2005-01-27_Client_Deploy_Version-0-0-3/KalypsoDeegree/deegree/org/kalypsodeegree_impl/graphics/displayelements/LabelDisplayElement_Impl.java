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
package org.deegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;

import org.deegree.graphics.displayelements.Label;
import org.deegree.graphics.displayelements.LabelDisplayElement;
import org.deegree.graphics.sld.ParameterValueType;
import org.deegree.graphics.sld.TextSymbolizer;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Object;

/**
 * <tt>DisplayElement</tt> that encapsulates a <tt>GM_Object</tt>
 * (geometry), a <tt>ParameterValueType</tt> (caption) and a
 * <tt>TextSymbolizer</tt> (style).
 * <p>
 * The graphical (say: screen) representations of this <tt>DisplayElement</tt>
 * are <tt>Label</tt> -instances. These are generated either when the
 * <tt>paint</tt> -method is called or assigned externally using the
 * <tt>setLabels</tt>- or <tt>addLabels</tt> -methods.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class LabelDisplayElement_Impl extends GeometryDisplayElement_Impl implements
    LabelDisplayElement, Serializable
{

  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -7870967255670858503L;

  private ParameterValueType label = null;

  // null means that the labels have to be created inside the paint-method
  // (and have not been set externally)
  private ArrayList labels = null;

  /**
   * Creates a new LabelDisplayElement_Impl object.
   * <p>
   * 
   * @param feature
   *          associated <tt>Feature</tt>
   * @param geometry
   *          associated <tt>GM_Object</tt>
   * @param symbolizer
   *          associated <tt>TextSymbolizer</tt>
   */
  LabelDisplayElement_Impl( Feature feature, GM_Object geometry, TextSymbolizer symbolizer )
  {
    super( feature, geometry, symbolizer );
    setLabel( symbolizer.getLabel() );
  }

  /**
   * Sets the caption of the label.
   */
  public void setLabel( ParameterValueType label )
  {
    this.label = label;
  }

  /**
   * Returns the caption of the label as <tt>ParameterValueType<tt>.
   */
  public ParameterValueType getLabel()
  {
    return label;
  }

  /**
   * Renders the <tt>DisplayElement</tt> to the submitted graphic context. If
   * the <tt>Label</tt> -represenations have been assigned externally, these
   * labels are used, else <tt>Label</tt> -instances are created automatically
   * using the <tt>LabelFactory</tt>.
   * <p>
   * 
   * @param g
   *          <tt>Graphics</tt> context to be used
   * @param projection
   *          <tt>GeoTransform</tt> to be used
   */
  public void paint( Graphics g, GeoTransform projection )
  {

    if( label == null )
      return;
    Graphics2D g2D = (Graphics2D)g;

    if( labels == null )
    {
      try
      {
        setLabels( LabelFactory.createLabels( this, projection, g2D ) );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }

    // paint all labels
    Iterator it = labels.iterator();
    while( it.hasNext() )
    {
      ( (Label)it.next() ).paint( g2D );
    }

    // mark the labels as unset (for the next paint-call)
    labels = null;
  }

  /**
   * Returns whether the <tt>DisplayElement</tt> should be painted at the
   * current scale or not.
   */
  public boolean doesScaleConstraintApply( double scale )
  {
    return ( symbolizer.getMinScaleDenominator() <= scale )
        && ( symbolizer.getMaxScaleDenominator() > scale );
  }

  /**
   * Removes all <tt>Label<tt> representations for this
   * <tt>LabelDisplayElement</tt>.
   */
  public void clearLabels()
  {
    labels = null;
  }

  /**
   * Adds a <tt>Label<tt> representation that is to be considered when the
   * <tt>LabelDisplayElement</tt> is painted to the view.
   */
  public void addLabel( Label label )
  {
    if( labels == null )
    {
      labels = new ArrayList( 100 );
    }
    labels.add( label );
  }

  /**
   * Adds <tt>Label<tt> representations that are to be considered when the
   * <tt>LabelDisplayElement</tt> is painted to the view.
   */
  public void addLabels( Label[] labels )
  {
    if( this.labels == null )
    {
      this.labels = new ArrayList( 100 );
    }
    for( int i = 0; i < labels.length; i++ )
    {
      this.labels.add( labels[i] );
    }
  }

  /**
   * Sets the <tt>Label<tt> representations that are to be considered when
   * the <tt>LabelDisplayElement</tt> is painted to the view.
   */
  public void setLabels( Label[] labels )
  {
    this.labels = new ArrayList( 100 );
    for( int i = 0; i < labels.length; i++ )
    {
      this.labels.add( labels[i] );
    }
  }
}