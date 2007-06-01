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
package org.kalypsodeegree.graphics.sld;

import org.eclipse.swt.graphics.GC;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;

/**
 * This is the basis of all symbolizers. It defines the method <tt>getGeometry</tt> that's common to all symbolizers.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface Symbolizer
{

  /**
   * The ScaleDenominator-information is optional and determines whether a rule (and thus a Symbolizer) is a to be
   * applied at a certain scale.
   * 
   * @return the MinScaleDenominator
   */
  double getMinScaleDenominator( );

  /**
   * Sets the MinScaleDenominator
   * 
   * @param minScaleDenominator
   *            the MinScaleDenominator
   */
  void setMinScaleDenominator( double minScaleDenominator );

  /**
   * The ScaleDenominator-information is optional and determines whether a rule (and thus a Symbolizer) is a to be
   * applied at a certain scale.
   * 
   * @return the MaxScaleDenominator
   */
  double getMaxScaleDenominator( );

  /**
   * Sets the MaxScaleDenominator
   * 
   * @param maxScaleDenominator
   *            the MaxScaleDenominator
   */
  void setMaxScaleDenominator( double maxScaleDenominator );

  /**
   * The Geometry element is optional and if it is absent then the default geometry property of the feature type that is
   * used in the containing FeatureStyleType is used. The precise meaning of default geometry property is
   * system-dependent. Most frequently, feature types will have only a single geometry property.
   * 
   * @return the geometry of the symbolizer
   */
  Geometry getGeometry( );

  /**
   * Sets the Geometry.
   * 
   * @param geometry
   *            the geometry of the symbolizer
   */
  void setGeometry( Geometry geometry );

  public void setUom( final UOM uom );

  public UOM getUom( );

  /** Paints this symbolizer suitable for a legend. */
  public void paint( final GC gc, final Feature feature ) throws FilterEvaluationException;
}