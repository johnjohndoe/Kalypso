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
package org.kalypsodeegree.model.coverage;

/**
 * In the case of a grid coverage layer, the range description adds several optional elements of the generic range
 * component description, using a repeatable GridRangeDescription
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public interface GridRangeDescription
{

  /**
   * indicates how to read the image colors
   */
  String getColorInterpretation();

  /**
   * details the encoding of image samples
   */
  String getSampleEncoding();

  /**
   * A <tt>PseudoColorTable</tt> lists indexed color values, expressed as red-green-blue-alpha values, or
   * hue-lightness-saturation, cyan-magenta-yellow-black, or grayscale.
   */
  PseudoColorTable getPseudoColorTable();

  /**
   * A <tt>Histogram</tt> records image statistics (mean and median value, etc.) and lists the counts and percentages
   * of pixels in each of several brightness “bins.”
   */
  Histogram getHistogram();

  /**
   * returns the ID of the range set
   */
  String getID();

  /**
   * returns the title of the range set
   */
  String getTitle();

  /**
   * returns a short description of the range set
   */
  String getDescription();

  /**
   * The optional <tt>Observable</tt> field provides a structured description of the observations (quantities or
   * properties) reported in this range component. This description consists of a name, a free-text description, a
   * docURL (an index into a registry of observation types); and a referenceSystem that associates the reported values
   * with real-world quantities or categories.
   */
  Observable getObservable();

  /**
   * The optional RangeAxis element is for compound observations. It describes and lists the values at which the range
   * component reports properties, or the “bins” by which the range componet reports counts or other aggregate values
   */
  RangeAxis[] getRangeAxis();
}