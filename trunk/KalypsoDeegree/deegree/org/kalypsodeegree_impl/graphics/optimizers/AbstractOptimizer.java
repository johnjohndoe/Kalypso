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

import java.awt.Graphics2D;
import java.util.HashSet;
import java.util.Set;

import org.deegree.graphics.MapView;
import org.deegree.graphics.Theme;
import org.deegree.graphics.optimizers.Optimizer;

/**
 * This is the abstract base class for <tt>Optimizer</tt> s that need to alter
 * the contents of <tt>Theme<tt>s before the parent <tt>MapView<tt> object is
 * painted. For example, the placements of <tt>LabelDisplayElement</tt>s
 * in a <tt>Theme</tt> may be optimized to minimize overlapping using the
 * <tt>LabelOptimizer</tt>.
 * <p>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
public abstract class AbstractOptimizer implements Optimizer
{

  // contains all registered Themes
  protected Set themes = new HashSet();

  // associated MapView-instance
  protected MapView mapView;

  /**
   * Sets the associated <tt>MapView</tt> -instance. Is needed for the scale
   * and projection information. Called by the <tt>MapView</tt>.
   * 
   * @param mapView
   */
  public void setMapView( MapView mapView )
  {
    this.mapView = mapView;
  }

  /**
   * Adds a <tt>Theme<tt> that the <tt>Optimizer<tt> should consider.
   * @param theme
   */
  public void addTheme( Theme theme )
  {
    themes.add( theme );
  }

  /**
   * Invokes the optimization process. The <tt>Optimizer<tt> will now
   * process and modify the contents of the attached <tt>Theme<tt>s.
   * @param g
   */
  public abstract void optimize( Graphics2D g );
}