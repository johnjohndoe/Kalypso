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
import java.util.ArrayList;

/**
 * Allows the chaining of <tt>Optimizer<tt>s. Does implement the
 * <tt>Optimizer<tt>-interface as well. 
 * <p>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
public class OptimizerChain extends AbstractOptimizer
{

  // stores the Optimizers
  private ArrayList optimizers = new ArrayList();

  /**
   * Constructs a new empty <tt>OptimizerChain</tt>
   */
  public OptimizerChain()
  {}

  /**
   * Constructs a new <tt>OptimizerChain</tt> that contains the submitted
   * <tt>Optimizer</tt> instances.
   * <p>
   * 
   * @param optimizers
   */
  public OptimizerChain( AbstractOptimizer[] optimizers )
  {
    for( int i = 0; i < optimizers.length; i++ )
    {
      this.optimizers.add( optimizers[i] );
    }
  }

  /**
   * Appends an <tt>Optimizer</tt> to the end of the processing chain.
   * <p>
   * 
   * @param optimizer
   */
  public void addOptimizer( AbstractOptimizer optimizer )
  {
    optimizers.add( optimizer );
  }

  /**
   * Performs the optimization. Calls the optimize-method of all contained
   * <tt>Optimizer</tt> instances subsequently.
   * <p>
   * 
   * @param g
   */
  public void optimize( Graphics2D g )
  {
    for( int i = 0; i < optimizers.size(); i++ )
    {
      ( (AbstractOptimizer)optimizers.get( i ) ).optimize( g );
    }
  }
}