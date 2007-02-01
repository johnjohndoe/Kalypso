/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.conv;

import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;

/**
 * An handler that convert the rma10s element events into
 * discretisation model elements and links
 * 
 * @author Patrice Congo
 *
 */
public class DiscretisationModel1d2dHandler implements IRMA10SModelElementHandler
{
  /**
   * The model to fill with the parsed fe element from 
   */
  private IFEDiscretisationModel1d2d model;
  
  public DiscretisationModel1d2dHandler(IFEDiscretisationModel1d2d model )
  {
    this.model=model;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
  public void end( )
  {
    
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int, int, int)
   */
  public void handleArc( String lineString, int id, int node1ID, int node2ID, int elementLeftID, int elementRightID, int middleNodeID )
  {
    
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int, int)
   */
  public void handleElement( String lineString, int id, int currentRougthnessClassID, int previousRoughnessClassID, int eleminationNumber )
  {
    
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double, double)
   */
  public void handleNode( String lineString, int id, double easting, double northing, double elevation )
  {
  
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String, org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  public void handlerError( String lineString, EReadError errorHints )
  {
    throw new RuntimeException("bad line="+lineString);
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  public void handlerUnIdentifyable( String lineString )
  {
    
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  public void start( )
  {
    
  }

}
