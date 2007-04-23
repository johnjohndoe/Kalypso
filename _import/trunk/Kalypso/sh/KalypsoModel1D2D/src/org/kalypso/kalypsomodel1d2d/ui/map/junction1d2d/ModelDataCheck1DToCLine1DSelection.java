/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d;

import java.util.Collection;

import org.kalypso.kalypsomodel1d2d.ops.EdgeOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Check for 1D edge selection for building a line junction (1d element to 2d element).
 * 
 * 
 * @author Patrice Congo
 *
 */
public class ModelDataCheck1DToCLine1DSelection implements IDataModelCheck//IMessageBuilder
{
  private String message = null;
  
  private IDataModelCheck.VALIDITY_STATE  messageType;
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck#update(String, Object, KeyBasedDataModel)
   */
  public void update(String key, Object newData, KeyBasedDataModel data ) throws IllegalArgumentException
  {
    
      JunctionContextWidgetDataModel dataModel =
                   (JunctionContextWidgetDataModel) data;
      
      
      update( 
          dataModel.getSelected1D(),
          dataModel.getSelected2D(),
          dataModel.getSelected(),
          dataModel.getModel1D2D());
    
  }
  private void update(
          
          Collection<IFE1D2DEdge> selected1DEdges, 
          Collection<IFE1D2DEdge> selected2DEdges, 
          Feature[] selectedFeatures, 
          IFEDiscretisationModel1d2d model1d2d )
  {
    if(selected1DEdges == null)
    {
      
    }
    else if(selected1DEdges.size()<1)
    {
      message = "W�hlen Sie eine 1D Kante";
      messageType = IDataModelCheck.VALIDITY_STATE.INVALID;
    }
    else if(selected1DEdges.size()>1)
    {
      message = "W�hlen Sie nur eine 1D Kante";
      messageType = IDataModelCheck.VALIDITY_STATE.INVALID;
    }
    //exactely e edge if the following if statements are considered
    else if(EdgeOps.isIsolatedEdge( selected1DEdges.iterator().next() ))
    {
      message = "Gew�hlte 1D Kante is isoliert";
      messageType = IDataModelCheck.VALIDITY_STATE.ACCEPTABLE;
    }
    else
    {
      message = null;
      messageType = IDataModelCheck.VALIDITY_STATE.VALID;
    }
  }

  public String getMessage( )
  {
    return message;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck#getValidityState()
   */
  public VALIDITY_STATE getValidityState( )
  {
    return messageType;
  }

}
