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
package org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d;

import java.util.Collection;

import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Composite check for the wohle selction base of checks for
 * 1d and 2d. 
 * This composite check gives a pririoty to to the 1d check.
 *  
 * @author Patrice Congo
 *
 */
public class ModelDataCompositeCheck1D2D implements IDataModelCheck//IMessageBuilder
{
  /**
   * checker for 1d selection
   */
  private IDataModelCheck  check1D;

  /**
   * checker for 2d selection
   */
  private IDataModelCheck check2D;

  /**
   * the current message corresponding to the current validity state 
   */
  private String message = null;
  
  /**
   * the validity state of this check
   */
  private IDataModelCheck.VALIDITY_STATE  messageType;
  
  /**
   * creates a composite check based on these 2 checks
   */
  public ModelDataCompositeCheck1D2D( 
                          IDataModelCheck check1D,
                          IDataModelCheck check2D)
  {
    Assert.throwIAEOnNullParam( check1D, "check1D" );
    Assert.throwIAEOnNullParam( check2D, "check2D" );
    this.check1D = check1D;
    this.check2D = check2D;
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck#update(String, Object, KeyBasedDataModel)
   */
  public void update(String key, Object newData, KeyBasedDataModel data ) throws IllegalArgumentException
  {
    JunctionContextWidgetDataModel dataModel =
                 (JunctionContextWidgetDataModel) data;
    check1D.update( key, newData, data );
    if( check1D.getValidityState()!=VALIDITY_STATE.VALID )
    {
      message = check1D.getMessage();
      messageType = check1D.getValidityState();
      return;
    }
    
    check2D.update( key, newData, data );
    if(check2D.getValidityState()!= VALIDITY_STATE.VALID)
    {
      message = check2D.getMessage();
      messageType = check2D.getValidityState();
      return;
    }
    
    update( 
        dataModel.getSelected1D(),
        dataModel.getSelected2D(),
        dataModel.getSelected(),
        dataModel.getModel1D2D());
  }
  
  /**
   * check update for the all selection
   */
  private void update( 
          Collection<IFE1D2DEdge> selected1DEdges, 
          Collection<IFE1D2DEdge> selected2DEdges, 
          Feature[] selectedFeatures, 
          IFEDiscretisationModel1d2d model1d2d )
  {
    if(selected1DEdges.size()+selected2DEdges.size()!=selectedFeatures.length)
    {
      message = "Nur 1d und 2d Kanten w‰hlen";
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
