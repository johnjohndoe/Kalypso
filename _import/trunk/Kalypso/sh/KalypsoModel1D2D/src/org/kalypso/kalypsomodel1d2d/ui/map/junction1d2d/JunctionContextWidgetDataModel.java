/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Daten model für junction context widgets
 * 
 * @author Patrice Congo
 */
public class JunctionContextWidgetDataModel extends KeyBasedDataModel
{
  public static final String SELECTED_ELEMENT1D = "_SELECTED_ELE_1D";
  
  public static final String SELECTED_ELEMENT2D = "_SELECTED_ELE_2D";
  
  public static final String SELECTED_ELEMENTS = "_SELECTED_ELE_";
  
  public static final String MESSAGE ="_MESSAGE_";
  
  public static final String MESSAGE_BUILDER ="_MESSAGE_BUILDER_";

  public static final String CREATE_MODEL_PART = "_create_model_part_";
  
  public static final String MODEL1D2D = "_model_1d2d_";
  
  public JunctionContextWidgetDataModel(IDataModelCheck messageBuilder )
  {
    super(
        new String[]{ 
              SELECTED_ELEMENTS, SELECTED_ELEMENT1D, 
              SELECTED_ELEMENT2D, MESSAGE, MESSAGE_BUILDER,
              CREATE_MODEL_PART},
        messageBuilder);
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel#setData(java.lang.String, java.lang.Object)
   */
  @Override
  public void setData( String key, Object newEntry )
  {
    if( SELECTED_ELEMENT1D.equals( key ) ||
        SELECTED_ELEMENT2D.equals( key ))
    {
      return ;
    }
    else if(SELECTED_ELEMENTS.equals( key ))
    {
      Collection<IFE1D2DEdge> selected1DEdges = ModelOps.collectAll1DEdges((Feature[]) newEntry );
      setData(
          SELECTED_ELEMENT1D,
          selected1DEdges,
          false);
      Collection<IFE1D2DEdge> selected2DEdges = ModelOps.collectAll2DEdges((Feature[]) newEntry );
      setData(
          SELECTED_ELEMENT2D,
          selected2DEdges,
          false);
//      final IDataModelCheck messageBuilder = getModelCheck();//getMessageBuilder();
//      if(messageBuilder !=null)
//      {
//            messageBuilder.update( 
//                      selected1DEdges, 
//                      selected2DEdges, 
//                      (Feature[])newEntry,//selectedFeatures, 
//                      getModel1D2D()//model1d2d 
//                      );            
//      }
      super.setData(key, newEntry);
    }
    else
    {
      super.setData(key, newEntry);
    }
  }
  
  
  public String getMessage()
  {
   IDataModelCheck messageBuilder = getModelCheck();
   if(messageBuilder == null )
   {
     return null;
   }
   else
   {
     return messageBuilder.getMessage();
   }
  }
  public IFEDiscretisationModel1d2d getModel1D2D( )
  {
    return (IFEDiscretisationModel1d2d) getData( MODEL1D2D );
  }

  public Collection<IFE1D2DEdge> getSelected1D()
  {
    return (Collection<IFE1D2DEdge>) getData( SELECTED_ELEMENT1D ); 
  }
  
  public Collection<IFE1D2DEdge> getSelected2D()
  {
    return (Collection<IFE1D2DEdge>) getData( SELECTED_ELEMENT2D ); 
  }
  
  public Feature[] getSelected()
  {
    return (Feature[]) getData( SELECTED_ELEMENTS ); 
  }
  
  public void setSelected(Feature[] selected)
  {
    setData( SELECTED_ELEMENTS, selected ); 
  }
  
  public Double getDistance1D2D()
  {
    return null;
  }

  public IDataModelCommand getCreateModelPart( )
  {
    return (IDataModelCommand) getData( CREATE_MODEL_PART );
  }

  public void resetSelections( )
  {
    setData( SELECTED_ELEMENTS, new Feature[]{} );
    
  }
  
  

}
