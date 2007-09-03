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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
/**
 * Command to delete a junction context.
 * This command is not undoable
 * 
 * @author Patrice Congo
 *
 */
public class DeleteJunctionContextCmd implements IDiscrModel1d2dChangeCommand
{

  private final ITransitionElement junctionContext;
  
  private final IFEDiscretisationModel1d2d model1d2d;
  
  private boolean done;
  
  private IElement1D deletedElement1D;
  
  private IFELine contiLine;
  
  /**
   * If the deleted context is a {@link IJunctionContext1DTo2D}
   */
  private IElement2D delElement2D;

  /**
   * Creates a command to delete the given junction context.
   * The constructor requires the juntion contex to have
   * a parent of type discretisation model 1d2d
   * 
   * @param junctionContext the junction context to delete
   * @throws IllegalArgumentException if junction context is null
   *            or its parent is null or not adatable to
   *            {@link IFEDiscretisationModel1d2d}
   */
  public DeleteJunctionContextCmd(
              ITransitionElement junctionContext )
  {
    Assert.throwIAEOnNullParam( junctionContext, "junctionContext" );
    this.junctionContext = junctionContext;
    final Feature wrappedFeature = 
          junctionContext.getWrappedFeature();
    Feature modelFeature = wrappedFeature.getParent();
    if( modelFeature == null )
    {
      final String message = 
        "junction context does not have a parent feature";
      throw new IllegalArgumentException(message);
    }
    this.model1d2d =(IFEDiscretisationModel1d2d)
      modelFeature.getAdapter( IFEDiscretisationModel1d2d.class );
    if( this.model1d2d == null )
    {
      String message = 
        String.format(
            "parent feature could not be adapted to "+
              "IFEDiscretisationModel1d2d"+
              "\n\t parentFeature=%s"+
              "\n\t adapted=%s",
              modelFeature,
              model1d2d );
      throw new IllegalArgumentException(message);
    }
  }
  
  /**
   * creates a new junction context delete command to delete 
   * the given junction passed as feature.
   * 
   * @param model1d2d the model containing the junction feature
   * @param feature  the junction context feature
   */
  public DeleteJunctionContextCmd(
              IFEDiscretisationModel1d2d model1d2d,
              Feature feature )
  {
      Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
      Assert.throwIAEOnNullParam( feature, "feature" );
      
      this.model1d2d = model1d2d;
      
      this.junctionContext = (ITransitionElement)
              feature.getAdapter( ITransitionElement.class );
      if( junctionContext == null )
      {
        throw new IllegalArgumentException(
            "Could not adapt feature to IJunctionContext1DToCLine ");
      }
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if( done )
    {
      final List<IFeatureWrapper2> wrappers =
        new ArrayList<IFeatureWrapper2>(16);
      wrappers.add( model1d2d );
      if(delElement2D!=null)
      {
        wrappers.add( delElement2D );        
      }
      if( deletedElement1D!=null )
      {
        wrappers.add( deletedElement1D );
      }
      return wrappers.toArray( new IFeatureWrapper2[]{} );
    }
    else
    {
      return new IFeatureWrapper2[]{ };
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return null;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return null;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
//    if(!done)
//    {
//      //delete link to 1d element
//      deletedElement1D = junctionContext.getElement1D();
//      if(deletedElement1D!=null)
//      {
//        LinksOps.delRelationshipElementAndComplexElement(
//                          deletedElement1D, junctionContext );
//      }
//      
//      //delete link to context and contiline
//      contiLine = junctionContext.getContinuityLine();
//      if( contiLine!= null )
//      {
//        LinksOps.delRelationshipElementAndComplexElement( 
//            contiLine, junctionContext );
//        try
//        {
//          IDiscrModel1d2dChangeCommand deleContiLine = DeleteCmdFactory.createDeleteCmd( 
//              contiLine.getWrappedFeature(), 
//              model1d2d );
//          deleContiLine.process();
//        }
//        catch( Throwable th )
//        {
//          th.printStackTrace();
//        }
//      }
//      
//      //delete link to 2d element if a
//      if( junctionContext instanceof IJunctionContext1DTo2D )
//      {
//        delElement2D = 
//          ((IJunctionContext1DTo2D)junctionContext).getElement2D();
//        if( delElement2D != null )
//        {
//          LinksOps.delRelationshipElementAndComplexElement( 
//              delElement2D, junctionContext );
//        }
//      }
//      
//    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    
  }

}
