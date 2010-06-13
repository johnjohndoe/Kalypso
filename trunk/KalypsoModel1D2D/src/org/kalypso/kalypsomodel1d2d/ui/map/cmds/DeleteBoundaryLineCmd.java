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

import org.kalypso.kalypsomodel1d2d.ops.LinksOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Command For deleting a boundary line. It does not support undo
 * 
 * @author Patrice Congo
 */
@SuppressWarnings("unchecked")
public class DeleteBoundaryLineCmd implements IDiscrModel1d2dChangeCommand
{

  /**
   * The model on which this command is working
   */
  final private IFEDiscretisationModel1d2d model1d2d;

  /**
   * The element to delete
   */
  final private IFELine bLine;

  private boolean done;

  public DeleteBoundaryLineCmd( final IFEDiscretisationModel1d2d model1d2d, final Feature bFeature )
  {
    this( model1d2d, (IFELine) bFeature.getAdapter( IFELine.class ) );
    // Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
    //    
    // this.model1d2d = model1d2d;
    // this.bLine = (IBoundaryLine) bFeature.getAdapter( IBoundaryLine.class );
    // if(this.bLine == null )
    // {
    // throw new IllegalArgumentException(
    // "Could not adapt the feature "+bFeature+" IBoundaryLine");
    // }
    // this.done = false;
  }

  public DeleteBoundaryLineCmd( final IFEDiscretisationModel1d2d model1d2d, final IFELine bLine )
  {
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( bLine, "bLine" ); //$NON-NLS-1$
    // if( TypeInfo.isJunctionContextLine( bLine ) )
    // {
    // throw new IllegalArgumentException( "Linien element ist in einem Komplungskontext und " + "kann daher nicht
    // entfernt werden" );
    // }
    this.model1d2d = model1d2d;
    this.bLine = bLine;
    this.done = false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteBoundaryLineCmd.2"); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    try
    {
      if( !done )
      {
        // unlinkEdges( bLine, model1d2d );
        // unlinkComplexElement( bLine );
        // boolean removed = model1d2d.getElements().remove( bLine );
        done = true;
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }

  private void unlinkComplexElement( IFELine line )
  {
    IFeatureWrapperCollection<IFE1D2DComplexElement> containers2 = line.getContainers();
    IFE1D2DComplexElement[] containers = containers2.toArray( new IFE1D2DComplexElement[] {} );
    for( IFE1D2DComplexElement ce : containers )
    {
      LinksOps.delRelationshipElementAndComplexElement( line, ce );
    }

  }

  // private static final void unlinkComplexElementAndEle( IFE1D2DComplexElement ce, IBoundaryLine line )
  // {
  // ce.getElements().removeAllRefs( line );
  // }
  //  
  // private static final void unlinkCalUnitBorder( ICalculationUnit ce, IBoundaryLine line )
  // {
  // ce.getElements().removeAllRefs( line );
  // final IBoundaryLine downStreamBL = ce.getDownStreamBoundaryLine();
  // if( downStreamBL!=null )
  // {
  // if(downStreamBL.equals( line ))
  // {
  // ce.setDownStreamBoundaryLine( null );
  // }
  // }
  //    
  // final IBoundaryLine upStreamBL = ce.getUpStreamBoundaryLine();
  // if( upStreamBL!=null )
  // {
  // if(upStreamBL.equals( line ))
  // {
  // ce.setDownStreamBoundaryLine( null );
  // }
  // }
  //    
  // }

  // private static final void unlinkEdges( ILineElement line, IFEDiscretisationModel1d2d model1d2d )
  // {
  // final IFeatureWrapperCollection<IFE1D2DEdge> edges = line.getEdges();
  // final IFeatureWrapperCollection<IFE1D2DEdge> model1d2dEdges = model1d2d.getEdges();
  // final IFeatureWrapperCollection<IFE1D2DNode> model1d2dNodes = model1d2d.getNodes();
  //
  // for( int i = edges.size() - 1; i >= 0; i-- )
  // {
  // final IFE1D2DEdge edge = edges.remove( i );
  // final IFeatureWrapperCollection edgeContainers = edge.getContainers();
  // edgeContainers.removeAllRefs( line );
  //
  // // if the element have been deleted before the line element
  // // and its the line element is was the last edge container
  // if( edgeContainers.isEmpty() )
  // {
  // final IFE1D2DEdge invertedEdge;
  // if( edge instanceof IEdgeInv )
  // {
  // invertedEdge = ((IEdgeInv) edge).getInverted();
  // invertedEdge.resetInvEdge();
  // model1d2dEdges.remove( edge );
  // }
  // else
  // {
  // invertedEdge = edge;
  // }
  // if( invertedEdge.getContainers().isEmpty() )
  // {
  // IFE1D2DNode[] nodes = (IFE1D2DNode[]) invertedEdge.getNodes().toArray( new IFE1D2DNode[] {} );
  // model1d2dEdges.remove( invertedEdge );
  // for( IFE1D2DNode node : nodes )
  // {
  // model1d2dNodes.remove( node );
  // }
  // }
  // }
  // }
  // }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {

  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    throw new UnsupportedOperationException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteBoundaryLineCmd.3") ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[] { bLine };
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model1d2d;
  }

}
