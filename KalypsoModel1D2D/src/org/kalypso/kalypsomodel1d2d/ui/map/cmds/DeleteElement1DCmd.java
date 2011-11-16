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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Command for deleting one 1D element. The change event has to fired from outside!
 * 
 * @author Patrice Congo
 * @author ig, barbarins
 */
@SuppressWarnings("unchecked")
public class DeleteElement1DCmd implements IDiscrModel1d2dChangeCommand
{
  private List<IFE1D2DComplexElement> complexElements;

  private List<Feature> m_listAffectedFeatures;

  private final IFEDiscretisationModel1d2d m_model1d2d;

  private Set<Feature> m_setFeatureToRemove = null;

  public DeleteElement1DCmd( final IFEDiscretisationModel1d2d model1d2d, final Feature pFeature )
  {
    m_model1d2d = model1d2d;
    m_listAffectedFeatures = new ArrayList<Feature>();
    m_setFeatureToRemove = new HashSet<Feature>();
    addElementToRemove( pFeature );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteElement1DCmd.0" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    Set<Feature> lSetEdges = new HashSet<Feature>();
    Set<Feature> lSet1DElements = new HashSet<Feature>();

    final RemoveEdgeWithoutContainerOrInvCmd lCmdEdgeRemove = new RemoveEdgeWithoutContainerOrInvCmd( m_model1d2d, null );

    for( final Feature lFeature : m_setFeatureToRemove )
    {
      IElement1D lElement = (IElement1D) lFeature.getAdapter( IElement1D.class );
      final String elementID = lElement.getGmlID();
      lSet1DElements.add( lFeature );
      m_listAffectedFeatures.add( lFeature );
      complexElements = lElement.getContainers();
      for( final IFE1D2DComplexElement complexElement : complexElements )
      {
        complexElement.getElements().remove( elementID );
        m_listAffectedFeatures.add( complexElement.getFeature() );
      }

      final IFE1D2DEdge edge = lElement.getEdge();

      final IFeatureWrapperCollection containers = edge.getContainers();
      boolean isRemoved = containers.remove( elementID );

      final IFeatureWrapperCollection nodes = edge.getNodes();
      for( Iterator iterator = nodes.iterator(); iterator.hasNext(); )
      {
        IFeatureWrapper2 featureWrapper = (IFeatureWrapper2) iterator.next();
        Feature wrappedFeature = featureWrapper.getFeature();
        m_listAffectedFeatures.add( wrappedFeature );
      }

      if( !isRemoved )
      {
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteElement1DCmd.1" ) ); //$NON-NLS-1$
      }
      lSetEdges.add( edge.getFeature() );

      m_listAffectedFeatures.add( edge.getFeature() );

      lCmdEdgeRemove.addEdgeToRemove( edge );
    }
    lCmdEdgeRemove.process();
    m_model1d2d.getElements().removeAllAtOnce( lSetEdges );
    m_model1d2d.getElements().removeAllAtOnce( lSet1DElements );
  }

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

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return null;
    // return new IFeatureWrapper2[] { m_element1D };
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return m_model1d2d;
  }

  public List<Feature> getChangedFeatureList( )
  {
    return m_listAffectedFeatures;
  }

  public void addElementToRemove( Feature pFeature )
  {
    if( pFeature != null )
      m_setFeatureToRemove.add( pFeature );
  }

}
