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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement.TRANSITION_TYPE;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

public class CreateTransitionElementCommand implements IDiscrModel1d2dChangeCommand
{
  private ITransitionElement m_transitionElement;

  private final IFEDiscretisationModel1d2d m_model;

  private final IContinuityLine1D m_line1D;

  private final IContinuityLine2D m_line2D;

  private final TRANSITION_TYPE m_transitionType;

  public CreateTransitionElementCommand( final IFEDiscretisationModel1d2d model, final IContinuityLine1D line1D, final IContinuityLine2D line2D, final ITransitionElement.TRANSITION_TYPE transitionType ) throws IllegalArgumentException
  {
    m_model = model;
    m_line1D = line1D;
    m_line2D = line2D;
    m_transitionType = transitionType;
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
    if( m_transitionElement == null )
    {
      try
      {
        m_transitionElement = m_model.getComplexElements().addNew( ITransitionElement.QNAME, ITransitionElement.class );
        m_transitionElement.addElementAsRef( m_line1D );
        m_transitionElement.addElementAsRef( m_line2D );
        m_transitionElement.setTransitionType( m_transitionType );
        final Feature feature = m_transitionElement.getFeature();
        feature.invalidEnvelope();
        final GMLWorkspace workspace = feature.getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, feature.getParent(), feature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        throw e;
      }
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    if( m_transitionElement == null )
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    if( m_transitionElement != null )
    {
      // TODO remove element and links to it edges
    }
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[] { m_transitionElement };
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return m_model;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.CreateTransitionElementCommand.0"); //$NON-NLS-1$
  }
}
