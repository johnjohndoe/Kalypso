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

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.ops.ContinuityLineOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ILineElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Undoable route Line element command
 * 
 * @author Patrice Congo
 */
public class RouteLineElementCommand implements IDiscrModel1d2dChangeCommand
{
  private ILineElement m_addedElement;

  final private IFEDiscretisationModel1d2d m_model;

  final private GM_Object m_routeGeometry;

  final private Class< ? extends ILineElement> m_adapterTargetClass;

  final private QName m_lineElementQName;

  /**
   * @param model
   * @param elementEdgeCmds
   *            an array the command used to create the edges of the element to be created by this command. the array
   *            must contains only {@link AddEdgeCommand} and {@link AddEdgeInvCommand} commands
   */
  public RouteLineElementCommand( final IFEDiscretisationModel1d2d model, final GM_Object routeGeometry, final Class< ? extends ILineElement> adapterTargetClass, final QName lineElementQName )
  {
    m_model = model;
    m_routeGeometry = routeGeometry;
    m_adapterTargetClass = adapterTargetClass;
    m_lineElementQName = lineElementQName;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Add FE element";
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    if( m_addedElement == null )
    {
      if( m_lineElementQName.equals( IBoundaryLine.QNAME ) )
        m_addedElement = ContinuityLineOps.lineElementFromCurve( m_lineElementQName, m_adapterTargetClass, (GM_Curve) m_routeGeometry, m_model );
      else
        m_addedElement = ContinuityLineOps.boundaryLine1DFromPoint( m_lineElementQName, m_adapterTargetClass, (GM_Point) m_routeGeometry, m_model );
      m_addedElement.getWrappedFeature().invalidEnvelope();

      final Feature parentFeature = m_model.getWrappedFeature();
      GMLWorkspace workspace = parentFeature.getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature, m_addedElement.getWrappedFeature(), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if( m_addedElement == null )
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if( m_addedElement != null )
    {
      // TODO remove element and links to it edges and delete element
    }
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[] { m_addedElement };
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return m_model;
  }
}
