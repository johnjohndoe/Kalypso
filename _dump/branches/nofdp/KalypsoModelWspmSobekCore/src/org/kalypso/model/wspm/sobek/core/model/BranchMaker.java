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
package org.kalypso.model.wspm.sobek.core.model;

import java.util.HashSet;
import java.util.Set;

import org.kalypso.model.wspm.sobek.core.SobekModelMember;
import org.kalypso.model.wspm.sobek.core.i18n.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranchMaker;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.INode.TYPE;
import org.kalypso.model.wspm.sobek.core.utils.FNGmlUtils;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * @author kuch
 */
public class BranchMaker implements IBranchMaker
{

  private final SobekModelMember m_model;

  public BranchMaker( final SobekModelMember model )
  {
    m_model = model;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranchMaker#connectBranches(org.kalypso.model.wspm.sobek.core.interfaces.IBranch[],
   *      org.kalypsodeegree.model.geometry.GM_Curve)
   */
  public void connectBranches( final IBranch[] branches, final GM_Curve curve ) throws Exception
  {
    if( branches.length != 2 )
      throw new IllegalStateException( Messages.BranchMaker_0 );

    FNGmlUtils.connectBranches( m_model, branches, curve );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranchMaker#createBranch(org.kalypsodeegree.model.geometry.GM_Curve)
   */
  public void createBranch( final GM_Curve curve ) throws Exception
  {
    final Set<IConnectionNode> nodes = new HashSet<IConnectionNode>();
    for( final INode node : m_model.getNodeMembers() )
    {
      if( node instanceof IConnectionNode )
        nodes.add( (IConnectionNode) node );
    }

    FNGmlUtils.createBranch( m_model, curve, nodes.toArray( new INode[] {} ), TYPE.eConnectionNode, TYPE.eConnectionNode );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranchMaker#createInflowBranch(org.kalypso.model.wspm.sobek.core.interfaces.IBranch,
   *      org.kalypsodeegree.model.geometry.GM_Curve)
   */
  public void createInflowBranch( final IBranch branch, final GM_Curve curve ) throws Exception
  {
    FNGmlUtils.createInflowBranch( m_model, branch, curve );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranchMaker#createOutflowBranch(org.kalypso.model.wspm.sobek.core.interfaces.IBranch,
   *      org.kalypsodeegree.model.geometry.GM_Curve)
   */
  public void createOutflowBranch( final IBranch branch, final GM_Curve curve ) throws Exception
  {
    FNGmlUtils.createOutflowBranch( m_model, branch, curve );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.IBranchMaker#extendBranch(org.kalypso.model.wspm.sobek.core.interfaces.IBranch,
   *      org.kalypsodeegree.model.geometry.GM_Curve)
   */
  public void extendBranch( final IBranch branch, final GM_Curve curve ) throws Exception
  {
    FNGmlUtils.extendBranch( m_model, branch, curve );
  }
}
