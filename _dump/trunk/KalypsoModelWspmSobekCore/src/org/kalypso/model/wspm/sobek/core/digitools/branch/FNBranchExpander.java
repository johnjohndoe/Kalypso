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
package org.kalypso.model.wspm.sobek.core.digitools.branch;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranchMaker;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.pub.FNGeoUtils;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;

/**
 * @author kuch
 */
public class FNBranchExpander
{
  private enum EXPAND_TYPE
  {
    eCreateBranch,
    eInflow,
    eOutflow,
    eExtendNode,
    eConnectBranches
  }

  private IBranch[] m_branchesToExtend = null;

  private final GM_Curve m_curve;

  private final MapPanel m_panel;

  private final IModelMember m_member;

  public FNBranchExpander( final IModelMember member, final MapPanel panel, final GM_Curve curve )
  {
    m_member = member;
    m_panel = panel;
    m_curve = curve;
  }

  private EXPAND_TYPE determineExpandType( ) throws GM_Exception
  {
    /* collect all branches which intersects m_curve */
    final Map<IBranch, GM_Curve> collected = new HashMap<IBranch, GM_Curve>();

    final IBranch[] branches = m_member.getBranchMembers();

    for( final IBranch branch : branches )
    {
      final GM_Curve curve = branch.getGeometryProperty();

      if( curve.intersects( m_curve.getStartPoint() ) || curve.intersects( m_curve.getEndPoint() ) )
        collected.put( branch, curve );
    }

    final Set<Entry<IBranch, GM_Curve>> entrySet = collected.entrySet();
    if( entrySet.size() == 2 )
    {
      m_branchesToExtend = collected.keySet().toArray( new IBranch[] {} );

      return EXPAND_TYPE.eConnectBranches;
    }
    else if( entrySet.size() == 1 )
    {
      final IBranch[] keys = collected.keySet().toArray( new IBranch[] {} );
      final GM_Curve[] curves = collected.values().toArray( new GM_Curve[] {} );

      final GM_Curve curve = curves[0];

      m_branchesToExtend = keys;

      final GM_LineString myLineString = m_curve.getAsLineString();
      final GM_LineString lineString = curve.getAsLineString();

      /* eExtension? then start or end point of lineString must be an snapPoint! */
      if( FNGeoUtils.snapsOnPoint( m_panel, lineString.getStartPoint(), myLineString.getStartPoint(), FNSnapPainterExtendBranches.RADIUS ) )
        return EXPAND_TYPE.eExtendNode;
      else if( FNGeoUtils.snapsOnPoint( m_panel, lineString.getEndPoint(), myLineString.getStartPoint(), FNSnapPainterExtendBranches.RADIUS ) )
        return EXPAND_TYPE.eExtendNode;

      /* eOutflow */
      if( FNGeoUtils.snapsOnBranch( m_panel, curve, myLineString.getStartPoint(), FNSnapPainterExtendBranches.RADIUS ) )
        return EXPAND_TYPE.eOutflow;

      /* eInflow */
      if( FNGeoUtils.snapsOnBranch( m_panel, curve, myLineString.getEndPoint(), FNSnapPainterExtendBranches.RADIUS ) )
        return EXPAND_TYPE.eInflow;
    }
    return EXPAND_TYPE.eCreateBranch;
  }

  // $ANALYSIS-IGNORE
  public void finish( ) throws Exception
  {
    final EXPAND_TYPE type = determineExpandType();
    final IBranchMaker maker = m_member.getBranchMaker();

    switch( type )
    {
      case eExtendNode:
        maker.extendBranch( m_branchesToExtend[0], m_curve );
        break;

      case eOutflow:
        maker.createOutflowBranch( m_branchesToExtend[0], m_curve );
        break;

      case eInflow:
        maker.createInflowBranch( m_branchesToExtend[0], m_curve );
        break;

      case eConnectBranches:
        maker.connectBranches( m_branchesToExtend, m_curve );
        break;

      case eCreateBranch:
        maker.createBranch( m_curve );
        break;

      default:
        throw new NotImplementedException();
    }
  }

}
