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
package org.kalypso.model.wspm.sobek.core.interfaces;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.model.BoundaryNode;

/**
 * @author kuch
 */
public interface IBoundaryNode extends IConnectionNode
{
  public enum BOUNDARY_TYPE
  {
    eW,
    eQ,
    eWQ;

    public static BOUNDARY_TYPE getType( final BoundaryNode node )
    {
      final String type = (String) node.getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_TYPE );

      // $ANALYSIS-IGNORE
      if( BOUNDARY_TYPE.eW.toString().equals( type ) )
        return BOUNDARY_TYPE.eW;
      else if( BOUNDARY_TYPE.eQ.toString().equals( type ) )
        return BOUNDARY_TYPE.eQ;
      else if( BOUNDARY_TYPE.eWQ.toString().equals( type ) )
        return BOUNDARY_TYPE.eWQ;

      throw new IllegalStateException( Messages.IBoundaryNode_0 + type );
    }

    /**
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString( )
    {
      final BOUNDARY_TYPE type = BOUNDARY_TYPE.valueOf( name() );
      switch( type )
      {
        case eW:
          return "bc_w"; //$NON-NLS-1$
        case eQ:
          return "bc_q"; //$NON-NLS-1$
        case eWQ:
          return "bc_wq"; //$NON-NLS-1$

        default:
          throw new IllegalStateException( Messages.IBoundaryNode_4 + name() );
      }
    }

    public String toZmlString( )
    {
      final BOUNDARY_TYPE type = BOUNDARY_TYPE.valueOf( name() );
      switch( type )
      {
        case eW:
          return "W"; //$NON-NLS-1$
        case eQ:
          return "Q"; //$NON-NLS-1$
        case eWQ:
          return "WQ";//$NON-NLS-1$

        default:
          throw new NotImplementedException();
      }
    }
  }

  public BOUNDARY_TYPE getBoundaryType( );

  public IBoundaryNodeLastfallCondition getLastfallCondition( ILastfall lastfall ) throws Exception;
}
