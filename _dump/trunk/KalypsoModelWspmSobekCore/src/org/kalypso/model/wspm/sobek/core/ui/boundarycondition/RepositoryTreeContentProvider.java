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
package org.kalypso.model.wspm.sobek.core.ui.boundarycondition;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.model.wspm.sobek.core.wizard.pages.IBoundaryConditionGeneral;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.timeseries.wq.WQTimeserieProxy;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationItem;
import org.kalypso.repository.IRepositoryItem;

/**
 * @author kuch
 */
public class RepositoryTreeContentProvider extends org.kalypso.ui.repository.view.RepositoryTreeContentProvider
{

  private final IBoundaryConditionGeneral m_settings;

  public RepositoryTreeContentProvider( final IBoundaryConditionGeneral settings )
  {
    m_settings = settings;
  }

  /**
   * @see org.kalypso.ui.repository.view.RepositoryTreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object parentElement )
  {
    final List<Object> myChildren = new ArrayList<Object>();

    final Object[] children = super.getChildren( parentElement );
    for( final Object object : children )
      if( object instanceof ZmlObservationItem )
      {
        final ZmlObservationItem item = (ZmlObservationItem) object;
        final Object adapter = item.getAdapter( IObservation.class );

        if( adapter instanceof WQTimeserieProxy )
        {
          // WQTimeseries have both defined! W and Q!!!
          final WQTimeserieProxy proxy = (WQTimeserieProxy) item.getAdapter( IObservation.class );
          if( proxy == null )
            continue;

          myChildren.add( item );
        }
        else if( adapter instanceof SimpleObservation )
        {
          final SimpleObservation obs = (SimpleObservation) adapter;
          final IAxis[] axisList = obs.getAxisList();
          for( final IAxis axis : axisList )
          {
            final String type = axis.getType();
            if( itemIsOfBoundaryNodeType( type ) )
            {
              myChildren.add( item );
              continue;
            }
          }
        }
        else
          throw new NotImplementedException();
      }
      else if( object instanceof IRepositoryItem )
      {
        final IRepositoryItem item = (IRepositoryItem) object;
        final String name = item.getName();

        if( name.startsWith( "." ) )
          continue;
        else if( name.equals( "CVS" ) )
          continue;

        myChildren.add( object );
      }
      else
        throw new NotImplementedException();

    return myChildren.toArray();
  }

  private boolean itemIsOfBoundaryNodeType( final String type )
  {
    final BOUNDARY_TYPE bnt = m_settings.getBoundaryNodeType();

    //$ANALYSIS-IGNORE
	if( bnt.toZmlString().equals( type ) )
	      return true;

    return false;
  }
}
