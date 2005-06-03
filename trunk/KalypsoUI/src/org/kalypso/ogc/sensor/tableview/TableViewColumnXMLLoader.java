/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.tableview;

import java.net.URL;
import java.util.Iterator;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.template.IObsProvider;
import org.kalypso.ogc.sensor.template.NameUtils;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.template.PooledObsProvider;
import org.kalypso.template.obstableview.TypeColumn;
import org.kalypso.template.obstableview.TypeObservation;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.PoolableObjectWaiter;

/**
 * A TableViewColumnXMLLoader wraps an IObservation. It can provide a TableViewColumn for
 * each value axis of the underlying observation.
 * 
 * @author schlienger
 */
public class TableViewColumnXMLLoader extends PoolableObjectWaiter
{
  /**
   * @param synchron Falls true, wird die Obersvation sofort geladen und im gleichen thread objectLoaded ausgeführt 
   */
  public TableViewColumnXMLLoader( final TableView view, final TypeObservation xmlObs, final URL context, final boolean synchron )
  {
    super( new PoolableObjectType( xmlObs.getLinktype(), xmlObs.getHref(), context ), new Object[] { view, xmlObs }, synchron );
  }

  /**
   * @see org.kalypso.util.pool.PoolableObjectWaiter#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  protected void objectLoaded( final IPoolableObjectType key, final Object newValue )
  {
    final IObservation obs = (IObservation)newValue;

    final IAxis keyAxis = ObservationUtilities.findAxesByKey( obs.getAxisList() )[0];

    final TypeObservation xmlObs = (TypeObservation)m_data[1];
    final TableView m_view = (TableView)m_data[0];
    
    for( final Iterator itCols = xmlObs.getColumn().iterator(); itCols.hasNext(); )
    {
      final TypeColumn tcol = (TypeColumn)itCols.next();

      final IAxis valueAxis = ObservationUtilities.findAxisByName( obs.getAxisList(), tcol
          .getAxis() );

      final String colName = tcol.getName() != null ? tcol.getName() : tcol.getAxis();
      final String name = NameUtils.replaceTokens( colName, obs, valueAxis );
      
      final IObsProvider provider = key == null ? (IObsProvider)new PlainObsProvider( obs, null ) : new PooledObsProvider( key, null );
      final TableViewColumn column = new TableViewColumn( m_view, provider, name, tcol.isEditable(), tcol
          .getWidth(), keyAxis, valueAxis );

      m_view.addItem( column );
    }
  }
}