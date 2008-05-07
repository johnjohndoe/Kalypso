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
import java.util.List;
import java.util.NoSuchElementException;
import java.util.logging.Logger;

import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.template.IObsProvider;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.template.PooledObsProvider;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.template.obstableview.TypeColumn;
import org.kalypso.template.obstableview.TypeObservation;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.PoolableObjectWaiter;

/**
 * A TableViewColumnXMLLoader wraps an IObservation. It can provide a TableViewColumn for each value axis of the
 * underlying observation.
 * 
 * @author schlienger
 */
public class TableViewColumnXMLLoader extends PoolableObjectWaiter
{
  /** the position of the column in the template is used to order the columns in the table */
  private final int m_columnPosition;

  /**
   * @param synchron
   *          Falls true, wird die Obersvation sofort geladen und im gleichen thread objectLoaded ausgeführt
   * @param columnPosition
   *          the position of the column in the template is used to order the columns in the table
   */
  public TableViewColumnXMLLoader( final TableView view, final TypeObservation xmlObs, final URL context,
      final boolean synchron, final int columnPosition )
  {
    super( new PoolableObjectType( xmlObs.getLinktype(), xmlObs.getHref(), context ), new Object[]
    { view, xmlObs }, synchron );

    m_columnPosition = columnPosition;
  }

  /**
   * @see org.kalypso.util.pool.PoolableObjectWaiter#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  @Override
  protected void objectLoaded( final IPoolableObjectType key, final Object newValue )
  {
    final IObservation obs = (IObservation)newValue;

    final IAxis[] keyAxes = ObservationUtilities.findAxesByKey( obs.getAxisList() );
    if( keyAxes.length == 0 )
      throw new IllegalStateException( Messages.getString("org.kalypso.ogc.sensor.tableview.TableViewColumnXMLLoader.0") ); //$NON-NLS-1$

    final IAxis keyAxis = keyAxes[0];

    final TypeObservation xmlObs = (TypeObservation)m_data[1];
    final TableView m_view = (TableView)m_data[0];

    final List ignoreTypeList = m_view.getIgnoreTypesAsList();

    for( final Iterator itCols = xmlObs.getColumn().iterator(); itCols.hasNext(); )
    {
      final TypeColumn tcol = (TypeColumn)itCols.next();

      try
      {
        final IAxis valueAxis = ObservationUtilities.findAxisByNameThenByType( obs.getAxisList(), tcol.getAxis() );

        if( !ignoreTypeList.contains( valueAxis.getType() ) )
        {
          final String colName = tcol.getName() != null ? tcol.getName() : tcol.getAxis();
          final String name = ObsViewUtils.replaceTokens( colName, obs, valueAxis );
          final String format = tcol.getFormat() != null ? tcol.getFormat() : TimeserieUtils
              .getDefaultFormatString( valueAxis.getType() );

          final IObsProvider provider = isSynchron() ? (IObsProvider)new PlainObsProvider( obs, null )
              : new PooledObsProvider( key, null );
          final TableViewColumn column = new TableViewColumn( m_view, provider, name, tcol.isEditable(), tcol
              .getWidth(), keyAxis, valueAxis, format, m_columnPosition );

          m_view.addItem( column );
        }
      }
      catch( final NoSuchElementException e )
      {
        Logger.getLogger( getClass().getName() ).warning(
            Messages.getString("org.kalypso.ogc.sensor.tableview.TableViewColumnXMLLoader.1") + e.getLocalizedMessage() ); //$NON-NLS-1$
      }
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
   */
  public void dirtyChanged( final IPoolableObjectType key, final boolean isDirty )
  {
    // TODO Auto-generated method stub
  }
}