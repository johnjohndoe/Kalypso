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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.template.AbstractObservationTheme;
import org.kalypso.template.obstableview.TypeColumn;
import org.kalypso.template.obstableview.TypeObservation;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * A TableViewTheme wraps an IObservation. It can provide a TableViewColumn
 * for each value axis of the underlying observation.
 * 
 * @author schlienger
 */
public class TableViewTheme extends AbstractObservationTheme
{
  /** list of columns */
  private final List m_columns = new ArrayList();

  /** the base xml if defined (can be null) */
  private final TypeObservation m_xmlObs;

  private final TableViewTemplate m_template;

  private boolean m_editableColumns = true;

  public TableViewTheme( TableViewTemplate template, String name,
      IVariableArguments args )
  {
    super( name, args );

    m_template = template;
    m_xmlObs = null;
  }

  public TableViewTheme( TableViewTemplate template, String name,
      TypeObservation xmlObs )
  {
    super( name, null );

    m_template = template;
    m_xmlObs = xmlObs;
  }
  
  private void addColumn( final TableViewColumn column )
  {
    m_columns.add( column );
  }

  public void dispose( )
  {
    super.dispose();

    m_columns.clear();
  }

  /**
   * @param editableColumns
   */
  public void setEditableColumns( boolean editableColumns )
  {
    m_editableColumns = editableColumns;
  }

  /**
   * @see org.kalypso.ogc.sensor.template.AbstractObservationTheme#afterObservationSet()
   */
  protected void afterObservationSet( )
  {
    synchronized( m_columns )
    {
      // reset columns (clearing them will force refresh once getColumns is
      // called)
      m_columns.clear();
    }
  }

  /**
   * Build the columns as defined in the xml template (if existing)
   */
  private void buildXmlColumns( )
  {
    final IObservation obs = getObservation();

    if( obs != null && m_xmlObs != null )
    {
      final IAxis keyAxis = ObservationUtilities.findAxisByKey( obs
          .getAxisList() )[0];

      for( Iterator itCols = m_xmlObs.getColumn().iterator(); itCols.hasNext(); )
      {
        final TypeColumn tcol = (TypeColumn) itCols.next();

        final IAxis valueAxis = ObservationUtilities.findAxisByName( obs
            .getAxisList(), tcol.getAxis() );

        final String colName = tcol.getName() != null ? tcol.getName() : tcol.getAxis();
        
        final TableViewColumn column = new TableViewColumn( colName,
            tcol.isEditable(), tcol.getWidth(), keyAxis, valueAxis, this,
            m_template );

        addColumn( column );
      }
    }
  }

  /**
   * Build the columns according to the observation, doing our best
   */
  private void buildDefaultColumns( )
  {
    final IObservation obs = getObservation();

    if( obs != null && isUseDefault() )
    {
      final IAxis[] axes = obs.getAxisList();

      // do not even continue if there are no axes
      if( axes == null || axes.length == 0 )
        return;

      // actually just the first key axis is relevant in our case
      final IAxis[] keyAxes = ObservationUtilities.findAxisByKey( axes );

      // do not continue if no key axis
      if( keyAxes.length == 0 )
        return;

      for( int i = 0; i < axes.length; i++ )
      {
        // ignore axis if it is a kalypso status axis
        if( !KalypsoStatusUtils.isStatusAxis( axes[i] )
            && !axes[i].equals( keyAxes[0] ) )
        {
          final IAxis valueAxis = ObservationUtilities.findAxisByName( axes,
              axes[i].getName() );

          if( !valueAxis.getType().equals( getIgnoreType() ) )
          {
            final TableViewColumn col = new TableViewColumn( createCurveName( getName(), obs, valueAxis ),
                m_editableColumns, 50, keyAxes[0], valueAxis, this, m_template );

            addColumn( col );
          }
        }
      }
    }
  }

  /**
   * @return all columns
   */
  public List getColumns( )
  {
    // TODO Marc: hier besser Array von columns zurückgeben (wgwn thread-safety)
    // und alle Methoden nach m_columns (oder egal was) synchronisieren
    
    synchronized( m_columns )
    {
      // still empty columns? look if we got some
      if( m_columns.size() == 0 )
      {
        buildXmlColumns();
        buildDefaultColumns();
      }

      return m_columns;
    }
  }
}