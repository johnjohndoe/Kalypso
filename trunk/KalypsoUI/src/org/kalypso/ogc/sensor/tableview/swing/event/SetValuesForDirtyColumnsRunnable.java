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
package org.kalypso.ogc.sensor.tableview.swing.event;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;

/**
 * SetValuesForDirtyColumns
 * 
 * @author schlienger
 */
public class SetValuesForDirtyColumnsRunnable implements IRunnableWithProgress
{
  private final ITableViewTemplate m_template;

  private final ObservationTableModel m_model;

  /**
   * Constructor
   * 
   * @param template
   * @param model
   */
  public SetValuesForDirtyColumnsRunnable( final ITableViewTemplate template,
      final ObservationTableModel model )
  {
    m_template = template;
    m_model = model;
  }

  /**
   * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void run( final IProgressMonitor monitor )
      throws InvocationTargetException
  {
    final Collection themes = m_template.getThemes();
    
    // this map is used to store the modified observations and their values
    // in order to perform the calls to IObservation.setValues() after
    // having proceeded the themes using the iterator.
    // No doing so leads to ConcurrentModificationExceptions...
    final Map obsAndValues = new HashMap();
    
    monitor.beginTask( "Zeitreihen speichern", themes.size() + 1 );
    
    for( final Iterator it = themes.iterator(); it.hasNext(); )
    {
      try
      {
        final ITableViewTheme theme = (ITableViewTheme) it.next();

        boolean dirty = false;

        for( final Iterator itcol = theme.getColumns().iterator(); itcol
            .hasNext(); )
        {
          final ITableViewColumn column = (ITableViewColumn) itcol.next();
          dirty = column.isDirty();

          // at least one col dirty?
          if( dirty )
            break;
        }

        final IObservation obs = theme.getObservation();

        if( dirty /*&& obs instanceof ZmlObservation*/ )
        {
          for( final Iterator itcol = theme.getColumns().iterator(); itcol
              .hasNext(); )
            ((ITableViewColumn) itcol.next()).setDirty( false );

          // TODO wird hier eine achse ignoriert ?
          final ITuppleModel values = m_model.getValues( theme );
          
          obsAndValues.put( obs, values );
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        throw new InvocationTargetException( e );
      }

      monitor.worked( 1 );
    }
    
    // iterate over the modified observations and save the values
    for( final Iterator it = obsAndValues.keySet().iterator(); it.hasNext(); )
    {
      final IObservation obs = (IObservation) it.next();
      final ITuppleModel values = (ITuppleModel) obsAndValues.get( obs );
      
      try
      {
        obs.setValues( values );
      }
      catch( SensorException e )
      {
        e.printStackTrace();
      }
    }
    
    monitor.worked(1);
    obsAndValues.clear();
    
    monitor.done();
  }
}