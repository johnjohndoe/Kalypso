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
import org.kalypso.ogc.sensor.zml.ZmlObservation;

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

          final ITuppleModel values = m_model.getValues( theme.getColumns() );
          
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