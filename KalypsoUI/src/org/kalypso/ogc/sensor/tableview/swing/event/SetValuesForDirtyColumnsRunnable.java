package org.kalypso.ogc.sensor.tableview.swing.event;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.Iterator;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
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
  public void run( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    final Collection themes = m_template.getThemes();

    monitor.beginTask( "Zeitreihen speichern", themes.size() );

    for( final Iterator it = themes.iterator(); it.hasNext(); )
    {
      try
      {
        final ITableViewTheme theme = (ITableViewTheme) it.next();

        boolean dirty = false;

        for( final Iterator itcol = theme.getColumns().iterator(); itcol
            .hasNext(); )
        {
          dirty = ((ITableViewColumn) itcol.next()).isDirty();

          // at least one col dirty?
          if( dirty )
            break;
        }

        final IObservation obs = theme.getObservation();

        if( dirty && obs instanceof ZmlObservation )
        {
          for( final Iterator itcol = theme.getColumns().iterator(); itcol
              .hasNext(); )
            ((ITableViewColumn) itcol.next()).setDirty( false );

          final ITuppleModel values = m_model.getValues( theme.getColumns() );
          obs.setValues( values );
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        throw new InvocationTargetException( e );
      }

      monitor.worked( 1 );
    }

    monitor.done();
  }
}