package org.kalypso.ogc.gml.featureview.dialog;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.gml.schema.DateWithoutTime;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

/**
 * @author belger
 */
public class CalendarFeatureDialog implements IFeatureDialog
{
  private DateFormat m_dateFormat = new SimpleDateFormat( "dd.MM.yyyy" );

  private FeatureChange m_change = null;

  private final Feature m_feature;

  private final FeatureTypeProperty m_ftp;

  public CalendarFeatureDialog( final Feature feature, final FeatureTypeProperty ftp )
  {
    m_feature = feature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( final Shell shell )
  {
    final SWTCalendarDialog dialog = new SWTCalendarDialog( shell, getDate() );

    final int open = dialog.open();
    if( open == Window.OK )
    {
      Date newDate = dialog.getDate();
      if( m_ftp.getType().equals( DateWithoutTime.class.getName() ) )
        newDate = new DateWithoutTime( newDate );

      m_change = new FeatureChange( m_feature, m_ftp.getName(), newDate );
    }
    
    return open;
  }

  private Date getDate()
  {
    if( m_change != null )
      return (Date)m_change.newValue;
    
    return (Date)m_feature.getProperty( m_ftp.getName() );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection c )
  {
    if( c != null && m_change != null )
      c.add( m_change );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel()
  {
    final Date date = getDate();

    return m_dateFormat.format( date );
  }
}