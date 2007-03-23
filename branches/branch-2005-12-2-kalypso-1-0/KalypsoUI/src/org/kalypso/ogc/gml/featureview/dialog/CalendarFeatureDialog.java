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
package org.kalypso.ogc.gml.featureview.dialog;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree_impl.gml.schema.DateWithoutTime;

/**
 * @author belger
 */
public class CalendarFeatureDialog implements IFeatureDialog
{
  private DateFormat m_dateFormat = new SimpleDateFormat( "dd.MM.yyyy" );

  private FeatureChange m_change = null;

  private final Feature m_feature;

  private final FeatureTypeProperty m_ftp;

  private static final String STR_NOT_SET = "<nicht gesetzt>";

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
    final Date date = getDate();
    final Date dialogDate = date == null ?  new Date() : date;
    final SWTCalendarDialog dialog = new SWTCalendarDialog( shell, dialogDate );

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
      return (Date)m_change.getNewValue();

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
    if( date == null )
      return STR_NOT_SET;

    return m_dateFormat.format( date );
  }
}