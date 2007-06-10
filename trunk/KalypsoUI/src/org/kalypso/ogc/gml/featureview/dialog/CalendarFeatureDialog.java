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

// import java.text.DateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.xml.DateWithoutTime;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;
import org.kalypsodeegree.model.feature.Feature;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * @author belger
 */
public class CalendarFeatureDialog implements IFeatureDialog
{
  private FeatureChange m_change = null;

  private final Feature m_feature;

  private final IValuePropertyType m_ftp;

  public CalendarFeatureDialog( final Feature feature, final IValuePropertyType ftp )
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
      if( m_ftp.getValueClass() == DateWithoutTime.class )
        newDate = new DateWithoutTime( newDate );
      final GregorianCalendar cal = new GregorianCalendar();
      cal.setTime( newDate );
      m_change = new FeatureChange( m_feature, m_ftp, new XMLGregorianCalendarImpl( cal ) );
    }

    return open;
  }

  private Date getDate( )
  {
    // If no calue is set, use today as default
    final XMLGregorianCalendar calendar = getCalendar();
    if( calendar == null )
      return new Date();
    
    return DateUtilities.toDate( calendar );
  }

  private XMLGregorianCalendar getCalendar( )
  {
    return (XMLGregorianCalendar) m_feature.getProperty( m_ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection<FeatureChange> c )
  {
    if( c != null && m_change != null )
      c.add( m_change );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel( )
  {
    // final Date date = getDate();
    // if( date == null )
    // return "";
    //    
    // return m_dateFormat.format( date );
    return "..."; //$NON-NLS-1$
  }
}