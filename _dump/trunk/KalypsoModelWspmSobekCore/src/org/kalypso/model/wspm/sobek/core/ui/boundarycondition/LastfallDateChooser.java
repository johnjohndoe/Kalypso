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

import java.text.DateFormat;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.kalypso.model.wspm.sobek.core.Messages;
/**
 * @author kuch
 */
public class LastfallDateChooser
{
  private final Set<Runnable> m_listener = new HashSet<Runnable>();

  protected GregorianCalendar m_calendar;

  private Text m_text;

  public LastfallDateChooser( final GregorianCalendar calendar )
  {
    m_calendar = calendar;
  }

  public void addModifyListener( final Runnable runnable )
  {
    m_listener.add( runnable );
  }

  public void draw( final Composite container, final GridData gridData )
  {
    final Composite composite = new Composite( container, SWT.NONE );
    final GridLayout layout = new GridLayout( 2, false );
    layout.marginWidth = layout.horizontalSpacing = 0;
    composite.setLayout( layout );
    composite.setLayoutData( gridData );

    m_text = new Text( composite, SWT.BORDER | SWT.READ_ONLY );
    m_text.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    if( m_calendar != null )
    {
      final DateFormat df = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.MEDIUM );
      final String date = df.format( m_calendar.getTime() );
      m_text.setText( date );
    }

    final Button button = new Button( composite, SWT.NONE );
    button.setText( Messages.LastfallDateChooser_0 );

    button.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {

        final DateTimeDialog dialog = new DateTimeDialog( button.getShell() );
        if( m_calendar != null )
          dialog.setDateTime( m_calendar );

        final int returnCode = dialog.open();

        if( returnCode == Window.OK )
        {
          m_calendar = dialog.getDateTime();
          final DateFormat df = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.MEDIUM );
          m_text.setText( df.format( m_calendar.getTime() ) );
        }

        processListener();

      }
    } );
  }

  public GregorianCalendar getDateTime( )
  {
    return m_calendar;
  }

  protected void processListener( )
  {
    for( final Runnable listener : m_listener )
      listener.run();
  }
}
