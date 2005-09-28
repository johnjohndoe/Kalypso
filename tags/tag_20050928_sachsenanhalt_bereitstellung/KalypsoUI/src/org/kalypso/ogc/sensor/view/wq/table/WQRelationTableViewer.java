/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor.view.wq.table;

import java.awt.Frame;

import javax.swing.BorderFactory;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

import org.eclipse.compare.internal.AbstractViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;

/**
 * @author schlienger
 */
public class WQRelationTableViewer extends AbstractViewer
{
  private Composite m_composite;
  private Combo m_combo;
  private WQTable[] m_tables;
  private JTable m_table;

  public WQRelationTableViewer( final Composite parent )
  {
    createControl( parent );
  }

  private final void createControl( final Composite parent )
  {
    m_composite = new Composite( parent, SWT.FILL );
    m_composite.setLayout( new GridLayout() );
    m_composite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Combo combo = new Combo( m_composite, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_combo = combo;
    m_combo.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    m_combo.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        comboSelected( combo );
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
      // empty
      }
    } );

    // SWT-AWT Brücke für die Darstellung von JTable
    final Composite embCmp = new Composite( m_composite, SWT.RIGHT | SWT.EMBEDDED );
    embCmp.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    final Frame vFrame = SWT_AWT.new_Frame( embCmp );

    m_table = new JTable();
    vFrame.setVisible( true );
    m_table.setVisible( true );

    final JScrollPane pane = new JScrollPane( m_table );
    pane.setBorder( BorderFactory.createEmptyBorder() );
    vFrame.add( pane );
  }

  protected void comboSelected( final Combo combo )
  {
    final WQTable table = m_tables[combo.getSelectionIndex()];
    m_table.setModel( WQRelationFactory.createTableModel( table ) );
  }

  public Control getControl()
  {
    return m_composite;
  }

  public void setInput( final WQTableSet wqs )
  {
    m_table.setModel( new DefaultTableModel() );

    if( wqs == null )
      return;

    m_tables = wqs.getTables();
    for( int i = 0; i < m_tables.length; i++ )
      m_combo.add( m_tables[i].toString() );

    if( m_tables.length > 0 )
    {
      m_combo.select( 0 );

      m_table.setModel( WQRelationFactory.createTableModel( m_tables[0] ) );
    }
  }
}
