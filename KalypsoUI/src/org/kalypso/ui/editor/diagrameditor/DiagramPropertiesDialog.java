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

package org.kalypso.ui.editor.diagrameditor;

import java.util.Arrays;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * @author schlienger
 */
public class DiagramPropertiesDialog extends TitleAreaDialog
{
  protected String m_diagTitle;

  protected boolean m_showLegend;

  protected String m_legendTitle;

  protected String m_tz;

  public DiagramPropertiesDialog( final Shell parentShell, final String diagTitle, final boolean showLegend, final String legendTitle, final String timezoneName )
  {
    super( parentShell );

    m_diagTitle = diagTitle;
    m_showLegend = showLegend;
    m_legendTitle = legendTitle;
    m_tz = timezoneName;
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( Composite parent )
  {
    setTitle( "Zeitreihen-Diagramm-Eigenschaften" );

    final Composite cmp = new Composite( parent, SWT.FILL );
    cmp.setLayout( new GridLayout( 2, false ) );
    cmp.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Label lblTitle = new Label( cmp, SWT.LEFT );
    lblTitle.setText( "Diagramm-Titel" );
    final Text txtTitle = new Text( cmp, SWT.BORDER );
    txtTitle.setText( m_diagTitle );
    txtTitle.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    txtTitle.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        m_diagTitle = txtTitle.getText();
      }
    } );

    final Label lblShowLegend = new Label( cmp, SWT.LEFT );
    lblShowLegend.setText( "Ledende anzeigen?" );
    final Button btnShowLegend = new Button( cmp, SWT.CHECK );
    btnShowLegend.setSelection( m_showLegend );
    btnShowLegend.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        m_showLegend = btnShowLegend.getSelection();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        // empty
      }
    } );

    final Label lblLegTitle = new Label( cmp, SWT.LEFT );
    lblLegTitle.setText( "Legende-Titel" );
    final Text txtLegTitle = new Text( cmp, SWT.BORDER );
    txtLegTitle.setText( m_legendTitle );
    txtLegTitle.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    txtLegTitle.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        m_legendTitle = txtLegTitle.getText();
      }
    } );

    final Label lblTz = new Label( cmp, SWT.LEFT );
    lblTz.setText( "Zeitzone" );
    final Combo cmbTz = new Combo( cmp, SWT.DROP_DOWN );
    cmbTz.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    
    final String[] tz = TimeZone.getAvailableIDs();
    Arrays.sort( tz );
    cmbTz.setItems( tz );
    
    cmbTz.setText( m_tz );
    
    cmbTz.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        m_tz = cmbTz.getText();
      }
    } );
    
    return cmp;
  }

  public String getDiagramTitle( )
  {
    return m_diagTitle;
  }

  public boolean isShowLegend( )
  {
    return m_showLegend;
  }

  public String getLegendTitle( )
  {
    return m_legendTitle;
  }
  
  public String getTimezoneName( )
  {
    return m_tz;
  }
}
