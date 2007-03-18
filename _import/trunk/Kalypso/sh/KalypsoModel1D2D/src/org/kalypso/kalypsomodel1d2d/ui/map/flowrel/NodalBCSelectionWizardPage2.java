/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
@SuppressWarnings("unused")
public class NodalBCSelectionWizardPage2 extends WizardPage
{
  private static final DateFormat DATETIMEFORMAT = new SimpleDateFormat( "dd.MM.yyyy HH:mm" );

  private static final DateFormat DATEFORMAT = new SimpleDateFormat( "dd.MM.yyyy 00:00" );

  private static final String DEFAULTSTEP = "60";

  private Text m_dateTimeFrom;

  private Text m_dateTimeTo;

  private Text m_dateTimeStep;

  private Text m_defaultValue;

  protected NodalBCSelectionWizardPage2( )
  {
    super( "Super title" );
    setTitle( "Zeitreihendefinition" );
    setDescription( "Auf dieser Seite kann die gewünschte Zeitreihe definiert werden." );
    setPageComplete( false );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    final GridData gridBeginning = new GridData( SWT.BEGINNING, SWT.CENTER, false, false );
    final GridData gridEnd = new GridData( SWT.END, SWT.CENTER, false, false );
    final GridData gridFillHorizontal = new GridData( SWT.FILL, SWT.CENTER, true, false );

    final Label label_1 = new Label( container, SWT.NONE );
    label_1.setLayoutData( gridBeginning );
    label_1.setText( "Von:" );

    m_dateTimeFrom = new Text( container, SWT.BORDER );
    m_dateTimeFrom.addModifyListener( new ModifyListener()
    {
      @SuppressWarnings("synthetic-access")
      public void modifyText( ModifyEvent e )
      {
        updatePageComplete();
      }
    } );
    m_dateTimeFrom.setText( DATEFORMAT.format( new Date() ) );
    m_dateTimeFrom.setLayoutData( gridFillHorizontal );

    final Button buttonDateTimeFrom = new Button( container, SWT.NONE );
    // buttonDateTimeFrom.addSelectionListener( new SelectionAdapter()
    // {
    // public void widgetSelected( SelectionEvent e )
    // {
    // browseForSourceFile();
    // }
    // } );
    buttonDateTimeFrom.setText( "..." );
    buttonDateTimeFrom.setEnabled( false );

    final Label label_2 = new Label( container, SWT.NONE );
    label_2.setLayoutData( gridBeginning );
    label_2.setText( "Bis:" );

    m_dateTimeTo = new Text( container, SWT.BORDER );
    m_dateTimeTo.addModifyListener( new ModifyListener()
    {
      @SuppressWarnings("synthetic-access")
      public void modifyText( ModifyEvent e )
      {
        updatePageComplete();
      }
    } );
    m_dateTimeTo.setText( DATETIMEFORMAT.format( new Date() ) );
    m_dateTimeTo.setLayoutData( gridFillHorizontal );

    final Button buttonDateTimeTo = new Button( container, SWT.NONE );
    // buttonDateTimeTo.addSelectionListener( new SelectionAdapter()
    // {
    // public void widgetSelected( SelectionEvent e )
    // {
    // browseForSourceFile();
    // }
    // } );
    buttonDateTimeTo.setText( "..." );
    buttonDateTimeTo.setEnabled( false );

    final Label label_3 = new Label( container, SWT.NONE );
    label_3.setLayoutData( gridBeginning );
    label_3.setText( "Schritt [min]:" );

    m_dateTimeStep = new Text( container, SWT.BORDER );
    m_dateTimeStep.addModifyListener( new ModifyListener()
    {
      @SuppressWarnings("synthetic-access")
      public void modifyText( ModifyEvent e )
      {
        updatePageComplete();
      }
    } );
    m_dateTimeStep.setText( DEFAULTSTEP );
    m_dateTimeStep.setLayoutData( gridFillHorizontal );
    final Label emptylabel_1 = new Label( container, SWT.NONE );
    emptylabel_1.setLayoutData( gridEnd );
    emptylabel_1.setText( "" );

    final Label label_4 = new Label( container, SWT.NONE );
    label_4.setLayoutData( gridBeginning );
    label_4.setText( "Standardwert:" );

    m_defaultValue = new Text( container, SWT.BORDER );
    m_defaultValue.addModifyListener( new ModifyListener()
    {
      @SuppressWarnings("synthetic-access")
      public void modifyText( ModifyEvent e )
      {
        updatePageComplete();
      }
    } );
    m_defaultValue.setText( "20.0" );
    m_defaultValue.setLayoutData( gridFillHorizontal );
    final Label emptylabel_2 = new Label( container, SWT.NONE );
    emptylabel_2.setLayoutData( gridEnd );
    emptylabel_2.setText( "" );
    
    parent.pack();
    parent.layout();
  }

  private void updatePageComplete( )
  {
    if( isCurrentPage() )
      setPageComplete( checkValues() );
    else
      setPageComplete( true );
  }

  private boolean checkValues( )
  {
    try
    {
      final Date fromDate = DATETIMEFORMAT.parse( m_dateTimeFrom.getText() );
      final Date toDate = DATETIMEFORMAT.parse( m_dateTimeTo.getText() );
      final Date stepDate = DATETIMEFORMAT.parse( m_dateTimeStep.getText() );
      final double value = Double.parseDouble( m_defaultValue.getText() );

      if( fromDate.after( toDate ) )
        return false;
    }
    catch( ParseException e )
    {
      return false;
    }
    catch( NumberFormatException e )
    {
      return false;
    }
    return true;
  }

  private Date getDateTime( Text text )
  {
    try
    {
      return DATETIMEFORMAT.parse( text.getText() );
    }
    catch( ParseException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public Date getFromDate( )
  {
    return getDateTime( m_dateTimeFrom );
  }

  public Date getToDate( )
  {
    return getDateTime( m_dateTimeTo );
  }

//  public Date getStep( )
//  {
//    return getDateTime( m_dateTimeStep );
//  }

  public double getDefaultValue( )
  {
    return Double.parseDouble( m_defaultValue.getText() );
  }
  
  public int getStep() {
    return Integer.parseInt( m_dateTimeStep.getText() );
  }
}
