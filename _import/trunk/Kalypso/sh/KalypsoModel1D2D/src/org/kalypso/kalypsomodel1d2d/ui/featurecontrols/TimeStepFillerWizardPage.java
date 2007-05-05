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
package org.kalypso.kalypsomodel1d2d.ui.featurecontrols;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

/**
 * @author Madanagopal
 *
 */
public class TimeStepFillerWizardPage extends WizardPage
{

  private Text m_dateFromTxt;
  private Text m_dateToTxt;
  private Text timeStep_Text;
  private int timeStep_val;
  protected Date m_dateFrom = new Date();
  protected Date m_dateTo = new Date();
  private Text _underRelaxationFactor_Text;
  protected int _underRelaxFactor_val;
  private Text _Q_Value_Text;
  protected int _Q_val;
  private static final DateFormat DATE_FORMAT = new SimpleDateFormat("dd.MM.yyyy HH:mm");

  protected TimeStepFillerWizardPage( )
  {
    super("Example");
    // TODO Auto-generated constructor stub
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    Composite container = new Composite(parent, SWT.NULL);
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout(gridLayout);
    setControl(container);

    
    GridData data = new GridData();
    Label vonLbl = new Label(container, SWT.NONE);
    
    data.horizontalAlignment = GridData.FILL;
    data.horizontalSpan = 1;
    data.grabExcessHorizontalSpace = true;
    vonLbl.setText( "Von:" );
    vonLbl.setAlignment( SWT.RIGHT);
    vonLbl.setLayoutData( data );
    
    m_dateFromTxt = new Text( container, SWT.BORDER );
    data = new GridData ();
    data.horizontalSpan = 1;
    data.horizontalAlignment = GridData.FILL;
    m_dateFromTxt.setLayoutData (data);
    //m_dateFromTxt.setEnabled( false );
    m_dateFromTxt.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        try
        {
          m_dateFrom = DATE_FORMAT.parse( m_dateFromTxt.getText());
      //    dateIntervalChanged();
          setPageComplete( true );
        }
        catch( ParseException e1 )
        {
          //m_diagView.removeAllItems();
          setPageComplete( false );
        }
        getWizard().getContainer().updateButtons();
      }
    } );

    final Button dateFromBtn = new Button( container, SWT.PUSH );
    data = new GridData();
    dateFromBtn.setText( "..." );
    //dateFromBtn.setEnabled( false );
    dateFromBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        final SWTCalendarDialog calendarDialog = new SWTCalendarDialog( getShell(), m_dateFrom );
        if( calendarDialog.open() == Window.OK )
        {
          m_dateFrom = calendarDialog.getDate();
          m_dateFromTxt.setText( DATE_FORMAT.format( m_dateFrom) );
        }
      }
    } );    

    Label bisLbl = new Label(container, SWT.NONE);
    data = new GridData ();
    data.horizontalAlignment = GridData.FILL;
    data.horizontalSpan = 1;
    data.grabExcessHorizontalSpace = true;
    bisLbl.setText( "Bis:" );
    bisLbl.setAlignment( SWT.RIGHT );
    bisLbl.setLayoutData( data );
    
    m_dateToTxt = new Text( container, SWT.BORDER );
    data = new GridData ();
    data.horizontalAlignment = GridData.FILL;
    data.horizontalSpan = 1;
    m_dateToTxt.setLayoutData (data);
    //m_dateToTxt.setEnabled( false );
    m_dateToTxt.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        try
        {
          m_dateTo = DATE_FORMAT.parse( m_dateToTxt.getText());
          setPageComplete( true );
        }
        catch( ParseException e1 )
        {
          setPageComplete( false );
        }
        getWizard().getContainer().updateButtons();
      }
    } );

    final Button dateToBtn = new Button( container, SWT.PUSH );
    dateToBtn.setText( "..." );
    dateToBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        final SWTCalendarDialog calendarDialog = new SWTCalendarDialog( getShell(), m_dateTo );
        
        if( calendarDialog.open() == Window.OK )
        {
          m_dateTo = calendarDialog.getDate();
          m_dateToTxt.setText( DATE_FORMAT.format( m_dateTo) );          
        }  
      }
    } );
    
    Label timeStepLbl = new Label(container, SWT.NONE);
    data = new GridData();
    data.horizontalSpan = 1;    
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    timeStepLbl.setText( "Time Steps:" );
    timeStepLbl.setAlignment( SWT.RIGHT );
    timeStepLbl.setLayoutData( data );
    
    timeStep_Text = new Text( container, SWT.BORDER );
    data = new GridData ();
    data.horizontalSpan = 2;
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;

    timeStep_Text.setLayoutData (data);
    timeStep_Text.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        timeStep_val = Integer.parseInt( timeStep_Text.getText());
      }
    } );

    Label _underRelaxationFactor = new Label(container, SWT.NONE);
    data = new GridData();
    data.horizontalSpan = 1;    
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    _underRelaxationFactor.setText( "Under Relaxation Factor:" );
    _underRelaxationFactor.setAlignment( SWT.RIGHT );
    _underRelaxationFactor.setLayoutData( data );
    
    _underRelaxationFactor_Text = new Text( container, SWT.BORDER );
    data = new GridData ();
    data.horizontalSpan = 2;
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    _underRelaxationFactor_Text.setLayoutData (data);
    _underRelaxationFactor_Text.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        _underRelaxFactor_val = Integer.parseInt( _underRelaxationFactor_Text.getText());
      }
    } );
    
//    Label _Q_Lbl = new Label(container, SWT.NONE);
//    data = new GridData();
//    data.horizontalSpan = 1;    
//    data.horizontalAlignment = GridData.FILL;
//    data.grabExcessHorizontalSpace = true;
//    _Q_Lbl.setText( "Q Value:" );
//    _Q_Lbl.setAlignment( SWT.RIGHT );
//    _Q_Lbl.setLayoutData( data );
//    
//    _Q_Value_Text = new Text( container, SWT.BORDER );
//    data = new GridData ();
//    data.horizontalSpan = 2;
//    data.horizontalAlignment = GridData.FILL;
//    data.grabExcessHorizontalSpace = true;
//    _Q_Value_Text.setLayoutData (data);
//    _Q_Value_Text.addModifyListener( new ModifyListener()
//    {
//      public void modifyText( ModifyEvent e )
//      {
//        _Q_val = Integer.parseInt( _Q_Value_Text.getText());
//      }
//    } );
    container.layout();
    setPageComplete( true );
    
  }

  public void init( IStructuredSelection initialSelection )
  {
    // TODO Auto-generated method stub
    
  }
  public Date getStartDate() {
    
    return m_dateFrom;
  }
  
  public Date getFinishDate() {
    return m_dateTo;
  }
  
  public int getTimeSteps() {
    return timeStep_val;
  }
  
  public int getUnderRelaxationFactorValue() {
    return _underRelaxFactor_val;
  }
  
//  public int getQValue() {
//    return _Q_val;
//  }
  

  
}
