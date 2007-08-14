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
package org.kalypso.ogc.sensor.view.wizard.cvssheet;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeSet;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.jface.viewers.FCVArrayDelegate;
import org.kalypso.contribs.eclipse.jface.viewers.FCVSimpleDateDelegate;
import org.kalypso.contribs.eclipse.jface.viewers.FacadeComboViewer;
import org.kalypso.ogc.sensor.view.wizard.cvssheet.CsvSheetImportDataModel.CSV_COLUMN_SEPERATORS;
import org.kalypso.ogc.sensor.view.wizard.cvssheet.CsvSheetImportDataModel.DECIMAL_NUMBER_SEPERATORS;
import org.kalypso.ogc.sensor.view.wizard.cvssheet.CsvSheetImportDataModel.TSM_KEY;
import org.kalypso.ogc.sensor.view.wizard.cvssheet.CsvSheetImportDataModel.WQ_KIND;

/**
 * @author kuch
 */
public class PageCSVImportCSVSetup extends WizardPage
{

  // TODO groupboxes common label width - one vertical alignment!

  protected final CsvSheetImportDataModel m_model;

  public PageCSVImportCSVSetup( final CsvSheetImportDataModel model )
  {
    super( "pageCsvImportCsvSetup" );
    m_model = model;

    setTitle( "CSV File Setup" );
    setDescription( "Please specifiy locations of csv files and common settings of these csv files (like seperators, aso)." );
  }

  protected void checkPage( )
  {
    final WQ_KIND kind = (WQ_KIND) m_model.getValue( TSM_KEY.eCsvTimeSeriesIsWorQ );
    if( kind == null )
    {
      setMessage( null );
      setErrorMessage( "Kind of import entry missing." );
      setPageComplete( false );

      return;
    }

    final File fTsm = (File) m_model.getValue( TSM_KEY.eCsvTimeSeriesFile );
    if( fTsm == null )
    {
      setMessage( null );
      setErrorMessage( "Time Series CSV file is missing." );
      setPageComplete( false );

      return;
    }

    if( !fTsm.exists() )
    {
      setMessage( null );
      setErrorMessage( "Time Series CSV file does not exists." );
      setPageComplete( false );

      return;
    }

    final CSV_COLUMN_SEPERATORS cSep = (CSV_COLUMN_SEPERATORS) m_model.getValue( TSM_KEY.eCsvColumnSeperator );
    if( cSep == null )
    {
      setMessage( null );
      setErrorMessage( "Column seperator entry is missing." );
      setPageComplete( false );

      return;
    }

    final DECIMAL_NUMBER_SEPERATORS dSep = (DECIMAL_NUMBER_SEPERATORS) m_model.getValue( TSM_KEY.eCsvDecimalNumberSeperator );
    if( dSep == null )
    {
      setMessage( null );
      setErrorMessage( "Decimal seperator entry is missing." );
      setPageComplete( false );

      return;
    }

    final SimpleDateFormat dateFormat = (SimpleDateFormat) m_model.getValue( TSM_KEY.eCsvDateFormat );
    if( dateFormat == null )
    {
      setMessage( null );
      setErrorMessage( "Date format entry is missing." );
      setPageComplete( false );

      return;
    }

    final String timeZoneId = (String) m_model.getValue( TSM_KEY.eCsvTimeZone );
    if( timeZoneId == null )
    {
      setMessage( null );
      setErrorMessage( "Time zone entry is missing." );
      setPageComplete( false );

      return;
    }

    setMessage( null );
    setErrorMessage( null );
    setPageComplete( true );

  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout( 2, false ) );
    container.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    setControl( container );

    /* WQ Kind */
    final Label lWQKind = new Label( container, SWT.NONE );
    lWQKind.setText( "Import W or Q?" );

    final WQ_KIND[] wqInput = new WQ_KIND[] { WQ_KIND.eW, WQ_KIND.eQ };
    final FacadeComboViewer wWq = new FacadeComboViewer( new FCVArrayDelegate( wqInput ) );
    wWq.draw( container, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    wWq.addSelectionChangedListener( new Runnable()
    {
      public void run( )
      {
        updateWQKind( (IStructuredSelection) wWq.getSelection() );

        checkPage();
      }
    } );

    updateWQKind( (IStructuredSelection) wWq.getSelection() );

    final Group grFiles = new Group( container, SWT.NONE );
    grFiles.setLayout( new GridLayout( 3, false ) );
    grFiles.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ) );
    grFiles.setText( "CSV data files" );

    /* File Time Series */
    final Label lFileTimeSeries = new Label( grFiles, SWT.NONE );
    lFileTimeSeries.setText( "Time Series CSV file" );

    final Text tFileTimeSeries = new Text( grFiles, SWT.READ_ONLY | SWT.BORDER );
    tFileTimeSeries.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final Button bFileTimeSeries = new Button( grFiles, SWT.NONE );
    bFileTimeSeries.setText( "..." );

    bFileTimeSeries.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final FileDialog fileDialog = new FileDialog( bFileTimeSeries.getShell() );
        fileDialog.setFilterNames( new String[] { "CSV-Sheet" } );
        fileDialog.setFilterExtensions( new String[] { "*.csv" } );

        tFileTimeSeries.setText( fileDialog.open() );

        if( tFileTimeSeries.getText() != null )
          m_model.setValue( TSM_KEY.eCsvTimeSeriesFile, new File( tFileTimeSeries.getText() ) );

        checkPage();
      }
    } );

    /* File w/q */
    final Label lFileWqConnection = new Label( grFiles, SWT.NONE );
    lFileWqConnection.setText( "W/Q Connection" );

    final Text tFileWqConnection = new Text( grFiles, SWT.READ_ONLY | SWT.BORDER );
    tFileWqConnection.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final Button bFileWqConnection = new Button( grFiles, SWT.NONE );
    bFileWqConnection.setText( "..." );

    bFileWqConnection.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final FileDialog fileDialog = new FileDialog( bFileTimeSeries.getShell() );
        fileDialog.setFilterNames( new String[] { "CSV-Sheet" } );
        fileDialog.setFilterExtensions( new String[] { "*.csv" } );

        tFileWqConnection.setText( fileDialog.open() );

        if( tFileWqConnection.getText() != null )
          m_model.setValue( TSM_KEY.eCsvTimeSeriesWqConnectionFile, new File( tFileWqConnection.getText() ) );

        checkPage();
      }
    } );

    /* first row is w / q */
    final Label lWQFirstColumn = new Label( grFiles, SWT.NONE );
    lWQFirstColumn.setText( "First connection sheet column is?" );

    final WQ_KIND[] wqFirstColumnInput = new WQ_KIND[] { WQ_KIND.eW, WQ_KIND.eQ };
    final FacadeComboViewer wFC = new FacadeComboViewer( new FCVArrayDelegate( wqFirstColumnInput ) );
    wFC.draw( grFiles, new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ), SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    wFC.addSelectionChangedListener( new Runnable()
    {
      public void run( )
      {
        updateWQFirstColumn( (IStructuredSelection) wFC.getSelection() );

        checkPage();
      }

    } );

    updateWQFirstColumn( (IStructuredSelection) wFC.getSelection() );

    final Group grSeperators = new Group( container, SWT.NONE );
    grSeperators.setLayout( new GridLayout( 2, false ) );
    grSeperators.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ) );
    grSeperators.setText( "CSV seperators and data formats" );

    /* cvs column seperator */
    final Label lColumnSeperator = new Label( grSeperators, SWT.NONE );
    lColumnSeperator.setText( "CSV column seperator" );

    final CSV_COLUMN_SEPERATORS[] columnSeperators = new CSV_COLUMN_SEPERATORS[] { CSV_COLUMN_SEPERATORS.eSemicolon, CSV_COLUMN_SEPERATORS.eTab, CSV_COLUMN_SEPERATORS.eSpace };
    final FacadeComboViewer wColumnSeperators = new FacadeComboViewer( new FCVArrayDelegate( columnSeperators ) );
    wColumnSeperators.draw( grSeperators, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    wColumnSeperators.addSelectionChangedListener( new Runnable()
    {
      public void run( )
      {
        updateColumnSeperator( (IStructuredSelection) wColumnSeperators.getSelection() );

        checkPage();
      }
    } );

    updateColumnSeperator( (IStructuredSelection) wColumnSeperators.getSelection() );

    /* decimal seperator */
    final Label lDecimalSeperator = new Label( grSeperators, SWT.NONE );
    lDecimalSeperator.setText( "Decimal Number seperator" );

    final DECIMAL_NUMBER_SEPERATORS[] decimalSeperators = new DECIMAL_NUMBER_SEPERATORS[] { DECIMAL_NUMBER_SEPERATORS.ePoint, DECIMAL_NUMBER_SEPERATORS.eComma };
    final FacadeComboViewer wDecimalSeperators = new FacadeComboViewer( new FCVArrayDelegate( decimalSeperators ) );
    wDecimalSeperators.draw( grSeperators, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    wDecimalSeperators.addSelectionChangedListener( new Runnable()
    {
      public void run( )
      {
        updateDecimalSeperator( (IStructuredSelection) wDecimalSeperators.getSelection() );

        checkPage();
      }
    } );

    updateDecimalSeperator( (IStructuredSelection) wDecimalSeperators.getSelection() );

    /* date format */
    final Label lDateFormat = new Label( grSeperators, SWT.NONE );
    lDateFormat.setText( "Date format" );

    final List<SimpleDateFormat> dates = new ArrayList<SimpleDateFormat>();
    dates.add( new SimpleDateFormat( "yyyy-MM-dd HH:mm" ) );
    dates.add( new SimpleDateFormat( "dd.MM.yyyy HH:mm" ) );

    final FacadeComboViewer wDateFormats = new FacadeComboViewer( new FCVSimpleDateDelegate( dates.toArray( new SimpleDateFormat[] {} ) ) );
    wDateFormats.draw( grSeperators, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    wDateFormats.addSelectionChangedListener( new Runnable()
    {
      public void run( )
      {
        updateTimeFormat( (IStructuredSelection) wDateFormats.getSelection() );

        checkPage();
      }
    } );

    updateTimeFormat( (IStructuredSelection) wDateFormats.getSelection() );

    /* time zones */
    final Label lTimeZones = new Label( grSeperators, SWT.NONE );
    lTimeZones.setText( "Time Zone" );

    final Set<String> timeZones = new TreeSet<String>();
    final String[] tz = TimeZone.getAvailableIDs();
    for( final String z : tz )
      if( z.contains( "Europe/" ) )
        timeZones.add( z );

    final FacadeComboViewer wTimeZones = new FacadeComboViewer( new FCVArrayDelegate( timeZones.toArray( new String[] {} ) ) );
    wTimeZones.draw( grSeperators, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE );
    wTimeZones.addSelectionChangedListener( new Runnable()
    {
      public void run( )
      {
        updateTimeZone( (IStructuredSelection) wTimeZones.getSelection() );

        checkPage();
      }
    } );

    updateTimeZone( (IStructuredSelection) wTimeZones.getSelection() );

    checkPage();
  }

  protected void updateColumnSeperator( final IStructuredSelection selection )
  {
    final Object element = selection.getFirstElement();
    m_model.setValue( TSM_KEY.eCsvColumnSeperator, element );
  }

  protected void updateDecimalSeperator( final IStructuredSelection selection )
  {
    final Object element = selection.getFirstElement();
    m_model.setValue( TSM_KEY.eCsvDecimalNumberSeperator, element );
  }

  protected void updateTimeFormat( final IStructuredSelection selection )
  {
    final Object element = selection.getFirstElement();
    m_model.setValue( TSM_KEY.eCsvDateFormat, element );

  }

  protected void updateTimeZone( final IStructuredSelection selection )
  {
    final Object element = selection.getFirstElement();
    m_model.setValue( TSM_KEY.eCsvTimeZone, element );
  }

  protected void updateWQKind( final IStructuredSelection selection )
  {
    final Object element = selection.getFirstElement();
    m_model.setValue( TSM_KEY.eCsvTimeSeriesIsWorQ, element );
  }

  protected void updateWQFirstColumn( final IStructuredSelection selection )
  {
    final Object element = selection.getFirstElement();
    m_model.setValue( TSM_KEY.eCsvWqConnectionFirstColumn, element );
  }
}
